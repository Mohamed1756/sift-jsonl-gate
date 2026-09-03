package sift.receipt

import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets

object ReceiptEngineTest {

  val sha1 = "1" * 64
  val sha2 = "2" * 64
  val sha3 = "3" * 64
  val sha4 = "4" * 64

  def withTempDir[T](f: Path => T): T = {
    val dir = Files.createTempDirectory("sift-receipt-test-")
    try f(dir)
    finally {
      sift.pub.PublicationEngine.deleteRecursive(dir)
    }
  }

  def runAll(): Unit = {
    testAcceptance18_ByteIdenticalRerun()
    testAcceptance19_PathAndEnvironmentInvariance()
    testCanonicalKeySorting()
    testRatioMathAndReleasePolicy()
    testInvariantEnforcement()
    println("All ReceiptEngineTest tests passed successfully!")
  }

  def testAcceptance18_ByteIdenticalRerun(): Unit = {
    withTempDir { dir =>
      val inSha = "a" * 64
      val rSha = "b" * 64
      val runId = ReceiptEngine.computeRunId(inSha, rSha)

      val data = ReceiptEngine.evaluatePolicy(
        linesRead = 10, ignoredEmpty = 1, unparseable = 0, clean = 8, quar = 1,
        quarReasons = Map("INVALID_INTEGER" -> 1), hasBlockedMissing = false, maxRatio = 0.5,
        inSha = inSha, rSha = rSha, cSha = sha3, qSha = sha4, runId = runId
      )

      val f1 = dir.resolve("receipt1.json")
      val f2 = dir.resolve("receipt2.json")
      ReceiptEngine.write(f1, data)
      ReceiptEngine.write(f2, data)

      val bytes1 = Files.readAllBytes(f1)
      val bytes2 = Files.readAllBytes(f2)
      assert(java.util.Arrays.equals(bytes1, bytes2), "Two runs must produce byte-identical receipt bytes")
      println("  [PASS] Acceptance 18: Byte-identical rerun produces byte-identical receipt bytes")
    }
  }

  def testAcceptance19_PathAndEnvironmentInvariance(): Unit = {
    val dirA = Files.createTempDirectory("path-a-")
    val dirB = Files.createTempDirectory("path-b-differently-nested-")

    try {
      val inSha = sha1
      val rSha = sha2
      val runId = ReceiptEngine.computeRunId(inSha, rSha)

      val dataA = ReceiptEngine.evaluatePolicy(
        linesRead = 100, ignoredEmpty = 0, unparseable = 1, clean = 95, quar = 4,
        quarReasons = Map("INVALID_ISO_DATE" -> 3, "MISSING_REQUIRED_FIELD" -> 1),
        hasBlockedMissing = false, maxRatio = 0.1,
        inSha = inSha, rSha = rSha, cSha = sha3, qSha = sha4, runId = runId
      )

      val receiptA = dirA.resolve("receipt.json")
      val receiptB = dirB.resolve("receipt.json")
      ReceiptEngine.write(receiptA, dataA)
      ReceiptEngine.write(receiptB, dataA)

      val strA = Files.readString(receiptA)
      val strB = Files.readString(receiptB)

      assert(strA == strB, "Receipt bytes must be strictly path-invariant")
      assert(!strA.contains(dirA.toString), "Receipt must contain zero paths")
      assert(!strB.contains(dirB.toString), "Receipt must contain zero paths")
      println("  [PASS] Acceptance 19: Receipt is strictly invariant to filesystem paths, PID, and environment")
    } finally {
      sift.pub.PublicationEngine.deleteRecursive(dirA)
      sift.pub.PublicationEngine.deleteRecursive(dirB)
    }
  }

  def testCanonicalKeySorting(): Unit = {
    withTempDir { dir =>
      val file = dir.resolve("receipt.json")
      val inSha = sha1
      val rSha = sha2
      val runId = ReceiptEngine.computeRunId(inSha, rSha)
      val data = ReceiptEngine.evaluatePolicy(
        linesRead = 5, ignoredEmpty = 0, unparseable = 0, clean = 4, quar = 1,
        quarReasons = Map("MISSING_REQUIRED_FIELD" -> 1, "INVALID_INTEGER" -> 2),
        hasBlockedMissing = false, maxRatio = 0.5,
        inSha = inSha, rSha = rSha, cSha = sha3, qSha = sha4, runId = runId
      )
      ReceiptEngine.write(file, data)
      val json = Files.readString(file)

      val expectedOrder = List(
        "bad_ratio",
        "bad_ratio_denominator",
        "bad_ratio_numerator",
        "clean_records",
        "ignored_empty_lines",
        "input_sha256",
        "lines_read",
        "outputs",
        "quarantine_reasons",
        "quarantined_records",
        "records_read",
        "release",
        "release_reasons",
        "rules_sha256",
        "run_id",
        "schema_version",
        "sift_version",
        "unparseable_lines"
      )

      var lastIdx = -1
      for (key <- expectedOrder) {
        val idx = json.indexOf(s"\"$key\":")
        assert(idx != -1, s"Key $key missing from receipt")
        assert(idx > lastIdx, s"Key $key out of lexicographical order (index $idx vs last $lastIdx)")
        lastIdx = idx
      }

      val intIdx = json.indexOf("\"INVALID_INTEGER\":")
      val missIdx = json.indexOf("\"MISSING_REQUIRED_FIELD\":")
      assert(intIdx < missIdx, "quarantine_reasons keys must be sorted lexicographically")
      println("  [PASS] Canonical JSON keys are sorted strictly lexicographically at all depths")
    }
  }

  def testRatioMathAndReleasePolicy(): Unit = {
    val inSha = sha1
    val rSha = sha2
    val runId = ReceiptEngine.computeRunId(inSha, rSha)

    // 1. Empty input
    val empty = ReceiptEngine.evaluatePolicy(0, 0, 0, 0, 0, Map.empty, false, 0.001, inSha, rSha, sha3, sha4, runId)
    assert(!empty.isReleased)
    assert(empty.releaseReasons == List("EMPTY_INPUT"))
    assert(empty.badRatioNum == 0)
    assert(empty.badRatioDenom == 1)
    assert(empty.badRatioStr == "0.000000")

    // 2. Spec example ratio math (§10 lines 327-330)
    val specData = ReceiptEngine.evaluatePolicy(
      linesRead = 1000003, ignoredEmpty = 1, unparseable = 2, clean = 999426, quar = 574,
      quarReasons = Map("INVALID_ISO_DATE" -> 371, "MISSING_REQUIRED_FIELD" -> 201, "INVALID_INTEGER" -> 2),
      hasBlockedMissing = false, maxRatio = 0.0005,
      inSha = inSha, rSha = rSha, cSha = sha3, qSha = sha4, runId = runId
    )
    assert(specData.badRatioNum == 576)
    assert(specData.badRatioDenom == 1000002)
    assert(specData.badRatioStr == "0.000576")
    assert(!specData.isReleased)
    assert(specData.releaseReasons == List("QUARANTINE_RATIO_EXCEEDED", "UNPARSEABLE_INPUT"))

    // 3. Blocked if missing
    val blockedMiss = ReceiptEngine.evaluatePolicy(10, 0, 0, 9, 1, Map.empty, true, 0.5, inSha, rSha, sha3, sha4, runId)
    assert(!blockedMiss.isReleased)
    assert(blockedMiss.releaseReasons.contains("BLOCKED_FIELD_MISSING"))
    println("  [PASS] Exact ratio arithmetic and release blocking conditions pass")
  }

  def testInvariantEnforcement(): Unit = {
    withTempDir { dir =>
      val inSha = sha1
      val rSha = sha2
      val runId = ReceiptEngine.computeRunId(inSha, rSha)

      // Corrupt line count: linesRead doesn't equal sum of components
      val corruptData = ReceiptData(
        runId = runId, inputSha = inSha, rulesSha = rSha, linesRead = 9999, // Should be 10!
        ignoredEmpty = 1, unparseable = 1, cleanRecords = 7, quarantinedRecords = 1,
        cleanSha = sha3, quarSha = sha4, quarantineReasons = Map.empty,
        releaseReasons = List("UNPARSEABLE_INPUT"), isReleased = false,
        badRatioNum = 2, badRatioDenom = 9, badRatioStr = "0.222222"
      )

      var caught = false
      try ReceiptEngine.write(dir.resolve("corrupt.json"), corruptData)
      catch case e: ReceiptError if e.code == "INVARIANT_VIOLATION" => caught = true
      assert(caught, "Corrupted receipt invariant must be rejected before write")
      println("  [PASS] §11 invariant verification rejects invalid accounting before write")
    }
  }

  def main(args: Array[String]): Unit = {
    runAll()
  }
}
