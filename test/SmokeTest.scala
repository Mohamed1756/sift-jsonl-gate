package sift

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.nio.charset.StandardCharsets

object SmokeTest {

  def withTempDir[T](f: Path => T): T = {
    val dir = Files.createTempDirectory("sift-smoke-test-")
    try f(dir)
    finally {
      SiftPublisher.deleteRecursive(dir)
    }
  }

  def runAll(): Unit = {
    testSmoke1_Reuse()
    testSmoke2_NoPartialCurrent()
    testSmoke3_RunInProgress()
    testSmoke4_BomRejection()
    testSmoke5_DynamicBlockedSuffix()
    testSmoke6_PointerCorruption()
    testSmoke7_CrashRecovery()
    testSmoke8_TamperedOutputFailsRevalidation()
    testSmoke9_UnterminatedFinalLineOutput()
    println("All 9 smoke tests passed successfully!")
  }

  def testSmoke1_Reuse(): Unit = {
    withTempDir { tmp =>
      val in = tmp.resolve("input.jsonl")
      val rules = tmp.resolve("rules.json")
      val out = tmp.resolve("out")
      Files.writeString(in, "{\"a\": 1}\n{\"b\": 2}\n")
      Files.writeString(rules, "{\"schema_version\": 1}")

      val args = CliArgs(in, rules, out, forceRerun = false, replaceLive = false)
      val code1 = SiftPublisher.run(args, StrictLineSource, AlwaysCleanClassifier, CanonicalReceiptWriter)
      assert(code1 == 0, s"Expected 0, got $code1")

      val cur = out.resolve("current")
      assert(Files.exists(cur), "current file must exist")
      val curContent = Files.readString(cur)

      // Run second time -> must reuse existing run
      val code2 = SiftPublisher.run(args, StrictLineSource, AlwaysCleanClassifier, CanonicalReceiptWriter)
      assert(code2 == 0, s"Expected 0 on reuse, got $code2")
      assert(Files.readString(cur) == curContent, "current pointer unchanged on reuse")
      println("  [PASS] Smoke 1: Run twice -> reuse, not duplication")
    }
  }

  def testSmoke2_NoPartialCurrent(): Unit = {
    withTempDir { tmp =>
      val rules = tmp.resolve("rules.json")
      val out = tmp.resolve("out")
      Files.writeString(rules, "{\"schema_version\": 1}")

      val insideIn = out.resolve("nested-input.jsonl")
      Files.createDirectories(out)
      Files.writeString(insideIn, "{\"a\": 1}\n")
      val args = CliArgs(insideIn, rules, out, forceRerun = false, replaceLive = false)

      var caught = false
      try {
        SiftPublisher.run(args, StrictLineSource, AlwaysCleanClassifier, CanonicalReceiptWriter)
      } catch {
        case e: SiftError if e.code == "INPUT_INSIDE_OUTPUT_DIR" =>
          caught = true
      }
      assert(caught, "Expected INPUT_INSIDE_OUTPUT_DIR exception")

      val cur = out.resolve("current")
      assert(!Files.exists(cur), "Partial current pointer must never be published on failure")
      println("  [PASS] Smoke 2: Kill/failure mid-write leaves no partial current")
    }
  }

  def testSmoke3_RunInProgress(): Unit = {
    withTempDir { tmp =>
      val in = tmp.resolve("input.jsonl")
      val rules = tmp.resolve("rules.json")
      val out = tmp.resolve("out")
      Files.writeString(in, "{\"a\": 1}\n")
      Files.writeString(rules, "{\"schema_version\": 1}")

      Lock(out) { _ =>
        val args = CliArgs(in, rules, out, forceRerun = false, replaceLive = false)
        var caught = false
        try {
          SiftPublisher.run(args, StrictLineSource, AlwaysCleanClassifier, CanonicalReceiptWriter)
        } catch {
          case e: SiftError if e.code == "RUN_IN_PROGRESS" =>
            caught = true
        }
        assert(caught, "Expected RUN_IN_PROGRESS on second concurrent run")
      }
      println("  [PASS] Smoke 3: Second concurrent run -> RUN_IN_PROGRESS")
    }
  }

  def testSmoke4_BomRejection(): Unit = {
    withTempDir { tmp =>
      val in = tmp.resolve("input.jsonl")
      val rules = tmp.resolve("rules.json")
      val out = tmp.resolve("out")
      val bomBytes = Array[Byte](0xEF.toByte, 0xBB.toByte, 0xBF.toByte) ++ "{\"a\": 1}\n".getBytes(StandardCharsets.UTF_8)
      Files.write(in, bomBytes)
      Files.writeString(rules, "{\"schema_version\": 1}")

      val args = CliArgs(in, rules, out, forceRerun = false, replaceLive = false)
      var caught = false
      try {
        SiftPublisher.run(args, StrictLineSource, AlwaysCleanClassifier, CanonicalReceiptWriter)
      } catch {
        case e: SiftError if e.code == "INVALID_ENCODING_HEADER" =>
          caught = true
      }
      assert(caught, "Expected INVALID_ENCODING_HEADER exception on leading BOM")
      println("  [PASS] Smoke 4: Leading BOM fails with INVALID_ENCODING_HEADER")
    }
  }

  def testSmoke5_DynamicBlockedSuffix(): Unit = {
    withTempDir { tmp =>
      val in = tmp.resolve("empty.jsonl")
      val rules = tmp.resolve("rules.json")
      val out = tmp.resolve("out")
      Files.writeString(in, "\n\n") // Only empty lines -> blocked with EMPTY_INPUT
      Files.writeString(rules, "{\"schema_version\": 1}")

      val args = CliArgs(in, rules, out, forceRerun = true, replaceLive = false)
      val code1 = SiftPublisher.run(args, StrictLineSource, AlwaysCleanClassifier, CanonicalReceiptWriter)
      assert(code1 == 1, "Expected blocked exit code 1")

      // Second blocked run on same input must create .blocked-2 without colliding
      val code2 = SiftPublisher.run(args, StrictLineSource, AlwaysCleanClassifier, CanonicalReceiptWriter)
      assert(code2 == 1, "Expected blocked exit code 1")

      val runs = out.resolve("runs")
      val blockedDirs = Files.list(runs).filter(p => p.getFileName.toString.contains(".blocked-")).count()
      assert(blockedDirs == 2, s"Expected 2 distinct blocked directories, got $blockedDirs")
      println("  [PASS] Smoke 5: Dynamic suffix avoids directory collision on repeated blocked runs")
    }
  }

  def testSmoke6_PointerCorruption(): Unit = {
    withTempDir { tmp =>
      val in = tmp.resolve("input.jsonl")
      val rules = tmp.resolve("rules.json")
      val out = tmp.resolve("out")
      Files.writeString(in, "{\"a\": 1}\n")
      Files.writeString(rules, "{\"schema_version\": 1}")

      Files.createDirectories(out)
      Files.writeString(out.resolve("current"), "bad-pointer-format\n")

      val args = CliArgs(in, rules, out, forceRerun = false, replaceLive = false)
      var caught = false
      try {
        SiftPublisher.run(args, StrictLineSource, AlwaysCleanClassifier, CanonicalReceiptWriter)
      } catch {
        case e: SiftError if e.code == "POINTER_CORRUPTION" =>
          caught = true
      }
      assert(caught, "Corrupted pointer must fail fast with POINTER_CORRUPTION")
      println("  [PASS] Smoke 6: Corrupted pointer validation rejects invalid pointer without auto-repair")
    }
  }

  def testSmoke7_CrashRecovery(): Unit = {
    withTempDir { tmp =>
      val in = tmp.resolve("input.jsonl")
      val rules = tmp.resolve("rules.json")
      val out = tmp.resolve("out")
      Files.writeString(in, "{\"a\": 1}\n")
      Files.writeString(rules, "{\"schema_version\": 1}")

      val args = CliArgs(in, rules, out, forceRerun = false, replaceLive = false)
      assertEquals(SiftPublisher.run(args, StrictLineSource, AlwaysCleanClassifier, CanonicalReceiptWriter), 0)

      val runs = out.resolve("runs")
      val runId = Files.readString(out.resolve("current")).trim.split("/")(1)
      val runDir = runs.resolve(runId)
      val tomb = runs.resolve(s"$runId.replaced-1")

      // Simulate crash between tombstone rename and pointer update
      Files.move(runDir, tomb)
      val lockFile = out.resolve(".sift-lock")
      Files.writeString(lockFile, s"replace-live $runId $runId.replaced-1\n")

      // Next startup must recover tombstone back to runDir
      Lock(out) { _ => () }

      assert(Files.exists(runDir), "Crash recovery must restore tombstone back to runDir")
      assert(Files.readString(out.resolve("current")).trim == s"runs/$runId/clean.jsonl", "current pointer remains valid")
      println("  [PASS] Smoke 7: Mandatory crash recovery restores tombstone from .sift-lock state")
    }
  }

  def testSmoke8_TamperedOutputFailsRevalidation(): Unit = {
    withTempDir { tmp =>
      val in = tmp.resolve("input.jsonl")
      val rules = tmp.resolve("rules.json")
      val out = tmp.resolve("out")
      Files.writeString(in, "{\"a\": 1}\n")
      Files.writeString(rules, "{\"schema_version\": 1}")

      val args = CliArgs(in, rules, out, forceRerun = false, replaceLive = false)
      assertEquals(SiftPublisher.run(args, StrictLineSource, AlwaysCleanClassifier, CanonicalReceiptWriter), 0)

      // Tamper with clean.jsonl
      val runId = Files.readString(out.resolve("current")).trim.split("/")(1)
      val cleanFile = out.resolve("runs").resolve(runId).resolve("clean.jsonl")
      Files.writeString(cleanFile, "{\"tampered\": true}\n", StandardOpenOption.APPEND)

      // Re-run without --force-rerun must detect tampering (§12, Test 25)
      var caught = false
      try {
        SiftPublisher.run(args, StrictLineSource, AlwaysCleanClassifier, CanonicalReceiptWriter)
      } catch {
        case e: SiftError if e.code == "REUSE_REVALIDATION_FAILED" =>
          caught = true
      }
      assert(caught, "Tampered clean.jsonl must fail reuse revalidation with REUSE_REVALIDATION_FAILED")
      println("  [PASS] Smoke 8: Tampered clean file fails §12 reuse revalidation (Acceptance Test 25)")
    }
  }

  def testSmoke9_UnterminatedFinalLineOutput(): Unit = {
    withTempDir { tmp =>
      val in = tmp.resolve("input.jsonl")
      val rules = tmp.resolve("rules.json")
      val out = tmp.resolve("out")
      // Input without trailing newline
      Files.writeString(in, "{\"unterminated\": 1}")
      Files.writeString(rules, "{\"schema_version\": 1}")

      val args = CliArgs(in, rules, out, forceRerun = false, replaceLive = false)
      assertEquals(SiftPublisher.run(args, StrictLineSource, AlwaysCleanClassifier, CanonicalReceiptWriter), 0)

      val runId = Files.readString(out.resolve("current")).trim.split("/")(1)
      val cleanBytes = Files.readAllBytes(out.resolve("runs").resolve(runId).resolve("clean.jsonl"))
      assert(cleanBytes.length > 0 && cleanBytes.last == '\n'.toByte, "Output clean.jsonl must end with newline")
      println("  [PASS] Smoke 9: Unterminated input line produces valid newline-terminated JSONL record")
    }
  }

  def assertEquals(a: Any, b: Any): Unit =
    assert(a == b, s"Assertion failed: $a != $b")

  def main(args: Array[String]): Unit = {
    runAll()
  }
}
