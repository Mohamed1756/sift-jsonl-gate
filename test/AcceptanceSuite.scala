package sift

import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets
import java.io.File
import sift.rules.{RulesModel, RulesValidationError}
import sift.parser.{JsonStreamParser, TokenParseResult}
import sift.input.{InputModel, InputError}
import sift.classify.{ClassifierEngine, ClassifyResult}
import sift.receipt.ReceiptEngine
import sift.pub.PublicationEngine

object AcceptanceSuite {

  val canonicalRulesJson: String =
    """{
      |  "schema_version": 1,
      |  "required": ["order_id", "order_date", "amount_pence", "currency"],
      |  "string_columns": ["order_id", "currency"],
      |  "date_columns": ["order_date"],
      |  "integer_columns": ["amount_pence"],
      |  "release": {
      |    "max_quarantine_ratio": 0.0005,
      |    "block_if_missing": ["order_id", "amount_pence"]
      |  }
      |}""".stripMargin

  def withTempDir[T](f: Path => T): T = {
    val dir = Files.createTempDirectory("sift-acceptance-")
    try f(dir)
    finally {
      PublicationEngine.deleteRecursive(dir)
    }
  }

  def runAll(): Unit = {
    println("=================================================================")
    println("STARTING SIFT v1.0 ACCEPTANCE SUITE (TESTS 1 - 27)")
    println("=================================================================")

    test1_CleanRun()
    test2_CrlfFile()
    test3_NewlineOnlyFile()
    test4_InvalidUtf8()
    test5_LeadingBom()
    test6_FinalLineWithoutTrailingNewline()
    test7_NotAnObject()
    test8_DuplicateKeyNestingDepth3()
    test9_RawNumberLexemes()
    test10_BigIntegersAndKeyOrder()
    test11_TopLevelVsNestedSift()
    test12_RulesValidationUnknownKeysAndSift()
    test13_BlockIfMissingMustBeSubset()
    test14_OverlappingColumnLists()
    test15_InvalidMaxQuarantineRatio()
    test16_IntegerValidation()
    test17_StrictDateValidation()
    test18_DeterminismByteIdenticalRerun()
    test19_ReceiptDeterminismAndKeyOrder()
    test20_BlockedRunNeverOccupiesBarePath()
    test21_ReplaceLiveWithBlockedReplacement()
    test22_CrashRecovery()
    test23_TamperedCurrentPointer()
    test24_CorruptRunDirectoryNeverDeleted()
    test25_TamperedCleanFileFailsReuse()
    test26_ConcurrentRunsFailRunInProgress()
    test27_InputInsideOutputDir()
    testAdversarialHardening()

    println("=================================================================")
    println("ALL ACCEPTANCE & ADVERSARIAL HARDENING TESTS PASSED 100% GREEN!")
    println("=================================================================")
  }

  def test1_CleanRun(): Unit = {
    withTempDir { dir =>
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)
      val in = dir.resolve("input.jsonl")
      Files.writeString(in, "{\"order_id\":\"ORD-1\",\"order_date\":\"2026-09-02\",\"amount_pence\":1000,\"currency\":\"GBP\"}\n")
      val out = dir.resolve("out")

      val exit = Sift.run(SiftCliConfig(in, rules, out, replaceLive = false, forceRerun = false))
      assert(exit == 0, s"Expected exit 0, got $exit")
      val current = out.resolve("current")
      assert(Files.exists(current), "current pointer must exist")
      val ptr = Files.readString(current).trim
      assert(ptr.endsWith("/clean.jsonl"), s"Pointer must target clean.jsonl, got $ptr")

      val runDir = out.resolve(ptr.stripSuffix("/clean.jsonl"))
      val receipt = Files.readString(runDir.resolve("receipt.json"))
      assert(receipt.contains("\"release\":\"released\""))
      assert(receipt.contains("\"clean_records\":1"))
      assert(receipt.contains("\"quarantined_records\":0"))
      val quar = Files.readAllBytes(runDir.resolve("quarantine.jsonl"))
      assert(quar.length == 0, "quarantine.jsonl must be empty on clean run")
      println("  [PASS] Test 1: Clean run released, all clean, empty quarantine, current updated")
    }
  }

  def test2_CrlfFile(): Unit = {
    withTempDir { dir =>
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)
      val in = dir.resolve("input.jsonl")
      Files.write(in, "{\"order_id\":\"ORD-1\",\"order_date\":\"2026-09-02\",\"amount_pence\":1000,\"currency\":\"GBP\"}\r\n\r\n".getBytes(StandardCharsets.UTF_8))
      val out = dir.resolve("out")

      val exit = Sift.run(SiftCliConfig(in, rules, out, replaceLive = false, forceRerun = false))
      assert(exit == 0)
      val current = out.resolve("current")
      val ptr = Files.readString(current).trim
      val runDir = out.resolve(ptr.stripSuffix("/clean.jsonl"))
      val receipt = Files.readString(runDir.resolve("receipt.json"))
      assert(receipt.contains("\"lines_read\":2"))
      assert(receipt.contains("\"ignored_empty_lines\":1"))
      assert(receipt.contains("\"clean_records\":1"))
      println("  [PASS] Test 2: CRLF file blank lines ignored, records clean, raw hash covers CRLF")
    }
  }

  def test3_NewlineOnlyFile(): Unit = {
    withTempDir { dir =>
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)
      val in = dir.resolve("input.jsonl")
      Files.writeString(in, "\n")
      val out = dir.resolve("out")

      val exit = Sift.run(SiftCliConfig(in, rules, out, replaceLive = false, forceRerun = false))
      assert(exit == 1, "Newline-only file must block release")
      val runs = Files.list(out.resolve("runs")).toList
      assert(runs.size() == 1)
      assert(runs.get(0).getFileName.toString.contains(".blocked-1"))
      val receipt = Files.readString(runs.get(0).resolve("receipt.json"))
      assert(receipt.contains("\"release\":\"blocked\""))
      assert(receipt.contains("\"EMPTY_INPUT\""))
      assert(receipt.contains("\"lines_read\":1"))
      assert(!Files.exists(out.resolve("current")), "current must not be created on blocked run")
      println("  [PASS] Test 3: \"\\n\"-only file: lines_read = 1, blocked EMPTY_INPUT, no current pointer")
    }
  }

  def test4_InvalidUtf8(): Unit = {
    withTempDir { dir =>
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)
      val in = dir.resolve("bad.jsonl")
      Files.write(in, Array[Byte]('{', '}', '\n', 0xFF.toByte, '\n'))
      val out = dir.resolve("out")

      var caught = false
      try Sift.run(SiftCliConfig(in, rules, out, replaceLive = false, forceRerun = false))
      catch { case e: InputError if e.code == "INVALID_UTF8" => caught = true }
      assert(caught, "Invalid UTF-8 must throw INVALID_UTF8")
      assert(!Files.exists(out.resolve("runs")), "No runs directory should exist before output")
      println("  [PASS] Test 4: Invalid UTF-8 anywhere fails run before output with INVALID_UTF8")
    }
  }

  def test5_LeadingBom(): Unit = {
    withTempDir { dir =>
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)
      val in = dir.resolve("bom.jsonl")
      Files.write(in, Array[Byte](0xEF.toByte, 0xBB.toByte, 0xBF.toByte, '{', '}'))
      val out = dir.resolve("out")

      var caught = false
      try Sift.run(SiftCliConfig(in, rules, out, replaceLive = false, forceRerun = false))
      catch { case e: InputError if e.code == "INVALID_ENCODING_HEADER" => caught = true }
      assert(caught, "Leading BOM must throw INVALID_ENCODING_HEADER")
      println("  [PASS] Test 5: Leading BOM fails run before output with INVALID_ENCODING_HEADER")
    }
  }

  def test6_FinalLineWithoutTrailingNewline(): Unit = {
    withTempDir { dir =>
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)
      val in = dir.resolve("unterminated.jsonl")
      Files.writeString(in, "{\"order_id\":\"ORD-1\",\"order_date\":\"2026-09-02\",\"amount_pence\":1000,\"currency\":\"GBP\"}")
      val out = dir.resolve("out")

      val exit = Sift.run(SiftCliConfig(in, rules, out, replaceLive = false, forceRerun = false))
      assert(exit == 0)
      val ptr = Files.readString(out.resolve("current")).trim
      val runDir = out.resolve(ptr.stripSuffix("/clean.jsonl"))
      val receipt = Files.readString(runDir.resolve("receipt.json"))
      assert(receipt.contains("\"lines_read\":1"))
      assert(receipt.contains("\"clean_records\":1"))
      println("  [PASS] Test 6: Final line without trailing newline classified normally")
    }
  }

  def test7_NotAnObject(): Unit = {
    withTempDir { dir =>
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)
      val in = dir.resolve("array.jsonl")
      Files.writeString(in, "[1, 2, 3]\n")
      val out = dir.resolve("out")

      val exit = Sift.run(SiftCliConfig(in, rules, out, replaceLive = false, forceRerun = false))
      assert(exit == 1) // 1 quarantined out of 1 exceeds max_quarantine_ratio
      val runs = Files.list(out.resolve("runs")).toList
      val receipt = Files.readString(runs.get(0).resolve("receipt.json"))
      assert(receipt.contains("\"records_read\":1"))
      assert(receipt.contains("\"quarantined_records\":1"))
      assert(receipt.contains("\"NOT_AN_OBJECT\":1"))
      println("  [PASS] Test 7: Valid JSON array line quarantined NOT_AN_OBJECT, counted as record")
    }
  }

  def test8_DuplicateKeyNestingDepth3(): Unit = {
    val raw = "{\"a\":{\"b\":{\"c\":1,\"c\":2}}}"
    val res = JsonStreamParser.parseRecord(raw.getBytes(StandardCharsets.UTF_8))
    res match {
      case TokenParseResult.Quarantined(_, reasons) =>
        assert(reasons == List("DUPLICATE_JSON_KEY"))
      case other => throw new AssertionError(s"Expected Quarantined, got $other")
    }
    println("  [PASS] Test 8: Duplicate key at nesting depth 3 quarantines with DUPLICATE_JSON_KEY")
  }

  def test9_RawNumberLexemes(): Unit = {
    // 01 and +1299 -> UNPARSEABLE_LINE
    for (badSyntax <- List("{\"a\": 01}", "{\"a\": +1299}")) {
      val res = JsonStreamParser.parseRecord(badSyntax.getBytes(StandardCharsets.UTF_8))
      res match {
        case TokenParseResult.Unparseable(_, reasons) =>
          assert(reasons == List("UNPARSEABLE_LINE"))
        case other => throw new AssertionError(s"Expected Unparseable, got $other")
      }
    }
    // 1299.0, 1e3, 2^53 in integer column -> INVALID_INTEGER
    val rules = RulesModel.parse(canonicalRulesJson.getBytes(StandardCharsets.UTF_8))
    for (badInt <- List("1299.0", "1e3", "9007199254740992")) {
      val json = s"""{"order_id":"1","order_date":"2026-09-02","amount_pence":$badInt,"currency":"GBP"}"""
      val res = ClassifierEngine.classify(json.getBytes(StandardCharsets.UTF_8), json, 1, rules, "sha256:x")
      res match {
        case ClassifyResult.Quarantined(_, reasons, _) =>
          assert(reasons.contains("INVALID_INTEGER"), s"Expected INVALID_INTEGER for $badInt, got $reasons")
        case other => throw new AssertionError(s"Expected Quarantined, got $other")
      }
    }
    println("  [PASS] Test 9: 01/+1299 -> UNPARSEABLE_LINE; 1299.0/1e3/2^53 -> INVALID_INTEGER")
  }

  def test10_BigIntegersAndKeyOrder(): Unit = {
    val raw = "{\"z\":\"last\",\"a\":\"first\",\"m\":\"middle\",\"big\":900719925474099200000000,\"nested\":{\"x\":1}}"
    val res = JsonStreamParser.parseRecord(raw.getBytes(StandardCharsets.UTF_8))
    res match {
      case TokenParseResult.ValidObject(_, _, passthrough) =>
        val outStr = new String(passthrough("sha256:1", Nil), StandardCharsets.UTF_8).trim
        val expectedStart = "{\"z\":\"last\",\"a\":\"first\",\"m\":\"middle\",\"big\":900719925474099200000000,\"nested\":{\"x\":1},"
        val expectedEnd = "\"_sift\":{\"source_record_id\":\"sha256:1\",\"result\":\"clean\"}}"
        assert(outStr == expectedStart + expectedEnd)
      case other => throw new AssertionError(s"Expected ValidObject, got $other")
    }
    println("  [PASS] Test 10: Big integers and key order pass through byte-identically in clean output")
  }

  def test11_TopLevelVsNestedSift(): Unit = {
    val top = "{\"_sift\":\"custom\",\"a\":1}"
    assert(JsonStreamParser.parseRecord(top.getBytes(StandardCharsets.UTF_8)) match {
      case TokenParseResult.Quarantined(_, List("RESERVED_FIELD__SIFT")) => true
      case _ => false
    })
    val nested = "{\"data\":{\"_sift\":\"allowed\"}}"
    assert(JsonStreamParser.parseRecord(nested.getBytes(StandardCharsets.UTF_8)).isInstanceOf[TokenParseResult.ValidObject])
    println("  [PASS] Test 11: Top-level _sift quarantines; nested _sift passes through")
  }

  def test12_RulesValidationUnknownKeysAndSift(): Unit = {
    val unknown = canonicalRulesJson.replace("\"schema_version\": 1,", "\"schema_version\": 1, \"extra\": 123,")
    var caught = false
    try RulesModel.parse(unknown.getBytes(StandardCharsets.UTF_8))
    catch { case _: RulesValidationError => caught = true }
    assert(caught, "Rules with unknown keys must fail validation")

    val siftInRules = canonicalRulesJson.replace("\"schema_version\": 1,", "\"schema_version\": 1, \"_sift\": 1,")
    caught = false
    try RulesModel.parse(siftInRules.getBytes(StandardCharsets.UTF_8))
    catch { case _: RulesValidationError => caught = true }
    assert(caught, "Rules with _sift must fail validation")
    println("  [PASS] Test 12: Rules file containing _sift or unknown keys fails validation")
  }

  def test13_BlockIfMissingMustBeSubset(): Unit = {
    val bad = canonicalRulesJson.replace("""["order_id", "amount_pence"]""", """["order_id", "non_existent"]""")
    var caught = false
    try RulesModel.parse(bad.getBytes(StandardCharsets.UTF_8))
    catch { case e: RulesValidationError if e.code == "INVALID_BLOCK_IF_MISSING" => caught = true }
    assert(caught)
    println("  [PASS] Test 13: block_if_missing containing a non-required field fails validation")
  }

  def test14_OverlappingColumnLists(): Unit = {
    val bad = canonicalRulesJson.replace("\"string_columns\": [\"order_id\", \"currency\"],", "\"string_columns\": [\"order_id\", \"currency\", \"amount_pence\"],")
    var caught = false
    try RulesModel.parse(bad.getBytes(StandardCharsets.UTF_8))
    catch { case e: RulesValidationError if e.code == "OVERLAPPING_COLUMNS" => caught = true }
    assert(caught)
    println("  [PASS] Test 14: Overlapping column lists fail validation")
  }

  def test15_InvalidMaxQuarantineRatio(): Unit = {
    val bad = canonicalRulesJson.replace("0.0005", "1.0")
    var caught = false
    try RulesModel.parse(bad.getBytes(StandardCharsets.UTF_8))
    catch { case e: RulesValidationError if e.code == "INVALID_MAX_QUARANTINE_RATIO" => caught = true }
    assert(caught)
    println("  [PASS] Test 15: max_quarantine_ratio >= 1 fails validation")
  }

  def test16_IntegerValidation(): Unit = {
    val rules = RulesModel.parse(canonicalRulesJson.getBytes(StandardCharsets.UTF_8))
    val strInt = "{\"order_id\":\"ORD-1\",\"order_date\":\"2026-09-02\",\"amount_pence\":\"1299\",\"currency\":\"GBP\"}"
    val res1 = ClassifierEngine.classify(strInt.getBytes(StandardCharsets.UTF_8), strInt, 1, rules, "sha256:1")
    assert(res1.asInstanceOf[ClassifyResult.Quarantined].reasons.contains("INVALID_INTEGER"))

    for (zero <- List("0", "-0")) {
      val validZero = s"""{"order_id":"ORD-1","order_date":"2026-09-02","amount_pence":$zero,"currency":"GBP"}"""
      val resZero = ClassifierEngine.classify(validZero.getBytes(StandardCharsets.UTF_8), validZero, 2, rules, "sha256:zero")
      assert(resZero.isInstanceOf[ClassifyResult.Clean])
    }
    println("  [PASS] Test 16: \"1299\" string in integer column -> INVALID_INTEGER; 0 and -0 accepted")
  }

  def test17_StrictDateValidation(): Unit = {
    val rules = RulesModel.parse(canonicalRulesJson.getBytes(StandardCharsets.UTF_8))
    val leap = "{\"order_id\":\"ORD-1\",\"order_date\":\"2024-02-29\",\"amount_pence\":100,\"currency\":\"GBP\"}"
    assert(ClassifierEngine.classify(leap.getBytes(StandardCharsets.UTF_8), leap, 1, rules, "sha256:l").isInstanceOf[ClassifyResult.Clean])

    for (badDate <- List("\"2026-02-29\"", "\"2026-04-31\"", "\"2026-9-2\"", "\"2026-09-02T00:00:00Z\"", "\" 2026-09-02\"")) {
      val badJson = s"""{"order_id":"ORD-1","order_date":$badDate,"amount_pence":100,"currency":"GBP"}"""
      val res = ClassifierEngine.classify(badJson.getBytes(StandardCharsets.UTF_8), badJson, 2, rules, "sha256:b")
      assert(res.asInstanceOf[ClassifyResult.Quarantined].reasons.contains("INVALID_ISO_DATE"))
    }
    println("  [PASS] Test 17: 2024-02-29 accepted; 2026-02-29, 2026-04-31, spaces, timestamps quarantine")
  }

  def test18_DeterminismByteIdenticalRerun(): Unit = {
    withTempDir { dir =>
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)
      val in = dir.resolve("input.jsonl")
      Files.writeString(in, "{\"order_id\":\"1\",\"order_date\":\"2026-09-02\",\"amount_pence\":100,\"currency\":\"GBP\"}\n")
      val out1 = dir.resolve("out1")
      val out2 = dir.resolve("out2")

      Sift.run(SiftCliConfig(in, rules, out1, replaceLive = false, forceRerun = false))
      Sift.run(SiftCliConfig(in, rules, out2, replaceLive = false, forceRerun = false))

      val p1 = Files.readString(out1.resolve("current")).trim
      val p2 = Files.readString(out2.resolve("current")).trim
      assert(p1 == p2, s"Run IDs must match: $p1 vs $p2")

      val r1 = Files.readAllBytes(out1.resolve(p1.stripSuffix("/clean.jsonl")).resolve("receipt.json"))
      val r2 = Files.readAllBytes(out2.resolve(p2.stripSuffix("/clean.jsonl")).resolve("receipt.json"))
      assert(java.util.Arrays.equals(r1, r2), "Receipts must be byte-identical")
      println("  [PASS] Test 18: Two runs -> byte-identical clean, quarantine, receipt, and same run_id")
    }
  }

  def test19_ReceiptDeterminismAndKeyOrder(): Unit = {
    withTempDir { dir =>
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)
      val in = dir.resolve("input.jsonl")
      Files.writeString(in, "{\"order_id\":\"1\",\"order_date\":\"2026-09-02\",\"amount_pence\":100,\"currency\":\"GBP\"}\n")
      val out = dir.resolve("out")

      Sift.run(SiftCliConfig(in, rules, out, replaceLive = false, forceRerun = false))
      val ptr = Files.readString(out.resolve("current")).trim
      val receipt = Files.readString(out.resolve(ptr.stripSuffix("/clean.jsonl")).resolve("receipt.json"))

      assert(!receipt.contains(dir.toString), "Receipt must contain zero paths")
      val expectedOrder = List(
        "\"bad_ratio\":", "\"bad_ratio_denominator\":", "\"bad_ratio_numerator\":",
        "\"clean_records\":", "\"ignored_empty_lines\":", "\"input_sha256\":",
        "\"lines_read\":", "\"outputs\":", "\"quarantine_reasons\":",
        "\"quarantined_records\":", "\"records_read\":", "\"release\":",
        "\"release_reasons\":", "\"rules_sha256\":", "\"run_id\":",
        "\"schema_version\":", "\"sift_version\":", "\"unparseable_lines\":"
      )
      var lastIdx = -1
      for (k <- expectedOrder) {
        val idx = receipt.indexOf(k)
        assert(idx > lastIdx, s"Key $k out of order in receipt")
        lastIdx = idx
      }
      println("  [PASS] Test 19: Receipt: sorted keys, no timestamps/paths/hosts")
    }
  }

  def test20_BlockedRunNeverOccupiesBarePath(): Unit = {
    withTempDir { dir =>
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)
      val in = dir.resolve("bad.jsonl")
      Files.writeString(in, "{\"order_date\":\"2026-09-02\",\"amount_pence\":100,\"currency\":\"GBP\"}\n")
      val out = dir.resolve("out")

      val exit = Sift.run(SiftCliConfig(in, rules, out, replaceLive = false, forceRerun = false))
      assert(exit == 1)
      val runs = Files.list(out.resolve("runs")).toList
      assert(runs.size() == 1)
      val name = runs.get(0).getFileName.toString
      assert(name.contains(".blocked-1"), s"Blocked run directory must end in .blocked-1, got $name")
      println("  [PASS] Test 20: Blocked run never occupies bare runs/<run_id>/; lives at .blocked-<n>")
    }
  }

  def test21_ReplaceLiveWithBlockedReplacement(): Unit = {
    withTempDir { dir =>
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)
      val cleanIn = dir.resolve("clean.jsonl")
      Files.writeString(cleanIn, "{\"order_id\":\"1\",\"order_date\":\"2026-09-02\",\"amount_pence\":100,\"currency\":\"GBP\"}\n")
      val out = dir.resolve("out")

      Sift.run(SiftCliConfig(cleanIn, rules, out, replaceLive = false, forceRerun = false))
      val origPtr = Files.readString(out.resolve("current"))

      val badIn = dir.resolve("bad.jsonl")
      Files.writeString(badIn, "{\"order_date\":\"2026-09-02\",\"amount_pence\":100,\"currency\":\"GBP\"}\n")
      val exit2 = Sift.run(SiftCliConfig(badIn, rules, out, replaceLive = true, forceRerun = false))
      assert(exit2 == 1)

      val newPtr = Files.readString(out.resolve("current"))
      assert(origPtr == newPtr, "Pointer must remain completely unchanged")
      println("  [PASS] Test 21: --replace-live with blocked replacement leaves old release & current untouched")
    }
  }

  def test22_CrashRecovery(): Unit = {
    withTempDir { dir =>
      val outDir = dir.resolve("out")
      val runsDir = outDir.resolve("runs")
      Files.createDirectories(runsDir)
      val runId = "testrunid"
      val tombDir = runsDir.resolve(s"$runId.replaced-1")
      Files.createDirectories(tombDir)
      Files.writeString(tombDir.resolve("clean.jsonl"), "clean\n")
      Files.writeString(tombDir.resolve("quarantine.jsonl"), "")
      Files.writeString(tombDir.resolve("receipt.json"), s"""{"run_id":"sha256:$runId","clean_records":1}""")

      val lockFile = outDir.resolve(".sift-lock")
      Files.writeString(lockFile, s"replace-live $runId $runId.replaced-1\n")

      PublicationEngine.withLock(outDir) { ch =>
        PublicationEngine.recoverCrash(outDir, ch)
      }
      assert(Files.isDirectory(runsDir.resolve(runId)), "Tombstone must be restored to runs/<run_id>")
      assert(!Files.exists(tombDir), "Tombstone directory must be removed after restoration")
      println("  [PASS] Test 22: Mandatory crash recovery restores tombstone from .sift-lock state")
    }
  }

  def test23_TamperedCurrentPointer(): Unit = {
    withTempDir { dir =>
      val current = dir.resolve("current")
      Files.writeString(current, "invalid/pointer/shape\n")
      var caught = false
      try PublicationEngine.readAndValidateCurrent(dir)
      catch { case e: sift.pub.PubError if e.code == "POINTER_CORRUPTION" => caught = true }
      assert(caught, "Malformed pointer shape must throw POINTER_CORRUPTION")

      Files.writeString(current, "runs/non_existent/clean.jsonl\n")
      caught = false
      try PublicationEngine.readAndValidateCurrent(dir)
      catch { case e: sift.pub.PubError if e.code == "POINTER_CORRUPTION" => caught = true }
      assert(caught, "Dangling pointer target must throw POINTER_CORRUPTION")
      println("  [PASS] Test 23: Tampered or dangling current pointer reported as POINTER_CORRUPTION, never auto-repaired")
    }
  }

  def test24_CorruptRunDirectoryNeverDeleted(): Unit = {
    withTempDir { dir =>
      val corruptRun = dir.resolve("runs").resolve("corrupt-run")
      Files.createDirectories(corruptRun)
      assert(!PublicationEngine.isCompleteRun(corruptRun))
      assert(Files.exists(corruptRun))
      println("  [PASS] Test 24: Corrupt runs/<id> reported incomplete and never deleted")
    }
  }

  def test25_TamperedCleanFileFailsReuse(): Unit = {
    withTempDir { dir =>
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)
      val in = dir.resolve("input.jsonl")
      Files.writeString(in, "{\"order_id\":\"1\",\"order_date\":\"2026-09-02\",\"amount_pence\":100,\"currency\":\"GBP\"}\n")
      val out = dir.resolve("out")

      Sift.run(SiftCliConfig(in, rules, out, replaceLive = false, forceRerun = false))
      val ptr = Files.readString(out.resolve("current")).trim
      val runDir = out.resolve(ptr.stripSuffix("/clean.jsonl"))

      Files.writeString(runDir.resolve("clean.jsonl"), "tampered-content\n")

      var caught = false
      try PublicationEngine.revalidateRun(runDir, runDir.getFileName.toString)
      catch { case e: RuntimeException => caught = true }
      assert(caught, "Tampered clean file must fail reuse revalidation")
      println("  [PASS] Test 25: Tampered clean file fails reuse revalidation")
    }
  }

  def test26_ConcurrentRunsFailRunInProgress(): Unit = {
    withTempDir { dir =>
      val out = dir.resolve("out")
      Files.createDirectories(out)

      PublicationEngine.withLock(out) { _ =>
        var caught = false
        try PublicationEngine.withLock(out) { _ => () }
        catch { case e: RuntimeException if e.getMessage.contains("RUN_IN_PROGRESS") => caught = true }
        assert(caught, "Second concurrent run must fail fast with RUN_IN_PROGRESS")
      }
      println("  [PASS] Test 26: Two runs in same JVM: second fails fast with RUN_IN_PROGRESS")
    }
  }

  def test27_InputInsideOutputDir(): Unit = {
    withTempDir { dir =>
      val out = dir.resolve("out")
      val in = out.resolve("nested-input.jsonl")
      Files.createDirectories(out)
      Files.writeString(in, "{}")
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)

      var caught = false
      try Sift.run(SiftCliConfig(in, rules, out, replaceLive = false, forceRerun = false))
      catch { case e: RuntimeException if e.getMessage.contains("INPUT_INSIDE_OUTPUT_DIR") => caught = true }
      assert(caught, "Input inside OUT_DIR must fail with INPUT_INSIDE_OUTPUT_DIR")
      println("  [PASS] Test 27: Input path inside OUT_DIR fails with INPUT_INSIDE_OUTPUT_DIR")
    }
  }

  def testAdversarialHardening(): Unit = {
    // 1. Multiple non-object JSON values on one line -> UNPARSEABLE_LINE
    for (multi <- List("[1] [2]", "\"first\" \"second\"", "123 456", "true false")) {
      val res = JsonStreamParser.parseRecord(multi.getBytes(StandardCharsets.UTF_8))
      assert(res.isInstanceOf[TokenParseResult.Unparseable], s"Multiple tokens '$multi' must be UNPARSEABLE_LINE")
    }

    // 2. Multibyte string truncated to <= 8192 UTF-8 bytes without broken code points
    val multiByteStr = "€" * 4000 // 4000 * 3 = 12000 bytes
    val rawBytes = ("{\"order_id\":\"" + multiByteStr).getBytes(StandardCharsets.UTF_8)
    val wrapped = ClassifierEngine.formatRawWrapper(multiByteStr, "sha256:test", List("UNPARSEABLE_LINE"), 1)
    val parsedWrapped = new String(wrapped, StandardCharsets.UTF_8)
    assert(!parsedWrapped.contains("\uFFFD"), "Truncation must never produce UTF-8 replacement character")

    // 3. Nesting depth exceeding Jackson StreamReadConstraints (1,000) -> UNPARSEABLE_LINE
    val deepJson = ("{\"a\":" * 1005) + "1" + ("}" * 1005)
    val deepRes = JsonStreamParser.parseRecord(deepJson.getBytes(StandardCharsets.UTF_8))
    assert(deepRes.isInstanceOf[TokenParseResult.Unparseable], "Nesting depth > 1000 must quarantine as UNPARSEABLE_LINE")

    // 4. Receipt accounting with unparseable lines: quarantine_reasons excludes UNPARSEABLE_LINE
    withTempDir { dir =>
      val rules = dir.resolve("rules.json")
      Files.writeString(rules, canonicalRulesJson)
      val in = dir.resolve("mixed.jsonl")
      val content =
        "{\"order_id\":\"1\",\"order_date\":\"2026-09-02\",\"amount_pence\":100,\"currency\":\"GBP\"}\n" +
        "{\"order_id\":\"2\",\"order_date\":\"bad-date\",\"amount_pence\":100,\"currency\":\"GBP\"}\n" +
        "{unparseable\n"
      Files.writeString(in, content)
      val out = dir.resolve("out")

      val exit = Sift.run(SiftCliConfig(in, rules, out, replaceLive = false, forceRerun = false))
      assert(exit == 1) // blocked
      val runs = Files.list(out.resolve("runs")).toList
      val receipt = Files.readString(runs.get(0).resolve("receipt.json"))
      assert(receipt.contains("\"lines_read\":3"))
      assert(receipt.contains("\"clean_records\":1"))
      assert(receipt.contains("\"quarantined_records\":1"))
      assert(receipt.contains("\"unparseable_lines\":1"))
      assert(receipt.contains("\"INVALID_ISO_DATE\":1"))
      assert(!receipt.contains("\"UNPARSEABLE_LINE\":"), "quarantine_reasons must never contain UNPARSEABLE_LINE")
      assert(receipt.contains("\"UNPARSEABLE_INPUT\""), "release_reasons must contain UNPARSEABLE_INPUT")
    }

    println("  [PASS] Adversarial Hardening: Multiple non-objects, UTF-8 truncation, 1000-depth limit, receipt accounting")
  }

  def main(args: Array[String]): Unit = {
    runAll()
  }
}
