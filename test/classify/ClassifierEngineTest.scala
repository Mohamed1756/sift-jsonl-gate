package sift.classify

import java.nio.charset.StandardCharsets
import sift.rules.{RulesModel, SiftRules}

object ClassifierEngineTest {

  val rulesJson =
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

  val rules: SiftRules = RulesModel.parse(rulesJson.getBytes(StandardCharsets.UTF_8))

  def runAll(): Unit = {
    testAcceptance16_IntegerValidation()
    testAcceptance17_StrictDateValidation()
    testMissingRequiredFieldAndBlockIfMissing()
    testPrecedenceMissingOverType()
    testMultiViolationLexicographicalSorting()
    testCleanRecord()
    testRawWrapperFormat()
    println("All ClassifierEngineTest tests passed successfully!")
  }

  def testAcceptance16_IntegerValidation(): Unit = {
    // "1299" string in integer column -> INVALID_INTEGER
    val strInt = "{\"order_id\":\"ORD-1\",\"order_date\":\"2026-09-02\",\"amount_pence\":\"1299\",\"currency\":\"GBP\"}"
    val res1 = ClassifierEngine.classify(strInt.getBytes(StandardCharsets.UTF_8), strInt, 1, rules, "sha256:1")
    res1 match {
      case ClassifyResult.Quarantined(_, reasons, _) =>
        assert(reasons.contains("INVALID_INTEGER"), s"Expected INVALID_INTEGER, got $reasons")
      case other => throw new AssertionError(s"Expected Quarantined, got $other")
    }

    // 0 and -0 accepted
    for (zeroVal <- List("0", "-0")) {
      val validZero = s"""{"order_id":"ORD-1","order_date":"2026-09-02","amount_pence":$zeroVal,"currency":"GBP"}"""
      val resZero = ClassifierEngine.classify(validZero.getBytes(StandardCharsets.UTF_8), validZero, 2, rules, "sha256:zero")
      assert(resZero.isInstanceOf[ClassifyResult.Clean], s"$zeroVal must be accepted as clean integer")
    }
    println("  [PASS] Acceptance 16: \"1299\" string in integer column -> INVALID_INTEGER; 0 and -0 accepted")
  }

  def testAcceptance17_StrictDateValidation(): Unit = {
    // Leap day 2024-02-29 accepted
    val leapDay = "{\"order_id\":\"ORD-1\",\"order_date\":\"2024-02-29\",\"amount_pence\":100,\"currency\":\"GBP\"}"
    val resLeap = ClassifierEngine.classify(leapDay.getBytes(StandardCharsets.UTF_8), leapDay, 10, rules, "sha256:leap")
    assert(resLeap.isInstanceOf[ClassifyResult.Clean], "2024-02-29 must be clean")

    // Invalid dates: 2026-02-29, 2026-04-31, 2026-9-2, timestamps, spaces, numbers
    val badDates = List(
      "\"2026-02-29\"",
      "\"2026-04-31\"",
      "\"2026-9-2\"",
      "\"2026/09/02\"",
      "\"2026-09-02T00:00:00Z\"",
      "\" 2026-09-02\"",
      "12345"
    )

    for (badDate <- badDates) {
      val json = s"""{"order_id":"ORD-1","order_date":$badDate,"amount_pence":100,"currency":"GBP"}"""
      val res = ClassifierEngine.classify(json.getBytes(StandardCharsets.UTF_8), json, 11, rules, "sha256:baddate")
      res match {
        case ClassifyResult.Quarantined(_, reasons, _) =>
          assert(reasons.contains("INVALID_ISO_DATE"), s"$badDate must trigger INVALID_ISO_DATE, got $reasons")
        case other => throw new AssertionError(s"Expected Quarantined for $badDate, got $other")
      }
    }
    println("  [PASS] Acceptance 17: 2024-02-29 accepted; invalid dates, spaces, and timestamps quarantined")
  }

  def testMissingRequiredFieldAndBlockIfMissing(): Unit = {
    // Missing order_id (key absent) -> block_if_missing triggers
    val missingId = "{\"order_date\":\"2026-09-02\",\"amount_pence\":100,\"currency\":\"GBP\"}"
    val res1 = ClassifierEngine.classify(missingId.getBytes(StandardCharsets.UTF_8), missingId, 5, rules, "sha256:missingId")
    res1 match {
      case ClassifyResult.Quarantined(_, reasons, hasBlockedMissing) =>
        assert(reasons.contains("MISSING_REQUIRED_FIELD"))
        assert(hasBlockedMissing, "order_id is in block_if_missing, hasBlockedMissing must be true")
      case other => throw new AssertionError(s"Expected Quarantined, got $other")
    }

    // null value in required field
    val nullCurrency = "{\"order_id\":\"ORD-1\",\"order_date\":\"2026-09-02\",\"amount_pence\":100,\"currency\":null}"
    val res2 = ClassifierEngine.classify(nullCurrency.getBytes(StandardCharsets.UTF_8), nullCurrency, 6, rules, "sha256:nullCurr")
    res2 match {
      case ClassifyResult.Quarantined(_, reasons, hasBlockedMissing) =>
        assert(reasons.contains("MISSING_REQUIRED_FIELD"))
        assert(!hasBlockedMissing, "currency is NOT in block_if_missing")
      case other => throw new AssertionError(s"Expected Quarantined, got $other")
    }

    // empty string "" in required field
    val emptyCurrency = "{\"order_id\":\"ORD-1\",\"order_date\":\"2026-09-02\",\"amount_pence\":100,\"currency\":\"\"}"
    val res3 = ClassifierEngine.classify(emptyCurrency.getBytes(StandardCharsets.UTF_8), emptyCurrency, 7, rules, "sha256:emptyCurr")
    res3 match {
      case ClassifyResult.Quarantined(_, reasons, _) =>
        assert(reasons.contains("MISSING_REQUIRED_FIELD"), "empty string in required field must fail presence")
      case other => throw new AssertionError(s"Expected Quarantined, got $other")
    }
    println("  [PASS] Missing required fields (absent, null, empty string) trigger MISSING_REQUIRED_FIELD and block_if_missing")
  }

  def testPrecedenceMissingOverType(): Unit = {
    // amount_pence is missing -> triggers MISSING_REQUIRED_FIELD, but NOT INVALID_INTEGER
    val missingAmount = "{\"order_id\":\"ORD-1\",\"order_date\":\"2026-09-02\",\"currency\":\"GBP\"}"
    val res = ClassifierEngine.classify(missingAmount.getBytes(StandardCharsets.UTF_8), missingAmount, 8, rules, "sha256:missAmt")
    res match {
      case ClassifyResult.Quarantined(_, reasons, _) =>
        assert(reasons == List("MISSING_REQUIRED_FIELD"), s"Expected only MISSING_REQUIRED_FIELD, got $reasons")
      case other => throw new AssertionError(s"Expected Quarantined, got $other")
    }
    println("  [PASS] Precedence: missing > type errors (no false INVALID_INTEGER on missing field)")
  }

  def testMultiViolationLexicographicalSorting(): Unit = {
    // Bad date ("2026-02-29") + bad integer ("not_a_number") + missing currency
    val multiBad = "{\"order_id\":\"ORD-1\",\"order_date\":\"2026-02-29\",\"amount_pence\":\"not_a_number\"}"
    val res = ClassifierEngine.classify(multiBad.getBytes(StandardCharsets.UTF_8), multiBad, 9, rules, "sha256:multi")
    res match {
      case ClassifyResult.Quarantined(bytes, reasons, _) =>
        val expected = List("INVALID_INTEGER", "INVALID_ISO_DATE", "MISSING_REQUIRED_FIELD")
        assert(reasons == expected, s"Expected $expected, got $reasons")
        val str = new String(bytes, StandardCharsets.UTF_8)
        assert(str.contains("\"reasons\":[\"INVALID_INTEGER\",\"INVALID_ISO_DATE\",\"MISSING_REQUIRED_FIELD\"]"))
      case other => throw new AssertionError(s"Expected Quarantined, got $other")
    }
    println("  [PASS] Multi-violations collected and sorted lexicographically in reasons array")
  }

  def testCleanRecord(): Unit = {
    val cleanJson = "{\"order_id\":\"ORD-1\",\"order_date\":\"2026-09-02\",\"amount_pence\":1299,\"currency\":\"GBP\"}"
    val res = ClassifierEngine.classify(cleanJson.getBytes(StandardCharsets.UTF_8), cleanJson, 1, rules, "sha256:clean1")
    res match {
      case ClassifyResult.Clean(bytes) =>
        val str = new String(bytes, StandardCharsets.UTF_8).trim
        assert(str.endsWith("\"_sift\":{\"source_record_id\":\"sha256:clean1\",\"result\":\"clean\"}}"))
      case other => throw new AssertionError(s"Expected Clean, got $other")
    }
    println("  [PASS] Clean record passes validation and appends _sift clean metadata")
  }

  def testRawWrapperFormat(): Unit = {
    // Unparseable line: {"order_id": (unterminated)
    val badSyntax = "{\"order_id\": "
    val res = ClassifierEngine.classify(badSyntax.getBytes(StandardCharsets.UTF_8), badSyntax, 12, rules, "sha256:syntax")
    res match {
      case ClassifyResult.Unparseable(bytes, reasons) =>
        val str = new String(bytes, StandardCharsets.UTF_8).trim
        assert(str.contains("\"reasons\":[\"UNPARSEABLE_LINE\"]"))
        assert(str.contains("\"line_number\":12"))
        assert(str.contains("\"raw\":\"{\\\"order_id\\\": \""))
        assert(str.startsWith("{\"_sift\":{"))
      case other => throw new AssertionError(s"Expected Unparseable, got $other")
    }

    // Non-object: "just a string"
    val strVal = "\"just a string\""
    val res2 = ClassifierEngine.classify(strVal.getBytes(StandardCharsets.UTF_8), strVal, 13, rules, "sha256:str")
    res2 match {
      case ClassifyResult.Quarantined(bytes, reasons, _) =>
        val str = new String(bytes, StandardCharsets.UTF_8).trim
        assert(str.contains("\"reasons\":[\"NOT_AN_OBJECT\"]"))
        assert(str.contains("\"line_number\":13"))
        assert(str.contains("\"raw\":\"\\\"just a string\\\"\""))
      case other => throw new AssertionError(s"Expected Quarantined, got $other")
    }
    println("  [PASS] Raw wrapper formats _sift, reasons, line_number, and escaped raw text per §3")
  }

  def main(args: Array[String]): Unit = {
    runAll()
  }
}
