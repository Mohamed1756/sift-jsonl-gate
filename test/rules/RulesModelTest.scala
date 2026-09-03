package sift.rules

import java.nio.charset.StandardCharsets

object RulesModelTest {

  def runAll(): Unit = {
    testAcceptance13_BlockIfMissingMustBeSubset()
    testAcceptance14_OverlappingColumnLists()
    testAcceptance15_InvalidMaxQuarantineRatio()
    testInvalidRuleFieldTypes()
    testValidRules()
    println("All RulesModelTest tests passed successfully!")
  }

  def testAcceptance13_BlockIfMissingMustBeSubset(): Unit = {
    val json =
      """{
        |  "schema_version": 1,
        |  "required": ["a", "b"],
        |  "string_columns": ["a"],
        |  "date_columns": [],
        |  "integer_columns": ["b"],
        |  "release": {
        |    "max_quarantine_ratio": 0.001,
        |    "block_if_missing": ["a", "non_existent"]
        |  }
        |}""".stripMargin

    var caught = false
    try {
      RulesModel.parse(json.getBytes(StandardCharsets.UTF_8))
    } catch {
      case e: RulesValidationError if e.code == "INVALID_BLOCK_IF_MISSING" =>
        caught = true
    }
    assert(caught, "block_if_missing containing non-required field must fail with INVALID_BLOCK_IF_MISSING")
    println("  [PASS] Acceptance 13: block_if_missing containing non-required field fails validation")
  }

  def testAcceptance14_OverlappingColumnLists(): Unit = {
    val json =
      """{
        |  "schema_version": 1,
        |  "required": ["x"],
        |  "string_columns": ["x"],
        |  "date_columns": [],
        |  "integer_columns": ["x"],
        |  "release": {
        |    "max_quarantine_ratio": 0.001,
        |    "block_if_missing": ["x"]
        |  }
        |}""".stripMargin

    var caught = false
    try {
      RulesModel.parse(json.getBytes(StandardCharsets.UTF_8))
    } catch {
      case e: RulesValidationError if e.code == "OVERLAPPING_COLUMNS" =>
        caught = true
    }
    assert(caught, "Overlapping type column lists must fail with OVERLAPPING_COLUMNS")
    println("  [PASS] Acceptance 14: Overlapping column lists fail validation")
  }

  def testAcceptance15_InvalidMaxQuarantineRatio(): Unit = {
    val json1 =
      """{
        |  "schema_version": 1,
        |  "required": ["a"],
        |  "string_columns": ["a"],
        |  "date_columns": [],
        |  "integer_columns": [],
        |  "release": {
        |    "max_quarantine_ratio": 1.0,
        |    "block_if_missing": []
        |  }
        |}""".stripMargin

    var caught = false
    try RulesModel.parse(json1.getBytes(StandardCharsets.UTF_8))
    catch case e: RulesValidationError if e.code == "INVALID_MAX_QUARANTINE_RATIO" => caught = true
    assert(caught, "max_quarantine_ratio >= 1.0 must fail")

    val json2 = json1.replace("1.0", "-0.01")
    caught = false
    try RulesModel.parse(json2.getBytes(StandardCharsets.UTF_8))
    catch case e: RulesValidationError if e.code == "INVALID_MAX_QUARANTINE_RATIO" => caught = true
    assert(caught, "max_quarantine_ratio < 0 must fail")
    println("  [PASS] Acceptance 15: max_quarantine_ratio >= 1 or < 0 fails validation")
  }

  def testInvalidRuleFieldTypes(): Unit = {
    // string instead of array for required
    val badReq =
      """{
        |  "schema_version": 1,
        |  "required": "not_an_array",
        |  "string_columns": [],
        |  "date_columns": [],
        |  "integer_columns": [],
        |  "release": { "max_quarantine_ratio": 0.001, "block_if_missing": [] }
        |}""".stripMargin

    var caught = false
    try RulesModel.parse(badReq.getBytes(StandardCharsets.UTF_8))
    catch case e: RulesValidationError if e.code == "INVALID_RULE_TYPE" => caught = true
    assert(caught, "string instead of array for required must fail with INVALID_RULE_TYPE")

    // number instead of object for release
    val badRel = badReq.replace("\"not_an_array\"", "[]").replace("""{ "max_quarantine_ratio": 0.001, "block_if_missing": [] }""", "123")
    caught = false
    try RulesModel.parse(badRel.getBytes(StandardCharsets.UTF_8))
    catch case e: RulesValidationError if e.code == "INVALID_RULE_TYPE" => caught = true
    assert(caught, "number instead of object for release must fail with INVALID_RULE_TYPE")
    println("  [PASS] Invalid rule field types rejected with INVALID_RULE_TYPE")
  }

  def testValidRules(): Unit = {
    val json =
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

    val rules = RulesModel.parse(json.getBytes(StandardCharsets.UTF_8))
    assert(rules.schemaVersion == 1)
    assert(rules.required == Set("order_id", "order_date", "amount_pence", "currency"))
    assert(rules.stringColumns == Set("order_id", "currency"))
    assert(rules.dateColumns == Set("order_date"))
    assert(rules.integerColumns == Set("amount_pence"))
    assert(rules.release.maxQuarantineRatio == 0.0005)
    assert(rules.release.blockIfMissing == Set("order_id", "amount_pence"))
    println("  [PASS] Canonical rules parse and validate successfully")
  }

  def main(args: Array[String]): Unit = {
    runAll()
  }
}
