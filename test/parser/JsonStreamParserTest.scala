package sift.parser

import java.nio.charset.StandardCharsets

object JsonStreamParserTest {

  def runAll(): Unit = {
    testAcceptance8_Depth3DuplicateKey()
    testAcceptance9_NumberSyntaxAndLexemes()
    testAcceptance10_BigIntAndKeyOrderPassthrough()
    testAcceptance11_TopLevelVsNestedSift()
    testAcceptance12_RulesValidation()
    testNonObjectRecord()
    println("All JsonStreamParser tests passed successfully!")
  }

  def testAcceptance8_Depth3DuplicateKey(): Unit = {
    val json = "{\"l1\": {\"l2\": {\"l3\": 1, \"l3\": 2}}}"
    val res = JsonStreamParser.parseRecord(json.getBytes(StandardCharsets.UTF_8))
    res match {
      case TokenParseResult.Quarantined(_, reasons) =>
        assert(reasons.contains("DUPLICATE_JSON_KEY"), s"Expected DUPLICATE_JSON_KEY, got $reasons")
      case other =>
        throw new AssertionError(s"Expected Quarantined(DUPLICATE_JSON_KEY), got $other")
    }
    println("  [PASS] Acceptance 8: Duplicate key at nesting depth 3 quarantines with DUPLICATE_JSON_KEY")
  }

  def testAcceptance9_NumberSyntaxAndLexemes(): Unit = {
    // 01 and +1299 must fail JSON syntax -> UNPARSEABLE_LINE
    val bad1 = JsonStreamParser.parseRecord("{\"val\": 01}".getBytes(StandardCharsets.UTF_8))
    assert(bad1.isInstanceOf[TokenParseResult.Unparseable], "01 must be UNPARSEABLE_LINE")

    val bad2 = JsonStreamParser.parseRecord("{\"val\": +1299}".getBytes(StandardCharsets.UTF_8))
    assert(bad2.isInstanceOf[TokenParseResult.Unparseable], "+1299 must be UNPARSEABLE_LINE")

    // 1299.0 and 1e3 are valid JSON, but fail integer lexeme check
    val floatJson = JsonStreamParser.parseRecord("{\"val\": 1299.0}".getBytes(StandardCharsets.UTF_8))
    floatJson match {
      case TokenParseResult.ValidObject(_, fields, _) =>
        assert(!fields("val").isValidInteger, "1299.0 must not be a valid integer")
      case other => throw new AssertionError(s"Expected ValidObject, got $other")
    }

    val expJson = JsonStreamParser.parseRecord("{\"val\": 1e3}".getBytes(StandardCharsets.UTF_8))
    expJson match {
      case TokenParseResult.ValidObject(_, fields, _) =>
        assert(!fields("val").isValidInteger, "1e3 must not be a valid integer")
      case other => throw new AssertionError(s"Expected ValidObject, got $other")
    }

    // 2^53 (9007199254740992) exceeds max safe integer -> invalid integer
    val bigJson = JsonStreamParser.parseRecord("{\"val\": 9007199254740992}".getBytes(StandardCharsets.UTF_8))
    bigJson match {
      case TokenParseResult.ValidObject(_, fields, _) =>
        assert(!fields("val").isValidInteger, "9007199254740992 (2^53) must exceed safe integer boundary")
      case other => throw new AssertionError(s"Expected ValidObject, got $other")
    }

    // 0, -0, 1299, -42 are valid integers
    assert(JsonStreamParser.checkIntegerLexeme("0"))
    assert(JsonStreamParser.checkIntegerLexeme("-0"))
    assert(JsonStreamParser.checkIntegerLexeme("1299"))
    assert(JsonStreamParser.checkIntegerLexeme("-42"))
    println("  [PASS] Acceptance 9: 01/+1299 -> UNPARSEABLE_LINE; 1299.0/1e3/2^53 -> INVALID_INTEGER")
  }

  def testAcceptance10_BigIntAndKeyOrderPassthrough(): Unit = {
    // Exact key order: z, a, m, big, nested
    val raw = "{\"z\":\"last\",\"a\":\"first\",\"m\":\"middle\",\"big\":900719925474099200000000,\"nested\":{\"x\":1}}"
    val res = JsonStreamParser.parseRecord(raw.getBytes(StandardCharsets.UTF_8))
    res match {
      case TokenParseResult.ValidObject(_, _, passthrough) =>
        val outBytes = passthrough("sha256:source-test", Nil)
        val outStr = new String(outBytes, StandardCharsets.UTF_8).trim
        
        // Check exact key ordering and verbatim big number in single pass
        val expectedStart = "{\"z\":\"last\",\"a\":\"first\",\"m\":\"middle\",\"big\":900719925474099200000000,\"nested\":{\"x\":1},"
        val expectedEnd = "\"_sift\":{\"source_record_id\":\"sha256:source-test\",\"result\":\"clean\"}}"
        assert(outStr == expectedStart + expectedEnd, s"Unexpected passthrough output:\n$outStr")
      case other =>
        throw new AssertionError(s"Expected ValidObject, got $other")
    }
    println("  [PASS] Acceptance 10: Big integers and key order pass through byte-identically; _sift appended last")
  }

  def testAcceptance11_TopLevelVsNestedSift(): Unit = {
    // Top level _sift must quarantine with RESERVED_FIELD__SIFT
    val topSift = "{\"_sift\": \"custom\", \"a\": 1}"
    val resTop = JsonStreamParser.parseRecord(topSift.getBytes(StandardCharsets.UTF_8))
    resTop match {
      case TokenParseResult.Quarantined(_, reasons) =>
        assert(reasons == List("RESERVED_FIELD__SIFT"), s"Expected RESERVED_FIELD__SIFT, got $reasons")
      case other => throw new AssertionError(s"Expected Quarantined(RESERVED_FIELD__SIFT), got $other")
    }

    // Nested _sift must pass through untouched
    val nestedSift = "{\"data\": {\"_sift\": \"allowed\"}}"
    val resNested = JsonStreamParser.parseRecord(nestedSift.getBytes(StandardCharsets.UTF_8))
    resNested match {
      case TokenParseResult.ValidObject(_, _, passthrough) =>
        val outStr = new String(passthrough("sha256:123", Nil), StandardCharsets.UTF_8).trim
        assert(outStr.contains("\"data\":{\"_sift\":\"allowed\"}"), s"Nested _sift lost in:\n$outStr")
      case other => throw new AssertionError(s"Expected ValidObject, got $other")
    }
    println("  [PASS] Acceptance 11: Top-level _sift quarantines; nested _sift passes through")
  }

  def testAcceptance12_RulesValidation(): Unit = {
    // Rules containing _sift at root or nested must fail
    val rulesWithSift = "{\"schema_version\": 1, \"_sift\": {}}".getBytes(StandardCharsets.UTF_8)
    val errs1 = JsonStreamParser.validateRulesJson(rulesWithSift)
    assert(errs1.contains("RESERVED_FIELD__SIFT"), s"Expected RESERVED_FIELD__SIFT in rules, got $errs1")

    // Rules containing unknown keys at root must fail
    val rulesUnknownRoot = "{\"schema_version\": 1, \"unknown_setting\": true}".getBytes(StandardCharsets.UTF_8)
    val errs2 = JsonStreamParser.validateRulesJson(rulesUnknownRoot)
    assert(errs2.contains("UNKNOWN_KEY_unknown_setting"), s"Expected UNKNOWN_KEY_unknown_setting, got $errs2")

    // Rules containing unknown keys inside release must fail
    val rulesUnknownNested = "{\"schema_version\": 1, \"release\": {\"bogus\": true}}".getBytes(StandardCharsets.UTF_8)
    val errs3 = JsonStreamParser.validateRulesJson(rulesUnknownNested)
    assert(errs3.contains("UNKNOWN_KEY_release.bogus"), s"Expected UNKNOWN_KEY_release.bogus, got $errs3")

    // Rules with duplicate keys must fail
    val rulesDupe = "{\"schema_version\": 1, \"schema_version\": 2}".getBytes(StandardCharsets.UTF_8)
    val errs4 = JsonStreamParser.validateRulesJson(rulesDupe)
    assert(errs4.contains("DUPLICATE_JSON_KEY"), s"Expected DUPLICATE_JSON_KEY in rules, got $errs4")
    println("  [PASS] Acceptance 12: Rules with _sift, unknown keys (root or release), or duplicate keys fail validation")
  }

  def testNonObjectRecord(): Unit = {
    // Array line -> NOT_AN_OBJECT
    val arr = "[1, 2, 3]".getBytes(StandardCharsets.UTF_8)
    val resArr = JsonStreamParser.parseRecord(arr)
    assert(resArr == TokenParseResult.Quarantined(arr, List("NOT_AN_OBJECT")))

    // String line -> NOT_AN_OBJECT
    val str = "\"just a string\"".getBytes(StandardCharsets.UTF_8)
    val resStr = JsonStreamParser.parseRecord(str)
    assert(resStr == TokenParseResult.Quarantined(str, List("NOT_AN_OBJECT")))
    println("  [PASS] Non-object JSON lines classify as NOT_AN_OBJECT")
  }

  def main(args: Array[String]): Unit = {
    runAll()
  }
}
