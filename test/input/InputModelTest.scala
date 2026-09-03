package sift.input

import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets
import sift.rules.{RulesModel, SiftRules}
import sift.classify.{ClassifierEngine, ClassifyResult}

object InputModelTest {

  val sampleRules: SiftRules = RulesModel.parse(
    """{
      |  "schema_version": 1,
      |  "required": ["id"],
      |  "string_columns": ["id"],
      |  "date_columns": [],
      |  "integer_columns": [],
      |  "release": {
      |    "max_quarantine_ratio": 0.5,
      |    "block_if_missing": ["id"]
      |  }
      |}""".stripMargin.getBytes(StandardCharsets.UTF_8)
  )

  def withTempFile[T](content: Array[Byte])(f: Path => T): T = {
    val file = Files.createTempFile("sift-input-test-", ".jsonl")
    try {
      Files.write(file, content)
      f(file)
    } finally {
      Files.deleteIfExists(file)
    }
  }

  def runAll(): Unit = {
    testAcceptance2_CrlfPreservedInSourceHash()
    testAcceptance3_NewlineOnlyFile()
    testAcceptance4_InvalidUtf8FailsPreflight()
    testAcceptance5_LeadingBomFailsPreflight()
    testAcceptance6_UnterminatedFinalLine()
    testAcceptance7_NotAnObjectCountedAsRecord()
    testTruncatedUtf8AtEof()
    println("All InputModelTest tests passed successfully!")
  }

  def testAcceptance2_CrlfPreservedInSourceHash(): Unit = {
    val crlfData = "{\"id\":\"1\"}\r\n{\"id\":\"2\"}\r\n".getBytes(StandardCharsets.UTF_8)
    val lfData = "{\"id\":\"1\"}\n{\"id\":\"2\"}\n".getBytes(StandardCharsets.UTF_8)

    withTempFile(crlfData) { crlfPath =>
      val crlfSha = InputModel.preflight(crlfPath)
      val crlfLines = scala.collection.mutable.ListBuffer[InputLine]()
      InputModel.readLines(crlfPath, crlfSha)(crlfLines += _)

      assert(crlfLines.size == 2)
      assert(crlfLines(0).lineText == "{\"id\":\"1\"}")
      assert(crlfLines(1).lineText == "{\"id\":\"2\"}")
      assert(crlfLines(0).rawBytes.last == '\r')
      assert(crlfLines(1).rawBytes.last == '\r')

      withTempFile(lfData) { lfPath =>
        val lfSha = InputModel.preflight(lfPath)
        val lfLines = scala.collection.mutable.ListBuffer[InputLine]()
        InputModel.readLines(lfPath, lfSha)(lfLines += _)

        assert(crlfLines(0).lineText == lfLines(0).lineText)
        assert(crlfLines(0).sourceRecordId != lfLines(0).sourceRecordId)
      }
    }
    println("  [PASS] Acceptance 2: CRLF line endings pass identically to LF; CRLF preserved in source hash")
  }

  def testAcceptance3_NewlineOnlyFile(): Unit = {
    withTempFile("\n".getBytes(StandardCharsets.UTF_8)) { path =>
      val sha = InputModel.preflight(path)
      val lines = scala.collection.mutable.ListBuffer[InputLine]()
      val total = InputModel.readLines(path, sha)(lines += _)

      assert(total == 1)
      assert(lines.size == 1)
      assert(lines.head.lineNumber == 1)
      assert(lines.head.lineText == "")

      val classifyRes = ClassifierEngine.classify(lines.head.rawBytes, lines.head.lineText, 1, sampleRules, lines.head.sourceRecordId)
      assert(classifyRes == ClassifyResult.IgnoredEmpty)
    }
    println("  [PASS] Acceptance 3: \"\\n\"-only file: lines_read = 1, line is ignored empty")
  }

  def testAcceptance4_InvalidUtf8FailsPreflight(): Unit = {
    val badUtf8 = Array[Byte]('{', '}', '\n', 0xFF.toByte, '\n')
    withTempFile(badUtf8) { path =>
      var caught = false
      try InputModel.preflight(path)
      catch {
        case e: InputError if e.code == "INVALID_UTF8" => caught = true
      }
      assert(caught, "Invalid UTF-8 anywhere in file must fail preflight with INVALID_UTF8")
    }
    println("  [PASS] Acceptance 4: Invalid UTF-8 anywhere fails run before output with INVALID_UTF8")
  }

  def testAcceptance5_LeadingBomFailsPreflight(): Unit = {
    val bomData = Array[Byte](0xEF.toByte, 0xBB.toByte, 0xBF.toByte, '{', '}')
    withTempFile(bomData) { path =>
      var caught = false
      try InputModel.preflight(path)
      catch {
        case e: InputError if e.code == "INVALID_ENCODING_HEADER" => caught = true
      }
      assert(caught, "Leading BOM must fail preflight with INVALID_ENCODING_HEADER")
    }
    println("  [PASS] Acceptance 5: Leading BOM fails run before output with INVALID_ENCODING_HEADER")
  }

  def testAcceptance6_UnterminatedFinalLine(): Unit = {
    val unterminated = "{\"id\":\"A\"}\n{\"id\":\"B\"}".getBytes(StandardCharsets.UTF_8)
    withTempFile(unterminated) { path =>
      val sha = InputModel.preflight(path)
      val lines = scala.collection.mutable.ListBuffer[InputLine]()
      val total = InputModel.readLines(path, sha)(lines += _)

      assert(total == 2, s"Expected 2 lines, got $total")
      assert(lines.size == 2)
      assert(lines(0).lineText == "{\"id\":\"A\"}")
      assert(lines(1).lineText == "{\"id\":\"B\"}")

      for (l <- lines) {
        val res = ClassifierEngine.classify(l.rawBytes, l.lineText, l.lineNumber, sampleRules, l.sourceRecordId)
        assert(res.isInstanceOf[ClassifyResult.Clean], s"Line ${l.lineNumber} must be clean")
      }
    }
    println("  [PASS] Acceptance 6: Unterminated final line classified normally, byte count and line count correct")
  }

  def testAcceptance7_NotAnObjectCountedAsRecord(): Unit = {
    val nonObjects = List(
      "[1, 2, 3]",
      "\"hello\"",
      "12345",
      "true"
    )

    for (no <- nonObjects) {
      val res = ClassifierEngine.classify(no.getBytes(StandardCharsets.UTF_8), no, 1, sampleRules, "sha256:test")
      res match {
        case ClassifyResult.Quarantined(bytes, reasons, _) =>
          assert(reasons.contains("NOT_AN_OBJECT"), s"$no must have reason NOT_AN_OBJECT")
          val str = new String(bytes, StandardCharsets.UTF_8).trim
          assert(str.contains("\"reasons\":[\"NOT_AN_OBJECT\"]"))
        case other => throw new AssertionError(s"Expected Quarantined, got $other for $no")
      }
    }
    println("  [PASS] Acceptance 7: Valid JSON array/string/number line: quarantined NOT_AN_OBJECT, counted as record")
  }

  def testTruncatedUtf8AtEof(): Unit = {
    // 0xC2 starts a 2-byte UTF-8 sequence, but has no continuation byte
    val truncated = Array[Byte]('{', '}', '\n', 0xC2.toByte)
    withTempFile(truncated) { path =>
      var caught = false
      try InputModel.preflight(path)
      catch {
        case e: InputError if e.code == "INVALID_UTF8" => caught = true
      }
      assert(caught, "Truncated multi-byte UTF-8 sequence at EOF must fail with INVALID_UTF8")
    }
    println("  [PASS] Incomplete multi-byte UTF-8 sequence at EOF fails with INVALID_UTF8")
  }

  def main(args: Array[String]): Unit = {
    runAll()
  }
}
