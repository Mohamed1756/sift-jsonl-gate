package sift.classify

import com.fasterxml.jackson.core.{JsonFactory, JsonToken}
import java.io.ByteArrayOutputStream
import java.time.LocalDate
import java.time.format.{DateTimeFormatter, ResolverStyle}
import java.nio.charset.StandardCharsets
import sift.rules.SiftRules
import sift.parser.{JsonStreamParser, TokenParseResult, ParsedField}

sealed trait ClassifyResult
object ClassifyResult {
  case class Clean(bytes: Array[Byte]) extends ClassifyResult
  case class Quarantined(bytes: Array[Byte], reasons: List[String], hasBlockedMissing: Boolean) extends ClassifyResult
  case class Unparseable(rawBytes: Array[Byte], reasons: List[String]) extends ClassifyResult
  case object IgnoredEmpty extends ClassifyResult
}

object ClassifierEngine {
  private val jsonFactory = new JsonFactory()
  private val DateFormatter = DateTimeFormatter.ofPattern("uuuu-MM-dd").withResolverStyle(ResolverStyle.STRICT)

  def checkStrictDate(s: String): Boolean = {
    if (s.length != 10) return false
    var i = 0
    while (i < 10) {
      val c = s.charAt(i)
      if (i == 4 || i == 7) {
        if (c != '-') return false
      } else {
        if (c < '0' || c > '9') return false
      }
      i += 1
    }
    try {
      LocalDate.parse(s, DateFormatter)
      true
    } catch {
      case _: Exception => false
    }
  }

  private def truncateTo8KiB(s: String): String = {
    val b = s.getBytes(StandardCharsets.UTF_8)
    if (b.length <= 8192) s
    else {
      var end = 8192
      while (end > 0 && (b(end) & 0xC0) == 0x80) end -= 1
      new String(b, 0, end, StandardCharsets.UTF_8)
    }
  }

  def formatRawWrapper(
    rawText: String,
    sourceRecordId: String,
    reasons: List[String],
    lineNumber: Long
  ): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val gen = jsonFactory.createGenerator(baos)
    try {
      gen.writeStartObject()
      gen.writeObjectFieldStart("_sift")
      gen.writeStringField("source_record_id", sourceRecordId)
      gen.writeStringField("result", "quarantined")
      gen.writeArrayFieldStart("reasons")
      reasons.sorted.foreach(gen.writeString)
      gen.writeEndArray()
      gen.writeNumberField("line_number", lineNumber)
      gen.writeStringField("raw", truncateTo8KiB(rawText))
      gen.writeEndObject()
      gen.writeEndObject()
      gen.flush()
      baos.write('\n')
      baos.toByteArray
    } finally {
      gen.close()
    }
  }

  def classify(
    rawBytes: Array[Byte],
    lineText: String,
    lineNumber: Long,
    rules: SiftRules,
    sourceRecordId: String
  ): ClassifyResult = {
    if (lineText.isEmpty) return ClassifyResult.IgnoredEmpty
    if (lineText.trim.isEmpty) {
      val wrapped = formatRawWrapper(lineText, sourceRecordId, List("UNPARSEABLE_LINE"), lineNumber)
      return ClassifyResult.Unparseable(wrapped, List("UNPARSEABLE_LINE"))
    }

    JsonStreamParser.parseRecord(rawBytes) match {
      case TokenParseResult.Unparseable(_, reasons) =>
        val wrapped = formatRawWrapper(lineText, sourceRecordId, reasons, lineNumber)
        ClassifyResult.Unparseable(wrapped, reasons)

      case TokenParseResult.Quarantined(_, reasons) =>
        val sortedReasons = reasons.distinct.sorted
        val wrapped = formatRawWrapper(lineText, sourceRecordId, sortedReasons, lineNumber)
        ClassifyResult.Quarantined(wrapped, sortedReasons, hasBlockedMissing = false)

      case TokenParseResult.ValidObject(_, fields, passthrough) =>
        val reasons = scala.collection.mutable.ListBuffer[String]()
        var hasBlockedMissing = false

        // 1. Presence check for required columns
        val missingCols = scala.collection.mutable.Set[String]()
        for (col <- rules.required) {
          fields.get(col) match {
            case None => missingCols += col
            case Some(f) =>
              if (f.tokenType == JsonToken.VALUE_NULL || (f.tokenType == JsonToken.VALUE_STRING && f.lexeme.isEmpty)) {
                missingCols += col
              }
          }
        }
        if (missingCols.nonEmpty) {
          reasons += "MISSING_REQUIRED_FIELD"
          if (missingCols.exists(rules.release.blockIfMissing.contains)) {
            hasBlockedMissing = true
          }
        }

        // 2. String columns (only if present and not missing)
        for (col <- rules.stringColumns if fields.contains(col) && !missingCols.contains(col)) {
          if (fields(col).tokenType != JsonToken.VALUE_STRING) reasons += "INVALID_STRING"
        }

        // 3. Integer columns (only if present and not missing)
        for (col <- rules.integerColumns if fields.contains(col) && !missingCols.contains(col)) {
          val f = fields(col)
          if (f.tokenType != JsonToken.VALUE_NUMBER_INT || !f.isValidInteger) reasons += "INVALID_INTEGER"
        }

        // 4. Date columns (only if present and not missing)
        for (col <- rules.dateColumns if fields.contains(col) && !missingCols.contains(col)) {
          val f = fields(col)
          if (f.tokenType != JsonToken.VALUE_STRING || !checkStrictDate(f.lexeme)) reasons += "INVALID_ISO_DATE"
        }

        if (reasons.isEmpty) {
          ClassifyResult.Clean(passthrough(sourceRecordId, Nil))
        } else {
          val sortedReasons = reasons.distinct.sorted.toList
          val quarBytes = passthrough(sourceRecordId, sortedReasons)
          ClassifyResult.Quarantined(quarBytes, sortedReasons, hasBlockedMissing)
        }
    }
  }
}
