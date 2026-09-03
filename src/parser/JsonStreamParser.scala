package sift.parser

import com.fasterxml.jackson.core.{JsonFactory, JsonGenerator, JsonParseException, JsonParser, JsonToken}
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

sealed trait TokenParseResult
object TokenParseResult {
  case class Unparseable(rawText: String, reasons: List[String]) extends TokenParseResult
  case class Quarantined(rawBytes: Array[Byte], reasons: List[String]) extends TokenParseResult
  case class ValidObject(
    rawBytes: Array[Byte],
    topLevelFields: Map[String, ParsedField],
    passthroughWithSift: (String, List[String]) => Array[Byte]
  ) extends TokenParseResult
}

case class ParsedField(
  name: String,
  tokenType: JsonToken,
  lexeme: String,
  isValidInteger: Boolean
)

object JsonStreamParser {
  private val jsonFactory = new JsonFactory()
  jsonFactory.enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)

  // Max safe IEEE 754 53-bit integer: 9007199254740991
  private val MaxSafeInt = BigInt("9007199254740991")
  private val MinSafeInt = BigInt("-9007199254740991")

  def checkIntegerLexeme(lexeme: String): Boolean = {
    if (lexeme.isEmpty) return false
    val (neg, digits) = if (lexeme.startsWith("-")) (true, lexeme.substring(1)) else (false, lexeme)
    if (digits.isEmpty) return false
    if (digits.length > 1 && digits.startsWith("0")) return false
    if (!digits.forall(_.isDigit)) return false
    try {
      val n = BigInt(lexeme)
      n >= MinSafeInt && n <= MaxSafeInt
    } catch {
      case _: NumberFormatException => false
    }
  }

  def parseRecord(rawBytes: Array[Byte]): TokenParseResult = {
    val rawText = new String(rawBytes, StandardCharsets.UTF_8).trim
    var parser: JsonParser = null
    val baos = new ByteArrayOutputStream()
    var gen: JsonGenerator = null

    try {
      parser = jsonFactory.createParser(rawBytes)
      val firstToken = parser.nextToken()
      if (firstToken == null) {
        return TokenParseResult.Unparseable(rawText, List("UNPARSEABLE_LINE"))
      }

      if (firstToken != JsonToken.START_OBJECT) {
        // Non-object valid JSON per §3
        var depth = if (firstToken == JsonToken.START_ARRAY) 1 else 0
        while (depth > 0) {
          val t = parser.nextToken()
          if (t == null) return TokenParseResult.Unparseable(rawText, List("UNPARSEABLE_LINE"))
          t match {
            case JsonToken.START_ARRAY | JsonToken.START_OBJECT => depth += 1
            case JsonToken.END_ARRAY | JsonToken.END_OBJECT => depth -= 1
            case _ => ()
          }
        }
        if (parser.nextToken() != null) {
          return TokenParseResult.Unparseable(rawText, List("UNPARSEABLE_LINE"))
        }
        return TokenParseResult.Quarantined(rawBytes, List("NOT_AN_OBJECT"))
      }

      gen = jsonFactory.createGenerator(baos)
      gen.writeStartObject()

      val fields = scala.collection.mutable.LinkedHashMap[String, ParsedField]()
      var currentField: Option[String] = None
      var hasTopLevelSift = false
      var depth = 1

      var token = parser.nextToken()
      while (token != null && depth > 0) {
        token match {
          case JsonToken.START_OBJECT =>
            depth += 1
            gen.writeStartObject()
            currentField = None

          case JsonToken.END_OBJECT =>
            depth -= 1
            if (depth > 0) {
              gen.writeEndObject()
            }
            currentField = None

          case JsonToken.START_ARRAY =>
            depth += 1
            gen.writeStartArray()
            currentField = None

          case JsonToken.END_ARRAY =>
            depth -= 1
            gen.writeEndArray()
            currentField = None

          case JsonToken.FIELD_NAME =>
            val name = parser.currentName()
            gen.writeFieldName(name)
            if (depth == 1) {
              if (name == "_sift") hasTopLevelSift = true
              currentField = Some(name)
            } else {
              currentField = None
            }

          case JsonToken.VALUE_STRING =>
            val text = parser.getText()
            gen.writeString(text)
            if (depth == 1 && currentField.isDefined) {
              fields(currentField.get) = ParsedField(currentField.get, token, text, isValidInteger = false)
            }
            currentField = None

          case JsonToken.VALUE_NUMBER_INT | JsonToken.VALUE_NUMBER_FLOAT =>
            val lexeme = parser.getText()
            gen.writeNumber(lexeme)
            if (depth == 1 && currentField.isDefined) {
              val isInt = if (token == JsonToken.VALUE_NUMBER_INT) checkIntegerLexeme(lexeme) else false
              fields(currentField.get) = ParsedField(currentField.get, token, lexeme, isInt)
            }
            currentField = None

          case JsonToken.VALUE_TRUE =>
            gen.writeBoolean(true)
            if (depth == 1 && currentField.isDefined) {
              fields(currentField.get) = ParsedField(currentField.get, token, "true", isValidInteger = false)
            }
            currentField = None

          case JsonToken.VALUE_FALSE =>
            gen.writeBoolean(false)
            if (depth == 1 && currentField.isDefined) {
              fields(currentField.get) = ParsedField(currentField.get, token, "false", isValidInteger = false)
            }
            currentField = None

          case JsonToken.VALUE_NULL =>
            gen.writeNull()
            if (depth == 1 && currentField.isDefined) {
              fields(currentField.get) = ParsedField(currentField.get, token, "null", isValidInteger = false)
            }
            currentField = None

          case _ => ()
        }

        if (depth > 0) {
          token = parser.nextToken()
        }
      }

      // Check for trailing tokens after root object
      if (parser.nextToken() != null) {
        return TokenParseResult.Unparseable(rawText, List("UNPARSEABLE_LINE"))
      }

      if (hasTopLevelSift) {
        TokenParseResult.Quarantined(rawBytes, List("RESERVED_FIELD__SIFT"))
      } else {
        TokenParseResult.ValidObject(
          rawBytes,
          fields.toMap,
          (sourceRecordId: String, reasons: List[String]) => {
            gen.writeObjectFieldStart("_sift")
            gen.writeStringField("source_record_id", sourceRecordId)
            if (reasons.isEmpty) {
              gen.writeStringField("result", "clean")
            } else {
              gen.writeStringField("result", "quarantined")
              gen.writeArrayFieldStart("reasons")
              reasons.sorted.foreach(gen.writeString)
              gen.writeEndArray()
            }
            gen.writeEndObject()
            gen.writeEndObject()
            gen.flush()
            baos.write('\n')
            baos.toByteArray
          }
        )
      }
    } catch {
      case e: JsonParseException =>
        val msg = e.getMessage
        if (msg != null && msg.contains("Duplicate field")) {
          TokenParseResult.Quarantined(rawBytes, List("DUPLICATE_JSON_KEY"))
        } else {
          TokenParseResult.Unparseable(rawText, List("UNPARSEABLE_LINE"))
        }
      case _: Exception =>
        TokenParseResult.Unparseable(rawText, List("UNPARSEABLE_LINE"))
    } finally {
      if (parser != null) parser.close()
    }
  }

  def validateRulesJson(rulesBytes: Array[Byte]): List[String] = {
    val parser = jsonFactory.createParser(rulesBytes)
    val rootAllowedKeys = Set(
      "schema_version", "required", "string_columns",
      "date_columns", "integer_columns", "release"
    )
    val releaseAllowedKeys = Set("max_quarantine_ratio", "block_if_missing")

    val errors = scala.collection.mutable.ListBuffer[String]()
    try {
      if (parser.nextToken() != JsonToken.START_OBJECT) {
        return List("RULES_NOT_AN_OBJECT")
      }
      var depth = 1
      var insideRelease = false

      while (depth > 0 && parser.nextToken() != null) {
        parser.currentToken() match {
          case JsonToken.START_OBJECT => depth += 1
          case JsonToken.END_OBJECT =>
            if (depth == 2 && insideRelease) insideRelease = false
            depth -= 1
          case JsonToken.START_ARRAY => depth += 1
          case JsonToken.END_ARRAY => depth -= 1
          case JsonToken.FIELD_NAME =>
            val name = parser.currentName()
            if (name == "_sift") {
              errors += "RESERVED_FIELD__SIFT"
            } else if (depth == 1) {
              if (!rootAllowedKeys.contains(name)) errors += s"UNKNOWN_KEY_$name"
              if (name == "release") insideRelease = true
            } else if (depth == 2 && insideRelease) {
              if (!releaseAllowedKeys.contains(name)) errors += s"UNKNOWN_KEY_release.$name"
            }
          case _ => ()
        }
      }
    } catch {
      case e: JsonParseException if e.getMessage != null && e.getMessage.contains("Duplicate field") =>
        errors += "DUPLICATE_JSON_KEY"
      case _: Exception =>
        errors += "INVALID_RULES_JSON"
    } finally {
      parser.close()
    }
    errors.toList
  }
}
