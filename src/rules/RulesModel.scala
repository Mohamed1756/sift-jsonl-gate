package sift.rules

import com.fasterxml.jackson.core.{JsonFactory, JsonParser, JsonParseException, JsonToken}
import java.nio.charset.StandardCharsets

case class RulesValidationError(code: String, message: String) extends RuntimeException(s"[$code] $message")
case class ReleasePolicy(maxQuarantineRatio: Double, blockIfMissing: Set[String])
case class SiftRules(
  schemaVersion: Int, required: Set[String], stringColumns: Set[String],
  dateColumns: Set[String], integerColumns: Set[String], release: ReleasePolicy
)

object RulesModel {
  private val jsonFactory = new JsonFactory()
  jsonFactory.enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)

  private val RootAllowed = Set("schema_version", "required", "string_columns", "date_columns", "integer_columns", "release")
  private val RelAllowed = Set("max_quarantine_ratio", "block_if_missing")

  def parse(bytes: Array[Byte]): SiftRules = {
    val p = jsonFactory.createParser(bytes)
    var (ver, req, strC, dateC, intC, maxR, blockMiss) = (
      Option.empty[Int], Set[String](), Set[String](), Set[String](), Set[String](), Option.empty[Double], Set[String]()
    )

    try {
      if (p.nextToken() != JsonToken.START_OBJECT) throw RulesValidationError("RULES_NOT_AN_OBJECT", "Rules must be a JSON object")
      var (depth, inRel) = (1, false)

      while (depth > 0 && p.nextToken() != null) {
        p.currentToken() match {
          case JsonToken.START_OBJECT => depth += 1
          case JsonToken.END_OBJECT => if (depth == 2 && inRel) inRel = false; depth -= 1
          case JsonToken.START_ARRAY => depth += 1
          case JsonToken.END_ARRAY => depth -= 1
          case JsonToken.FIELD_NAME =>
            val k = p.currentName()
            if (k == "_sift") throw RulesValidationError("RESERVED_FIELD__SIFT", "Rules cannot contain _sift")
            if (depth == 1) {
              if (!RootAllowed.contains(k)) throw RulesValidationError(s"UNKNOWN_KEY_$k", s"Unknown key: $k")
              val vt = p.nextToken()
              k match {
                case "schema_version" =>
                  if (vt != JsonToken.VALUE_NUMBER_INT) throw RulesValidationError("INVALID_RULE_TYPE", "schema_version must be integer")
                  ver = Some(p.getIntValue)
                case "release" =>
                  if (vt != JsonToken.START_OBJECT) throw RulesValidationError("INVALID_RULE_TYPE", "release must be an object")
                  inRel = true; depth += 1
                case arrField =>
                  if (vt != JsonToken.START_ARRAY) throw RulesValidationError("INVALID_RULE_TYPE", s"$arrField must be an array")
                  depth += 1
                  val arr = readArray(p); depth -= 1
                  arrField match {
                    case "required" => req = arr
                    case "string_columns" => strC = arr
                    case "date_columns" => dateC = arr
                    case "integer_columns" => intC = arr
                  }
              }
            } else if (depth == 2 && inRel) {
              if (!RelAllowed.contains(k)) throw RulesValidationError(s"UNKNOWN_KEY_release.$k", s"Unknown key: release.$k")
              val vt = p.nextToken()
              k match {
                case "max_quarantine_ratio" =>
                  if (vt != JsonToken.VALUE_NUMBER_FLOAT && vt != JsonToken.VALUE_NUMBER_INT)
                    throw RulesValidationError("INVALID_RULE_TYPE", "max_quarantine_ratio must be a number")
                  maxR = Some(p.getDoubleValue)
                case "block_if_missing" =>
                  if (vt != JsonToken.START_ARRAY) throw RulesValidationError("INVALID_RULE_TYPE", "block_if_missing must be an array")
                  depth += 1; blockMiss = readArray(p); depth -= 1
              }
            }
          case _ => ()
        }
      }
    } catch {
      case e: RulesValidationError => throw e
      case e: JsonParseException if e.getMessage != null && e.getMessage.contains("Duplicate field") =>
        throw RulesValidationError("DUPLICATE_JSON_KEY", "Duplicate key detected in rules JSON")
      case e: Exception => throw RulesValidationError("INVALID_RULES_JSON", s"Parse error: ${e.getMessage}")
    } finally p.close()

    validate(ver, req, strC, dateC, intC, maxR, blockMiss)
  }

  private def readArray(p: JsonParser): Set[String] = {
    val items = scala.collection.mutable.Set[String]()
    var t = p.nextToken()
    while (t != null && t != JsonToken.END_ARRAY) {
      if (t == JsonToken.VALUE_STRING) {
        if (!items.add(p.getText())) throw RulesValidationError("DUPLICATE_COLUMN_NAME", s"Duplicate column: ${p.getText()}")
      } else throw RulesValidationError("INVALID_COLUMN_TYPE", "Column list elements must be strings")
      t = p.nextToken()
    }
    items.toSet
  }

  private def validate(
    ver: Option[Int], req: Set[String], strC: Set[String], dateC: Set[String],
    intC: Set[String], maxR: Option[Double], blockMiss: Set[String]
  ): SiftRules = {
    if (!ver.contains(1)) throw RulesValidationError("INVALID_SCHEMA_VERSION", s"schema_version must be 1")
    if (strC.intersect(dateC).nonEmpty || strC.intersect(intC).nonEmpty || dateC.intersect(intC).nonEmpty)
      throw RulesValidationError("OVERLAPPING_COLUMNS", "string_columns, date_columns, and integer_columns must be pairwise disjoint")
    val invalidBlock = blockMiss.diff(req)
    if (invalidBlock.nonEmpty) throw RulesValidationError("INVALID_BLOCK_IF_MISSING", s"block_if_missing not in required: $invalidBlock")
    val r = maxR.getOrElse(throw RulesValidationError("MISSING_MAX_QUARANTINE_RATIO", "max_quarantine_ratio required"))
    if (r < 0.0 || r >= 1.0) throw RulesValidationError("INVALID_MAX_QUARANTINE_RATIO", s"max_quarantine_ratio must be 0 <= r < 1.0")
    SiftRules(1, req, strC, dateC, intC, ReleasePolicy(r, blockMiss))
  }
}
