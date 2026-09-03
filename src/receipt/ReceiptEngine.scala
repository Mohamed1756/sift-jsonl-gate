package sift.receipt

import com.fasterxml.jackson.core.JsonFactory
import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.math.{BigDecimal => JBigDec, RoundingMode}

case class ReceiptError(code: String, message: String) extends RuntimeException(s"[$code] $message")
case class ReceiptData(
  runId: String, inputSha: String, rulesSha: String, linesRead: Long, ignoredEmpty: Long,
  unparseable: Long, cleanRecords: Long, quarantinedRecords: Long, cleanSha: String,
  quarSha: String, quarantineReasons: Map[String, Long], releaseReasons: List[String],
  isReleased: Boolean, badRatioNum: Long, badRatioDenom: Long, badRatioStr: String
)

object ReceiptEngine {
  private val jsonFactory = new JsonFactory()
  private val Hex = "0123456789abcdef".toCharArray
  private val mdTl = java.lang.ThreadLocal.withInitial[MessageDigest](() => MessageDigest.getInstance("SHA-256"))

  def sha256(b: Array[Byte]): String = {
    val md = mdTl.get(); md.reset()
    val d = md.digest(b); val c = new Array[Char](64)
    var i = 0
    while (i < 32) {
      val v = d(i) & 0xFF
      c(i * 2) = Hex(v >>> 4); c(i * 2 + 1) = Hex(v & 0x0F); i += 1
    }
    new String(c)
  }

  def computeRunId(inSha: String, rSha: String, ver: String = "1.0.0"): String =
    sha256(s"sift-run-v1\n$inSha\n$rSha\n$ver".getBytes(StandardCharsets.UTF_8))

  def isValidSha(h: String): Boolean =
    h.length == 64 && h.forall(c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))

  def evaluatePolicy(
    linesRead: Long, ignoredEmpty: Long, unparseable: Long,
    clean: Long, quar: Long, quarReasons: Map[String, Long],
    hasBlockedMissing: Boolean, maxRatio: Double, inSha: String,
    rSha: String, cSha: String, qSha: String, runId: String
  ): ReceiptData = {
    val (records, denom, num) = (clean + quar, clean + quar + unparseable, quar + unparseable)
    val blockReasons = scala.collection.mutable.ListBuffer[String]()

    val (ratioNum, ratioDenom, ratioStr) = if (denom == 0) {
      blockReasons += "EMPTY_INPUT"; (0L, 1L, "0.000000")
    } else {
      val (nB, dB, mB) = (JBigDec.valueOf(num), JBigDec.valueOf(denom), new JBigDec(maxRatio.toString))
      if (unparseable > 0) blockReasons += "UNPARSEABLE_INPUT"
      if (hasBlockedMissing) blockReasons += "BLOCKED_FIELD_MISSING"
      if (nB.compareTo(mB.multiply(dB)) > 0) blockReasons += "QUARANTINE_RATIO_EXCEEDED"
      (num, denom, nB.divide(dB, 6, RoundingMode.HALF_UP).toPlainString)
    }

    val sortedBlock = blockReasons.distinct.sorted.toList
    ReceiptData(
      runId, inSha, rSha, linesRead, ignoredEmpty, unparseable, clean, quar,
      cSha, qSha, quarReasons, sortedBlock, sortedBlock.isEmpty, ratioNum, ratioDenom, ratioStr
    )
  }

  def verifyInvariants(r: ReceiptData): Unit = {
    if (r.linesRead != r.cleanRecords + r.quarantinedRecords + r.unparseable + r.ignoredEmpty)
      throw ReceiptError("INVARIANT_VIOLATION", "I1: lines_read mismatch")
    if (r.badRatioNum != r.quarantinedRecords + r.unparseable || r.badRatioDenom <= 0 || r.badRatioNum > r.badRatioDenom)
      throw ReceiptError("INVARIANT_VIOLATION", "I3-I6: ratio math mismatch")
    if (r.isReleased != r.releaseReasons.isEmpty)
      throw ReceiptError("INVARIANT_VIOLATION", "I12: release state mismatch")
    if (!isValidSha(r.runId) || !isValidSha(r.inputSha) || !isValidSha(r.rulesSha) || !isValidSha(r.cleanSha) || !isValidSha(r.quarSha))
      throw ReceiptError("INVARIANT_VIOLATION", "Invalid SHA-256 hash in receipt")
  }

  def write(dest: Path, r: ReceiptData): Unit = {
    verifyInvariants(r)
    val g = jsonFactory.createGenerator(Files.newOutputStream(dest))
    g.writeStartObject()
    g.writeStringField("bad_ratio", r.badRatioStr)
    g.writeNumberField("bad_ratio_denominator", r.badRatioDenom)
    g.writeNumberField("bad_ratio_numerator", r.badRatioNum)
    g.writeNumberField("clean_records", r.cleanRecords)
    g.writeNumberField("ignored_empty_lines", r.ignoredEmpty)
    g.writeStringField("input_sha256", s"sha256:${r.inputSha}")
    g.writeNumberField("lines_read", r.linesRead)
    g.writeObjectFieldStart("outputs")
    g.writeStringField("clean", s"runs/${r.runId}/clean.jsonl")
    g.writeStringField("clean_sha256", s"sha256:${r.cleanSha}")
    g.writeStringField("quarantine", s"runs/${r.runId}/quarantine.jsonl")
    g.writeStringField("quarantine_sha256", s"sha256:${r.quarSha}")
    g.writeEndObject()
    g.writeObjectFieldStart("quarantine_reasons")
    r.quarantineReasons.toSeq.sortBy(_._1).foreach { case (k, v) => g.writeNumberField(k, v) }
    g.writeEndObject()
    g.writeNumberField("quarantined_records", r.quarantinedRecords)
    g.writeNumberField("records_read", r.cleanRecords + r.quarantinedRecords)
    g.writeStringField("release", if (r.isReleased) "released" else "blocked")
    g.writeArrayFieldStart("release_reasons")
    r.releaseReasons.sorted.foreach(g.writeString); g.writeEndArray()
    g.writeStringField("rules_sha256", s"sha256:${r.rulesSha}")
    g.writeStringField("run_id", s"sha256:${r.runId}")
    g.writeNumberField("schema_version", 1)
    g.writeStringField("sift_version", "1.0.0")
    g.writeNumberField("unparseable_lines", r.unparseable)
    g.writeEndObject(); g.flush(); g.close()
  }
}
