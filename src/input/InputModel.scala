package sift.input

import java.io.{InputStream, ByteArrayOutputStream}
import java.nio.file.{Files, Path}
import java.nio.charset.{CharsetDecoder, CodingErrorAction, StandardCharsets}
import java.nio.ByteBuffer
import java.security.MessageDigest
import sift.receipt.ReceiptEngine

case class InputError(code: String, message: String) extends RuntimeException(s"[$code] $message")
case class InputLine(lineNumber: Long, rawBytes: Array[Byte], lineText: String, sourceRecordId: String)

object InputModel {

  def preflight(path: Path): String = {
    val in = Files.newInputStream(path)
    val md = MessageDigest.getInstance("SHA-256")
    val dec = StandardCharsets.UTF_8.newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    val buf = new Array[Byte](65536)
    val charBuf = java.nio.CharBuffer.allocate(65536)
    var total = 0L
    var rem = new Array[Byte](0)

    try {
      val head = in.readNBytes(3)
      if (head.length == 3 && (head(0) & 0xFF) == 0xEF && (head(1) & 0xFF) == 0xBB && (head(2) & 0xFF) == 0xBF)
        throw InputError("INVALID_ENCODING_HEADER", "Leading UTF-8 BOM detected")

      if (head.nonEmpty) {
        md.update(head)
        total += head.length
        val byteBuf = ByteBuffer.wrap(head)
        val res = dec.decode(byteBuf, charBuf, false)
        if (res.isError) throw InputError("INVALID_UTF8", "Invalid UTF-8 at byte 1")
        charBuf.clear()
        if (byteBuf.hasRemaining) {
          rem = new Array[Byte](byteBuf.remaining)
          byteBuf.get(rem)
        }
      }

      var n = in.read(buf)
      while (n != -1) {
        md.update(buf, 0, n)
        total += n
        val toDecode = if (rem.isEmpty) ByteBuffer.wrap(buf, 0, n) else {
          val combined = new Array[Byte](rem.length + n)
          System.arraycopy(rem, 0, combined, 0, rem.length)
          System.arraycopy(buf, 0, combined, rem.length, n)
          ByteBuffer.wrap(combined)
        }
        val res = dec.decode(toDecode, charBuf, false)
        if (res.isError) throw InputError("INVALID_UTF8", s"Invalid UTF-8 at byte $total")
        charBuf.clear()
        rem = if (toDecode.hasRemaining) {
          val r = new Array[Byte](toDecode.remaining)
          toDecode.get(r)
          r
        } else Array.emptyByteArray
        n = in.read(buf)
      }
      if (rem.nonEmpty) throw InputError("INVALID_UTF8", "Incomplete UTF-8 sequence at EOF")
      val endRes = dec.decode(ByteBuffer.allocate(0), charBuf, true)
      if (endRes.isError) throw InputError("INVALID_UTF8", "Incomplete UTF-8 sequence at EOF")
      val flushRes = dec.flush(charBuf)
      if (flushRes.isError) throw InputError("INVALID_UTF8", "Incomplete UTF-8 sequence at EOF")
    } finally in.close()

    ReceiptEngine.sha256(md.digest())
  }

  def computeSourceId(inputSha: String, lineNum: Long, rawBytes: Array[Byte]): String = {
    val rawHash = ReceiptEngine.sha256(rawBytes)
    val str = s"sift-source-v1\n$inputSha\n$lineNum\n$rawHash"
    s"sha256:${ReceiptEngine.sha256(str.getBytes(StandardCharsets.UTF_8))}"
  }

  def readLines(path: Path, inputSha: String)(handler: InputLine => Unit): Long = {
    val in = Files.newInputStream(path)
    val buf = new Array[Byte](65536)
    val lineBuf = new ByteArrayOutputStream()
    var lineNum = 0L

    def emitLine(): Unit = {
      lineNum += 1
      val raw = lineBuf.toByteArray
      lineBuf.reset()
      val (textBytes, rawBytes) = if (raw.nonEmpty && raw.last == '\r') (raw.dropRight(1), raw) else (raw, raw)
      val text = new String(textBytes, StandardCharsets.UTF_8)
      val srcId = computeSourceId(inputSha, lineNum, rawBytes)
      handler(InputLine(lineNum, rawBytes, text, srcId))
    }

    try {
      var n = in.read(buf)
      while (n != -1) {
        var i = 0
        var start = 0
        while (i < n) {
          if (buf(i) == '\n') {
            if (i > start) lineBuf.write(buf, start, i - start)
            emitLine()
            start = i + 1
          }
          i += 1
        }
        if (start < n) lineBuf.write(buf, start, n - start)
        n = in.read(buf)
      }
      if (lineBuf.size() > 0) emitLine()
    } finally in.close()

    lineNum
  }
}
