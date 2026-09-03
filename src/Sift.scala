package sift

import java.io.{OutputStream, ByteArrayOutputStream}
import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import sift.input.{InputModel, InputLine, InputError}
import sift.rules.{RulesModel, SiftRules, RulesValidationError}
import sift.parser.{JsonStreamParser, TokenParseResult}
import sift.classify.{ClassifierEngine, ClassifyResult}
import sift.receipt.{ReceiptEngine, ReceiptData, ReceiptError}
import sift.pub.PublicationEngine

case class SiftCliConfig(
  input: Path, rules: Path, out: Path, replaceLive: Boolean, forceRerun: Boolean
)

object Sift {

  def parseArgs(args: Array[String]): SiftCliConfig = {
    if (args.isEmpty || args(0) != "run") {
      throw new IllegalArgumentException("Usage: sift run --input <PATH> --rules <PATH> --out <DIR> [--replace-live] [--force-rerun]")
    }
    var (in, r, out, repLive, force) = (
      Option.empty[Path], Option.empty[Path], Option.empty[Path], false, false
    )
    var i = 1
    while (i < args.length) {
      args(i) match {
        case "--input" if i + 1 < args.length => in = Some(Paths.get(args(i + 1))); i += 2
        case "--rules" if i + 1 < args.length => r = Some(Paths.get(args(i + 1))); i += 2
        case "--out" if i + 1 < args.length => out = Some(Paths.get(args(i + 1))); i += 2
        case "--replace-live" => repLive = true; i += 1
        case "--force-rerun" => force = true; i += 1
        case other => throw new IllegalArgumentException(s"Unknown or invalid argument: $other")
      }
    }
    SiftCliConfig(
      in.getOrElse(throw new IllegalArgumentException("--input is required")),
      r.getOrElse(throw new IllegalArgumentException("--rules is required")),
      out.getOrElse(throw new IllegalArgumentException("--out is required")),
      repLive, force
    )
  }

  def run(cfg: SiftCliConfig): Int = {
    val outDir = cfg.out.toAbsolutePath.normalize()
    Files.createDirectories(outDir)
    PublicationEngine.validatePathContainment(cfg.input, cfg.rules, outDir)

    PublicationEngine.withLock(outDir) { ch =>
      PublicationEngine.recoverCrash(outDir, ch)

      val inPath = cfg.input.toAbsolutePath.normalize()
      val rulesBytes = Files.readAllBytes(cfg.rules)
      val ruleSyntaxErrs = JsonStreamParser.validateRulesJson(rulesBytes)
      if (ruleSyntaxErrs.nonEmpty) throw new RulesValidationError(ruleSyntaxErrs.head, s"Rules syntax error: ${ruleSyntaxErrs.head}")
      val rules = RulesModel.parse(rulesBytes)
      val rulesSha = ReceiptEngine.sha256(rulesBytes)

      val inSha = InputModel.preflight(inPath)
      val runId = ReceiptEngine.computeRunId(inSha, rulesSha)

      val runsDir = outDir.resolve("runs")
      Files.createDirectories(runsDir)
      val targetRunDir = runsDir.resolve(runId)

      // Step 3: Reuse check
      if (Files.isDirectory(targetRunDir) && PublicationEngine.isCompleteRun(targetRunDir) && !cfg.forceRerun) {
        PublicationEngine.revalidateRun(targetRunDir, runId)
        val receiptJson = Files.readString(targetRunDir.resolve("receipt.json"))
        if (receiptJson.contains("\"release\":\"released\"")) 0 else 1
      } else {
        // Step 4: Create temporary directory
        val tmpDir = outDir.resolve(s"tmp-$runId")
        if (Files.exists(tmpDir)) PublicationEngine.deleteRecursive(tmpDir)
        Files.createDirectory(tmpDir)

        val cleanFile = tmpDir.resolve("clean.jsonl")
        val quarFile = tmpDir.resolve("quarantine.jsonl")
        val cleanOut = new java.io.BufferedOutputStream(Files.newOutputStream(cleanFile), 65536)
        val quarOut = new java.io.BufferedOutputStream(Files.newOutputStream(quarFile), 65536)
        val cleanMd = MessageDigest.getInstance("SHA-256")
        val quarMd = MessageDigest.getInstance("SHA-256")

        var (ignoredEmpty, unparseable, cleanRecords, quarRecords) = (0L, 0L, 0L, 0L)
        val quarReasons = scala.collection.mutable.Map[String, Long]()
        var hasBlockedMissing = false

        try {
          val totalLines = InputModel.readLines(inPath, inSha) { line =>
            ClassifierEngine.classify(line.rawBytes, line.lineText, line.lineNumber, rules, line.sourceRecordId) match {
              case ClassifyResult.IgnoredEmpty =>
                ignoredEmpty += 1

              case ClassifyResult.Clean(bytes) =>
                cleanRecords += 1
                cleanOut.write(bytes)
                cleanMd.update(bytes)

              case ClassifyResult.Quarantined(bytes, reasons, blockedMiss) =>
                quarRecords += 1
                if (blockedMiss) hasBlockedMissing = true
                for (r <- reasons) quarReasons(r) = quarReasons.getOrElse(r, 0L) + 1L
                quarOut.write(bytes)
                quarMd.update(bytes)

              case ClassifyResult.Unparseable(bytes, _) =>
                unparseable += 1
                quarOut.write(bytes)
                quarMd.update(bytes)
            }
          }

          cleanOut.flush(); cleanOut.close()
          quarOut.flush(); quarOut.close()

          val cSha = ReceiptEngine.sha256(cleanMd.digest())
          val qSha = ReceiptEngine.sha256(quarMd.digest())

          val receiptData = ReceiptEngine.evaluatePolicy(
            linesRead = totalLines, ignoredEmpty = ignoredEmpty, unparseable = unparseable,
            clean = cleanRecords, quar = quarRecords, quarReasons = quarReasons.toMap,
            hasBlockedMissing = hasBlockedMissing, maxRatio = rules.release.maxQuarantineRatio,
            inSha = inSha, rSha = rulesSha, cSha = cSha, qSha = qSha, runId = runId
          )

          val receiptFile = tmpDir.resolve("receipt.json")
          ReceiptEngine.write(receiptFile, receiptData)

          PublicationEngine.publish(outDir, runId, tmpDir, receiptData.isReleased, cfg.forceRerun, cfg.replaceLive, ch)
          if (receiptData.isReleased) 0 else 1
        } catch {
          case e: Throwable =>
            try { cleanOut.close() } catch { case _: Throwable => () }
            try { quarOut.close() } catch { case _: Throwable => () }
            PublicationEngine.deleteRecursive(tmpDir)
            throw e
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      val cfg = parseArgs(args)
      val code = run(cfg)
      sys.exit(code)
    } catch {
      case e: Throwable =>
        System.err.println(s"Error: ${e.getMessage}")
        sys.exit(2)
    }
  }
}
