package sift.scale

import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets
import java.io.BufferedOutputStream
import sift.pub.PublicationEngine

object ScaleProofTest {

  val canonicalRulesJson: String =
    """{
      |  "schema_version": 1,
      |  "required": ["order_id", "order_date", "amount_pence", "currency"],
      |  "string_columns": ["order_id", "currency", "payload"],
      |  "date_columns": ["order_date"],
      |  "integer_columns": ["amount_pence"],
      |  "release": {
      |    "max_quarantine_ratio": 0.0005,
      |    "block_if_missing": ["order_id", "amount_pence"]
      |  }
      |}""".stripMargin

  def main(args: Array[String]): Unit = {
    println("=================================================================")
    println("STARTING ACCEPTANCE TEST 28: 10 GB INPUT IN 256 MB HEAP PROOF")
    println("=================================================================")

    val tempDir = Files.createTempDirectory("sift-scale-test-")
    val inputPath = tempDir.resolve("scale_input_10gb.jsonl")
    val rulesPath = tempDir.resolve("rules.json")
    val outDir = tempDir.resolve("out")

    try {
      Files.writeString(rulesPath, canonicalRulesJson)

      // 10 GB = 10,737,418,240 bytes. Each line ~1 KB (1024 bytes).
      val targetBytes = 10L * 1024L * 1024L * 1024L
      val padding = "X" * 900
      val record = s"""{"order_id":"ORD-SCALE","order_date":"2026-09-02","amount_pence":1299,"currency":"GBP","payload":"$padding"}\n""".getBytes(StandardCharsets.UTF_8)
      val recordLen = record.length
      val chunkMultiplier = 65536 / recordLen
      val chunk = new Array[Byte](chunkMultiplier * recordLen)
      var offset = 0
      for (_ <- 0 until chunkMultiplier) {
        System.arraycopy(record, 0, chunk, offset, recordLen)
        offset += recordLen
      }

      println(s"Generating 10 GB input file at $inputPath...")
      val genStart = System.currentTimeMillis()
      val outStream = new BufferedOutputStream(Files.newOutputStream(inputPath), 1048576)
      val totalChunks = (targetBytes / chunk.length) + 1
      var c = 0L
      while (c < totalChunks) {
        outStream.write(chunk)
        c += 1
      }
      outStream.flush()
      outStream.close()
      val genSec = (System.currentTimeMillis() - genStart) / 1000.0
      val actualGb = Files.size(inputPath).toDouble / (1024.0 * 1024.0 * 1024.0)
      println(f"Generated 10 GB file: $actualGb%.2f GB in $genSec%.2f seconds.")

      // Launch separate JVM with hard -Xmx256m limit
      println("Launching Sift sub-process with -Xmx256m hard heap limit...")
      val javaBin = Paths.get(System.getProperty("java.home"), "bin", "java").toString
      val classpath = System.getProperty("java.class.path")

      val runStart = System.currentTimeMillis()
      val pb = new ProcessBuilder(
        javaBin,
        "-Xmx256m",
        "-Xms256m",
        "-cp",
        classpath,
        "sift.Sift",
        "run",
        "--input",
        inputPath.toAbsolutePath.toString,
        "--rules",
        rulesPath.toAbsolutePath.toString,
        "--out",
        outDir.toAbsolutePath.toString
      )
      pb.inheritIO()
      val proc = pb.start()
      val exitCode = proc.waitFor()
      val runSec = (System.currentTimeMillis() - runStart) / 1000.0

      assert(exitCode == 0, s"Sift failed with exit code $exitCode under -Xmx256m heap")
      val current = outDir.resolve("current")
      assert(Files.exists(current), "current pointer must exist")
      val ptr = Files.readString(current).trim
      val runDir = outDir.resolve(ptr.stripSuffix("/clean.jsonl"))
      val receipt = Files.readString(runDir.resolve("receipt.json"))
      assert(receipt.contains("\"release\":\"released\""), s"Run must be released:\n$receipt")

      println(f"SUCCESS: 10 GB streaming run completed in $runSec%.2f seconds inside a 256 MB heap!")
      println("  [PASS] Acceptance 28: Generated 10 GB input completes in a 256 MB heap")
    } finally {
      println("Cleaning up temporary 10 GB scale files...")
      PublicationEngine.deleteRecursive(tempDir)
      println("Cleanup completed.")
    }
  }
}
