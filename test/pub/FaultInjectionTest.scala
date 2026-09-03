package sift.pub

import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets

object FaultRunner {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      System.err.println("Usage: FaultRunner <outDir> <crashPoint>")
      Runtime.getRuntime.halt(1)
    }
    val out = Paths.get(args(0))
    val crashPoint = args(1)
    val runId = "07ad000000000000000000000000000000000000000000000000000000000000"

    val hook = new FaultHook {
      override def crashAt(point: String): Unit = {
        if (point == crashPoint) {
          // kill -9: immediate abrupt OS process death
          Runtime.getRuntime.halt(137)
        }
      }
    }

    PublicationEngine.withLock(out) { ch =>
      if (crashPoint == "AFTER_POINTER_TMP_WRITE") {
        PublicationEngine.updateCurrent(out, "ffff000000000000000000000000000000000000000000000000000000000000", hook)
      } else {
        val tmp = out.resolve(s"tmp-$runId")
        FaultInjectionTest.createDummyRun(tmp, runId, "{\"v\":2}\n")
        PublicationEngine.publish(out, runId, tmp, isReleased = true, forceRerun = true, replaceLive = true, ch, hook)
      }
    }
  }
}

object FaultInjectionTest {

  def withTempDir[T](f: Path => T): T = {
    val dir = Files.createTempDirectory("sift-proc-fault-")
    try f(dir)
    finally {
      PublicationEngine.deleteRecursive(dir)
    }
  }

  def createDummyRun(dir: Path, runId: String, cleanContent: String = "{\"a\":1}\n"): Unit = {
    Files.createDirectories(dir)
    Files.writeString(dir.resolve("clean.jsonl"), cleanContent)
    Files.writeString(dir.resolve("quarantine.jsonl"), "")
    val cleanSha = "sha256:" + computeSha256(cleanContent.getBytes(StandardCharsets.UTF_8))
    val quarSha = "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    val receipt = s"""{"run_id":"sha256:$runId","clean_sha256":"$cleanSha","quarantine_sha256":"$quarSha"}"""
    Files.writeString(dir.resolve("receipt.json"), receipt)
  }

  def computeSha256(bytes: Array[Byte]): String = {
    val md = java.security.MessageDigest.getInstance("SHA-256")
    md.update(bytes)
    md.digest().map("%02x".format(_)).mkString
  }

  def spawnCrashingProcess(out: Path, crashPoint: String): Int = {
    val javaHome = System.getProperty("java.home")
    val javaBin = Paths.get(javaHome).resolve("bin").resolve("java").toString
    val classPath = System.getProperty("java.class.path")

    val pb = new ProcessBuilder(
      javaBin,
      "-cp", classPath,
      "sift.pub.FaultRunner",
      out.toAbsolutePath.toString,
      crashPoint
    )
    pb.redirectError(ProcessBuilder.Redirect.INHERIT)
    val proc = pb.start()
    proc.waitFor()
  }

  def runAll(): Unit = {
    testProcessCrashAfterTombstoneMove()
    testProcessCrashAfterReplaceRunMove()
    testProcessCrashAfterPointerTmpWrite()
    println("All FaultInjectionTest true OS-kill tests passed successfully!")
  }

  def testProcessCrashAfterTombstoneMove(): Unit = {
    withTempDir { out =>
      val runId = "07ad000000000000000000000000000000000000000000000000000000000000"
      val runDir = out.resolve("runs").resolve(runId)
      createDummyRun(runDir, runId, "{\"v\":1}\n")
      PublicationEngine.updateCurrent(out, runId)

      // Sub-process executes and is abruptly killed with halt(137)
      val exitCode = spawnCrashingProcess(out, "AFTER_TOMBSTONE_MOVE")
      assert(exitCode == 137, s"Expected process to be killed with 137, got $exitCode")

      // Verify that after abrupt OS process death, the old release was moved to tombstone
      assert(!Files.exists(runDir), "Old release was moved to tombstone prior to kill")

      // Next startup: withLock recovers the tombstone
      PublicationEngine.withLock(out) { _ => () }

      // Invariant: tombstone restored back to runDir, current pointer points to valid release
      assert(Files.exists(runDir), "Crash recovery must restore tombstone back to runs/<run_id>/")
      assert(Files.readString(runDir.resolve("clean.jsonl")) == "{\"v\":1}\n", "Restored release is v1")
      val curTarget = PublicationEngine.readAndValidateCurrent(out)
      assert(curTarget.contains(runId), "current must still resolve to valid run")
      println("  [PASS] Acceptance 22 (Point 1): Real kill -9 AFTER_TOMBSTONE_MOVE restores old release and preserves current")
    }
  }

  def testProcessCrashAfterReplaceRunMove(): Unit = {
    withTempDir { out =>
      val runId = "07ad000000000000000000000000000000000000000000000000000000000000"
      val runDir = out.resolve("runs").resolve(runId)
      createDummyRun(runDir, runId, "{\"v\":1}\n")
      PublicationEngine.updateCurrent(out, runId)

      // Sub-process killed with halt(137) after new run is in place
      val exitCode = spawnCrashingProcess(out, "AFTER_REPLACE_RUN_MOVE")
      assert(exitCode == 137, s"Expected process to be killed with 137, got $exitCode")

      // Next startup: withLock recovers
      PublicationEngine.withLock(out) { _ => () }

      // Invariant: new release v2 finalized, current pointer updated, tombstone removed
      assert(Files.exists(runDir), "New release is in place at runs/<run_id>")
      assert(Files.readString(runDir.resolve("clean.jsonl")) == "{\"v\":2}\n", "Finalized release is v2")
      val curTarget = PublicationEngine.readAndValidateCurrent(out)
      assert(curTarget.contains(runId), "current points to valid run")

      val runs = out.resolve("runs")
      val tombstones = Files.list(runs).filter(p => p.getFileName.toString.contains(".replaced-")).count()
      assert(tombstones == 0, "Tombstones cleaned up after finalization")
      println("  [PASS] Acceptance 22 (Point 2): Real kill -9 AFTER_REPLACE_RUN_MOVE finalizes new release & pointer")
    }
  }

  def testProcessCrashAfterPointerTmpWrite(): Unit = {
    withTempDir { out =>
      val runId = "07ad000000000000000000000000000000000000000000000000000000000000"
      val runDir = out.resolve("runs").resolve(runId)
      createDummyRun(runDir, runId, "{\"v\":1}\n")
      PublicationEngine.updateCurrent(out, runId)
      val initialCurrent = Files.readString(out.resolve("current"))

      val exitCode = spawnCrashingProcess(out, "AFTER_POINTER_TMP_WRITE")
      assert(exitCode == 137, s"Expected process to be killed with 137, got $exitCode")

      // current.tmp exists, but current must remain untouched
      assert(Files.exists(out.resolve("current.tmp")), "current.tmp exists")
      assert(Files.readString(out.resolve("current")) == initialCurrent, "current pointer was never partially updated")
      println("  [PASS] Acceptance 22 (Point 3): Real kill -9 AFTER_POINTER_TMP_WRITE leaves current pointer atomically intact")
    }
  }

  def main(args: Array[String]): Unit = {
    runAll()
  }
}
