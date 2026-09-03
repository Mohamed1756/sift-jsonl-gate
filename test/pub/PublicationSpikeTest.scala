package sift.pub

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.nio.charset.StandardCharsets

object PublicationSpikeTest {

  def withTempDir[T](f: Path => T): T = {
    val dir = Files.createTempDirectory("sift-pub-test-")
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

  def runAll(): Unit = {
    testAcceptance20_BlockedNeverOccupiesBarePath()
    testAcceptance21_ReplaceLiveWithBlockedReplacement()
    testAcceptance23_PointerCorruption()
    testAcceptance24_CorruptRunReportedNeverDeleted()
    testAcceptance25_TamperedCleanRevalidation()
    testAcceptance26_SameJvmConcurrentRuns()
    testAcceptance27_InputInsideOutputDir()
    println("All PublicationSpikeTest unit tests passed successfully!")
  }

  def testAcceptance20_BlockedNeverOccupiesBarePath(): Unit = {
    withTempDir { out =>
      val runId = "07ad000000000000000000000000000000000000000000000000000000000000"
      val tmp = out.resolve(s"tmp-$runId")
      createDummyRun(tmp, runId)

      PublicationEngine.withLock(out) { ch =>
        val published = PublicationEngine.publish(out, runId, tmp, isReleased = false, forceRerun = false, replaceLive = false, ch)
        assert(published.getFileName.toString == s"$runId.blocked-1", "Must move to .blocked-1")
      }

      val bare = out.resolve("runs").resolve(runId)
      assert(!Files.exists(bare), "Bare path must never exist for blocked run")
      assert(!Files.exists(out.resolve("current")), "current must never exist for blocked run")
      println("  [PASS] Acceptance 20: Blocked run never occupies bare runs/<run_id>/")
    }
  }

  def testAcceptance21_ReplaceLiveWithBlockedReplacement(): Unit = {
    withTempDir { out =>
      val runId = "07ad000000000000000000000000000000000000000000000000000000000000"
      val initialRun = out.resolve("runs").resolve(runId)
      createDummyRun(initialRun, runId, "{\"original\": 1}\n")
      PublicationEngine.updateCurrent(out, runId)

      val originalCurrent = Files.readString(out.resolve("current"))

      // Staged replacement that is blocked
      val tmp = out.resolve(s"tmp-$runId")
      createDummyRun(tmp, runId, "{\"blocked_attempt\": 1}\n")

      PublicationEngine.withLock(out) { ch =>
        val published = PublicationEngine.publish(out, runId, tmp, isReleased = false, forceRerun = true, replaceLive = true, ch)
        assert(published.getFileName.toString == s"$runId.blocked-1")
      }

      // Invariant: original release untouched, current untouched, blocked run at sibling
      assert(Files.readString(initialRun.resolve("clean.jsonl")) == "{\"original\": 1}\n")
      assert(Files.readString(out.resolve("current")) == originalCurrent)
      println("  [PASS] Acceptance 21: --replace-live with blocked replacement leaves old release & pointer untouched")
    }
  }

  def testAcceptance23_PointerCorruption(): Unit = {
    withTempDir { out =>
      Files.createDirectories(out)
      // 1. Missing newline
      Files.writeString(out.resolve("current"), "runs/07ad000000000000000000000000000000000000000000000000000000000000/clean.jsonl")
      var caught = false
      try PublicationEngine.readAndValidateCurrent(out)
      catch case e: PubError if e.code == "POINTER_CORRUPTION" => caught = true
      assert(caught, "Missing newline must fail as POINTER_CORRUPTION")

      // 2. Dangling pointer (target directory missing)
      Files.writeString(out.resolve("current"), "runs/07ad000000000000000000000000000000000000000000000000000000000000/clean.jsonl\n")
      caught = false
      try PublicationEngine.readAndValidateCurrent(out)
      catch case e: PubError if e.code == "POINTER_CORRUPTION" => caught = true
      assert(caught, "Dangling target must fail as POINTER_CORRUPTION")
      println("  [PASS] Acceptance 23: Tampered or dangling current pointer reported as POINTER_CORRUPTION")
    }
  }

  def testAcceptance24_CorruptRunReportedNeverDeleted(): Unit = {
    withTempDir { out =>
      val runId = "07ad000000000000000000000000000000000000000000000000000000000000"
      val runDir = out.resolve("runs").resolve(runId)
      Files.createDirectories(runDir)
      // Missing receipt.json -> incomplete run
      Files.writeString(runDir.resolve("clean.jsonl"), "{}")

      var caught = false
      try PublicationEngine.revalidateRun(runDir, runId)
      catch case e: PubError if e.code == "CORRUPT_RUN" => caught = true
      assert(caught, "Incomplete run must fail with CORRUPT_RUN")
      assert(Files.exists(runDir), "Corrupt run must NEVER be deleted automatically")
      println("  [PASS] Acceptance 24: Corrupt run directory reported, never deleted")
    }
  }

  def testAcceptance25_TamperedCleanRevalidation(): Unit = {
    withTempDir { out =>
      val runId = "07ad000000000000000000000000000000000000000000000000000000000000"
      val runDir = out.resolve("runs").resolve(runId)
      createDummyRun(runDir, runId, "{\"clean\": true}\n")

      // Tamper clean.jsonl
      Files.writeString(runDir.resolve("clean.jsonl"), "{\"tampered\": true}\n", StandardOpenOption.APPEND)

      var caught = false
      try PublicationEngine.revalidateRun(runDir, runId)
      catch case e: PubError if e.code == "REUSE_REVALIDATION_FAILED" => caught = true
      assert(caught, "Tampered output must fail reuse revalidation with REUSE_REVALIDATION_FAILED")
      println("  [PASS] Acceptance 25: Tampered clean file fails reuse revalidation")
    }
  }

  def testAcceptance26_SameJvmConcurrentRuns(): Unit = {
    withTempDir { out =>
      PublicationEngine.withLock(out) { _ =>
        var caught = false
        try {
          PublicationEngine.withLock(out) { _ => () }
        } catch {
          case e: PubError if e.code == "RUN_IN_PROGRESS" => caught = true
        }
        assert(caught, "Concurrent run must fail fast with RUN_IN_PROGRESS")
      }
      println("  [PASS] Acceptance 26: Same-JVM concurrent run fails fast with RUN_IN_PROGRESS")
    }
  }

  def testAcceptance27_InputInsideOutputDir(): Unit = {
    withTempDir { out =>
      val nestedInput = out.resolve("input.jsonl")
      val rules = out.resolve("rules.json")
      Files.createDirectories(out)
      Files.writeString(nestedInput, "{}")
      Files.writeString(rules, "{}")

      var caught = false
      try PublicationEngine.validatePathContainment(nestedInput, rules, out)
      catch case e: PubError if e.code == "INPUT_INSIDE_OUTPUT_DIR" => caught = true
      assert(caught, "Input inside OUT_DIR must fail with INPUT_INSIDE_OUTPUT_DIR")
      println("  [PASS] Acceptance 27: Input path inside OUT_DIR fails with INPUT_INSIDE_OUTPUT_DIR")
    }
  }

  def main(args: Array[String]): Unit = {
    runAll()
  }
}
