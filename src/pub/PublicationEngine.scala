package sift.pub

import java.nio.file.{Files, LinkOption, Path, Paths, StandardCopyOption, StandardOpenOption}
import java.nio.channels.{FileChannel, FileLock}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.concurrent.ConcurrentHashMap
import com.fasterxml.jackson.core.{JsonFactory, JsonToken}

case class PubError(code: String, message: String) extends RuntimeException(s"[$code] $message")

trait FaultHook {
  def crashAt(point: String): Unit
}
object NoOpFaultHook extends FaultHook {
  def crashAt(point: String): Unit = ()
}

object PublicationEngine {
  private val activeLocks = ConcurrentHashMap.newKeySet[String]()
  private val PointerRegex = """^runs/([0-9a-f]{64})/clean\.jsonl$""".r

  def withLock[T](outDir: Path)(action: FileChannel => T): T = {
    Files.createDirectories(outDir)
    val realOut = outDir.toRealPath()
    val key = realOut.toString
    if (!activeLocks.add(key)) throw PubError("RUN_IN_PROGRESS", s"In-process lock held for $key")

    val lockFile = realOut.resolve(".sift-lock")
    val channel = FileChannel.open(lockFile, StandardOpenOption.CREATE, StandardOpenOption.READ, StandardOpenOption.WRITE)
    val fileLock = channel.tryLock()
    if (fileLock == null) {
      activeLocks.remove(key)
      channel.close()
      throw PubError("RUN_IN_PROGRESS", s"OS file lock held for $lockFile")
    }

    try {
      recoverCrash(realOut, channel)
      action(channel)
    } finally {
      try { if (fileLock != null && fileLock.isValid) fileLock.release() } finally channel.close()
      activeLocks.remove(key)
    }
  }

  def recoverCrash(out: Path, ch: FileChannel): Unit = {
    ch.position(0)
    val buf = java.nio.ByteBuffer.allocate(1024)
    val n = ch.read(buf)
    if (n > 0) {
      val state = new String(buf.array(), 0, n, StandardCharsets.UTF_8).trim
      if (state.startsWith("replace-live ")) {
        val parts = state.split(" ")
        if (parts.length == 3) {
          val (runId, tombName) = (parts(1), parts(2))
          val runs = out.resolve("runs")
          val runDir = runs.resolve(runId)
          val tomb = runs.resolve(tombName)
          if (isCompleteRun(runDir)) {
            updateCurrent(out, runId)
            if (Files.exists(tomb, LinkOption.NOFOLLOW_LINKS)) deleteRecursive(tomb)
          } else if (Files.exists(tomb, LinkOption.NOFOLLOW_LINKS)) {
            if (Files.exists(runDir, LinkOption.NOFOLLOW_LINKS)) deleteRecursive(runDir)
            Files.move(tomb, runDir, StandardCopyOption.ATOMIC_MOVE)
          }
        }
      }
      ch.truncate(0)
      ch.force(true)
    }
  }

  def publish(
    outDir: Path,
    runId: String,
    tmpDir: Path,
    isReleased: Boolean,
    forceRerun: Boolean,
    replaceLive: Boolean,
    lockChannel: FileChannel,
    hook: FaultHook = NoOpFaultHook
  ): Path = {
    val realOut = outDir.toRealPath()
    val runs = realOut.resolve("runs")
    Files.createDirectories(runs)
    val runDir = runs.resolve(runId)
    val liveRunIdOpt = readAndValidateCurrent(realOut)

    fsyncDirectoryAndContents(tmpDir)

    if (isReleased) {
      if (Files.exists(runDir, LinkOption.NOFOLLOW_LINKS)) {
        if (!forceRerun) {
          throw PubError("RUN_ALREADY_EXISTS", s"Run directory $runDir already exists. Use --force-rerun.")
        }
        if (liveRunIdOpt.contains(runId) && !replaceLive) {
          throw PubError("REPLACE_LIVE_REQUIRED", "--replace-live required to replace live release")
        }
        val tomb = nextAvailable(runs, s"$runId.replaced")
        val tombName = tomb.getFileName.toString

        // 1. Truncate and record transition before moving tombstone
        lockChannel.position(0)
        lockChannel.truncate(0)
        lockChannel.write(java.nio.ByteBuffer.wrap(s"replace-live $runId $tombName\n".getBytes(StandardCharsets.UTF_8)))
        lockChannel.force(true)

        Files.move(runDir, tomb, StandardCopyOption.ATOMIC_MOVE)
        fsyncDirectory(runs)
        hook.crashAt("AFTER_TOMBSTONE_MOVE")

        Files.move(tmpDir, runDir, StandardCopyOption.ATOMIC_MOVE)
        fsyncDirectory(runs)
        hook.crashAt("AFTER_REPLACE_RUN_MOVE")

        updateCurrent(realOut, runId, hook)
        lockChannel.truncate(0)
        lockChannel.force(true)

        deleteRecursive(tomb)
        fsyncDirectory(runs)
        runDir
      } else {
        Files.move(tmpDir, runDir, StandardCopyOption.ATOMIC_MOVE)
        fsyncDirectory(runs)
        hook.crashAt("AFTER_NEW_RUN_MOVE")
        updateCurrent(realOut, runId, hook)
        runDir
      }
    } else {
      // Publication invariant (§8): bare runs/<run_id> NEVER contains blocked output
      val blockedDir = nextAvailable(runs, s"$runId.blocked")
      Files.move(tmpDir, blockedDir, StandardCopyOption.ATOMIC_MOVE)
      fsyncDirectory(runs)
      hook.crashAt("AFTER_BLOCKED_MOVE")
      blockedDir
    }
  }

  def fsyncFile(p: Path): Unit = {
    val ch = FileChannel.open(p, StandardOpenOption.READ)
    try ch.force(true) finally ch.close()
  }

  def fsyncDirectory(dir: Path): Unit = {
    try {
      val ch = FileChannel.open(dir, StandardOpenOption.READ)
      try ch.force(true) finally ch.close()
    } catch {
      case _: Exception => ()
    }
  }

  def fsyncDirectoryAndContents(dir: Path): Unit = {
    val stream = Files.newDirectoryStream(dir)
    try {
      stream.forEach { f =>
        if (Files.isRegularFile(f, LinkOption.NOFOLLOW_LINKS)) fsyncFile(f)
      }
    } finally stream.close()
    fsyncDirectory(dir)
  }

  def updateCurrent(out: Path, runId: String, hook: FaultHook = NoOpFaultHook): Unit = {
    val curTmp = out.resolve("current.tmp")
    Files.writeString(curTmp, s"runs/$runId/clean.jsonl\n", StandardCharsets.UTF_8)
    fsyncFile(curTmp)
    hook.crashAt("AFTER_POINTER_TMP_WRITE")
    Files.move(curTmp, out.resolve("current"), StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    fsyncDirectory(out)
  }

  def readAndValidateCurrent(out: Path): Option[String] = {
    val cur = out.resolve("current")
    if (!Files.exists(cur, LinkOption.NOFOLLOW_LINKS)) return None
    if (Files.isSymbolicLink(cur)) throw PubError("POINTER_CORRUPTION", "current pointer cannot be a symlink")
    val content = Files.readString(cur, StandardCharsets.UTF_8)
    if (!content.endsWith("\n")) throw PubError("POINTER_CORRUPTION", "current pointer must end with newline")
    content.stripLineEnd match {
      case PointerRegex(id) =>
        val target = out.resolve("runs").resolve(id)
        if (!isCompleteRun(target)) throw PubError("POINTER_CORRUPTION", s"Dangling pointer to incomplete run $id")
        Some(id)
      case _ =>
        throw PubError("POINTER_CORRUPTION", s"Malformed current pointer format: $content")
    }
  }

  def validatePathContainment(input: Path, rules: Path, out: Path): Unit = {
    val realOut = out.toRealPath()
    val realIn = input.toRealPath()
    val realRules = rules.toRealPath()
    if (realIn.startsWith(realOut)) throw PubError("INPUT_INSIDE_OUTPUT_DIR", s"Input inside out dir: $realIn")
    if (realRules.startsWith(realOut)) throw PubError("RULES_INSIDE_OUTPUT_DIR", s"Rules inside out dir: $realRules")
  }

  def isCompleteRun(dir: Path): Boolean = {
    if (!Files.isDirectory(dir, LinkOption.NOFOLLOW_LINKS)) return false
    val clean = dir.resolve("clean.jsonl")
    val quar = dir.resolve("quarantine.jsonl")
    val receipt = dir.resolve("receipt.json")
    if (!Files.isRegularFile(clean, LinkOption.NOFOLLOW_LINKS) ||
        !Files.isRegularFile(quar, LinkOption.NOFOLLOW_LINKS) ||
        !Files.isRegularFile(receipt, LinkOption.NOFOLLOW_LINKS)) return false

    // Verify receipt is non-empty and syntactically valid JSON
    try {
      if (Files.size(receipt) == 0) return false
      val parser = new JsonFactory().createParser(Files.newInputStream(receipt))
      try {
        var hasRunId = false
        while (parser.nextToken() != null) {
          if (parser.currentToken() == JsonToken.FIELD_NAME && parser.currentName() == "run_id") {
            hasRunId = true
          }
        }
        hasRunId
      } finally parser.close()
    } catch {
      case _: Exception => false
    }
  }

  def revalidateRun(dir: Path, expectedRunId: String): Unit = {
    if (!isCompleteRun(dir)) throw PubError("CORRUPT_RUN", s"Incomplete run directory at $dir")
    val parsed = parseReceiptStrings(dir.resolve("receipt.json"))
    if (parsed.getOrElse("run_id", "") != s"sha256:$expectedRunId") {
      throw PubError("REUSE_REVALIDATION_FAILED", s"Receipt run_id mismatch in $dir")
    }
    val actualCleanSha = s"sha256:${sha256File(dir.resolve("clean.jsonl"))}"
    val actualQuarSha = s"sha256:${sha256File(dir.resolve("quarantine.jsonl"))}"
    if (parsed.getOrElse("clean_sha256", "") != actualCleanSha) {
      throw PubError("REUSE_REVALIDATION_FAILED", s"Tampered clean.jsonl in $dir. Use --force-rerun.")
    }
    if (parsed.getOrElse("quarantine_sha256", "") != actualQuarSha) {
      throw PubError("REUSE_REVALIDATION_FAILED", s"Tampered quarantine.jsonl in $dir. Use --force-rerun.")
    }
  }

  def nextAvailable(parent: Path, prefix: String): Path = {
    var n = 1
    while (Files.exists(parent.resolve(s"$prefix-$n"), LinkOption.NOFOLLOW_LINKS)) n += 1
    parent.resolve(s"$prefix-$n")
  }

  def deleteRecursive(p: Path): Unit = {
    if (Files.exists(p, LinkOption.NOFOLLOW_LINKS)) {
      if (Files.isDirectory(p, LinkOption.NOFOLLOW_LINKS) && !Files.isSymbolicLink(p)) {
        val stream = Files.newDirectoryStream(p)
        try stream.forEach(deleteRecursive) finally stream.close()
      }
      Files.deleteIfExists(p)
    }
  }

  private def parseReceiptStrings(receiptFile: Path): Map[String, String] = {
    val parser = new JsonFactory().createParser(Files.newInputStream(receiptFile))
    val map = scala.collection.mutable.Map[String, String]()
    var currentField = ""
    try {
      while (parser.nextToken() != null) {
        val token = parser.currentToken()
        if (token == JsonToken.FIELD_NAME) currentField = parser.currentName()
        else if (token == JsonToken.VALUE_STRING) map(currentField) = parser.getText()
      }
    } finally parser.close()
    map.toMap
  }

  private def sha256File(p: Path): String = {
    val in = Files.newInputStream(p)
    val md = MessageDigest.getInstance("SHA-256")
    val buf = new Array[Byte](65536)
    try {
      var n = in.read(buf)
      while (n != -1) { md.update(buf, 0, n); n = in.read(buf) }
      sift.receipt.ReceiptEngine.sha256(md.digest())
    } finally in.close()
  }
}
