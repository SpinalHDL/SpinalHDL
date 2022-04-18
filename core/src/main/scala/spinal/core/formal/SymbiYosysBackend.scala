package spinal.core.formal

import java.io.{File, PrintWriter, BufferedInputStream, FileInputStream, FileFilter}
import java.security.MessageDigest
import org.apache.commons.io.FileUtils

import scala.collection.mutable.{ArrayBuffer, HashMap}
import spinal.sim._ //TODO: decouple the rely on sim package.
import sys.process._

class SymbiYosysBackendConfig {
  val rtlSourcesPaths = ArrayBuffer[String]()
  val rtlIncludeDirs = ArrayBuffer[String]()
  var toplevelName: String = null
  var maxCacheEntries: Int = 100
  var cachePath: String = null
  var workspacePath: String = null
  var workspaceName: String = null
  var waveFormat: WaveFormat = WaveFormat.NONE
  var logPath: String = null
  var logPrefix: String = null
  var customFlags = ArrayBuffer[String]()
}

object SymbiYosysBackend {
  private val cacheGlobalLock = new Object()
  private val cachePathLockMap = HashMap[String, Object]()
}

class SymbiYosysBackend(val config: SymbiYosysBackendConfig) extends Backend {
  import Backend._

  val cachePath = config.cachePath
  val cacheEnabled = cachePath != null
  val maxCacheEntries = config.maxCacheEntries
  val workspaceName = config.workspaceName
  val workspacePath = config.workspacePath
  val smt2FileName = s"${config.toplevelName}.smt2"
  val smt2FilePath = new File(s"${workspacePath}/${workspaceName}/$smt2FileName").getAbsolutePath

  def cacheGlobalSynchronized(function: => Unit) = {
    if (cacheEnabled) {
      SymbiYosysBackend.cacheGlobalLock.synchronized {
        function
      }
    } else {
      function
    }
  }

  def cacheSynchronized(cacheFile: File)(function: => Unit): Unit = {
    if (cacheEnabled) {
      var lock: Object = null
      SymbiYosysBackend.cachePathLockMap.synchronized {
        lock = SymbiYosysBackend.cachePathLockMap.getOrElseUpdate(cacheFile.getCanonicalPath(), new Object())
      }

      lock.synchronized {
        function
      }
    } else {
      function
    }
  }

  def clean(): Unit = {
    FileUtils.deleteQuietly(new File(s"${workspacePath}/${workspaceName}"))
  }

  val availableFormats = Array(WaveFormat.VCD, WaveFormat.DEFAULT, WaveFormat.NONE)

  val format = if (availableFormats contains config.waveFormat) {
    config.waveFormat
  } else {
    println("Wave format " + config.waveFormat + " not supported by Verilator")
    WaveFormat.NONE
  }

  def genSmt2(): Unit = {}

  class Logger extends ProcessLogger {
    override def err(s: => String): Unit = { if (!s.startsWith("ar: creating ")) println(s) }
    override def out(s: => String): Unit = {}
    override def buffer[T](f: => T) = f
  }

  def compileYosys(): Unit = {
    val binFilename = "yosys"
    val topLevel = config.toplevelName
    val readFiles =
      config.rtlSourcesPaths.map(x => s"""read_verilog -sv -formal ${new File(x).getAbsolutePath}; """)

    var yosysCommand = s"""${binFilename} -p'""" +
      readFiles.reduce(_ + _) +
      s"""prep -top ${topLevel}; """ +
      "memory_nordff; " +
      "async2sync; " +
      "chformal -assume -early; " +
      "chformal -live -fair -cover -remove; " +
      "opt_clean; " +
      "setundef -anyseq; " +
      "opt -keepdc -fast; " +
      "dffunmap; " +
      "check; " +
      "hierarchy -simcheck; " +
      s"""write_smt2 ${smt2FilePath}'"""

    val workspaceDir = new File(s"${workspacePath}/${workspaceName}")
    var workspaceCacheDir: File = null
    var hashCacheDir: File = null

    if (cacheEnabled) {
      // calculate hash of yosys version+options and source file contents

      val md = MessageDigest.getInstance("SHA-1")

      md.update(cachePath.getBytes())
      md.update(0.toByte)
      md.update(config.toplevelName.getBytes())
      md.update(0.toByte)
      md.update(config.customFlags.mkString(" ").getBytes())
      md.update(0.toByte)

      val versionProcess = Process(Seq(binFilename, "--version"), new File(workspacePath))
      val version =
        versionProcess.lineStream.mkString("\n") // blocks and throws an exception if exit status != 0
      md.update(version.getBytes())

      def hashFile(md: MessageDigest, file: File) = {
        val bis = new BufferedInputStream(new FileInputStream(file))
        val buf = new Array[Byte](1024)

        Iterator
          .continually(bis.read(buf, 0, buf.length))
          .takeWhile(_ >= 0)
          .foreach(md.update(buf, 0, _))

        bis.close()
      }

      // config.rtlIncludeDirs.foreach { dirname =>
      //   FileUtils.listFiles(new File(dirname), null, true).asScala.foreach { file =>
      //     hashFile(md, file)
      //     md.update(0.toByte)
      //   }

      //   md.update(0.toByte)
      // }

      // config.rtlSourcesPaths.foreach { filename =>
      //   hashFile(md, new File(filename))
      //   md.update(0.toByte)
      // }

      val hash = md.digest().map(x => (x & 0xff).toHexString.padTo(2, '0')).mkString("")
      workspaceCacheDir = new File(s"${cachePath}/${hash}/${workspaceName}")
      hashCacheDir = new File(s"${cachePath}/${hash}")

      cacheGlobalSynchronized {
        // remove old cache entries

        val cacheDir = new File(cachePath)
        if (cacheDir.isDirectory()) {
          if (maxCacheEntries > 0) {
            val cacheEntriesArr = cacheDir
              .listFiles()
              .filter(_.isDirectory())
              .sortWith(_.lastModified() < _.lastModified())

            val cacheEntries = cacheEntriesArr.toBuffer
            val cacheEntryFound = workspaceCacheDir.isDirectory()

            while (
              cacheEntries.length > maxCacheEntries || (!cacheEntryFound && cacheEntries.length >= maxCacheEntries)
            ) {
              if (cacheEntries(0).getCanonicalPath() != hashCacheDir.getCanonicalPath()) {
                cacheSynchronized(cacheEntries(0)) {
                  FileUtils.deleteQuietly(cacheEntries(0))
                }
              }

              cacheEntries.remove(0)
            }
          }
        }
      }
    }

    cacheSynchronized(hashCacheDir) {
      var useCache = false

      if (cacheEnabled) {
        if (workspaceCacheDir.isDirectory()) {
          println("[info] Found cached yosys files")
          useCache = true
        }
      }

      var lastTime = System.currentTimeMillis()

      def bench(msg: String): Unit = {
        val newTime = System.currentTimeMillis()
        val sec = (newTime - lastTime) * 1e-3
        println(msg + " " + sec)
        lastTime = newTime
      }

      val scriptFile = new PrintWriter(new File(workspacePath + "/yosysScript.sh"))
      scriptFile.write(yosysCommand)
      scriptFile.close

      // invoke yosys or copy cached files depending on whether cache is not used or used
      if (!useCache) {
        workspaceDir.mkdir()
        val shCommand = if (isWindows) "sh.exe" else "sh"
        assert(
          Process(Seq(shCommand, "yosysScript.sh"), new File(workspacePath)).!(new Logger()) == 0,
          "Yosys invocation failed"
        )
      } else {
        FileUtils.copyDirectory(workspaceCacheDir, workspaceDir)
      }

      // FileUtils.copyFile(
      //   new File(s"${workspacePath}/${workspaceName}/V${config.toplevelName}${if (isWindows) ".exe" else ""}"),
      //   new File(s"${workspacePath}/${workspaceName}/${workspaceName}_$uniqueId.${if (isWindows) "dll"
      //   else (if (isMac) "dylib"
      //         else "so")}")
      // )

      if (cacheEnabled) {
        // update cache

        if (!useCache) {
          FileUtils.deleteQuietly(workspaceCacheDir)

          // copy only needed files to save disk space
          FileUtils.copyDirectory(
            workspaceDir,
            workspaceCacheDir,
            new FileFilter() {
              def accept(file: File): Boolean = file.getName() == s"${smt2FileName}" || file.getName().endsWith(".smt2")
            }
          )
        }

        FileUtils.touch(hashCacheDir)
      }
    }
  }

  def checks(): Unit = {}

  clean()
  checks()
  compileYosys()

  override def isBufferedWrite: Boolean = false
}
