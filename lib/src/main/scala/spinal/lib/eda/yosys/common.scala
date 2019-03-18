package spinal.lib.eda.yosys

import java.io._
import java.io.PrintWriter
import scala.sys.process._
import scala.collection._
import java.nio.file.{Path, Paths}

trait WorkDir{
  def workDir: String
  // def getPath(rel: String): File = new File(workDir, rel)
  def getPath(rel: String) = Paths.get(".",workDir,rel).normalize.toString
  def createDir: Unit = new File(workDir).mkdir()
}

trait Executable extends WorkDir{
  val logFile: String = "verbose.log"
  val logPath: String = "logs"

  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")
  def runComand: String = this.toString

  def run(): Int = {
    val dir = new File(workDir)
    dir.mkdir
    val lgDir = new File(dir, logPath)
    lgDir.mkdir
    val lgFile = new File(lgDir, logFile)
    val str = new PrintWriter(lgFile)

    str.println("WORDIR: " + dir.toString())
    str.println("COMMAND: " + runComand)
    val log = new ProcessLogger {
      override def err(s: => String): Unit = {
        str.println(s)
        stderr.println(s)
      }
      override def out(s: => String): Unit = {
        str.println(s)
        //stdout.println(s)
      }
      override def buffer[T](f: => T) = f
    }
    val ret: Int = if (isWindows)
                Process("cmd /C " + this.runComand, dir) ! (log)
              else
                Process(this.runComand, dir) ! (log)
    str.close()
    ret
  }

}