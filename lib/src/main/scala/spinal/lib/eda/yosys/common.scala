package spinal.lib.eda.yosys

import java.io._
import java.io.PrintWriter
import scala.sys.process._
import scala.collection._
import java.nio.file.{Path, Paths}

trait Executable{
  val dir: Option[Path] = Some(Paths.get("."))
  val logFile: Option[Path]

  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")

  /** The command that will be run, default to this.toString */
  def runComand: String = this.toString

  def run(opt:String*): Int = {
    //val dir = if(logFile.nonEmpty) Option(logFile.get.getParent) else None
    if(dir.nonEmpty) dir.get.toFile.mkdir

    // val str = new PrintWriter(logFile.getOrElse.toFile)
    val str = if(logFile.nonEmpty) Option(new PrintWriter(logFile.get.toFile)) else None

    val log = new ProcessLogger {
      override def err(s: => String): Unit = {
        if(str.nonEmpty) str.get.println(s)
        stderr.println(s)
      }
      override def out(s: => String): Unit = {
        if(str.nonEmpty) str.get.println(s) else
        stdout.println(s)
      }
      override def buffer[T](f: => T) = f
    }
    val ret: Int = if (isWindows)
                Process("cmd /C " + this.runComand + opt.mkString(" "," ",""), dir.getOrElse(Paths.get(".").normalize()).toFile) ! (log)
              else
                Process(this.runComand + opt.mkString(" "," ",""), dir.getOrElse(Paths.get(".")).toFile) ! (log)
    if(logFile.nonEmpty) str.get.close()
    ret
  }
}