package spinal.lib.eda.yosys

import java.io._
//import scala.tools.nsc.io._
import java.io.PrintWriter
import scala.sys.process._
import scala.collection._

trait Executable{
  val logFile : String = "verbose.log"
  val logPath : String = "logs"
  val workDir : String
  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")
  def runCommand : String = this.toString

  def run(): Unit ={
    val wrkDir = new File(workDir)
    val lgDir = new File(wrkDir,logPath)
    val lgFile = new File(lgDir,logFile)
    lgDir.mkdir
    val str = new PrintWriter(lgFile)

    str.println("WORDIR: " + wrkDir.toString())
    str.println("COMMAND: "+ runCommand)
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
    if(isWindows)
      Process("cmd /C " + this.runCommand,wrkDir) !(log)
    else
      Process(this.runCommand,wrkDir) !(log)
      str.close()
  }

}

trait Makeable{
  def target: Seq[String]
  def prerequisite: Seq[String]
  def makeCommand : String = this.toString
  def makefile : String = {
    val targets = target.mkString(""," "," : ")
    val prerequisites = prerequisite.mkString(" ")
    targets + prerequisites + "\n\t" + makeCommand
  }
}