package spinal.lib.eda.yosys

import java.io.File
import java.io.PrintWriter
import scala.sys.process._

trait Executable{
  val logFile : String = "verbose.log"
  val workDir : String
  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")

  def run(): Unit ={
    val str = new PrintWriter(new File(workDir,logFile))
    str.println(this.toString)
    val log = new ProcessLogger {
      override def err(s: => String): Unit = {
        str.println(s)
        stderr.println(s)
      }
      override def out(s: => String): Unit = {
        str.println(s)
        stdout.println(s)
      }
      override def buffer[T](f: => T) = f
    }
    if(isWindows)
      Process("cmd /C " + this.toString) !(log)
    else
      Process(this.toString) !(log)
      str.close()
  }

}