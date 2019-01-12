package spinal.lib.eda.yosys

import java.io.File
import java.io.PrintWriter
import scala.sys.process._
import scala.collection.mutable._

case class YosysComand(command: String, opt: String*){
  def toList: List[String] = {
    val ret = List(command) ++ opt
    println(ret)
    ret
  }
  override def toString(): String = (command +: opt).mkString(" ").toString
}

class Yosys(val workDir: String = "."){
  val logFile = "yosys.log"
  val commands = ListBuffer[YosysComand]()

  def addCommand(command: YosysComand*): Unit = commands ++ command
  def addCommand(command: String, opt: String*): Unit = commands += YosysComand(command,opt: _*)

  def append(yosys: Yosys*): Unit = commands ++= yosys.flatMap(_.commands)
  def +(yosys: Yosys): Yosys = {
    this.append(yosys)
    this
  }

  def save(file: String = "yosys.sy") = {
    val sy = new PrintWriter(new File(workDir,file))
    commands.foreach(c => sy.println(c.toString))
    sy.close()
  }

  def run(file: String = "yosys.sy"): Unit = {                                  // the copypaste of shame
    save(file)
    val isWindows = System.getProperty("os.name").toLowerCase().contains("win")
    val str = new PrintWriter(new File(workDir,logFile))
    str.println(this.toString)
    val log = new ProcessLogger {
      override def err(s: => String): Unit = {
        str.println(s)
        stderr.println(s)
      }
      override def out(s: => String): Unit = {
        str.println(s)
      }
      override def buffer[T](f: => T) = f
    }
    if(isWindows)
      Process("cmd /C " + List("yosys","-s",file), new File(workDir)) !(log)
    else
      Process(List("yosys","-s",file), new File(workDir)) !(log)
      str.close()
  }

  override def toString(): String = commands.mkString("yosys -p'","; ","'") //I want to belive
}

