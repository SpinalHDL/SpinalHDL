package spinal.lib.eda.yosys
import scala.collection.mutable._
import java.io.File
import java.io.PrintWriter
import java.nio.file.Paths

//import org.apache.commons.io.FileUtils
//import spinal.core._
import scala.sys.process._


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

object testo{
    def main(args: Array[String]): Unit = {
      // val file: String = "testCounter"
      // val command : Yosys = YosysFlow.read_sv(file+".sv","testCounter") + YosysFlow.model("testCounter",Mode.bmc) + YosysFlow.write_smt2(file)
      // command.run("ttttt.sy")
      // val t = FormalCommand("testCounter.smt2",mode=Mode.bmc, step = 100,dumpVCD="test.vcd",dumpSmtc="trace.smt").toString
      // println(t)
      // t.!
      //"yosys-smtbmc -s z3 --presat --noprogress -t 50 --append 0 --dump-vcd trace.vcd --dump-vlogtb trace_tb.v --dump-smtc trace.smtc testCounter.sv.smt2".!
      println(NextPNR_ice40("dddd","fghhgfh"))
    }
}
