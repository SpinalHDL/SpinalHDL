package spinal.lib.eda.yosys
import scala.collection.mutable._
import java.io.File
import java.io.PrintWriter
import java.nio.file.Paths

import org.apache.commons.io.FileUtils
import spinal.core._
import scala.sys.process._

case class YosysComand(command: String, opt: String*){
  def toList: List[String] = {
    val ret = List(command) ++ opt
    println(ret)
    ret
  }
  override def toString(): String = command + " " + opt.mkString(" ").toString
}

class Yosys(workPath: String = "."){
  val commands = ListBuffer[YosysComand]()

  def addCommand(command: YosysComand*): Unit = commands ++ command
  def addCommand(command: String, opt: String*): Unit = commands += YosysComand(command,opt: _*)

  def append(yosys: Yosys*): Unit = commands ++= yosys.flatMap(_.commands)
  def +(yosys: Yosys): Unit = commands ++= yosys.commands

  def save(file: String) = {
    val sy = new PrintWriter(new File(file))
    sy.write(this.toString)
    sy.close()
  }

  def run(file: String): Unit = {
    save(file)
    Yosys.doCmd(List("yosys","-s",file))
  }

  override def toString(): String = commands.mkString("\n").toString
}

object Yosys{
  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")

  def doCmd(cmd : List[String]): String ={
    val str =  new StringBuilder()
    val log = new ProcessLogger {
      override def err(s: => String): Unit = {
        str ++= s
        stderr.println(s)
      }

      override def out(s: => String): Unit = {
        str ++= s
        stdout.println(s)
      }

      override def buffer[T](f: => T) = f
    }
    if(isWindows)
      Process("cmd /C " + cmd) !(log)
    else
      Process(cmd) !(log)

    return str.toString()
  }

  def read_sv(file: String, top: String): Yosys = {
      val read = new Yosys()
      read.addCommand("read_verilog","-sv",file)
      read.addCommand("prep","-top",top)
      read
  }

  def model(file: String, name: String, mode: String, multiclock: Boolean=false) : Yosys ={
    val modelGen = new Yosys()
    modelGen + read_sv(file,name)
    print(modelGen.commands)
    if(name == "base") modelGen.addCommand("memory_nordff") else modelGen.addCommand("memory_map")
    if(multiclock){
      modelGen.addCommand("clk2fflogic")
    } else {
      modelGen.addCommand("async2sync")
      modelGen.addCommand("chformal","-assume","-early")
    }
    if(mode == Mode.bmc || mode == Mode.prove) modelGen.addCommand("chformal","-live","-fair","-cover","-remove")
    if(mode == Mode.cover) modelGen.addCommand("chformal","-live","-fair","-remove")
    if(mode == Mode.live){
      modelGen.addCommand("chformal","-assert2assume")
      modelGen.addCommand("chformal","-cover","-remove")
    }
    modelGen.addCommand("opt_clean")
    modelGen.addCommand("setundef","-anyseq")
    modelGen.addCommand("opt","-keepdc","-fast")
    modelGen.addCommand("check")
    modelGen.addCommand("hierarchy","-simcheck")

    modelGen.addCommand("write_smt2","-wires",file+".smt2")
    modelGen
  }

}

object testo{
    def main(args: Array[String]): Unit = {
        val command = Yosys.model("testCounter.sv","testCounter",Mode.bmc)
        command.run("ttttt.sy")
        val t = FormalCommand("testCounter.sv.smt2",mode=Mode.bmc, step = 100,dumpVCD="test.vcd",dumpSmtc="trace.smt").toString
        println(t)
        // command.doCmd(List(t))
        // Process(t)
        t.!!
        //"yosys-smtbmc -s z3 --presat --noprogress -t 50 --append 0 --dump-vcd trace.vcd --dump-vlogtb trace_tb.v --dump-smtc trace.smtc testCounter.sv.smt2".!
    }
}
