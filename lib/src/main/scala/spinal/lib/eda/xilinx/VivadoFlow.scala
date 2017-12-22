package spinal.lib.eda.xilinx

import java.io.File
import java.nio.file.Paths

import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.lib.eda.bench.Report

import scala.sys.process._

object VivadoFlow {
  def doCmd(cmd : String): Unit ={
    println(cmd)
    if(isWindows)
      Process("cmd /C " + cmd) !
    else
      Process(cmd) !
  }
  def doCmd(cmd : String, path : String): Unit ={
    println(cmd)
    if(isWindows)
      Process("cmd /C " + cmd, new java.io.File(path)) !
    else
      Process(cmd, new java.io.File(path)) !

  }

  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")

  def apply(vivadoPath : String,workspacePath : String,toplevelPath : String,family : String,device : String,frequencyTarget : HertzNumber = null,processorCount : Int = 1) : Report = {
    val projectName = toplevelPath.split("/").last.split("[.]").head
    val targetPeriod = (if(frequencyTarget != null) frequencyTarget else 400 MHz).toTime

    val workspacePathFile = new File(workspacePath)
    FileUtils.deleteDirectory(workspacePathFile)
    workspacePathFile.mkdir()
    FileUtils.copyFileToDirectory(new File(toplevelPath), workspacePathFile)


    val isVhdl = toplevelPath.endsWith(".vhd") || toplevelPath.endsWith(".vhdl")

    val tcl = new java.io.FileWriter(Paths.get(workspacePath,"doit.tcl").toFile)
    tcl.write(
s"""read_${if(isVhdl) "vhdl" else "verilog"} $toplevelPath
read_xdc doit.xdc

synth_design -part $device -top ${toplevelPath.split("\\.").head}
opt_design
place_design
route_design

report_utilization
report_timing"""
    )

    tcl.flush();
    tcl.close();


    val xdc = new java.io.FileWriter(Paths.get(workspacePath,"doit.xdc").toFile)
    xdc.write(s"""create_clock -period ${(targetPeriod*1e9) toBigDecimal} [get_ports clk]""")

    xdc.flush();
    xdc.close();

    doCmd(s"$vivadoPath/vivado -nojournal -log doit.log -mode batch -source doit.tcl", workspacePath)

    new Report{
      override def getFMax(): Double =  {
        import scala.io.Source
        val report = Source.fromFile(Paths.get(workspacePath,"doit.log").toFile).getLines.mkString
        val intFind = "-?(\\d+\\.?)+".r
        val slack = try {
          (family match {
            case "Artix 7" =>
              intFind.findFirstIn("-?(\\d+.?)+ns  \\(required time - arrival time\\)".r.findFirstIn(report).get).get
          }).toDouble
        }catch{
          case e : Exception => -1.0
        }
        return 1.0/(targetPeriod.toDouble-slack*1e-9)
      }
      override def getArea(): String =  {
        import scala.io.Source
        val report = Source.fromFile(Paths.get(workspacePath,"doit.log").toFile).getLines.mkString
        val intFind = "(\\d+,?)+".r
        val leArea = try {
          family match {
            case "Artix 7" =>
              intFind.findFirstIn("Slice LUTs[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " LUT " +
              intFind.findFirstIn("Slice Registers[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " FF "
          }
        }catch{
          case e : Exception => "???"
        }
        return leArea
      }
    }
  }

  def main(args: Array[String]) {
    val report = VivadoFlow(
      vivadoPath="/eda/Xilinx/Vivado/2017.2/bin",
      workspacePath="/home/spinalvm/tmp",
      toplevelPath="TopLevel.vhd",
      family="Artix 7",
      device="xc7k70t-fbg676-3",
      frequencyTarget = 1 MHz
    )
    println(report.getArea())
    println(report.getFMax())
  }
}


object QuartusTest {
  def main(args: Array[String]) {

    Paths.get("~")
  }
}
