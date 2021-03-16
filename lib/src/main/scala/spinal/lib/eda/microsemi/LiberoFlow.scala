package spinal.lib.eda.microsemi

import java.io.File
import java.nio.file.Paths
import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.lib.DoCmd.doCmd
import spinal.lib.StreamFifo
import spinal.lib.eda.bench.Report

import scala.sys.process._

object LiberoFlow {
  def apply(liberoPath : String,workspacePath : String,toplevelPath : String,family : String,device : String,frequencyTarget : HertzNumber = null,processorCount : Int = 1) : Report = {
    val projectName = toplevelPath.split("/").last.split("[.]").head
    val targetPeriod = (if(frequencyTarget != null) frequencyTarget else 100 MHz).toTime

    val workspacePathFile = new File(workspacePath)
    FileUtils.deleteDirectory(workspacePathFile)
    workspacePathFile.mkdir()

    val isVhdl = toplevelPath.endsWith(".vhd") || toplevelPath.endsWith(".vhdl")
    val device_parameters = device.split("-")
    val device_die = device_parameters(0)
    val device_package = device_parameters(1)
    val device_speed = device_parameters(2)

    val tcl = new java.io.FileWriter(Paths.get(workspacePath, "doit.tcl").toFile)
    tcl.write(
s"""new_project\\
  -location {${Paths.get(workspacePath, projectName)}} \\
  -name {$projectName} \\
  -project_description {} \\
  -hdl {${if(isVhdl) "VHDL" else "VERILOG"}} \\
  -family {$family} \\
  -die {$device_die} \\
  -package {$device_package} \\
  -speed {-$device_speed}

create_links \\
  -hdl_source {${new File(toplevelPath).getAbsolutePath}}

run_tool -name {SYNTHESIZE} -script {} -defvar {} -defvars {}

run_tool -name {PLACEROUTE} -script {} -defvar {} -defvars {}

run_tool -name {VERIFYTIMING} -script {} -defvar {} -defvars {}
"""
    )

    tcl.flush();
    tcl.close();

    doCmd(s"$liberoPath/libero SCRIPT:doit.tcl", Paths.get(workspacePath).toString)

    new Report{
      override def getFMax(): Double =  {
        import scala.io.Source
        val report = Source.fromFile(Paths.get(workspacePath, projectName, "designer/impl1" , s"${projectName}_maxdelay_timing_report.txt").toFile).getLines.mkString
        val intFind = "-?(\\d+\\.?)+".r
        val leFreq = try {
          (family match {
            case "ProASIC3E" =>
              intFind.findFirstIn("Frequency[ ]\\(MHz\\)\\:[ ]*(\\d+.\\d+,?)+".r.findFirstIn(report).get).get
          }).toDouble
        }catch{
          case e : Exception => -1.0
        }
        return leFreq*1e6
      }
      override def getArea(): String =  {
        import scala.io.Source
        val report = Source.fromFile(Paths.get(workspacePath, projectName, "designer/impl1" , s"${projectName}_place_and_route_report.txt").toFile).getLines.mkString
        val intFind = "(\\d+\\.?)+".r
        val leArea = try {
          family match {
            case "ProASIC3E" =>
              intFind.findFirstIn("COMB[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " COMB, " +
              intFind.findFirstIn("SEQ[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " SEQ -> " +
              intFind.findAllIn("CORE[ ]*Used:[ ]*\\d+[ ]*Total:[ ]*\\d+[ ]*\\((\\d+.\\d+,?)".r.findFirstIn(report).get).toList(2) + "% Total CORE "
          }
        }catch{
          case e : Exception => "???"
        }
        return leArea
      }
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new StreamFifo(Bits(8 bits), 128).setDefinitionName("fifo128"))
    val report = LiberoFlow(
      liberoPath="C:/eda/Microsemi/Libero_v11.0/Designer/bin",
      workspacePath="E:/tmp/test1",
      toplevelPath="fifo128.vhd",
      family="ProASIC3E",
      device="a3pe3000-fg484-2",
      frequencyTarget = 1 MHz
    )
    println(report.getArea())
    println(report.getFMax())
  }
}
