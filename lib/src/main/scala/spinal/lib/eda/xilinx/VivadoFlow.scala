package spinal.lib.eda.xilinx

import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.lib.DoCmd.doCmd
import spinal.lib.eda.bench.{Report, Rtl}

import java.io.File
import java.nio.file.Paths
import scala.io.Source

object VivadoFlow {

  /**
   * Use vivado to run eda flow
   *
   * @param vivadoPath      The path to vivado (e.g. /opt/Xilinx/Vivado/2019.2/bin)
   * @param workspacePath   The temporary workspace path (e.g. /tmp/test)
   * @param toplevelPath    The path to top level hdl file
   * @param family          Xilinx device family (Artix 7, Kintex 7, Kintex UltraScale, Kintex UltraScale+ or Virtex UltraScale+)
   * @param device          Xilinx device part
   * @param frequencyTarget Target clock frequency
   * @param processorCount  Number of processor count used
   * @return Report
   */
  def apply(vivadoPath: String, workspacePath: String, rtl: Rtl, family: String, device: String, frequencyTarget: HertzNumber = null, processorCount: Int = 1): Report = {
    val targetPeriod = (if (frequencyTarget != null) frequencyTarget else 400 MHz).toTime

    val workspacePathFile = new File(workspacePath)
    FileUtils.deleteDirectory(workspacePathFile)
    workspacePathFile.mkdir()
    for (file <- rtl.getRtlPaths()) {
      FileUtils.copyFileToDirectory(new File(file), workspacePathFile)
    }

    val isVhdl = (file: String) => file.endsWith(".vhd") || file.endsWith(".vhdl")
    val readRtl = rtl.getRtlPaths().map(file => s"""read_${if(isVhdl(file)) "vhdl" else "verilog"} ${Paths.get(file).getFileName()}""").mkString("\n")

    // generate tcl script
    val tcl = new java.io.FileWriter(Paths.get(workspacePath, "doit.tcl").toFile)
    tcl.write(
s"""${readRtl}
read_xdc doit.xdc

synth_design -part $device -top ${rtl.getTopModuleName()}
opt_design
place_design
route_design

report_utilization
report_timing"""
    )
    tcl.flush();
    tcl.close();

    // generate xdc constraint
    val xdc = new java.io.FileWriter(Paths.get(workspacePath, "doit.xdc").toFile)
    xdc.write(s"""create_clock -period ${(targetPeriod * 1e9) toBigDecimal} [get_ports clk]""")
    xdc.flush();
    xdc.close();

    // run vivado
    doCmd(s"$vivadoPath/vivado -nojournal -log doit.log -mode batch -source doit.tcl", workspacePath)
    val log = Source.fromFile(Paths.get(workspacePath, "doit.log").toFile)
    val report = log.getLines().mkString

    new Report {
      override def getFMax(): Double = {
        val intFind = "-?(\\d+\\.?)+".r
        val slack = try {
          (family match {
            case "Artix 7" | "Kintex 7" | "Kintex UltraScale" | "Kintex UltraScale+" | "Virtex UltraScale+" =>
              intFind.findFirstIn("-?(\\d+.?)+ns  \\(required time - arrival time\\)".r.findFirstIn(report).get).get
          }).toDouble
        } catch {
          case e : Exception => -100000.0
        }
        return 1.0 / (targetPeriod.toDouble - slack * 1e-9)
      }
      override def getArea(): String =  {
        val intFind = "(\\d+,?)+".r
        val leArea = try {
          family match {
            case "Artix 7" | "Kintex 7" =>
              intFind.findFirstIn("Slice LUTs[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " LUT " +
              intFind.findFirstIn("Slice Registers[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " FF "
            case "Kintex UltraScale" | "Kintex UltraScale+" | "Virtex UltraScale+" =>
              intFind.findFirstIn("CLB LUTs[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " LUT " +
              intFind.findFirstIn("CLB Registers[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " FF "
          }
        } catch {
          case e : Exception => "???"
        }
        return leArea
      }
    }
  }
}

