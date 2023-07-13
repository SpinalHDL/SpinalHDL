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
    val targetPeriod = (if (frequencyTarget != null) frequencyTarget else 500 MHz).toTime

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

synth_design -mode out_of_context -part $device -top ${rtl.getTopModuleName()}
opt_design
place_design
route_design

report_utilization
report_timing_summary -warn_on_violation
report_pulse_width -warn_on_violation -all_violators
report_design_analysis -logic_level_distribution
"""
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
      // Non-logic elements such as PLL or BRAMs may have stricter timing then logic, check for their pulse slack
      // getFMax() will then take this into account later. Uses "report_pulse_width -warn_on_violation -all_violators"
      //
      // Pulse Width Checks
      // <...>
      // Check Type        Corner  Lib Pin             Reference Pin  Required(ns)  Actual(ns)  Slack(ns)  Location      Pin
      // Min Period        n/a     RAMB18E2/CLKARDCLK  n/a            1.569         2.857       1.288      RAMB18_X9Y68  fifo128x32_inst/f/logic_ram_reg/CLKARDCLK
      // Low Pulse Width   Slow    RAMB18E2/CLKARDCLK  n/a            0.542         1.429       0.887      RAMB18_X9Y68  fifo128x32_inst/f/logic_ram_reg/CLKARDCLK
      // High Pulse Width  Slow    RAMB18E2/CLKARDCLK  n/a            0.542         1.429       0.887      RAMB18_X9Y68  fifo128x32_inst/f/logic_ram_reg/CLKARDCLK

      def getPulseSlack(): Double /*nanoseconds*/ = {
        // if not found, assume only logic is involved and do not take pulse slack into account
        var lowest_pulse_slack : Double = 100000.0
        val pulse_strings = "(Min Period|Low Pulse Width|High Pulse Width)(?:\\s+\\S+){5}(?:\\s+)-?(\\d+.?\\d+)+".r.findAllIn(report).toList
        // iterate through pulse slack lines
        for (pulse_string <- pulse_strings) {
          // iterate through number columns
          val pulse_slack_numbers = "\\s-?([0-9]+\\.?[0-9]+)+".r.findAllIn(pulse_string).toList
          // third number column is pulse slack
          if (pulse_slack_numbers.length >= 3) {
            if (pulse_slack_numbers.apply(2).toDouble < lowest_pulse_slack) {
              lowest_pulse_slack = pulse_slack_numbers.apply(2).toDouble
            }
          }
        }       
        return lowest_pulse_slack 
      }
      override def getFMax(): Double = {
        val intFind = "-?(\\d+\\.?)+".r
        var slack = try {
          (family match {
            case "Artix 7" | "Kintex 7" | "Kintex UltraScale" | "Kintex UltraScale+" | "Virtex UltraScale+" =>
              intFind.findFirstIn("-?(\\d+.?)+ns  \\(required time - arrival time\\)".r.findFirstIn(report).get).get
          }).toDouble
        } catch {
          case e : Exception => -100000.0
        }
        val pulse_slack = getPulseSlack()
        if (pulse_slack < slack) {
          slack = pulse_slack
        }
        return 1.0 / (targetPeriod.toDouble - slack * 1e-9)
      }
      override def getArea(): String =  {
        // 0, 30, 0.5, 15,5
        val intFind = "(\\d+,?\\.?\\d*)".r
        val leArea = try {
          family match {
            case "Artix 7" | "Kintex 7" =>
              intFind.findFirstIn("Slice LUTs[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " LUT " +
              intFind.findFirstIn("Slice Registers[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get).get + " FF "
            // Assume the the resources table is the only one with 5 columns (this is the case in Vivado 2021.2)
            // (Not very version-proof, we should actually first look at the right table header first...)
            case "Kintex UltraScale" | "Kintex UltraScale+" | "Virtex UltraScale+" =>
              intFind.findFirstIn("\\| CLB LUTs[ ]*\\|([ ]*\\S+\\s+\\|){5}".r.findFirstIn(report).get).get + " LUT " +
              intFind.findFirstIn("\\| CLB Registers[ ]*\\|([ ]*\\S+\\s+\\|){5}".r.findFirstIn(report).get).get + " FF " +
              intFind.findFirstIn("\\| Block RAM Tile[ ]*\\|([ ]*\\S+\\s+\\|){5}".r.findFirstIn(report).get).get + " BRAM " + 
              intFind.findFirstIn("\\| URAM[ ]*\\|([ ]*\\S+\\s+\\|){5}".r.findFirstIn(report).get).get + " URAM "
          }
        } catch {
          case e : Exception => "???"
        }
        return leArea
      }
    }
  }
}
