package spinal.lib.eda.efinix

import org.apache.commons.io.FileUtils
import spinal.core._
import spinal.lib.DoCmd.doCmd
import spinal.lib.eda.bench.{Report, Rtl}

import java.io.File
import java.nio.file.Paths
import scala.io.Source

object EfinixFlow {

  def apply(efinixPath: String, workspacePath: String, rtl: Rtl, family: String, device: String, timing: String, frequencyTarget: HertzNumber = null): Report = {
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
    val tcl = new java.io.FileWriter(Paths.get(workspacePath, "test.xml").toFile)
    tcl.write(

      s"""
<efx:project xmlns:efx="http://www.efinixinc.com/enf_proj" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" name="test" description="" last_change="1783841893" sw_version="2025.2.288.4.15" last_run_state="pass" last_run_flow="bitstream" config_result_in_sync="sync" design_ood="sync" place_ood="sync" route_ood="sync" xsi:schemaLocation="http://www.efinixinc.com/enf_proj enf_proj.xsd">
    <efx:device_info>
        <efx:family name="$family"/>
        <efx:device name="$device"/>
        <efx:timing_model name="$timing"/>
    </efx:device_info>
    <efx:design_info def_veri_version="verilog_2k" def_vhdl_version="vhdl_2008" unified_flow="false">
        <efx:top_module name="${rtl.getTopModuleName()}" />
        <efx:design_file name="${rtl.getRtlPaths().mkString(" ")}" version="default" library="default" />
        <efx:top_vhdl_arch name="" />
    </efx:design_info>
    <efx:constraint_info>
        <efx:sdc_file name="test.sdc" />
        <efx:inter_file name="" />
    </efx:constraint_info>
    <efx:sim_info />
    <efx:misc_info />
    <efx:ooc_info />
    <efx:ip_info />
    <efx:synthesis tool_name="efx_map">
        <efx:param name="work_dir" value="work_syn" value_type="e_string" />
        <efx:param name="write_efx_verilog" value="on" value_type="e_bool" />
        <efx:param name="allow-const-ram-index" value="0" value_type="e_option" />
        <efx:param name="blackbox-error" value="1" value_type="e_option" />
        <efx:param name="blast_const_operand_adders" value="1" value_type="e_option" />
        <efx:param name="bram_output_regs_packing" value="1" value_type="e_option" />
        <efx:param name="bram-push-tco-outreg" value="0" value_type="e_option" />
        <efx:param name="create-onehot-fsms" value="0" value_type="e_option" />
        <efx:param name="fanout-limit" value="0" value_type="e_integer" />
        <efx:param name="hdl-compile-unit" value="1" value_type="e_option" />
        <efx:param name="hdl-loop-limit" value="20000" value_type="e_integer" />
        <efx:param name="infer-clk-enable" value="3" value_type="e_option" />
        <efx:param name="infer-sync-set-reset" value="1" value_type="e_option" />
        <efx:param name="enable-mark-debug" value="1" value_type="e_option" />
        <efx:param name="max_ram" value="-1" value_type="e_integer" />
        <efx:param name="max_mult" value="-1" value_type="e_integer" />
        <efx:param name="max-bit-blast-mem-size" value="10240" value_type="e_integer" />
        <efx:param name="min-sr-fanout" value="0" value_type="e_integer" />
        <efx:param name="min-ce-fanout" value="0" value_type="e_integer" />
        <efx:param name="mode" value="speed" value_type="e_option" />
        <efx:param name="mult-auto-pipeline" value="0" value_type="e_integer" />
        <efx:param name="mult-decomp-retime" value="1" value_type="e_option" />
        <efx:param name="operator-sharing" value="0" value_type="e_option" />
        <efx:param name="optimize-adder-tree" value="0" value_type="e_option" />
        <efx:param name="optimize-zero-init-rom" value="1" value_type="e_option" />
        <efx:param name="peri-syn-instantiation" value="0" value_type="e_option" />
        <efx:param name="peri-syn-inference" value="0" value_type="e_option" />
        <efx:param name="ram-decomp-mode" value="0" value_type="e_option" />
        <efx:param name="retiming" value="1" value_type="e_option" />
        <efx:param name="seq_opt" value="1" value_type="e_option" />
        <efx:param name="seq-opt-sync-only" value="0" value_type="e_option" />
        <efx:param name="use-logic-for-small-mem" value="64" value_type="e_integer" />
        <efx:param name="use-logic-for-small-rom" value="64" value_type="e_integer" />
        <efx:param name="max_threads" value="-1" value_type="e_integer" />
        <efx:param name="suppress_info_msgs" value="off" value_type="e_bool" />
        <efx:param name="suppress_warning_msgs" value="off" value_type="e_bool" />
        <efx:param name="mult_input_regs_packing" value="1" value_type="e_option" />
        <efx:param name="mult_output_regs_packing" value="1" value_type="e_option" />
    </efx:synthesis>
    <efx:place_and_route tool_name="efx_pnr">
        <efx:param name="work_dir" value="work_pnr" value_type="e_string" />
        <efx:param name="verbose" value="off" value_type="e_bool" />
        <efx:param name="seed" value="1" value_type="e_integer" />
        <efx:param name="placer_effort_level" value="2" value_type="e_option" />
        <efx:param name="max_threads" value="-1" value_type="e_integer" />
        <efx:param name="print_critical_path" value="10" value_type="e_integer" />
        <efx:param name="suppress_info_msgs" value="off" value_type="e_bool" />
        <efx:param name="suppress_warning_msgs" value="off" value_type="e_bool" />
    </efx:place_and_route>
    <efx:bitstream_generation tool_name="efx_pgm">
        <efx:param name="mode" value="active" value_type="e_string" />
        <efx:param name="width" value="1" value_type="e_string" />
        <efx:param name="enable_roms" value="smart" value_type="e_option" />
        <efx:param name="spi_low_power_mode" value="on" value_type="e_bool" />
        <efx:param name="io_weak_pullup" value="on" value_type="e_bool" />
        <efx:param name="oscillator_clock_divider" value="DIV8" value_type="e_option" />
        <efx:param name="bitstream_compression" value="on" value_type="e_bool" />
        <efx:param name="enable_external_master_clock" value="off" value_type="e_bool" />
        <efx:param name="active_capture_clk_edge" value="posedge" value_type="e_option" />
        <efx:param name="jtag_usercode" value="0xFFFFFFFF" value_type="e_string" />
        <efx:param name="release_tri_then_reset" value="on" value_type="e_bool" />
        <efx:param name="cold_boot" value="off" value_type="e_bool" />
        <efx:param name="cascade" value="off" value_type="e_option" />
        <efx:param name="generate_bit" value="on" value_type="e_bool" />
        <efx:param name="generate_bitbin" value="off" value_type="e_bool" />
        <efx:param name="generate_hex" value="on" value_type="e_bool" />
        <efx:param name="generate_hexbin" value="off" value_type="e_bool" />
    </efx:bitstream_generation>
    <efx:debugger>
        <efx:param name="work_dir" value="work_dbg" value_type="e_string" />
        <efx:param name="auto_instantiation" value="off" value_type="e_bool" />
        <efx:param name="profile" value="NONE" value_type="e_string" />
    </efx:debugger>
</efx:project>
"""
    )

    tcl.flush();
    tcl.close();

    // generate sdc constraint
    val sdc = new java.io.FileWriter(Paths.get(workspacePath, "test.sdc").toFile)
    sdc.write(f"""create_clock -period ${(targetPeriod * 1e9).toBigDecimal.bigDecimal.toPlainString} clk""")
    sdc.flush();
    sdc.close();

    val sh = new java.io.FileWriter(Paths.get(workspacePath, "test.sh").toFile)
    sh.write(s"""source $efinixPath/setup.sh && efx_run.py test.xml""")
    sh.flush();
    sh.close();

    doCmd(s"bash test.sh", workspacePath)
    val log = Source.fromFile(Paths.get(workspacePath, "outflow/test.log").toFile)
    val report = log.getLines().mkString("\n")
    new Report {
      override def getFMax() = {
        val matches = "Maximum possible analyzed clocks frequency\\n[^\\n]*[\\n][ ]*clk[ ]*([+-]?[0-9]*[.]?[0-9]+)[ ]*([+-]?[0-9]*[.]?[0-9]+)".r.findAllMatchIn(report).toArray
        assert(matches.size == 1)
        matches(0).group(2).toDouble*1e6
      }
      override def getArea() = {
        val luts = "EFX_LUT4        : \t([0-9]+)".r.findAllMatchIn(report).next().group(1)
        val ff = "EFX_FF          : \t([0-9]+)".r.findAllMatchIn(report).next().group(1)
        s"LUT $luts   FF $ff"
      }
    }
  }
}


object Dev extends App{
  val log = Source.fromFile(new File("/media/data2/proj/tmp/synthesisBench8_Titanium_fmax/outflow/test.log")).getLines().mkString("\n")
  val x = new Report {
    override def getFMax() = {
      val matches = "Maximum possible analyzed clocks frequency\\n[^\\n]*[\\n]clk[ ]*([+-]?[0-9]*[.]?[0-9]+)[ ]*([+-]?[0-9]*[.]?[0-9]+)".r.findAllMatchIn(log).toArray
      assert(matches.size == 1)
      matches(0).group(2).toDouble
    }

    override def getArea() = {
      val luts = "EFX_LUT4        : \t([0-9]+)".r.findAllMatchIn(log).next().group(1).toInt
      val add = "EFX_ADD         : \t([0-9]+)".r.findAllMatchIn(log).next().group(1).toInt
      val ff = "EFX_FF          : \t([0-9]+)".r.findAllMatchIn(log).next().group(1)
      s"LUT ${luts+add}   FF $ff"
    }
  }
  println(x.getFMax())
  println(x.getArea())
}