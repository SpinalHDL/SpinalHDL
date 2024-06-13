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
<efx:project xmlns:efx="http://www.efinixinc.com/enf_proj" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" name="test" description="" last_change_date="Wed Feb 14 2024 02:46:20 PM" location="$workspacePath" sw_version="2021.2.323.4.6" last_run_state="pass" last_run_tool="efx_pgm" last_run_flow="bitstream" config_result_in_sync="sync" design_ood="sync" place_ood="sync" route_ood="sync" xsi:schemaLocation="http://www.efinixinc.com/enf_proj enf_proj.xsd">
    <efx:device_info>
        <efx:family name="$family" />
        <efx:device name="$device" />
        <efx:timing_model name="$timing" />
    </efx:device_info>
    <efx:design_info def_veri_version="verilog_2k" def_vhdl_version="vhdl_2008">
        <efx:top_module name="" />
        <efx:design_file name="${rtl.getRtlPaths().mkString(" ")}" version="default" library="default" />
        <efx:top_vhdl_arch name="" />
    </efx:design_info>
    <efx:constraint_info>
        <efx:sdc_file name="test.sdc" />
        <efx:inter_file name="" />
    </efx:constraint_info>
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
      val luts = "EFX_LUT4        : \t([0-9]+)".r.findAllMatchIn(log).next().group(1)
      val ff = "EFX_FF          : \t([0-9]+)".r.findAllMatchIn(log).next().group(1)
      s"LUT $luts   FF $ff"
    }
  }
  println(x.getFMax())
  println(x.getArea())
}