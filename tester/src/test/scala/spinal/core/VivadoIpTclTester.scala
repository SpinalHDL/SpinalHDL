package spinal.core

import org.scalatest.funsuite.AnyFunSuite

import spinal.core.sim._
import java.io.File
import scala.io.Source

case class FpAddBbConfig(expWidth: Int, fracWidth: Int, latency: Int)

class VivadoIpTclTester extends AnyFunSuite {

  case class FpAddBb(cfg: FpAddBbConfig) extends BlackBox with VivadoIpTcl {
    val io = new Bundle {
      val aclk                 = in Bool ()
      val s_axis_a_tvalid      = in Bool ()
      val s_axis_a_tready      = out Bool ()
      val s_axis_a_tdata       = in Bits (cfg.expWidth + cfg.fracWidth + 1 bits)
      val s_axis_b_tvalid      = in Bool ()
      val s_axis_b_tready      = out Bool ()
      val s_axis_b_tdata       = in Bits (cfg.expWidth + cfg.fracWidth + 1 bits)
      val m_axis_result_tvalid = out Bool ()
      val m_axis_result_tready = in Bool ()
      val m_axis_result_tdata  = out Bits (cfg.expWidth + cfg.fracWidth + 1 bits)
    }

    // Map BlackBox IO to Vivado IP ports
    noIoPrefix()
    setDefinitionName(s"FpAddBb_${cfg.expWidth}_${cfg.fracWidth}_${cfg.latency}")
    mapClockDomain(clock = io.aclk)

    override def vivadoIpTcl(): String = {
      s"""
      |create_ip -name floating_point -vendor xilinx.com -library ip -version 7.1 -module_name ${definitionName}
      |set_property -dict [list \\
      |  CONFIG.Add_Sub_Value {Add} \\
      |  CONFIG.A_Precision_Type {Custom} \\
      |  CONFIG.C_A_Exponent_Width {${cfg.expWidth}} \\
      |  CONFIG.C_A_Fraction_Width {${cfg.fracWidth + 1}} \\
      |  CONFIG.Result_Precision_Type {Custom} \\
      |  CONFIG.C_Result_Exponent_Width {${cfg.expWidth}} \\
      |  CONFIG.C_Result_Fraction_Width {${cfg.fracWidth + 1}} \\
      |  CONFIG.C_Mult_Usage {Full_Usage} \\
      |  CONFIG.C_Latency {${cfg.latency}} \\
      |  CONFIG.C_Rate {1} \\
      |] [get_ips ${definitionName}]
      """.stripMargin
    }
  }

  case class TopLevel() extends Component {
    val io = new Bundle {
      val a, b = in Bits (32 bits)
      val res0, res1, res2 = out Bits (32 bits)
    }

    // Two instances with same config (should share same TCL definition)
    val cfg0 = FpAddBbConfig(8, 23, 3)
    val ip0 = FpAddBb(cfg0)
    ip0.io.s_axis_a_tdata := io.a
    ip0.io.s_axis_b_tdata := io.b
    ip0.io.s_axis_a_tvalid := True
    ip0.io.s_axis_b_tvalid := True
    ip0.io.m_axis_result_tready := True
    io.res0 := ip0.io.m_axis_result_tdata

    val ip1 = FpAddBb(cfg0)
    ip1.io.s_axis_a_tdata := io.a
    ip1.io.s_axis_b_tdata := io.b
    ip1.io.s_axis_a_tvalid := True
    ip1.io.s_axis_b_tvalid := True
    ip1.io.m_axis_result_tready := True
    io.res1 := ip1.io.m_axis_result_tdata

    // One instance with different config
    val cfg1 = FpAddBbConfig(8, 23, 5)
    val ip2 = FpAddBb(cfg1)
    ip2.io.s_axis_a_tdata := io.a
    ip2.io.s_axis_b_tdata := io.b
    ip2.io.s_axis_a_tvalid := True
    ip2.io.s_axis_b_tvalid := True
    ip2.io.m_axis_result_tready := True
    io.res2 := ip2.io.m_axis_result_tdata
  }

  test("VivadoIpTcl - Default path and consolidated") {
    val targetDir = "target/VivadoIpTclTester/default"
    val config = SpinalConfig(targetDirectory = targetDir)
    val report = config.generateVerilog(TopLevel())

    val tclFile = new File(targetDir, "TopLevel_ips.tcl")
    assert(tclFile.exists(), "Consolidated TCL file should exist")

    val content = Source.fromFile(tclFile).getLines().mkString("\n")
    // Should contain both definitions
    assert(content.contains("FpAddBb_8_23_3"), "Should contain FpAddBb_8_23_3")
    assert(content.contains("FpAddBb_8_23_5"), "Should contain FpAddBb_8_23_5")

    // Check that it only contains one create_ip for FpAddBb_8_23_3 despite 2 instances
    val createIpOccurrences = "create_ip.*FpAddBb_8_23_3".r.findAllIn(content).length
    assert(createIpOccurrences == 1, s"Should only have one create_ip command for FpAddBb_8_23_3, found $createIpOccurrences occurrences")
    
    // Verify that the actual set_property command exists and is not duplicated
    val setPropertyOccurrences = "(?s)set_property.*?FpAddBb_8_23_3".r.findAllIn(content).length
    assert(setPropertyOccurrences == 1, s"Should only have one set_property command for FpAddBb_8_23_3, found $setPropertyOccurrences occurrences")

    assert(report.ipTclSourcesPaths.exists(_.endsWith("TopLevel_ips.tcl")), "Report should contain TCL path")
  }

  test("VivadoIpTcl - Custom path and separate") {
    val targetDir = "target/VivadoIpTclTester/custom_rtl"
    val ipTclDir = "target/VivadoIpTclTester/custom_tcl"
    val config = SpinalConfig(
      targetDirectory = targetDir,
      ipTclTargetDirectory = Some(ipTclDir),
      separateIpTcl = true
    )
    val report = config.generateVerilog(TopLevel())

    assert(new File(ipTclDir, "FpAddBb_8_23_3.tcl").exists(), "Separate TCL for FpAddBb_8_23_3 should exist")
    assert(new File(ipTclDir, "FpAddBb_8_23_5.tcl").exists(), "Separate TCL for FpAddBb_8_23_5 should exist")
    assert(!new File(ipTclDir, "TopLevel_ips.tcl").exists(), "Consolidated TCL should NOT exist")

    assert(report.ipTclSourcesPaths.size == 2, "Report should contain 2 TCL paths")
  }

  test("VivadoIpTcl - XSim simulation integration") {
    if(sys.env.contains("VIVADO_HOME") || new File("/tools/Xilinx/Vivado").exists()) {
      val simDir = "target/VivadoIpTclTester/sim"
      val compiled = SimConfig
        .withConfig(SpinalConfig(targetDirectory = simDir))
        .withXSim
        .compile(TopLevel())

      val xsimTcl = new File(new File(new File(simDir, "TopLevel"), "xsim"), "spinal_xsim.tcl")
      assert(xsimTcl.exists())
      val content = Source.fromFile(xsimTcl).mkString
      assert(content.contains("source"), "spinal_xsim.tcl should contain source command for IP TCL")
      assert(content.contains("TopLevel_ips.tcl"), "spinal_xsim.tcl should source TopLevel_ips.tcl")
    } else {
      println("VIVADO_HOME not set, skipping XSim integration test")
    }
  }
}
