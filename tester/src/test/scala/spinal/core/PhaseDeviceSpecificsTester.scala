package spinal.core

import spinal.tester.SpinalAnyFunSuite
import spinal.lib._
import spinal.core._
import scala.util.matching.Regex

class PhaseDeviceSpecificsTester extends SpinalAnyFunSuite {
  test("Test ") {
    val report = SpinalVerilog(SpinalConfig(device=Device.ALTERA))(new Component {
      val io = new Bundle {
        val i = slave port Stream(Bits(8 bit))
        val o = master port Stream(Bits(8 bit))
      }
      val cd1 = ClockDomain.external("cd1")
      val cd2 = ClockDomain.external("cd2")
      setDefinitionName("PhaseDeviceSpecificsTester")
      cd2 {
        io.o << io.i.ccToggle(cd1, cd2)
      }
    })
    val string = report.getRtlString
    println(report.toplevel)
    assert("-name ADV_NETLIST_OPT_ALLOWED NEVER_ALLOW.*pushArea_data;".r.findFirstIn(string).isDefined)
    assert("-name ADV_NETLIST_OPT_ALLOWED NEVER_ALLOW.*popArea_stream_rData;".r.findFirstIn(string).isDefined)
  }

}
