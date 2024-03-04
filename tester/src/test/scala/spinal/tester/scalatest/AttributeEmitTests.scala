package spinal.tester.scalatest

import spinal.tester.SpinalAnyFunSuite
import spinal.core._

class AttributeEmitTests extends SpinalAnyFunSuite {
  test("check escaping of quotes in attributes") {
    class Test extends Component {
      val a = RegInit(False)
      a.addAttribute("testname", "test \"value\" bar")
    }
    val verilog = SpinalVerilog(new Test)
    assert(verilog.getRtlString().contains(raw""""test \"value\" bar""""))
    val vhdl = SpinalVhdl(new Test)
    assert(vhdl.getRtlString().contains(raw""""test ""value"" bar""""))
  }
}
