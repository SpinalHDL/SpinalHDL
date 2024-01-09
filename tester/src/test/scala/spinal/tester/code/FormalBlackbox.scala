package spinal.tester.code

import spinal.core._
import spinal.core.formal._
import spinal.tester.SpinalAnyFunSuite
/*
 * This test is for Issue #1256, to verify that SpinalHDL copies any
 * external rtl specified in blackboxes or added by
 * FormalConfig.addRtl() to the formal verification directory and .sbt.
 * The blackboxed rtl is Verilog because spinal defaults to generating
 * verilog when it exports for formal verification
 */

class FormalBlackboxTest extends BlackBox {
  val io = new Bundle {
    val clk = in Bool()
    val a = in UInt(8 bits)
    val b = in UInt(8 bits)
    val o = out UInt(8 bits)

  }
  noIoPrefix()
  addTag(noNumericType)
  mapCurrentClockDomain(io.clk)
  addRTLPath("tester/src/test/resources/FormalBlackboxTest.v")
}

class FormalBlackboxAsserts extends Component {
  val dut = FormalDut(new FormalBlackboxTest())
  anyseq(dut.io.a)
  anyseq(dut.io.b)
  when(!initstate()){
    assert(dut.io.o === (past(dut.io.a) + past(dut.io.b)))
  }
}


class FormalBlackbox extends SpinalAnyFunSuite {
  test("formal"){
    FormalConfig.withBMC(4).doVerify(new FormalBlackboxAsserts)
  }
}
