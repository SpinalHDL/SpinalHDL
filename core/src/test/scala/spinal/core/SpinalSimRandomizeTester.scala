package spinal.core

import sim._
import spinal.tester.SpinalAnyFunSuite

object SomeEnum extends SpinalEnum {
  val sIdle, sStart, sData, sParity, sStop = newElement()
}

case class SimRandomizeTester() extends Component {
  val io = new Bundle {
    val b = in(Bool())
    val ui = in(UInt(4 bit))
    val uil = in(UInt(33 bit))
    val si = in(SInt(4 bit))
    val sil = in(UInt(33 bit))
    val e = in(SomeEnum())
    val uf = in(UFix(peak = 8 exp, resolution = -2 exp))
    val sf = in(SFix(peak = 8 exp, resolution = -2 exp))
    val af = in(AFix.UQ(2 bit, 2 bit))
    val af_out_of_range = in(AFix(8, -4, -2 exp))
  }
}

class SpinalSimRandomizeTest extends SpinalAnyFunSuite {
  var dut : SimCompiled[SimRandomizeTester] = null

  test("compile"){
    dut = SimConfig.compile { SimRandomizeTester() }
  }

  test("randomize returns the value driven next") {
    dut.doSim("randomize returns the value driven next") { dut =>
      for (_ <- 0 to 100) {
        val b = dut.io.b.randomize()
        val ui = dut.io.ui.randomize()
        val uil = dut.io.uil.randomize()
        val si = dut.io.si.randomize()
        val sil = dut.io.sil.randomize()
        val e = dut.io.e.randomize()
        val uf = dut.io.uf.randomize()
        val sf = dut.io.sf.randomize()
        val af = dut.io.af.randomize()
        val af_out_of_range = dut.io.af_out_of_range.randomize(inRange = false)

        sleep(10)

        assert(b == dut.io.b.toBoolean)
        assert(ui == dut.io.ui.toBigInt)
        assert(uil == dut.io.uil.toBigInt)
        assert(si == dut.io.si.toBigInt)
        assert(sil == dut.io.sil.toBigInt)
        assert(e == dut.io.e.toEnum)
        assert(uf == dut.io.uf.toBigDecimal)
        assert(sf == dut.io.sf.toBigDecimal)
        assert(af == dut.io.af.toBigDecimal)
        assert(af_out_of_range == dut.io.af_out_of_range.toBigDecimal)
      }
    }
  }
}
