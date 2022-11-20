package spinal.core

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

class XFixTest extends AnyFunSuite {
  test("UFix Mux") {
    case class MuxDut() extends Component {
      val io = new Bundle {
        val choose = in Bool()
        val uf2e_8b = in UFix(2 exp, 8 bit)
        val uf2e_10b = in UFix(2 exp, 10 bit)
        val uf4e_10b = in UFix(4 exp, 10 bit)
        val uf_muxed = out UFix(2 exp, 8 bit)
      }

      // Testcase for #947
      io.uf_muxed := io.choose ? io.uf2e_8b | io.uf2e_10b.truncated

      // truncated input should not dictate mux size
      val muxed1 = io.choose ? io.uf2e_8b | io.uf2e_10b.truncated
      muxed1.simPublic()
      assert(muxed1.maxExp == 2)
      assert(muxed1.bitCount == 8)

      // truncated input should not dictate mux size, even if ordered first
      val muxed2 = io.choose ? io.uf2e_10b.truncated | io.uf2e_8b
      muxed2.simPublic()
      assert(muxed2.maxExp == 2)
      assert(muxed2.bitCount == 8)

      // larger bitcount with same exponent dictates bitcount (lowest minExp is used)
      val muxed3 = io.choose ? io.uf2e_10b | io.uf2e_8b
      muxed3.simPublic()
      assert(muxed3.maxExp == 2)
      assert(muxed3.bitCount == 10)

      // if not marked so, no input should be truncated -> bitwidth should be able to hold all input bits (largest maxExp is used)
      val muxed4 = io.choose ? io.uf4e_10b | io.uf2e_10b
      muxed4.simPublic()
      assert(muxed4.maxExp == 4)
      assert(muxed4.bitCount == 12)
    }

    SimConfig.compile(MuxDut()).doSim("UFix Mux") { dut =>
      for (_ <- 0 to 10) {
        dut.io.uf2e_8b.randomize()
        dut.io.uf2e_10b.randomize()
        dut.io.uf4e_10b.randomize()

        dut.io.choose #= true
        sleep(1)
        val uf2_10_trunc2_8 = (dut.io.uf2e_10b.raw.toBigInt >> 2)

        assert(dut.io.uf_muxed.toBigDecimal == dut.io.uf2e_8b.toBigDecimal)
        assert(dut.muxed1.toBigDecimal == dut.io.uf2e_8b.toBigDecimal)
        assert(dut.muxed2.raw.toBigInt == uf2_10_trunc2_8)
        assert(dut.muxed3.toBigDecimal == dut.io.uf2e_10b.toBigDecimal)
        assert(dut.muxed4.toBigDecimal == dut.io.uf4e_10b.toBigDecimal)

        dut.io.choose #= false
        sleep(1)

        assert(dut.io.uf_muxed.raw.toBigInt == uf2_10_trunc2_8)
        assert(dut.muxed1.raw.toBigInt == uf2_10_trunc2_8)
        assert(dut.muxed2.toBigDecimal == dut.io.uf2e_8b.toBigDecimal)
        assert(dut.muxed3.toBigDecimal == dut.io.uf2e_8b.toBigDecimal)
        assert(dut.muxed4.toBigDecimal == dut.io.uf2e_10b.toBigDecimal)
      }
    }
  }
  test("UFix.truncated") {
    case class TruncatedDut() extends Component {
      val io = new Bundle {
        val uf2e_10b = in UFix(2 exp, 10 bit)
        val uf2e_4b_o = out UFix(2 exp, 4 bit)
        val uf2e_8b_o = out UFix(2 exp, 8 bit)
        val uf2e_6e_o = out UFix(2 exp, -6 exp)
      }
      io.uf2e_4b_o := io.uf2e_10b.truncated
      io.uf2e_8b_o := io.uf2e_10b.truncated(2 exp, -6 exp)
      io.uf2e_6e_o := io.uf2e_10b.truncated(2 exp, 8 bit)
    }

    SimConfig.compile(TruncatedDut()).doSim("UFix.truncated") {
      dut =>
        for (_ <- 0 to 10) {
          dut.io.uf2e_10b.randomize()

          sleep(1)

          assert(dut.io.uf2e_4b_o.raw.toBigInt == dut.io.uf2e_10b.raw.toBigInt >> 6)
          assert(dut.io.uf2e_6e_o.raw.toBigInt == dut.io.uf2e_10b.raw.toBigInt >> 2)
          assert(dut.io.uf2e_8b_o.raw.toBigInt == dut.io.uf2e_10b.raw.toBigInt >> 2)
        }
    }
  }
}
