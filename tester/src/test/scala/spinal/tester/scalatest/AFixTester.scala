package spinal.tester.scalatest

import spinal.core._
import spinal.core.{Bundle, Component, Vec, in, out}
import spinal.tester.scalatest.AFixTester.AFixTester


import org.scalatest.funsuite.AnyFunSuite
import spinal.core.HardType
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb.sim.{BmbDriver, BmbMemoryAgent}
import spinal.lib.bus.bmb.{Bmb, BmbParameter, BmbSlaveFactory}
import spinal.lib.bus.bsb.{Bsb, BsbDownSizerAlignedMultiWidth, BsbDownSizerSparse, BsbParameter, BsbUpSizerDense, BsbUpSizerSparse}
import spinal.lib.bus.bsb.sim.BsbBridgeTester
import spinal.lib._
import spinal.lib.system.dma.sg.{DmaSg, DmaSgTester, SgDmaTestsParameter}

class SpinalSimAFixTester extends AnyFunSuite {
  def check(max : Int, min : Int, exp : Int)(f : AFix): Unit ={
    assert(f.maxValue == max && f.minValue == min && f.exp.value == exp)
  }
  test("instanciate") {
    SpinalVerilog(new Component{

      check(4095,0,0)(AFix.U(12 bits)) //Q12.0
      check(4095,0,-4)(AFix.U(8 exp, 12 bits)) //Q8.4
      check(4095,0,-4)(AFix.U(8 exp, -4 exp)) //Q8.4
      check(4095,0,-4)(AFix.UQ(8 bits, 4 bits)) //Q8.4
//      check(4095,0,-4)(AFix.U(255, -4 exp)) //Q8.4
//      check(4095,2048,-4)(AFix.U(255, 128, -4 exp)) //Q8.4
      check(2047,-2048,0)(AFix.S(12 bits)) //Q11.0 + sign bit
      check(2047,-2048,-4)(AFix.S(7 exp, 12 bits)) //Q7.4  + sign bit
      check(2047,-2048,-4)(AFix.S(7 exp, -4 exp)) //Q7.4  + sign bit
      check(2047,-2048,-4)(AFix.SQ(7 bits, 4 bits)) //Q8.4 + sign bit
//      check(2047,-2048,-4)(AFix.S(127, -128, -4 exp)) //Q7.4 + sign bit
    })
  }

  test("assign") {
    SpinalVerilog(new Component{
      val u_8_4_a = AFix.U(8 exp, -4 exp)
      val u_8_4_b = AFix.U(8 exp, -4 exp)
      val u_10_4_b = AFix.U(10 exp, -4 exp)
      val u_8_6_b = AFix.U(8 exp, -6 exp)
      val u_6_4_b = AFix.U(6 exp, -4 exp)
      val u_8_2_b = AFix.U(6 exp, -4 exp)

      u_8_4_b := u_8_4_a
      u_10_4_b := u_8_4_a
      u_8_6_b := u_8_4_a
      u_6_4_b := u_8_4_a.truncated(saturation = true)
      u_8_2_b := u_8_4_a.truncated(rounding = RoundType.FLOOR)
    })
  }

}

object AFixTester {

  class AFixTester extends Component {
    val io = new Bundle {
      val inFix = in(Vec(Seq(new AFix(32767, -32768, -4 exp),
                             new AFix(32767, -32768, -6 exp))))
      val outRaw = out(new AFix(68719476735L, -68719476736L, -13 exp))
      val outFix = out(new AFix(8388607, -8388608, 0 exp))
      val opMode = in(Bits(2 bit))
      val roundMode = in(Bits(4 bit))
    }

    val opResultsSeq = Seq(
      io.inFix(0) + io.inFix(1),
      io.inFix(0) - io.inFix(1),
      io.inFix(0) * io.inFix(1)
    )
    for (res <- opResultsSeq) {
      println(res)
    }
    val rangeExpMin = opResultsSeq.minBy(_.exp.value).exp
    val rangeMax = opResultsSeq.map(af => af.maxValue*BigInt(2).pow(af.exp.value - rangeExpMin.value)).max
    val rangeMin = opResultsSeq.map(af => af.minValue*BigInt(2).pow(af.exp.value - rangeExpMin.value)).min
    val opResults = Vec(opResultsSeq.map(af => {
      val resized_af = new AFix(rangeMax, rangeMin, rangeExpMin)
      resized_af := af
      resized_af
    }))

    val chosenOp = opResults(io.opMode.asUInt)
    io.outRaw := chosenOp.resized.sat(io.outRaw)

    val roundResults = Vec(Seq(
      chosenOp.ceil(),
      chosenOp.floor(),
      chosenOp.floorToZero(),
      chosenOp.ceilToInf(),
      chosenOp.roundHalfUp(),
      chosenOp.roundHalfDown(),
      chosenOp.roundHalfToZero(),
      chosenOp.roundHalfToInf(),
      chosenOp.roundHalfToEven(),
      chosenOp.roundHalfToOdd()
    ))

    io.outFix := roundResults(io.roundMode.asUInt).sat(io.outFix)
  }

}

class AFixTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "AFixTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/AFixTester"
  override def createToplevel: Component = new AFixTester
}

