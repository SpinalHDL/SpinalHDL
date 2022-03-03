package spinal.tester.scalatest

import spinal.core._
import spinal.core.{Bundle, Component, Vec, in, out}
import spinal.tester.scalatest.AFixTester.AFixTester

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
    io.outRaw := chosenOp.sat(68719476735L, -68719476736L)

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

    io.outFix := roundResults(io.roundMode.asUInt).sat(8388607, -8388608)
  }

}

class AFixTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "AFixTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/AFixTester"
  override def createToplevel: Component = new AFixTester
}