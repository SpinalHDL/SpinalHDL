package spinal.tester.scalatest

import spinal.core._
import spinal.core.{Bundle, Component, SFix, Vec, in, out, tagTruncated}
import spinal.tester.scalatest.FixedPointNewTester.FixedPointNewTester

object FixedPointNewTester {

  class FixedPointNewTester extends Component {
    val io = new Bundle {
      val inFix = in(Vec(Seq(new Fix(12, 16, true),
                             new Fix(10, 16, true))))
      val outRaw = out(new Fix(23, 35, true))
      val outFix = out(new Fix(23, 23, true))
      val opMode = in(Bits(3 bit))
      val roundMode = in(Bits(4 bit))
    }

    val opResultsSeq = Seq(
      io.inFix(0) + io.inFix(1),
      io.inFix(0) - io.inFix(1),
      io.inFix(0) * io.inFix(1),
      io.inFix(0) / io.inFix(1),
      io.inFix(0) % io.inFix(1)
    )
    val maxIntWidth: Int = opResultsSeq.map(_.intWidth).max
    val maxFracWidth: Int = opResultsSeq.map(_.fracWidth).max
    val opResults = Vec(
      opResultsSeq.map(f => f.expand(maxIntWidth-f.intWidth, maxFracWidth-f.fracWidth))
    )

    val chosenOp = opResults(io.opMode.asUInt)
    io.outRaw := chosenOp

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

    io.outFix := roundResults(io.roundMode.asUInt)
  }

}

class FixedPointNewTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "FixedPointNewTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/FixedPointNewTester"
  override def createToplevel: Component = new FixedPointNewTester
}