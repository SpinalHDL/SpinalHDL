package spinal.tester.scalatest

import spinal.core._
import spinal.lib.com.uart._

class FixedPointTester extends Component {
  val io = new Bundle {
    val inSFix = in(Vec(Seq(SFix(4,16),SFix(2,12))))
    val outSFix = out(Vec(2,SFix(4,16)))
  }
  io.outSFix(0) := io.inSFix(0) + io.inSFix(1)
  io.outSFix(1) := io.inSFix(0) * io.inSFix(1)
}



class FixedPointTesterBoot extends SpinalTesterBase {
  override def getName: String = "FixedPointTester"
  override def createToplevel: Component = new FixedPointTester
}