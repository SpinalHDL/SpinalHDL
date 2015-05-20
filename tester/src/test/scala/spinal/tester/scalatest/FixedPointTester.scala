package spinal.tester.scalatest

import spinal.core._
import spinal.lib.com.uart._

class FixedPointTester extends Component {
  val io = new Bundle {
    val inSFix = in(Vec(Seq(SFix(4,16),SFix(2,12))))
    val outSFix = out(Vec(Seq(SFix(4,16),SFix(8,24))))
  }
  io.outSFix(0) := io.inSFix(0) + io.inSFix(1)
  //28 bit  point at 28-6=22   target is at 24-8=16
  io.outSFix(1) := io.inSFix(0) * io.inSFix(1)


//  val xx = UInt(3 bit)
//  xx := True
//  val fix = SFix(3,4)
//  fix := xx
//
//  io.outSFix.apply(0) := Trues
}



class FixedPointTesterBoot extends SpinalTesterBase {
  override def getName: String = "FixedPointTester"
  override def createToplevel: Component = new FixedPointTester
}