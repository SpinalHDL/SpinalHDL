package spinal.tester.scalatest

import spinal.core._
import spinal.lib.{LatencyAnalysis, Delay}
import spinal.lib.com.uart._
import spinal.lib.math.SIntMath
import spinal.tester.scalatest.FixedPointTester.FixedPointTester

object LibTester{


  class LibTester extends Component {
    val io = new Bundle {
      val inSIntA = in SInt (16 bit)
      val inSIntB = in SInt (16 bit)
      val outSInt = out SInt (32 bit)
      val outSIntRef = out SInt (32 bit)
    }
    io.outSInt := SIntMath.mul(io.inSIntA, io.inSIntB, 4, 0,1,(s,l) => RegNext(s))
    io.outSIntRef := Delay(io.inSIntA * io.inSIntB, LatencyAnalysis(io.inSIntA, io.outSInt))
  }

}


class LibTesterBoot extends SpinalTesterBase {
  override def getName: String = "LibTester"
  override def createToplevel: Component = new LibTester.LibTester
}