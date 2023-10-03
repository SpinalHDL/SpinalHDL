package spinal.core

import spinal.lib.{Delay, LatencyAnalysis}
import spinal.lib.com.uart._
import spinal.lib.math.SIntMath
import spinal.tester.SpinalTesterCocotbBase

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


class LibTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "LibTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/LibTester"
  override def createToplevel: Component = new LibTester.LibTester
  withWaveform = true
}


//class LibTesterGhdlBoot extends SpinalTesterGhdlBase {
//  override def getName: String = "LibTester"
//  override def createToplevel: Component = new LibTester.LibTester
//}
