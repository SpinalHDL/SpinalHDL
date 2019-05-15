package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.lib.{Delay, LatencyAnalysis}
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


class LibTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "LibTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/LibTester"
  override def createToplevel: Component = new LibTester.LibTester
}



class CoreMiscTester extends FunSuite{
  import spinal.core.sim._
  test("SlowArea"){
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(4000 Hz))).compile(new Component{
      val counter = out(RegInit(U"0000"))
      counter := counter + 1
      assert(clockDomain.samplingRate.getValue.toInt == 4000)

      val slowArea = new SlowArea(4){
        val counter = out(RegInit(U"0000"))
        counter := counter + 1
        assert(clockDomain.samplingRate.getValue.toInt == 1000)
      }

    }).doSim{dut =>
      dut.clockDomain.forkStimulus(10)

      for(i <- 0 until 1000){
        dut.clockDomain.waitSampling()
        assert(dut.counter.toInt == i % 16)
        assert(dut.slowArea.counter.toInt == (i-1)/4 % 16)
      }
    }
  }

}

//class LibTesterGhdlBoot extends SpinalTesterGhdlBase {
//  override def getName: String = "LibTester"
//  override def createToplevel: Component = new LibTester.LibTester
//}
