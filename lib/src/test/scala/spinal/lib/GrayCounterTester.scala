package spinal.lib

import spinal.core._
import spinal.tester.SpinalTesterCocotbBase
import language.postfixOps

object GrayCounterTester {


  class GrayCnt(size : Int = 4) extends Component {

    // All IO signals for the Gray counter
    val io = new Bundle {

      // Counter output port
      val gval = out UInt(size bits)

    }

    // Helper bit for toggling
    val toggle = Reg(Bool()) init(False)

    // Counter register
    val cnt = Reg(UInt(size bits)) init(0)

    // Toggle the helper
    toggle := !toggle

    // Calculate the LSB
    cnt.lsb := !(cnt.lsb ^ toggle)

    // Handle all 'middle' bits
    for(i <- 1 to size - 2) {

      // This equation checks the 0^* pattern
      val tmp = cnt(i - 1) && cnt(i - 2 downto 0).asBools.fold(True)((lastResult,cntBit) => lastResult && !cntBit) && toggle

      // Calculate the ith bit of the counter
      cnt(i) := cnt(i) ^ tmp

    }

    // Calculate the MSB
    cnt.msb := cnt.msb ^ (cnt(size - 3 downto 0) === 0 && toggle)

    // Map the register to the output logic;
    io.gval := cnt

  }

  class GrayCounterTester(n: Int) extends Component {
    val enable = in Bool()
    val gray = out(GrayCounter(n, enable))
    val customGrayCounter = ClockDomain(ClockDomain.current.clock,ClockDomain.current.reset,clockEnable = enable)(new GrayCnt(n))
    assert(gray === customGrayCounter.io.gval,"Gray mismatch :(",FAILURE)
  }

}


//class GrayCounterTesterGhdlBoot extends SpinalTesterGhdlBase {
//  override def getName: String = "GrayCounterTester"
//
//  withWaveform = true
//  override def createToplevel: Component = new GrayCounterTester(8)
//}

class GrayCounterTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "GrayCounterTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/GrayCounterTester"
  override def createToplevel: Component = new GrayCounterTester.GrayCounterTester(8)
}