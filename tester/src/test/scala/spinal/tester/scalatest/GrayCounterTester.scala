package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.tester.scalatest.GrayCounterTester.GrayCounterTester
import language.postfixOps

object GrayCounterTester {
  def grayCounter(n: Int, enable: Bool): UInt = {
    val gray = RegInit(U(0, n bit))
    var even = RegInit(True)
    val word = Cat(True, gray(n-3, 0), even)
    when(enable) {
      var found = False
      for (i <- 0 until n) {
        when(word(i) && !found) {
          gray(i) := !gray(i)
          found \= True
        }
      }
      even := !even
    }
    return gray
  }


  class GrayCnt(size : Int = 4) extends Component {

    // All IO signals for the Gray counter
    val io = new Bundle {

      // Counter output port
      val gval = out UInt(size bits)

    }

    // Helper bit for toggling
    val toggle = Reg(Bool) init(False)

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
    val enable = in Bool
    val gray = out(grayCounter(n, enable))
    val customGrayCounter = ClockDomain(ClockDomain.current.clock,ClockDomain.current.reset,clockEnable = enable)(new GrayCnt(n))
    assert(gray === customGrayCounter.io.gval,"Gray missmatch :(",FAILURE)
  }

}


class GrayCounterTesterGhdlBoot extends SpinalTesterGhdlBase {
  override def getName: String = "GrayCounterTester"

  withWaveform = true
  override def createToplevel: Component = new GrayCounterTester(8)
}

class GrayCounterTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "GrayCounterTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/GrayCounterTester"
  override def createToplevel: Component = new GrayCounterTester(8)
}