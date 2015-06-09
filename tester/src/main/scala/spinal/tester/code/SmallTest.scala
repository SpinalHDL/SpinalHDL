package spinal.tester.code

import spinal.core._
import spinal.lib._

class SmallTest extends Component {
  val io = new Bundle {
    val toggledOutput = out Bool
  }
  val dividedClk = CounterFreeRun(16)(3)
  val myDividedClockDomain = ClockDomain(dividedClk)
  val myDividedClockingArea = new ClockingArea(myDividedClockDomain){
    io.toggledOutput := RegNext(!io.toggledOutput)
  }
}


object SmallTest {
  def main(args: Array[String]) {
    SpinalVhdl(new SmallTest)
  }
}