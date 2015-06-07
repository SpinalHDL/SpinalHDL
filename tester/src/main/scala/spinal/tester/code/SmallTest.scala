package spinal.tester.code

import spinal.core._

class SmallTest extends Component {
  val io = new Bundle {
    val toggledOutput = out Bool
  }
  io.toggledOutput := RegNext(!io.toggledOutput)
}


object SmallTest {
  def main(args: Array[String]) {
    SpinalVhdl(new SmallTest)
  }
}


class SmallTest2 extends Component {
  val io = new Bundle {
    val toggledOutput = out Bool
  }

  val myClockDomain = ClockDomain("myClockDomain")
  val myClockingArea = new ClockingArea(myClockDomain) {
    io.toggledOutput := RegNext(!io.toggledOutput)
  }
}


object SmallTest2 {
  def main(args: Array[String]) {
    SpinalVhdl(new SmallTest2)
  }
}