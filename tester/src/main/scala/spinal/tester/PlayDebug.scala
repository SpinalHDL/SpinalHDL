package spinal.tester

import spinal.core._
import spinal.lib._


object PlayDebug{


  class TopLevel extends Component {
    val rxd = in Bool()
    val result = out Bool()

    result := RegNext(rxd) init(False)
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel

  }
}
