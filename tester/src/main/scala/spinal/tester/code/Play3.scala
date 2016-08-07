package spinal.tester.code

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahb._

object PlayAhb3{
  class TopLevel extends Component{
    val ahbConfig = Ahb3Config(addressWidth = 16,dataWidth = 32)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}