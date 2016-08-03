package spinal.tester.code

import spinal.core._

/**
 * Created by PIC32F_USER on 07/08/2015.
 */
class WhyNot extends Component{
  val ram = Mem(Bits(4 bit),16)
  out(ram.readWriteSync(in UInt(4 bit),in Bits(4 bit),in Bool,in Bool))
  out(ram.readWriteSync(in UInt(4 bit),in Bits(4 bit),in Bool,in Bool))
}


object WhyNot{
  def main(args: Array[String]) {
    SpinalVhdl(new WhyNot)
  }
}