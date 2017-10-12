package spinal.tester.bug

import spinal.core._
import spinal.lib._
case class comms_engine_control() extends Bundle with IMasterSlave{
  val enable = Bool
  val buffer_num = Bits(4 bits)
  val busy = Bool
  val address = Bits(4 bits)
  override def asMaster() : Unit = {
    out(enable,buffer_num)
    in(busy,address)
  }
  override def asSlave() : Unit = {
    in(enable,buffer_num,address)
    out(busy)
  }
}

object Miaou{
  def main(args: Array[String]) {
    SpinalVhdl(new Component{
      val inputs = Vec((comms_engine_control()),1)
      val output = inputs(0).address
    })
  }
}
