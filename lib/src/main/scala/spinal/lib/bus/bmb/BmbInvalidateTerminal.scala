package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

object BmbInvalidateTerminal{
  def outputParameter(inputParameter : BmbParameter) = inputParameter.copy(canInvalidate = false)
}

case class BmbInvalidateTerminal(inputParameter : BmbParameter) {
  assert(inputParameter.alignment == BmbParameter.BurstAlignement.LENGTH)
  val io = new Bundle {
    val input = slave(Bmb(inputParameter))
    val output = master(Bmb(BmbInvalidateTerminal.outputParameter(inputParameter)))
  }


}
