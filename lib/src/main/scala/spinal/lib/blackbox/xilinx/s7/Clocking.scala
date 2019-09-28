package spinal.lib.blackbox.xilinx.s7

import spinal.core._

object BUFG{
  def on(that : Bool) : Bool = {
    val bufg = BUFG()
    bufg.setCompositeName(that, "BUFG")
    bufg.I := that
    bufg.O
  }
}

case class BUFG() extends BlackBox {
  val I = in Bool()
  val O = out Bool()
}
