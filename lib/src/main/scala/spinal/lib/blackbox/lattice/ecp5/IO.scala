package spinal.lib.blackbox.lattice.ecp5

import spinal.core._

case class BB() extends BlackBox {
  val I, T = in Bool()
  val O = out Bool()
  val B = inout(Analog(Bool()))
}


case class TSFF() extends BlackBox{
  val TD   = in Bool()
  val SCLK = in Bool()
  val RST  = in Bool()
  val TQ =  out Bool()
}