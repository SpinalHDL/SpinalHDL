package spinal.demo.phy

import spinal.core._
case class PllClk() extends BlackBox {
  val io = new Bundle {
    val reset = in Bool ()
    val locked = out Bool ()
    val clk = new Bundle {
      val in1 = in Bool ()
      val out1 = out Bool ()
      val out2 = out Bool ()
      val out3 = out Bool ()
      val out4 = out Bool ()
    }
  }
  noIoPrefix()
}
