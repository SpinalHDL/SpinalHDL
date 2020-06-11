package spinal.lib.blackbox.xilinx.s7

import spinal.core._

object STARTUPE2{
  def driveSpiClk(clk : Bool): STARTUPE2 ={
    val bb = STARTUPE2()
    bb.CLK := False
    bb.GSR := False
    bb.GTS := False
    bb.KEYCLEARB := True
    bb.PACK := False
    bb.USRCCLKO := clk
    bb.USRCCLKTS := False
    bb.USRDONEO := True
    bb.USRDONETS := False
    bb
  }
}

case class STARTUPE2() extends BlackBox{
  addGeneric("PROG_USR", "FALSE")
  addGeneric("SIM_CCLK_FREQ", 0.0)

  val CFGCLK = out Bool()
  val CFGMCLK = out Bool()
  val EOS = out Bool()
  val PREQ = out Bool()
  val CLK = in Bool()
  val GSR = in Bool()
  val GTS = in Bool()
  val KEYCLEARB = in Bool()
  val PACK = in Bool()
  val USRCCLKO = in Bool()
  val USRCCLKTS = in Bool()
  val USRDONEO = in Bool()
  val USRDONETS = in Bool()
}


case class BSCANE2(userId : Int) extends BlackBox{
  addGeneric("DISABLE_JTAG", "FALSE")
  addGeneric("JTAG_CHAIN", userId)

  val CAPTURE  = out Bool()
  val DRCK  = out Bool()
  val RESET  = out Bool()
  val RUNTEST  = out Bool()
  val SEL  = out Bool()
  val SHIFT  = out Bool()
  val TCK  = out Bool()
  val TDI  = out Bool()
  val TMS  = out Bool()
  val UPDATE  = out Bool()
  val TDO  = in Bool()
}