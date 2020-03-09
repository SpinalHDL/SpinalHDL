package spinal.lib.blackbox.anlogic.eagle

import spinal.core._


case class EG_LOGIC_BUFG() extends BlackBox{
  val o = out Bool()
  val i = in Bool()
}


case class EG_LOGIC_ODDR() extends BlackBox{
  val q = out Bool()
  val clk = in Bool()
  val d1 = in Bool()
  val d0 = in Bool()
  val rst = in Bool()
}


/* 64 EMB9K */
case class EG_PHY_BRAM() extends BlackBox{
  val doa = out Bits(9 bits)
  val dob = out Bits(9 bits)
  val dia = in Bits(9 bits)
  val dib = in Bits(9 bits)
  val csa = in Bits(3 bits)
  val csb = in Bits(3 bits)
  val cea = in Bool()
  val ocea = in Bool()
  val clka = in Bool()
  val wea = in Bool()
  val rsta = in Bool()
  val ceb = in Bool()
  val oceb = in Bool()
  val clkb = in Bool()
  val web = in Bool()
  val rstb = in Bool()
  val addra = in Bits(13 bits)
  val addrb = in Bits(13 bits)
}

/* 16 BRAM32K */
case class EG_PHY_BRAM32K() extends BlackBox{
  val doa = out Bits(16 bits)
  val dob = out Bits(16 bits)
  val dia = in Bits(16 bits)
  val dib = in Bits(16 bits)
  val addra = in UInt(11 bits)
  val addrb = in UInt(11 bits)
  val bytea = in Bool()
  val bytewea = in Bool()
  val byteb = in Bool()
  val byteweb = in Bool()
  val csa = in Bool()
  val wea = in Bool()
  val csb = in Bool()
  val web = in Bool()
  val clka = in Bool()
  val rsta = in Bool()
  val clkb = in Bool()
  val rstb = in Bool()
  val ocea = in Bool()
  val oceb = in Bool()
  mapCurrentClockDomain(clka)
}


/* SDRAM 8MB */
case class EG_PHY_SDRAM_2M_32() extends BlackBox{
  val clk = in Bool()
  val ras_n = in Bool()
  val cas_n = in Bool()
  val we_n = in Bool()
  val addr = in Bits(11 bits)
  val ba = in Bits(2 bits)
  val dq = inout Bits(32 bits)
  val cs_n = in Bool()
  val dm0 = in Bool()
  val dm1 = in Bool()
  val dm2 = in Bool()
  val dm3 = in Bool()
  val cke = in Bool()
}
