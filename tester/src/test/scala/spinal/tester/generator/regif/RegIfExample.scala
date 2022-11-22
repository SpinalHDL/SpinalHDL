package spinal.tester.generator.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.apb._
import spinal.lib.bus.regif.AccessType._
import spinal.lib.bus.regif._

class RegIfExample extends Component {
  val io = new Bundle{
    val apb = slave(Apb3(Apb3Config(16,32)))
    val a, b, c, d, e = in Bool()
  }

  val busif = BusInterface(io.apb,(0x000,1 KiB), 0, regPre = "AP")

  val M_TURBO_EARLY_QUIT    = busif.newReg(doc = "Turbo Hardware-mode register1")
  val early_quit  = M_TURBO_EARLY_QUIT.field(Bool(), RW, 0, doc = "CRC validate early quit enable").asOutput()
  val early_count = M_TURBO_EARLY_QUIT.field(Bits(2 bit), RW, 2, doc = "CRC validate early quit enable").asOutput()
  val M_TURBO_THETA         = busif.newReg(doc = "Turbo Hardware-mode register2")
  val theta       = M_TURBO_THETA.field(Bits(4 bit), RW, 0x6, doc = "Back-Weight, UQ(4,3), default 0.75").asOutput()
  val bib_order   = M_TURBO_THETA.field(Bool(), RW, 1, doc = "bib in Byte, defalut. \n 0:[76543210] \n 1:[01234567]").asOutput()
  val M_TURBO_START         = busif.newReg(doc = "Turbo Start register")
  val turbo_start = M_TURBO_START.field(Bool(), W1P, 0, doc = "turbo start pulse").asOutput()
  val M_TURBO_MOD           = busif.newReg(doc = "Turbo Mode")
  val umts_on   = M_TURBO_MOD.field(Bool(), RW, 1, doc = "1: umts mode\n0: emtc mode").asOutput()
  val M_TURBO_CRC_POLY      = busif.newReg(doc = "Turbo CRC Poly")
  val crc_mode  = M_TURBO_CRC_POLY.field(Bool(), RW, 1, doc = "0: CRC24; 1: CRC16").asOutput()
  val crc_poly  = M_TURBO_CRC_POLY.field(Bits(24 bit), RW, 0x864cfb, doc = "(D24+D23+D18+D17+D14+D11+D10+D7+D6+D5+D4+D3+D+1)").asOutput()
  val M_TURBO_K             = busif.newReg(doc = "Turbo block size")
  val K         = M_TURBO_K.field(Bits(12 bit), RW, 32, doc="decode block size \n max: 4032").asOutput()
  val M_TURBO_F1F2          = busif.newReg(doc = "Turbo Interleave Parameter")
  val f1        = M_TURBO_F1F2.field(Bits(9 bit), RW, 0x2f, doc="turbo interleave parameter f1").asOutput()
  M_TURBO_F1F2.reserved(7 bits)
  val f2        = M_TURBO_F1F2.field(Bits(9 bit), RW, 0x2f, doc="turbo interleave parameter f2").asOutput()
  val M_TURBO_MAX_ITER      = busif.newReg(doc = "Turbo Max Iter Times")
  val max_iter  = M_TURBO_MAX_ITER.field(Bits(6 bits), RW, 0, doc="Max iter times 1~63 avaliable").asOutput()
  val M_TURBO_FILL_NUM      = busif.newReg(doc = "Turbo block-head fill number")
  val fill_num  = M_TURBO_FILL_NUM.field(Bits(6 bits), RW, 0, doc="0~63 avaliable, Head fill Number").asOutput()
  val M_TURBO_3G_INTER_PV   = busif.newReg(doc = "Turbo UMTS Interleave Parameter P,V")
  val p     =  M_TURBO_3G_INTER_PV.field(Bits(9 bits), RW, 0,doc="parameter of prime").asOutput()
  M_TURBO_3G_INTER_PV.reserved(7 bits)
  val v     =  M_TURBO_3G_INTER_PV.field(Bits(5 bits), RW, 0,doc="Primitive root v").asOutput()
  val M_TURBO_3G_INTER_CRP  = busif.newReg(doc = "Turbo UMTS Interleave Parameter C,R,p")
  val Rtype     =  M_TURBO_3G_INTER_CRP.field(Bits(2 bits), RW, 0,doc="inter Row number\n0:5,1:10,2:20,3:20other").asOutput()
  val CPtype    =  M_TURBO_3G_INTER_CRP.field(Bits(2 bits), RW, 0,doc="CP relation\n0: C=P-1\n1: C=p\n2: C=p+1").asOutput()
  val KeqRxC    =  M_TURBO_3G_INTER_CRP.field(Bool(), RW, 0,doc="1:K=R*C else 0").asOutput()
  M_TURBO_3G_INTER_CRP.reserved(3 bits)
  val C         =  M_TURBO_3G_INTER_CRP.field(Bits(9 bits), RW, 0x7,doc="interlave Max Column Number").asOutput()
  val M_TURBO_3G_INTER_FILL = busif.newReg(doc = "Turbo UMTS Interleave Fill number")
  val fillPos   =  M_TURBO_3G_INTER_FILL.field(Bits(9 bits), RW, doc="interlave start Column of fill Number").asOutput()
  M_TURBO_3G_INTER_FILL.reserved(7 bits)
  val fillRow   =  M_TURBO_3G_INTER_FILL.field(Bits(2 bits), RW, doc="interlave fill Row number, 0~2 avaliable\n n+1 row").asOutput()

  busif.interruptFactory("T", io.a, io.b, io.c, io.d, io.e)

  busif.accept(CHeaderGenerator("header", "AP"))
  busif.accept(HtmlGenerator("regif", "AP"))
  busif.accept(JsonGenerator("regif"))
  busif.accept(RalfGenerator("regif"))
}

object playregif extends App{
  val sp = SpinalConfig()
    .copy(targetDirectory = "./out/playregif")
  sp.generateVerilog(new RegIfExample)
  sp.generateVerilog(new RegIfBasicAccessTest("apb3"))
  sp.generateVerilog(new RegIfBasicAccessTest("apb4"))
}