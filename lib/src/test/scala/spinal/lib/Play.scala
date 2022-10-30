package spinal.lib

import spinal.core._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.regif.AccessType._
import spinal.lib.bus.regif._

import scala.collection.mutable.ArrayBuffer

object playbinary extends App{
  assert("16".hexToBinInts == List(0,1,1,0,1))
  println("16".hexToBinInts)
  println(32.toBinInts)
  println(77.binString)
  println(77.binString(16))
  println(77.toBinInts(num = 16))
  println("123".getBytes.map(_.toBinaryString).toList)
  println("qwe".getBytes().flatMap(_.toBinInts).toList)
  println("qwe".getBytes().toList)
  println("中文字体".getBytes().toList)
  println("中文字体".getBytes().toList.flatMap(_.binString))
  println("123".hexToBinInts)
}

object playbinary2 extends App{
  assert("16".hexToBinInts == List(0,1,1,0,1))
  println(x"32456".toBinInts())
  println(x"32456" , x"32456".toBinInts().binIntsToHex)
  println(x"32456" , x"32456".toBinInts().binIntsToHexAlignHigh)
  assert("32456" == o"32456".toBinInts().binIntsToOct)
  assert("32456" == x"32456".toBinInts().binIntsToHex)
  assert(324565 == 324565.toBinInts().binIntsToInt)
  assert(32 == 32.toBinInts().binIntsToInt)
  assert(134 == 134.toBinInts().binIntsToInt)
  assert(BigInt("abcdef0123456789abcdef", 16) == BigInt("abcdef0123456789abcdef", 16).toBinInts().binIntsToBigInt)
}

object VecPlay extends App {
  SpinalVhdl(new Component {
    val io = new Bundle {
      val i = in(Bits(8 bit))
      val o = out(Vec(Bits(4 bit), 2))
    }

    val a = Bits(4 bit)
    val b = Bits(4 bit)

    val v = Vec(List(a, b))
    val data = v.as(Bits(8 bit))

    data := io.i
    io.o := v
  })
}

class RegIfExample0 extends Component {
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

object RegIfExample0 extends App {
  SpinalVhdl(new RegIfExample0())
}