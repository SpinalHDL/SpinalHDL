package spinal.lib.bus.regif

import AccessType._

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.apb._

class RegIfExample extends Component {
  val io = new Bundle{
    val apb = slave(Apb3(Apb3Config(16,32)))
    val a, b, c, d, e = in Bool()
  }

  val busif = BusInterface(io.apb, (0x000,4 KiB), 0, regPre = "AP")

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
  val M_LONG = busif.newReg(doc = "Long Long regname")
  val version = M_LONG.field(Bits(16 bit), RW, doc = "Device version")
  val device = busif.newReg(doc= "deviceVersion").field(B(0x240503, 32 bit), ROV, doc = "Device version")
  val M_DEVICE = busif.newReg(doc = "Device Infomation")
  M_DEVICE.field(Bits(16 bit), ROV, resetValue = 0xf234, doc = "i2c version")
  M_DEVICE.field(Bits(4 bit), RW, resetValue = 0xa, doc = "test number")
  M_DEVICE.field(Bits(8 bit), ROV, resetValue = 0xf2, doc = "i3c version")
  M_DEVICE.field(Bits(2 bit), W1CRS, resetValue = 0x2, doc = "test2")

  val hw_set = in Bool()
  val hw_set_val = in Bits(10 bit)
  val M_HWSET0 = busif.newReg(doc = "HSRW Infomation")
  val reg_hsrw = M_HWSET0.fieldHSRW(hw_set, hw_set_val, resetValue = 0x0a, doc = "hsrw")
  val reg_rwhs = M_HWSET0.fieldRWHSAt(10, hw_set, hw_set_val, resetValue = 0x0b, doc = "rwhs")

  val M_HWSET1 = busif.newReg(doc = "HSRW Infomation")
  val reg_hsrw1 = M_HWSET1.fieldHSRWAt(10,hw_set, hw_set_val, resetValue = 0x0a, doc = "hsrw")
  val reg_rwhs1 = M_HWSET1.fieldRWHS( hw_set, hw_set_val, resetValue = 0x0b, doc = "rwhs")

  val M_CSTM = busif.newReg(doc = "cstm Infomation")
  val a = M_CSTM.field(Bits(16 bit), acc = CSTM("BMRW"), resetValue = 0x134, doc = "cstm")
  a := 0
  val b = M_CSTM.field(Bits( 4 bit), acc = RW, resetValue = 0x4, doc = "cstm")
  val c = M_CSTM.field(Bits(8 bit), acc = CSTM("DBRW"), resetValue = 0x34, doc = "cstm")
  c := 0
  busif.newReg(doc = "Device Infomation").field(Bits(32 bit), NA, 0, doc ="na")
  val irq = busif.interruptFactory("T", io.a, io.b, io.c, io.d, io.e)
  out(irq)

  val RAM = busif.newRAM(256 Byte, doc = "ram test")
  RAM.field(2, "doc ...")("ram_fd0")
  RAM.field(8, "doc ...")("ram_fd1")
  RAM.fieldAt(16, 2, "doc ...")("ram_fd2")
  RAM.field(3, "doc ...")("ram_fd3")
  RAM.bus.asMaster()

  val RAM2 = busif.newRAM(256 Byte, doc = "ram test")
  RAM2.field(2, "doc ...")("mem_fd0")
  RAM2.field(8, "doc ...")("mem_fd1")
  RAM2.fieldAt(16, 2, "doc ...")("mem_fd2")
  RAM2.field(3, "doc ...")("mem_fd3")
  RAM2.bus.asMaster()
  RAM2.updateReadBits = Mux(io.a, RAM2.readBits, B(0x12345678, 32 bit))

  val cc = busif.regPart("turbo"){
    val M_TURBO_MAX_ITER      = busif.newReg(doc = "Turbo Max Iter Times")
    val max_iter  = M_TURBO_MAX_ITER.field(Bits(6 bits), RW, 0, doc="Max iter times 1~63 avaliable").asOutput()
    val M_TURBO_FILL_NUM      = busif.newReg(doc = "Turbo block-head fill number")
    val fill_num  = M_TURBO_FILL_NUM.field(Bits(6 bits), RW, 0, doc="0~63 avaliable, Head fill Number").asOutput()
    val M_TURBO_3G_INTER_PVJJ   = busif.newReg(doc = "Turbo UMTS Interleave Parameter P,V")
    val testjjj   =  M_TURBO_3G_INTER_PVJJ.field(Bits(9 bits), RW, 0,doc="parameter of prime").asOutput()
    M_TURBO_3G_INTER_PVJJ.reserved(7 bits)
    val v     =  M_TURBO_3G_INTER_PVJJ.field(Bits(5 bits), RW, 0,doc="Primitive root v").asOutput()
  }

  val fifo = busif.newWrFifo(doc = "fifo test")
  fifo.field(16, "doc ...")("fifo_fd0")
  fifo.fieldNA(4)
  fifo.field(4, "testNA ...")("fifo_fd0")
  fifo.fieldAt(24, 1, "doc ...")("fifo_fd2")
  fifo.fieldAt(31, 1, "doc ...")("fifo_fd3")
  fifo.bus.asMaster()
  fifo.updateReadBits = RAM.readBits

  val fifo2 = busif.newWrFifoAt(0x300, doc = "fifo test")
  fifo2.field(16, "doc ...")("fifo_fd0")
  fifo2.bus.asMaster()

  (0 to 10).foreach{i =>
    busif.newBlockTag(s"r${i}")("Turbo")
    val Reg = busif.newReg("reg0")(SymbolName(s"RegA"))
    val Rtype = Reg.field(Bits(2 bits), RW, 0, doc = "inter Row number\n0:5,1:10,2:20,3:20other")(SymbolName("rta")).asOutput()
    val CPtype = Reg.field(Bits(2 bits), RW, 0, doc = "CP relation\n0: C=P-1\n1: C=p\n2: C=p+1")(SymbolName("rtb")).asOutput()
    val KeqRxC = Reg.field(Bool(), RW, 0, doc = "1:K=R*C else 0")(SymbolName("rtc")).asOutput()
    val Reg2 = busif.newReg(doc = "Turbo CRC Poly")(SymbolName(s"RegB_$i"))
    val crc_mode = Reg2.field(Bool(), RW, 1, doc = "0: CRC24; 1: CRC16")(SymbolName(s"crca_${i}")).asOutput()
    val crc_poly = Reg2.field(Bits(24 bit), RW, 0x864cfb, doc = "(D24+D23+D18+D17+D14+D11+D10+D7+D6+D5+D4+D3+D+1)")(SymbolName(s"crcb_${i}")).asOutput()
    busif.resetBlockTag()
  }

  val fifo3 = busif.newWrFifo(doc = "fifo test")
  fifo3.field(16, "doc ...")("fifo_fd0")
  fifo3.bus.asMaster()

  val grp0 = busif.newGrp(512 Byte, doc = "reg group test")
  val Reg = grp0.newReg("reg0")
  val ttype = Reg.field(Bits(2 bits), RW, 0, doc = "inter Row number\n0:5,1:10,2:20,3:20other").asOutput()
  val tPtype = Reg.field(Bits(2 bits), RW, 0, doc = "CP relation\n0: C=P-1\n1: C=p\n2: C=p+1").asOutput()
  val teqRxC = Reg.field(Bool(), RW, 0, doc = "1:K=R*C else 0").asOutput()
  val Reg2 = grp0.newReg(doc = "Turbo CRC Poly")
  val trc_mode = Reg2.field(Bool(), RW, 1, doc = "0: CRC24; 1: CRC16").asOutput()
  val trc_poly = Reg2.field(Bits(24 bit), RW, 0x864cfb, doc = "(D24+D23+D18+D17+D14+D11+D10+D7+D6+D5+D4+D3+D+1)").asOutput()
  val RAM3 = grp0.newRAM(256 Byte, doc = "ram test")
      RAM3.fieldAt(16, 2, "doc ...")("mem_fd2")
      RAM3.fieldNA(4 )
      RAM3.field(3, "doc TestRAM NA...")("mem_fd3")
      RAM3.bus.asMaster()

  val fifo4 = grp0.newFifo(doc = "fifo test")
     fifo4.field(16, "doc ...")("fifo_fd0")
     fifo4.bus.asMaster()

  val TPC = busif.newGrp(512 Byte, doc = "reg group test")
  val Reg1 = TPC.newReg("reg0")
  val ttype1 = Reg1.field(Bits(2 bits), RW, 0, doc = "inter Row number\n0:5,1:10,2:20,3:20other").asOutput()
  val tPtype1 = Reg1.field(Bits(2 bits), RW, 0, doc = "CP relation\n0: C=P-1\n1: C=p\n2: C=p+1").asOutput()
  val teqRxC1 = Reg1.field(Bool(), RW, 0, doc = "1:K=R*C else 0").asOutput()
  val Reg21 = TPC.newReg(doc = "Turbo CRC Poly")
  val trc_mode1 = Reg21.field(Bool(), RW, 1, doc = "0: CRC24; 1: CRC16").asOutput()
  val trc_poly1 = Reg21.field(Bits(24 bit), RW, 0x864cfb, doc = "(D24+D23+D18+D17+D14+D11+D10+D7+D6+D5+D4+D3+D+1)").asOutput()
  val RAM31 = TPC.newRAM(256 Byte, doc = "ram test")
  RAM31.fieldAt(16, 2, "doc ...")("mem_fd2")
  RAM31.field(3, "doc ...")("mem_fd3")
  RAM31.bus.asMaster()

  val fifo41 = TPC.newFifo(doc = "fifo test")
  fifo41.field(16, "doc ...")("fifo_fd0")
  fifo41.bus.asMaster()

  val CRG = busif.newRegSCR("SCR")
  val cg1 = CRG.field(Bool(), 0, "doc ...")
  val cg2 = CRG.field(Bits(3 bit), 0x4, "doc ...")
  val cg3 = CRG.fieldAt(16, Bits(8 bit), 0xf4, "doc ...")

  val CRG2 = busif.newRegSCRAt(0x0600, "SCR")
  val cg4 = CRG2.field(Bits(3 bit), 0x4, "doc ...")
  val cg5 = CRG2.fieldAt(16, Bits(8 bit), 0xf4, "doc ...")

  val M1 = busif.newIntrRFMS4(doc = "Interrupt")
  M1.field(io.a, 1, "event a")
  M1.field(io.b, 0, "event b")
  M1.fieldAt(16, io.c, 0, "event c")
  M1.field(io.d, 1, "event d")
  M1.field(io.e, 1, "event e")
  val intr = M1.intr()

  M_TURBO_3G_INTER_CRP.updateReadBits = M_TURBO_3G_INTER_FILL.readBits

  def part0(addr: BigInt, tname: String = "") = new Area {
    busif.newBlockTag(tname)("RAM")
    val RAM2 = busif.newRAMAt(addr, 16 Byte, doc = "Area ram test")
    RAM2.field(2, "doc ...")("mem_fd0")
    RAM2.field(8, "doc ...")("mem_fd1")
    RAM2.fieldAt(16, 2, "doc ...")("mem_fd2")
    RAM2.field(3, "doc ...")("mem_fd3")
    RAM2.updateReadBits = Mux(io.a, RAM2.readBits, B(0x12345678, 32 bit))
    val TEST = busif.newReg("TESTJJ")
    val param0 = TEST.field(Bits(8 bits), RW, 0, doc = "test param0")
    val param1 = TEST.field(Bits(8 bits), RW, 0, doc = "test param0")
    val bus = RAM2.bus
    val M_LONG = busif.newReg(doc = "Long Long regname")
    val version = M_LONG.field(Bits(32 bit), RW, 0x34afba00, doc = "Device version")
    val CRGB = busif.newRegSCR("SCR")
    val cg4 = CRGB.field(Bits(3 bit), 0x4, "doc ...")
    val cg5 = CRGB.fieldAt(16, Bits(8 bit), 0xf4, "doc ...")
    busif.resetBlockTag()
  }

  val x0 = part0(busif.getRegPtr(), "x0")
  val x1 = part0(busif.getRegPtr(), "x1")
  x0.bus.asMaster()
  x1.bus.asMaster()
  x0.param0.asOutput()
  x0.param1.asOutput()
  x1.param0.asOutput()
  x1.param1.asOutput()
  //  val x1 = part0(busif.getRegPtr())
  val ptr = busif.getRegPtr()
  for (i <- 0 to 6) {
    val t = part0(ptr + i * 12 * 4,  s"bran${i}ch")
    t.bus.setName(s"bus_ram${i}").asMaster()
    t.param0.asOutput()
    t.param1.asOutput()
  }

  val fiford = busif.newRdFifo(doc = "Readfifo test")
  fiford.field(16, "doc ...")("fifo_fd0")
  fiford.fieldNA(4)
  fiford.field(4, "testNA ...")("fifo_fd0")
  fiford.fieldAt(24, 1, "doc ...")("fifo_fd2")
  fiford.fieldAt(31, 1, "doc ...")("fifo_fd3")
  fiford.bus.asSlave()


  val hw_set2 = in Bool()
  val hw_set_val2 = in Bits(8 bit)
  val M_W1SHS = busif.newReg(doc = "HSRW Infomation")
  val reg_t0 = M_W1SHS.fieldW1HSAt(1, hw_set2, hw_set_val2, W1SHS, resetValue = 0x0a, doc = "hsrw")
  val reg_t1 = M_W1SHS.fieldW1HSAt(10,hw_set2, hw_set_val2, W1SHS, resetValue = 0x0a, doc = "hsrw")
  val reg_t2 = M_W1SHS.fieldW1HS  (   hw_set2, hw_set_val2, W1CHS, resetValue = 0x0b, doc = "rwhs")

  busif.accept(CHeaderGenerator("header", "AP", headers = List("Size: 4 KiB", "Author: Jack", "Version: 0.1(2022.11.24 22:29)")))
  busif.accept(HtmlGenerator("regif", "AP"))
  busif.accept(JsonGenerator("regif"))
  busif.accept(RalfGenerator("regif"))
  busif.accept(DocPlay("regif"))
  busif.accept(DocSVHeader("regif"))
}

object playregif extends App{
  val sp = SpinalConfig()
    .copy(targetDirectory = "./out/playregif", removePruned = true)
  sp.generateVerilog(new RegIfExample)
  sp.generateVerilog(new RegIfBasicAccessTest("apb3"))
  sp.generateVerilog(new RegIfBasicAccessTest("apb4"))
}