package spinal.tester.code

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif.AccessType._
import spinal.lib.bus.regif._

class RegIfExample extends Component {
  val io = new Bundle{
    val apb = slave(Apb3(Apb3Config(16,32)))
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
  val fill_num  = M_TURBO_FILL_NUM.field(Bits(6 bit), RW, 0, doc="0~63 avaliable, Head fill Number").asOutput()
  val M_TURBO_3G_INTER_PV   = busif.newReg(doc = "Turbo UMTS Interleave Parameter P,V")
  val p     =  M_TURBO_3G_INTER_PV.field(Bits(9 bit), RW, 0,doc="parameter of prime").asOutput()
  M_TURBO_3G_INTER_PV.reserved(7 bits)
  val v     =  M_TURBO_3G_INTER_PV.field(Bits(5 bit), RW, 0,doc="Primitive root v").asOutput()
  val M_TURBO_3G_INTER_CRP  = busif.newReg(doc = "Turbo UMTS Interleave Parameter C,R,p")
  val Rtype     =  M_TURBO_3G_INTER_CRP.field(UInt(2 bits), RW, 0,doc="inter Row number\n0:5,1:10,2:20,3:20other").asOutput()
  val CPtype    =  M_TURBO_3G_INTER_CRP.field(SInt(2 bits), RW, 0,doc="CP relation\n0: C=P-1\n1: C=p\n2: C=p+1").asOutput()
  val KeqRxC    =  M_TURBO_3G_INTER_CRP.field(Bool(), RW, 0,doc="1:K=R*C else 0").asOutput()
  M_TURBO_3G_INTER_CRP.reserved(3 bits)
  val C         =  M_TURBO_3G_INTER_CRP.field(UInt(9 bit), RW, 0x7,doc="interlave Max Column Number").asOutput()
  val M_TURBO_3G_INTER_FILL = busif.newReg(doc = "Turbo UMTS Interleave Fill number")
  val fillPos   =  M_TURBO_3G_INTER_FILL.field(UInt(9 bit), RW, doc="interlave start Column of fill Number").asOutput()
  M_TURBO_3G_INTER_FILL.reserved(7 bits)
  val fillRow   =  M_TURBO_3G_INTER_FILL.field(Bits(9 bit), RW, doc="interlave fill Row number, 0~2 avaliable\n n+1 row").asOutput()

  val M_TEST = busif.newReg("TEST REGISTER")
  val reg = SInt(8 bit)
  reg := 33
  M_TEST.registerInOnlyReadLogic(reg, RW, 0x32, "onlyregister not generate WriteLogic but have readLogic")
  M_TEST.registerAtOnlyReadLogic(10, reg, RW, 0x32, "onlyregister not generate WriteLogic but have readLogic")
  val txx = Reg(UInt(6 bit)) init 0
  M_TEST.registerInWithWriteLogic(txx, RW, 0x332, "register with writeLogic")
}

class RegIfExample2 extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(16, 32)))
  }
  val busif = Apb3BusInterface(io.apb, (0x000, 100 Byte))
  val M_REG0  = busif.newReg(doc="Word 0")
  val M_REG1  = busif.newReg(doc="Word 1")
  val M_REG2  = busif.newReg(doc="Word 2")
  val M_REG3  = busif.newReg(doc="中文测试")

  val M_REGn  = busif.newRegAt(address= 0x40, doc="Word n")
  val M_REGn1 = busif.newReg(doc="Word n+1")

  val M_REGm  = busif.newRegAt(address= 0x100, doc="Word m")
  val M_REGm1 = busif.newReg(doc="Word m+1")

  val fd0 = M_REG0.field(2 bits, RW, doc= "fields 0")
  M_REG0.reserved(5 bits)
  val fd1 = M_REG0.field(3 bits, RW, doc= "fields 0")
  val fd2 = M_REG0.field(3 bits, RW, doc= "fields 0")
  //auto reserved 2 bits
  val fd3 = M_REG0.fieldAt(pos=16, 4 bits, RW, doc= "fields 3")
//  val fd3 = M_REG0.field(4 bits, RW, doc= "fields 3")
  //auto reserved 12 bits
  busif.accept(HtmlGenerator("regif.html", "Example"))
}

class InterruptRegIf extends Component {
  val io = new Bundle{
    val psc_done, pth_done, ssc_done, grp_done, scd_done, srch_finish = in Bool()
    val interrupt   = out Bool()
    val apb = slave(Apb3(Apb3Config(16, 32)))
  }

  val busif = Apb3BusInterface(io.apb, (0x000, 100 Byte))

  val M_SRCH_INT_EN   = busif.newReg(doc="srch int enable register")
  val psc_int_en      = M_SRCH_INT_EN.field(Bool(), RW, doc="psc interrupt enable register")
  val pth_int_en      = M_SRCH_INT_EN.field(Bool(), RW, doc="pth interrupt enable register")
  val ssc_int_en      = M_SRCH_INT_EN.field(Bool(), RW, doc="ssc interrupt enable register")
  val grp_int_en      = M_SRCH_INT_EN.field(Bool(), RW, doc="grp interrupt enable register")
  val scd_int_en      = M_SRCH_INT_EN.field(Bool(), RW, doc="scd interrupt enable register")
  val srch_finish_en  = M_SRCH_INT_EN.field(Bool(), RW, doc="srch finish interrupt enable register")

  val M_SRCH_INT_MASK  = busif.newReg(doc="srch int mask register")
  val psc_int_mask     = M_SRCH_INT_MASK.field(Bool(), RW, doc="psc interrupt mask register")
  val pth_int_mask     = M_SRCH_INT_MASK.field(Bool(), RW, doc="pth interrupt mask register")
  val ssc_int_mask     = M_SRCH_INT_MASK.field(Bool(), RW, doc="ssc interrupt mask register")
  val grp_int_mask     = M_SRCH_INT_MASK.field(Bool(), RW, doc="grp interrupt mask register")
  val scd_int_mask     = M_SRCH_INT_MASK.field(Bool(), RW, doc="scd interrupt mask register")
  val srch_finish_mask = M_SRCH_INT_MASK.field(Bool(), RW, doc="srch finish interrupt mask register")

  val M_SRCH_INT_STATUS = busif.newReg(doc="srch int status register")

  val psc_int_status     = M_SRCH_INT_STATUS.field(Bool(), RC, doc="psc interrupt status register")
  val pth_int_status     = M_SRCH_INT_STATUS.field(Bool(), RC, doc="pth interrupt status register")
  val ssc_int_status     = M_SRCH_INT_STATUS.field(Bool(), RC, doc="ssc interrupt status register")
  val grp_int_status     = M_SRCH_INT_STATUS.field(Bool(), RC, doc="grp interrupt status register")
  val scd_int_status     = M_SRCH_INT_STATUS.field(Bool(), RC, doc="scd interrupt status register")
  val srch_finish_status = M_SRCH_INT_STATUS.field(Bool(), RC, doc="srch finish interrupt status register")

  when(io.psc_done && psc_int_en){psc_int_status.set()}
  when(io.pth_done && pth_int_en){pth_int_status.set()}
  when(io.ssc_done && ssc_int_en){ssc_int_status.set()}
  when(io.grp_done && grp_int_en){grp_int_status.set()}
  when(io.scd_done && scd_int_en){scd_int_status.set()}
  when(io.srch_finish && srch_finish_en){srch_finish_status(0)}

  io.interrupt := (psc_int_status & pth_int_status & ssc_int_status &
                   grp_int_status & scd_int_status & srch_finish_status)
}
class InterruptRegIf3 extends Component {
  val io = new Bundle {
    val psc_done, pth_done, ssc_done, grp_done, scd_done, srch_finish = in Bool()
    val interrupt = out Bool()
    val apb = slave(Apb3(Apb3Config(16, 32)))
  }
  val busif = Apb3BusInterface(io.apb, (0x000, 100 Byte))

  val int = InterruptFactory(busif,"M_INT",io.psc_done,
    io.pth_done,io.ssc_done,io.grp_done,io.scd_done,io.srch_finish)

  io.interrupt := int
}

class cpInterruptExample extends Component {
  val io = new Bundle {
    val tx_done, rx_done, frame_end = in Bool()
    val interrupt = out Bool()
    val apb = slave(Apb3(Apb3Config(16, 32)))
  }
  val busif = Apb3BusInterface(io.apb, (0x000, 100 Byte))

  val M_CP_INT_EN    = busif.newReg(doc="cp int enable register")
  val tx_int_en      = M_CP_INT_EN.field(Bool(), RW, doc="tx interrupt enable register")
  val rx_int_en      = M_CP_INT_EN.field(Bool(), RW, doc="rx interrupt enable register")
  val frame_int_en   = M_CP_INT_EN.field(Bool(), RW, doc="frame interrupt enable register")
  val M_CP_INT_MASK  = busif.newReg(doc="cp int mask register")
  val tx_int_mask      = M_CP_INT_MASK.field(Bool(), RW, doc="tx interrupt mask register")
  val rx_int_mask      = M_CP_INT_MASK.field(Bool(), RW, doc="rx interrupt mask register")
  val frame_int_mask   = M_CP_INT_MASK.field(Bool(), RW, doc="frame interrupt mask register")
  val M_CP_INT_STATE   = busif.newReg(doc="cp int state register")
  val tx_int_state      = M_CP_INT_STATE.field(Bool(), RW, doc="tx interrupt state register")
  val rx_int_state      = M_CP_INT_STATE.field(Bool(), RW, doc="rx interrupt state register")
  val frame_int_state   = M_CP_INT_STATE.field(Bool(), RW, doc="frame interrupt state register")

  when(io.rx_done && rx_int_en){tx_int_state.set()}
  when(io.tx_done && tx_int_en){tx_int_state.set()}
  when(io.frame_end && frame_int_en){tx_int_state.set()}

  io.interrupt := (tx_int_mask && tx_int_state  ||
    rx_int_mask && rx_int_state ||
    frame_int_mask && frame_int_state)
}

class cpInterruptFactoryExample extends Component {
  val io = new Bundle {
    val tx_done, rx_done, frame_end = in Bool()
    val interrupt = out Bool()
    val apb = slave(Apb3(Apb3Config(16, 32)))
  }
  val busif2 = Apb3BusInterface(io.apb, (0x000, 100 Byte))

  val tx = io.tx_done
  val rx = io.rx_done
  val frame = io.frame_end

  io.interrupt := InterruptFactory(busif2,"M_CP",tx,rx,frame)
}

class ahbregifExample extends Component {
  import spinal.lib.bus.amba3.ahblite._
  val io = new Bundle {
    val tx_done, rx_done, frame_end = in Bool()
    val interrupt = out Bool()
    val ahb = slave(AhbLite3(AhbLite3Config(16, 32)))
  }
//  val busif2 = AhbLite3BusInterface(io.ahb, (0x000, 100 Byte))
  val busif2 = BusInterface(io.ahb, (0x000, 100 Byte))

  val tx = io.tx_done
  val rx = io.rx_done
  val frame = io.frame_end

  io.interrupt := InterruptFactory(busif2,"M_CP",tx,rx,frame)
}

object getRegIfExample {
  def main(args: Array[String]) {
    val example = SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
    mergeAsyncProcess              = true,
    targetDirectory="tmp/")
      .generate(new RegIfExample)

    example.toplevel.busif.accept(CHeaderGenerator("header.h", "AP"))
    example.toplevel.busif.accept(HtmlGenerator("regif.html", "AP"))
    example.toplevel.busif.accept(JsonGenerator("regif.json"))
  }
}
