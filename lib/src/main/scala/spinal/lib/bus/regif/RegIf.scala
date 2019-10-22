package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import AccessType._

class MyRegBank extends Component {
  val apb = slave(Apb3(Apb3Config(16,32)))

  val busSlave = Apb3BusInterface(apb,0)

  val M_TURBO_EARLY_QUIT    = busSlave.newReg(doc = "Turbo Hardware-mode register1")
  val M_TURBO_THETA         = busSlave.newReg(doc = "Turbo Hardware-mode register2")

  val early_quit  = M_TURBO_EARLY_QUIT.field(1 bit, RW, 0, doc = "CRC validate early quit enable").asOutput()
  M_TURBO_EARLY_QUIT.field(3 bits, NA)
  val early_count = M_TURBO_EARLY_QUIT.field(2 bit, RW, 2, doc = "CRC validate early quit enable").asOutput()
  M_TURBO_EARLY_QUIT.reserved(2 bits)
  val same_count = M_TURBO_EARLY_QUIT.field(5 bit, RW, 3, doc = "CRC validate early quit enable").asOutput()

  val theta     = M_TURBO_THETA.field(1 bit, RW, 0, doc = "Back-Weight, UQ(4,3), default 0.75").asOutput()
  M_TURBO_THETA.field(5 bits, NA)
  val bib_order = M_TURBO_THETA.field(8 bit, RW, 0x23, doc = "bib in Byte, defalut. \n 0:[76543210] \n 1:[01234567]").asOutput()
  val readBase = M_TURBO_THETA.fieldoffset(16, 10 bit, RW, 0, doc = "bib in Byte, defalut. \n 0:[76543210] \n 1:[01234567]").asOutput()
  val M_TURBO_MOD           = busSlave.newReg(doc = "Turbo Mode")
  val M_TURBO_CRC_POLY      = busSlave.newReg(doc = "Turbo CRC Poly")
  val M_TURBO_K             = busSlave.newReg(doc = "Turbo block size")
  val M_TURBO_F1F2          = busSlave.newReg(doc = "Turbo Interleave Parameter")
  val M_TURBO_MAX_ITER      = busSlave.newReg(doc = "Turbo Max Iter Times")
  val M_TURBO_FILL_NUM      = busSlave.newReg(doc = "Turbo block-head fill number")
  val M_TURBO_3G_INTER_PV   = busSlave.newReg(doc = "Turbo UMTS Interleave Parameter P,V")
  val M_TURBO_3G_INTER_CRP  = busSlave.newReg(doc = "Turbo UMTS Interleave Parameter C,R,p")
  val M_TURBO_3G_INTER_FILL = busSlave.newReg(doc = "Turbo UMTS Interleave Fill number")
  val umts_on   = M_TURBO_MOD.field(1 bit, RW, 1, doc = "")

  val crc_mode  = M_TURBO_CRC_POLY.field(1 bit, RW, 1, doc = "0: CRC24; 1: CRC16")
  val crc_poly  = M_TURBO_CRC_POLY.field(24 bit, RW, 0x864cfb, doc = "(D24+D23+D18+D17+D14+D11+D10+D7+D6+D5+D4+D3+D+1)")
  val K         = M_TURBO_K.field(12 bit, RW, 32, doc="decode block size \n max: 4032")
  val f1        = M_TURBO_F1F2.field(10 bit, RW, 0x2f, doc="turbo interleave parameter f1")
  M_TURBO_F1F2.reserved(6 bits)
  val f2        = M_TURBO_F1F2.field(10 bit, RW, 0x2f, doc="turbo interleave parameter f2")

  busSlave.readGenerator
  busSlave.docGenerator()
}

object getFIRVerilog {
  def main(args: Array[String]) {
    SpinalConfig(mode = Verilog,
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC,
        clockEdge = RISING,
        resetActiveLevel = LOW),
      targetDirectory="tmp/")
      .generate(new MyRegBank)
  }
}
