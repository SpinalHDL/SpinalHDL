package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}
import spinal.lib.bus.regif.AccessType.RW

class RegIfSecurity extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(16, 32)))
    val tzpc = in Bool()
  }

  val busif = BusInterface(io.apb, (0x000,4 KiB), 0, regPre = "AP", withSecFireWall = true)

  val M_TURBO_EARLY_QUIT = busif.newReg(doc = "Turbo Hardware-mode register1", Secure.AS())
  val early_quit = M_TURBO_EARLY_QUIT.field(Bool(), RW, 0, doc = "CRC validate early quit enable").asOutput()
  val early_count = M_TURBO_EARLY_QUIT.field(Bits(2 bit), RW, 2, doc = "CRC validate early quit enable").asOutput()

  val REG2 = busif.newReg(doc ="test", Secure.MS(false, true))
  val test = REG2.field(Bits(32 bit), RW, 0x00, doc ="test").asOutput()

  val M_TURBO_THETA = busif.newReg(doc = "Turbo Hardware-mode register2", Secure.CS(io.tzpc))
  val theta = M_TURBO_THETA.field(Bits(4 bit), RW, 0x6, doc = "Back-Weight, UQ(4,3), default 0.75").asOutput()
  val bib_order = M_TURBO_THETA.field(Bool(), RW, 1, doc = "bib in Byte, defalut. \n 0:[76543210] \n 1:[01234567]").asOutput()

  val REG3 = busif.newReg(doc = "test", Secure.MS(true, false))
  val test1 = REG3.field(Bits(32 bit), RW, 0x00, doc = "test2").asOutput()

  val REG4 = busif.newReg(doc = "test", Secure.CS(io.tzpc))
  val test2 = REG4.field(Bits(32 bit), RW, 0x00, doc = "test2").asOutput()
}

object playregifsec extends App{
  val sp = SpinalConfig()
    .copy(targetDirectory = "./out/playregif", removePruned = true, headerWithDate = true)
  sp.generateVerilog(new RegIfSecurity)
}
