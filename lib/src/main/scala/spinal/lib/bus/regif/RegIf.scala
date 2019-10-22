package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import AccessType._

class MyRegBank extends Component {
  val apb = slave(Apb3(Apb3Config(16,32)))

  val busif = Apb3BusInterface(apb,0)

  val M_TURBO_EARLY_QUIT    = busif.newReg(doc = "Turbo Hardware-mode register1")
  val M_TURBO_THETA         = busif.newReg(doc = "Turbo Hardware-mode register2")

  val early_quit  = M_TURBO_EARLY_QUIT.field(1 bit, RW, B(0), doc = "CRC validate early quit enable").asOutput()
  M_TURBO_EARLY_QUIT.field(3 bits, NA)
  val early_count = M_TURBO_EARLY_QUIT.field(2 bit, RW, B(2,2 bits), doc = "CRC validate early quit enable").asOutput()
  M_TURBO_EARLY_QUIT.reserved(2 bits)
  val same_count = M_TURBO_EARLY_QUIT.field(5 bit, RW, B(3), doc = "CRC validate early quit enable").asOutput()

  val theta     = M_TURBO_THETA.field(1 bit, RW, B(1,1 bits), doc = "Back-Weight, UQ(4,3), default 0.75").asOutput()
  M_TURBO_THETA.field(5 bits, NA)
  val bib_order = M_TURBO_THETA.field(4 bit, RW, B(5,4 bits), doc = "bib in Byte, defalut. \n 0:[76543210] \n 1:[01234567]").asOutput()
  val readBase = M_TURBO_THETA.fieldoffset(16, 10 bit, RW, B(0), doc = "bib in Byte, defalut. \n 0:[76543210] \n 1:[01234567]").asOutput()
  M_TURBO_EARLY_QUIT.bmiRead()
  M_TURBO_THETA.bmiRead()
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
