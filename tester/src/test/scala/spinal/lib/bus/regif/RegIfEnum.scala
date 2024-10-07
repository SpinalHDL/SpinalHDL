package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.regif._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.misc.SizeMapping
import spinal.tester.code.SpinalAnyFunSuite

object UartCtrlTxState extends SpinalEnum {
  val sIdle, sStart, sData, sParity, sStop = newElement()
}

class RegBankExample extends Component{
  val io = new Bundle{
    val a, b, c, d, e = in Bool()
    val apb = slave(Apb3(Apb3Config(16,32)))
    val input = slave Stream(Bits(8 bits))
    val output = master Stream(Bits(8 bits))
    val intr = out Bool()
    val is_start = out Bool()
  }

  io.output << io.input.queue(64)

  val busif = Apb3BusInterface(io.apb,(0x0000, 100 Byte))

  io.intr := busif.interruptFactory("my_int", io.a, io.b, io.c, io.d, io.e)

  val M_REG0  = busif.newReg(doc="REG0")
  val M_REG1  = busif.newReg(doc="REG1")
  val r1fd0 = M_REG1.field(Bits(16 bits), AccessType.RW, doc="fields 1").init(7)
  val r1fd2 = M_REG1.field(Bits(16 bits), AccessType.RW, doc="fields 2")
  val M_REG2  = busif.newReg(doc="REG2")
  val r2fd0 = M_REG2.field(UartCtrlTxState(), AccessType.RW, doc="state")
  io.is_start := r2fd0 === UartCtrlTxState.sStart

  val M_REGn  = busif.newRegAt(address=0x40, doc="REGn")
  val M_REGn1 = busif.newReg(doc="REGn1")

  busif.accept(CHeaderGenerator("header", "AP", regType = "unsigned int"))
  busif.accept(JsonGenerator("regif"))
}

object RegIfVerilog extends App {
  SpinalVerilog(new RegBankExample)
}

class RegIfVerilog extends SpinalAnyFunSuite {
  val rpt = SpinalVerilog(new RegBankExample)
  rpt.printRtl()
}
