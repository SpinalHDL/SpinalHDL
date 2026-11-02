package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.tester.SpinalAnyFunSuite

class RegIfSizeMapTester extends SpinalAnyFunSuite {
  class SizeMapDut(body: BusIf => Unit) extends Component {
    val io = new Bundle {
      val apb = slave(Apb3(Apb3Config(16, 32)))
    }
    val busif = BusInterface(io.apb, (0x000, 16 Byte))
    body(busif)
  }

  def addRegs(busif: BusIf, n: Int): Unit = for (i <- 0 until n) {
    busif.newReg(doc = s"reg$i")(SymbolName(s"REG$i")).field(Bits(32 bit), AccessType.RW)(SymbolName(s"f$i"))
  }

  def addRegAt(busif: BusIf, address: BigInt, name: String): Unit = {
    busif.newRegAt(address, doc = name)(SymbolName(name)).field(Bits(32 bit), AccessType.RW)(SymbolName(s"${name}_f"))
  }

  def shouldFailWith(msg: String)(body: BusIf => Unit): Unit = {
    val e = intercept[Exception](SpinalVerilog(new SizeMapDut(body)))
    assert(e.getMessage.contains(msg), e.getMessage)
  }

  test("regs filling the size map") {
    SpinalVerilog(new SizeMapDut(addRegs(_, 4)))
  }

  test("newReg past the end of the size map") {
    shouldFailWith("exceeds the bus interface address space")(addRegs(_, 5))
  }

  test("newRegAt outside the size map") {
    shouldFailWith("exceeds the bus interface address space") { busif =>
      addRegAt(busif, 0x100, "REG")
    }
  }

  test("newRAMAt crossing the end of the size map") {
    shouldFailWith("exceeds the bus interface address space") { busif =>
      busif.newRAMAt(0x8, 16 Byte, doc = "ram")(SymbolName("RAM"))
    }
  }

  test("newRegAt on an address already used") {
    shouldFailWith("already used before") { busif =>
      addRegs(busif, 1)
      addRegAt(busif, 0x0, "REG_AGAIN")
    }
  }
}
