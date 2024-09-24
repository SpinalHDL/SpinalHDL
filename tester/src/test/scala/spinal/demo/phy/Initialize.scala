package spinal.demo.phy

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi.interface.{DfiControlInterface, TaskParameterAggregate}
case class Initialize(tpa: TaskParameterAggregate) extends Component {
  import tpa._
  import tpa.config._
  val io = new Bundle {
    val control = master(DfiControlInterface(config))
    val initDone = out Bool ()
  }
  val refreshTimer = Reg(UInt(tp.refWidth bits)).init(sdram.ddrStartdelay)
  val cmd = new Bundle {
    val weN = cloneOf(io.control.weN)
    val casN = cloneOf(io.control.casN)
    val rasN = cloneOf(io.control.rasN)
    val csN = cloneOf(io.control.csN)
  }
  cmd.setAll()
  def cmdphase = cmd
  def NOP: Bits = B"b0111"
  def ACTIVE: Bits = B"b0011".setName("ACTIVE")
  def WRITE: Bits = B"b0100".setName("WRITE")
  def READ: Bits = B"b0101".setName("READ")
  def PRECHARGE: Bits = B"b0010".setName("PRECHARGE")
  def REFRESH: Bits = B"b0001".setName("REFRESH")
  def LOAD_MODE: Bits = B"b0000"
  def ZQCL: Bits = B"b0110".setName("ZQCL")
  // Mode Configuration
  // - DLL disabled (low speed only)
  // - CL=6
  // - AL=0
  // - CWL=6
  def MR0_REG: Bits = B"h0120".resize(sdram.rowWidth)
  def MR1_REG: Bits = B"h0001".resize(sdram.rowWidth)
  def MR2_REG: Bits = B"h0008".resize(sdram.rowWidth)
  def MR3_REG: Bits = B"h0000".resize(sdram.rowWidth)
  def ALL_BANKS_BIT = 10 // Precharge all banks
  def ZQCL_BIT = 10 // Precharge all banks

  io.initDone.clear()
  io.control.cke.setAll()
  io.control.odt.clearAll()
  io.control.resetN.setAll()
  io.control.address.clearAll()
  io.control.bank.clearAll()

  when(refreshTimer === 0) {
    refreshTimer.clearAll()
    io.initDone.set()
  } otherwise {
    refreshTimer := refreshTimer - 1
  }

  when(refreshTimer >= 50000 / (1000 / sdram.ddrMHZ)) {
    io.control.cke.clearAll()
  }
  when(refreshTimer === 48000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(LOAD_MODE.setName("LOAD_MODE2"))
    io.control.bank := 2
    io.control.address := MR2_REG.resized
  }
  when(refreshTimer === 46000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(LOAD_MODE.setName("LOAD_MODE3"))
    io.control.bank := 3
    io.control.address := MR3_REG.resized
  }
  when(refreshTimer === 44000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(LOAD_MODE.setName("LOAD_MODE1"))
    io.control.bank := 1
    io.control.address := MR1_REG.resized
  }
  when(refreshTimer === 42000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(LOAD_MODE.setName("LOAD_MODE0"))
    io.control.bank := 0
    io.control.address := MR0_REG.resized
  }
  when(refreshTimer === 40000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(ZQCL)
    io.control.address(ZQCL_BIT).set()
  }
  when(refreshTimer === 200 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(PRECHARGE)
    io.control.address(ALL_BANKS_BIT).set()
  }
  io.control.assignUnassignedByName(cmd)

}
