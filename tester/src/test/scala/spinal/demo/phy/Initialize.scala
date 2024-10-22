package spinal.demo.phy

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi.interface.{DfiConfig, DfiControlInterface, TaskConfig}
case class Initialize(taskConfig: TaskConfig, ddrIoDfiConfig: DfiConfig) extends Component {
  import ddrIoDfiConfig._
  import taskConfig._
  val io = new Bundle {
    val control = master(DfiControlInterface(ddrIoDfiConfig))
    val initDone = out Bool ()
  }
  val refreshTimer = Reg(UInt(taskParameter.refWidth bits)).init(sdram.ddrStartdelay)
//  val refreshTimer = Reg(UInt(taskParameter.refWidth bits)).init(0)
  val cmd = new Bundle {
    val weN = cloneOf(io.control.weN)
    val casN = cloneOf(io.control.casN)
    val rasN = cloneOf(io.control.rasN)
    val csN = cloneOf(io.control.csN)
  }
  val control = cloneOf(io.control)
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
  def MR0_REG: Int = 0x0120
  def MR1_REG: Int = 0x0001
  def MR2_REG: Int = 0x0008
  def MR3_REG: Int = 0x0000
  def ALL_BANKS_BIT = 10 // Precharge all banks
  def ZQCL_BIT = 10 // Precharge all banks

  io.initDone.clear()
  control.cke.setAll()
  control.odt.clearAll()
  control.resetN.setAll()
  control.address.clearAll()
  control.bank.clearAll()

  when(refreshTimer === 0) {
    refreshTimer.clearAll()
    io.initDone.set()
  } otherwise {
    refreshTimer := refreshTimer - 1
  }

  when(refreshTimer >= 50000 / (1000 / sdram.ddrMHZ)) {
    control.cke.clearAll()
  }
  when(refreshTimer === 48000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(LOAD_MODE.setName("LOAD_MODE2"))
    control.bank := 2
    control.address := MR2_REG
  }
  when(refreshTimer === 46000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(LOAD_MODE.setName("LOAD_MODE3"))
    control.bank := 3
    control.address := MR3_REG
  }
  when(refreshTimer === 44000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(LOAD_MODE.setName("LOAD_MODE1"))
    control.bank := 1
    control.address := MR1_REG
  }
  when(refreshTimer === 42000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(LOAD_MODE.setName("LOAD_MODE0"))
    control.bank := 0
    control.address := MR0_REG
  }
  when(refreshTimer === 40000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(ZQCL)
    control.address(ZQCL_BIT).set()
  }
  when(refreshTimer === 200 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(PRECHARGE)
    control.address(ALL_BANKS_BIT).set()
  }
  control.assignUnassignedByName(cmd)
  io.control := RegNext(control)

}
