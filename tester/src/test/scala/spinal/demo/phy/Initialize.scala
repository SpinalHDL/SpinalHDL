package spinal.demo.phy

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi._
case class Initialize(taskConfig: TaskConfig, ddrIoDfiConfig: DfiConfig) extends Component {
  val io = new Bundle {
    val control = master(DfiControlInterface(ddrIoDfiConfig))
    val initDone = out Bool ()
  }
  val sdram = ddrIoDfiConfig.sdram
  val refreshTimer = Reg(UInt(taskConfig.taskParameter.refWidth bits)).init(606000 / (1000 / sdram.ddrMHZ))
//    val refreshTimer = Reg(UInt(taskConfig.taskParameter.refWidth bits)).init(0)
  val cmd = new Bundle {
    val weN = cloneOf(io.control.weN)
    val casN = cloneOf(io.control.casN)
    val rasN = cloneOf(io.control.rasN)
    val csN = cloneOf(io.control.csN)
  }
  val control = cloneOf(io.control)
  cmd.setAll()
  def cmdphase = cmd
  def NOP: Bits = B(0, ddrIoDfiConfig.chipSelectNumber bits) ## B"b111"
  def ACTIVE: Bits = B(0, ddrIoDfiConfig.chipSelectNumber bits) ## B"b011".setName("ACTIVE")
  def WRITE: Bits = B(0, ddrIoDfiConfig.chipSelectNumber bits) ## B"b100".setName("WRITE")
  def READ: Bits = B(0, ddrIoDfiConfig.chipSelectNumber bits) ## B"b101".setName("READ")
  def PRECHARGE: Bits = B(0, ddrIoDfiConfig.chipSelectNumber bits) ## B"b010".setName("PRECHARGE")
  def REFRESH: Bits = B(0, ddrIoDfiConfig.chipSelectNumber bits) ## B"b001".setName("REFRESH")
  def LOAD_MODE: Bits = B(0, ddrIoDfiConfig.chipSelectNumber bits) ## B"b000"
  def ZQCL: Bits = B(0, ddrIoDfiConfig.chipSelectNumber bits) ## B"b110".setName("ZQCL")
  // Mode Configuration
  // - DLL disabled (low speed only)
  // - CL=6
  // - AL=0
  // - CWL=6
//  def MR0_REG: Int = 0x0120
//  def MR1_REG: Int = 0x0001
//  def MR2_REG: Int = 0x0008
//  def MR3_REG: Int = 0x0000
  def MR0_REG: Int = 0x0120
  def MR1_REG: Int = 0x0040
  def MR2_REG: Int = 0x0208
  def MR3_REG: Int = 0x0000
  def MR4_REG: Int = 0x0000
  def MR5_REG: Int = 0x0400
  def MR6_REG: Int = 0x0800
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

  when(refreshTimer >= 56000 / (1000 / sdram.ddrMHZ)) {
    control.cke.clearAll()
  }
  when(refreshTimer === 54000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(LOAD_MODE.setName("LOAD_MODE3"))
    control.bank := 3
    control.address := MR3_REG
  }
  when(refreshTimer === 52000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(LOAD_MODE.setName("LOAD_MODE6"))
    control.bank := 6
    control.address := MR6_REG
  }
  when(refreshTimer === 50000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(LOAD_MODE.setName("LOAD_MODE5"))
    control.bank := 5
    control.address := MR5_REG
  }
  when(refreshTimer === 48000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(LOAD_MODE.setName("LOAD_MODE4"))
    control.bank := 4
    control.address := MR4_REG
  }
  when(refreshTimer === 46000 / (1000 / sdram.ddrMHZ)) {
    cmdphase.assignFromBits(LOAD_MODE.setName("LOAD_MODE2"))
    control.bank := 2
    control.address := MR2_REG
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
