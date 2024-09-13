package spinal.lib.memory.sdram.Dfi.PHY

import spinal.lib._
import spinal.core._
import spinal.lib.memory.sdram.Dfi.Interface.{TaskParameter, TaskParameterAggregate, DfiAddr, DfiCmd, SdramConfig}
case class Initialize(tpa:TaskParameterAggregate) extends Component{
  import tpa._
  import tpa.pl._
  val io = new Bundle{
    val cmd      = Vec(master(Flow(DfiCmd(config))),config.frequencyRatio)
    val address  = Vec(master(Flow(DfiAddr(config))),config.frequencyRatio)
    val ckeN     = out Bits(config.chipSelectNumber * config.frequencyRatio bits)
    val initDone = out Bool()
  }
  def cmdphase(i:Int) = io.cmd(i)
  def addrphase(i:Int) = io.address(i)

  def NOP:Bits       = (B(0,config.chipSelectNumber bits) ## B"b111")
  def ACTIVE:Bits    = (B(0,config.chipSelectNumber bits) ## B"b011").setName("ACTIVE")
  def WRITE:Bits     = (B(0,config.chipSelectNumber bits) ## B"b100").setName("WRITE")
  def READ:Bits      = (B(0,config.chipSelectNumber bits) ## B"b101").setName("READ")
  def PRECHARGE:Bits = (B(0,config.chipSelectNumber bits) ## B"b010")
  def REFRESH:Bits   = (B(0,config.chipSelectNumber bits) ## B"b001").setName("REFRESH")
  def LOAD_MODE:Bits = (B(0,config.chipSelectNumber bits) ## B"b000")
  def ZQCL:Bits      = (B(0,config.chipSelectNumber bits) ## B"b110").setName("ZQCL")

  // Mode Configuration
  // - DLL disabled (low speed only)
  // - CL=6
  // - AL=0
  // - CWL=6
  def MR0_REG:Bits = B"h0120".resize(sdram.rowWidth)
  def MR1_REG:Bits = B"h0001".resize(sdram.rowWidth)
  def MR2_REG:Bits = B"h0008".resize(sdram.rowWidth)
  def MR3_REG:Bits = B"h0000".resize(sdram.rowWidth)

  def ALL_BANKS_BIT =10        // Precharge all banks
  def ZQCL_BIT =10        // Precharge all banks

  val refreshTimer = Reg(UInt(tp.refWidth bits)).init(sdram.ddrStartdelay)

  io.cmd.foreach(_.valid.set())
  io.cmd.foreach(_.payload.assignFromBits(NOP))
  io.address.foreach(_.valid.set())
  io.address.foreach(_.payload.clearAll())
  io.initDone.clear()
  io.ckeN.clearAll()

  when(refreshTimer === 0){
    refreshTimer.clearAll()
    io.initDone.set()
    io.cmd.foreach(_.valid.clear())
    io.address.foreach(_.valid.clear())
  }otherwise{
    refreshTimer := refreshTimer - 1
  }

  when(refreshTimer >= 50000 / (1000 / sdram.ddrMHZ)){
    io.ckeN.setAll()
  }
  when(refreshTimer === 48000 / (1000 / sdram.ddrMHZ)){
    cmdphase(config.cmdPhase).payload.assignFromBits(LOAD_MODE)
    addrphase(config.cmdPhase).bank := 2
    addrphase(config.cmdPhase).address := MR2_REG.resized
  }
  when(refreshTimer === 46000 / (1000 / sdram.ddrMHZ)){
    cmdphase(config.cmdPhase).payload.assignFromBits(LOAD_MODE)
    addrphase(config.cmdPhase).bank := 3
    addrphase(config.cmdPhase).address := MR3_REG.resized
  }
  when(refreshTimer === 44000 / (1000 / sdram.ddrMHZ)){
    cmdphase(config.cmdPhase).payload.assignFromBits(LOAD_MODE)
    addrphase(config.cmdPhase).bank := 1
    addrphase(config.cmdPhase).address := MR1_REG.resized
  }
  when(refreshTimer === 42000 / (1000 / sdram.ddrMHZ)){
    cmdphase(config.cmdPhase).payload.assignFromBits(LOAD_MODE)
    addrphase(config.cmdPhase).bank := 0
    addrphase(config.cmdPhase).address := MR0_REG.resized
  }
  when(refreshTimer === 40000 / (1000 / sdram.ddrMHZ)){
    cmdphase(config.cmdPhase).payload.assignFromBits(ZQCL)
    addrphase(config.cmdPhase).address(ZQCL_BIT).set()
  }
  when(refreshTimer === 200 / (1000 / sdram.ddrMHZ)){
    cmdphase(config.cmdPhase).payload.assignFromBits(PRECHARGE)
    addrphase(config.cmdPhase).address(ALL_BANKS_BIT).set()
  }

}
