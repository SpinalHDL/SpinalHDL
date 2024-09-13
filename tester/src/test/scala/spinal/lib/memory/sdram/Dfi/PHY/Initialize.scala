package spinal.lib.memory.sdram.Dfi.PHY

import spinal.lib._
import spinal.core._
import spinal.lib.memory.sdram.Dfi.Interface.{TaskParameter, TaskParameterAggregate, DfiAddr, DfiCmd, SdramConfig}
case class Initialize(tpa:TaskParameterAggregate) extends Component{
  import tpa._
  import tpa.pl._
  val io = new Bundle{
    val cmd      = master(Flow{new Bundle{
      val weN = out Bool()
      val casN = out Bool()
      val rasN = out Bool()
      val csN = out Bool()
    }})
    val address  = master(Flow(new Bundle{
      val address = Bits(sdram.rowWidth bits)
      val bank = Bits(sdram.bankWidth bits)
    }))
    val cke     = out Bool()
    val initDone = out Bool()
  }
  def cmdphase = io.cmd
  def addrphase = io.address

  def NOP:Bits       =  B"b0111"
  def ACTIVE:Bits    =  B"b0011".setName("ACTIVE")
  def WRITE:Bits     =  B"b0100".setName("WRITE")
  def READ:Bits      =  B"b0101".setName("READ")
  def PRECHARGE:Bits =  B"b0010".setName("PRECHARGE")
  def REFRESH:Bits   =  B"b0001".setName("REFRESH")
  def LOAD_MODE:Bits =  B"b0000"
  def ZQCL:Bits      =  B"b0110".setName("ZQCL")

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

  io.cmd.valid.set()
  io.cmd.payload.assignFromBits(NOP.setName("NOP"))
  io.address.valid.set()
  io.address.payload.clearAll()
  io.initDone.clear()
  io.cke.set()

  when(refreshTimer === 0){
    refreshTimer.clearAll()
    io.initDone.set()
    io.cmd.valid.clear()
    io.address.valid.clear()
  }otherwise{
    refreshTimer := refreshTimer - 1
  }

  when(refreshTimer >= 50000 / (1000 / sdram.ddrMHZ)){
    io.cke.clear()
  }
  when(refreshTimer === 48000 / (1000 / sdram.ddrMHZ)){
    cmdphase.payload.assignFromBits(LOAD_MODE.setName("LOAD_MODE2"))
    addrphase.bank := 2
    addrphase.address := MR2_REG.resized
  }
  when(refreshTimer === 46000 / (1000 / sdram.ddrMHZ)){
    cmdphase.payload.assignFromBits(LOAD_MODE.setName("LOAD_MODE3"))
    addrphase.bank := 3
    addrphase.address := MR3_REG.resized
  }
  when(refreshTimer === 44000 / (1000 / sdram.ddrMHZ)){
    cmdphase.payload.assignFromBits(LOAD_MODE.setName("LOAD_MODE1"))
    addrphase.bank := 1
    addrphase.address := MR1_REG.resized
  }
  when(refreshTimer === 42000 / (1000 / sdram.ddrMHZ)){
    cmdphase.payload.assignFromBits(LOAD_MODE.setName("LOAD_MODE0"))
    addrphase.bank := 0
    addrphase.address := MR0_REG.resized
  }
  when(refreshTimer === 40000 / (1000 / sdram.ddrMHZ)){
    cmdphase.payload.assignFromBits(ZQCL)
    addrphase.address(ZQCL_BIT).set()
  }
  when(refreshTimer === 200 / (1000 / sdram.ddrMHZ)){
    cmdphase.payload.assignFromBits(PRECHARGE)
    addrphase.address(ALL_BANKS_BIT).set()
  }

}
