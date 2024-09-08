package spinal.lib.memory.sdram.xdr.Dfi.Tools

import spinal.core._
import spinal.lib.memory.sdram.xdr.Dfi.Interface.DDR.DDRConfig
import spinal.lib.memory.sdram.xdr.Dfi.Interface.{CoreParameterAggregate, CoreTasks, DfiAddr, DfiCmd, DfiConfig}
import spinal.lib._

case class CmdTxd(cpa:CoreParameterAggregate) extends Component{
import cpa._
  val io = new Bundle{
    val task    = slave(CoreTasks(cpa))
    val cmd     = Vec(master(Flow(DfiCmd(config))),config.frequencyRatio)
    val address = Vec(master(Flow(DfiAddr(config))),config.frequencyRatio)
  }
  def cmdphase(i:Int) = io.cmd(i)
  def addrphase(i:Int) = io.address(i)

  def ACTIVE:Bits    = (~(B(1) << io.task.task.address.csAddr).resize(config.chipSelectNumber) ## B"b011").setName("ACTIVE")
  def WRITE:Bits     = (~(B(1) << io.task.task.address.csAddr).resize(config.chipSelectNumber) ## B"b100").setName("WRITE")
  def READ:Bits      = (~(B(1) << io.task.task.address.csAddr).resize(config.chipSelectNumber) ## B"b101").setName("READ")
  def PRECHARGE:Bits = (~(B(1) << io.task.task.address.csAddr).resize(config.chipSelectNumber) ## B"b010")
  def REFRESH:Bits   = (~(B(1) << io.task.task.address.csAddr).resize(config.chipSelectNumber) ## B"b001").setName("REFRESH")

  def AUTO_PRECHARGE_BIT = 10  // Disable auto precharge (auto close of row)
  def ALL_BANKS_BIT =10        // Precharge all banks
  def COULMNRang = 0 until(cpa.pl.sdram.columnWidth)
  def BANK:Bits   = io.task.task.address.bank.asBits
  def ROW:Bits    =  io.task.task.address.row.asBits
  def COLUMN:Bits    =  io.task.task.address.column.asBits

  def active = io.task.task.active
  def write = io.task.task.write
  def read  = io.task.task.read
  def precharge  = io.task.task.precharge
  def prechargeAll = io.task.prechargeAll
  def refresh = io.task.refresh

  io.cmd.foreach(_.valid.clear())
  io.cmd.foreach(_.payload.setAll())
  io.address.foreach(_.valid.clear())
  io.address.foreach(_.address.clearAll())
  io.address.foreach(_.bank.clearAll())

  when(active){
    cmdphase(config.cmdPhase).valid.set()
    cmdphase(config.cmdPhase).payload.assignFromBits(ACTIVE)
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).bank := BANK.resized
    addrphase(config.cmdPhase).address := ROW.resized
  }
  when(write){
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(WRITE)
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).bank := BANK.resized
    addrphase(config.cmdPhase).address(COULMNRang) := COLUMN
    addrphase(config.cmdPhase).address(AUTO_PRECHARGE_BIT).clear()
  }
  when(read){
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(READ)
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).bank := BANK.resized
    addrphase(config.cmdPhase).address(COULMNRang) := COLUMN
    addrphase(config.cmdPhase).address(AUTO_PRECHARGE_BIT).clear()
  }
  when(precharge){
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(PRECHARGE.setName("PRECHARGE"))
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).bank := BANK.resized
    addrphase(config.cmdPhase).address(ALL_BANKS_BIT).clear()
  }
  when(prechargeAll){
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(PRECHARGE.setName("PRECHARGEALL"))
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).address(ALL_BANKS_BIT).set()

  }
  when(refresh){
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(REFRESH)
    addrphase(config.cmdPhase).valid.set()
  }

}
