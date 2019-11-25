package spinal.lib.memory.sdram.xdr.phy

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.memory.sdram.SdramLayout
import spinal.lib.memory.sdram.sdr.SdramInterface
import spinal.lib.memory.sdram.xdr.{Phy, PhyParameter}

case class RtlPhyWriteCmd(pl : PhyParameter) extends Bundle {
  val address = UInt(pl.sdram.wordAddressWidth bits)
  val data    = Bits(pl.wordWidth bits)
}
case class RtlPhyInterface(pl : PhyParameter) extends Bundle with IMasterSlave {
  val clk = in Bool()
  val write = Flow(RtlPhyWriteCmd(pl))

  override def asMaster(): Unit = {
    in(clk)
    slave(write)
  }
}

case class RtlPhy(sl : SdramLayout) extends Phy[RtlPhyInterface](SdrInferedPhy.memoryLayoutToPhyLayout(sl)){
  override def MemoryBus(): RtlPhyInterface = RtlPhyInterface(pl)
  override def driveFrom(mapper: BusSlaveFactory): Unit = {}

  val ram = Mem(Bits(pl.wordWidth bits), 1l << (sl.bankWidth + sl.columnWidth + sl.rowWidth))
  ClockDomain(io.memory.clk){
    ram.write(
    address = io.memory.write.address,
    data = io.memory.write.data,
    enable = io.memory.write.valid
    )
  }

  val banks = for(bankId <- 0 until sl.bankCount) yield new Area {
    val active = Reg(Bool) init (False)
    val row = Reg(UInt(sl.rowWidth bits))
  }

  case class Address() extends Bundle {
    val bank = UInt(sl.bankWidth bits)
    val column = UInt(sl.columnWidth bits)
  }

  val readCtrl = Flow(Address())
  readCtrl.valid := False
  readCtrl.bank := U(io.ctrl.BA)
  readCtrl.column := U(io.ctrl.ADDR).resized

  val writeCtrl = Flow(Address())
  writeCtrl.valid := False
  writeCtrl.bank := U(io.ctrl.BA)
  writeCtrl.column := U(io.ctrl.ADDR).resized

  for(phase <- io.ctrl.phases){
    when(phase.CKE && !phase.CSn){
      //Precharge
      when(!phase.RASn && phase.CASn && !phase.WEn){
        for((bank, bankId) <- banks.zipWithIndex){
          when(io.ctrl.ADDR(10) || io.ctrl.BA === bankId){
            bank.active := False
          }
        }
      }
      //Active
      when(!phase.RASn && phase.CASn && phase.WEn){
        for((bank, bankId) <- banks.zipWithIndex){
          when(io.ctrl.BA === bankId){
            bank.active := True
            bank.row := U(io.ctrl.ADDR).resized
          }
        }
      }
      //Write
      when(phase.RASn && !phase.CASn && !phase.WEn){
        writeCtrl.valid := True
      }
      //Read
      when(phase.RASn && !phase.CASn && phase.WEn){
        readCtrl.valid := True
      }
    }
  }

  val (write, writeCounter) = writeCtrl.toStream.queueLowLatency(32).repeat(pl.beatCount)
  val (read, readCounter) = readCtrl.toStream.queueLowLatency(32).repeat(pl.beatCount)

  val writeTrigger = Delay(io.ctrl.writeEnable, pl.writeDelay, init = False)
  val readTrigger = Delay(io.ctrl.readEnable, pl.readDelay, init = False)

  write.ready := writeTrigger
  read.ready := readTrigger

  ram.write(
    address = banks.map(_.row).read(write.bank) @@ write.bank @@ write.column,
    data = io.ctrl.phases.flatMap(_.DQw).asBits(),
    enable = writeTrigger,
    mask = ~io.ctrl.phases.flatMap(_.DM).asBits()
  )

  io.ctrl.readValid := readTrigger
  val readed = Bits(pl.wordWidth bits).assignDontCare()
  when(readTrigger) {
    readed := ram.readAsync(
      address = banks.map(_.row).read(read.bank) @@ read.bank @@ read.column
    )
  }
  Vec(io.ctrl.phases.flatMap(_.DQr)).assignFromBits(readed)
}