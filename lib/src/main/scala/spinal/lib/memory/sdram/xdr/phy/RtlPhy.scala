package spinal.lib.memory.sdram.xdr.phy

import java.nio.file.{Files, Paths}

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.memory.sdram.SdramLayout
import spinal.lib.memory.sdram.sdr.SdramInterface
import spinal.lib.memory.sdram.xdr.{PhyLayout, SdramXdrPhyCtrl}

case class RtlPhyWriteCmd(pl : PhyLayout) extends Bundle {
  val address = UInt(pl.sdram.wordAddressWidth-log2Up(pl.phaseCount*pl.dataRate) bits)
  val data    = Bits(pl.beatWidth bits)
}

case class RtlPhyInterface(pl : PhyLayout) extends Bundle with IMasterSlave {
  val clk = in Bool()
  val cmd = Flow(RtlPhyWriteCmd(pl))

  override def asMaster(): Unit = {
    in(clk)
    slave(cmd)
  }

  def loadBin(offset : Long, path : String): Unit ={
    import spinal.core.sim._
    val bytePerBeat = pl.bytePerBeat
    assert(offset % bytePerBeat == 0)
    var bin = Files.readAllBytes(Paths.get(path))
    bin = bin ++ Array.fill(bytePerBeat-(bin.size % bytePerBeat))(0.toByte)
    for(beatId <- 0 until bin.size/bytePerBeat){
      var data = BigInt(0)
      for(byteId <- 0 until bytePerBeat){
        data = data | (BigInt(bin(beatId*bytePerBeat + byteId).toInt & 0xFF) << (byteId*8))
      }
      clk #= false
      cmd.valid #= true
      cmd.address #= offset/bytePerBeat + beatId
      cmd.data #= data
      sleep(0)
      clk #= true
      sleep(0)
    }
  }
}


case class RtlPhy(pl : PhyLayout) extends Component{
  def sl = pl.sdram

  val io = new Bundle {
    val ctrl = slave(SdramXdrPhyCtrl(pl))
    val write = master(RtlPhyInterface(pl))
  }

  val columnPerBeatLog2Up = log2Up(pl.phaseCount*pl.dataRate)

  import spinal.core.sim._
  val ram = Mem(Bits(pl.beatWidth bits), (1l << (sl.bankWidth + sl.columnWidth + sl.rowWidth))/(pl.phaseCount*pl.dataRate)).simPublic()

  ClockDomain(io.write.clk){
    ram.write(
      address = io.write.cmd.address,
      data = io.write.cmd.data,
      enable = io.write.cmd.valid
    )
  }

  val banks = for(bankId <- 0 until sl.bankCount) yield new Area {
    val active = Reg(Bool()) init (False)
    val row = Reg(UInt(sl.rowWidth bits))
  }

  case class Address() extends Bundle {
    val row = UInt(sl.rowWidth bits)
    val bank = UInt(sl.bankWidth bits)
    val column = UInt(sl.columnWidth bits)
  }

  val readCtrl = Flow(Address())
  readCtrl.valid := False
  readCtrl.row := banks.map(_.row).read(U(io.ctrl.BA))
  readCtrl.bank := U(io.ctrl.BA)
  readCtrl.column := U(io.ctrl.ADDR).resized

  val writeCtrl = Flow(Address())
  writeCtrl.valid := False
  writeCtrl.row := banks.map(_.row).read(U(io.ctrl.BA))
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
    address = (write.row @@ write.bank @@ (write.column >> columnPerBeatLog2Up)) | writeCounter.resized,
    data = io.ctrl.phases.flatMap(_.DQw).asBits(),
    enable = writeTrigger,
    mask = ~io.ctrl.phases.flatMap(_.DM).asBits()
  )

  io.ctrl.readValid := readTrigger
  val readed = Bits(pl.beatWidth bits).assignDontCare()
  when(readTrigger) {
    readed := ram.readAsync(
      address = (read.row @@ read.bank @@ (read.column >> columnPerBeatLog2Up)) | readCounter.resized
    ).addTag(crossClockDomain)
  }
  Vec(io.ctrl.phases.flatMap(_.DQr)).assignFromBits(readed)

  //Sim usage
  def loadBin(offset : Long, path : String): Unit ={
    import spinal.core.sim._
    val bytePerBeat = pl.bytePerBeat
    assert(offset % bytePerBeat == 0)
    var bin = Files.readAllBytes(Paths.get(path))
    bin = bin ++ Array.fill(bytePerBeat-(bin.size % bytePerBeat))(0.toByte)
    for(beatId <- 0 until bin.size/bytePerBeat){
      var data = BigInt(0)
      for(byteId <- 0 until bytePerBeat){
        data = data | (BigInt(bin(beatId*bytePerBeat + byteId).toInt & 0xFF) << (byteId*8))
      }
      ram.setBigInt(offset/bytePerBeat + beatId, data)
    }
  }

  def loadBytes(offset : Long, data : Seq[Byte]): Unit ={
    import spinal.core.sim._
    val bytePerBeat = pl.bytePerBeat
    assert(offset % bytePerBeat == 0)
    var bin = data
    bin = bin ++ Array.fill(bytePerBeat-(bin.size % bytePerBeat))(0.toByte)
    for(beatId <- 0 until bin.size/bytePerBeat){
      var data = BigInt(0)
      for(byteId <- 0 until bytePerBeat){
        data = data | (BigInt(bin(beatId*bytePerBeat + byteId).toInt & 0xFF) << (byteId*8))
      }
      ram.setBigInt(offset/bytePerBeat + beatId, data)
    }
  }
}