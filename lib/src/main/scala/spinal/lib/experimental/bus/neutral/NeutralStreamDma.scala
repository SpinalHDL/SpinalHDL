package spinal.lib.experimental.bus.neutral

import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon._

/**
 * Created by PIC32F_USER on 23/04/2016.
 */
object NeutralStreamDma {
  case class Config( addressWidth: Int,
                     dataWidth : Int,
                     memCmdCountMax : BigInt,
                     burstLengthMax : Int,
                     fifoSize : Int,
                     pendingRequetMax : Int,
                     ctrlRspClock : ClockDomain = null){
    val burstWidth = log2Up(burstLengthMax+1)
    val memCmdCountWidth = log2Up(memCmdCountMax+1)

    def getAvalonConfig = AvalonMMConfig.bursted(
      addressWidth = addressWidth,
      dataWidth = dataWidth,
      burstCountWidth = burstWidth
    ).getReadOnlyConfig.copy(
      addressUnits = WORDS,
      maximumPendingReadTransactions = pendingRequetMax
    )
  }

  case class CtrlCmd(c: Config) extends Bundle {
    val startAt = UInt(c.addressWidth bit)
    val memCmdCount = UInt(c.memCmdCountWidth bit)
    val burstLength = UInt(c.burstWidth bit)
  }

  case class Ctrl(c : Config) extends Bundle with IMasterSlave{
    val cmd = Stream(CtrlCmd(c))
    val rsp = Stream Fragment(Bits(c.dataWidth bit))
    override def asMaster(): Unit = {
      master(cmd)
      slave(rsp)
    }
  }

  case class MemCmd(c:Config) extends Bundle{
    val address = UInt(c.addressWidth bit)
    val length = UInt(c.burstWidth bit)
  }


  case class Mem(c: Config) extends Bundle with IMasterSlave{
    val cmd = Stream(MemCmd(c))
    val rsp = Flow Fragment(Bits(c.dataWidth bit))
    override def asMaster(): Unit = {
      master(cmd)
      slave(rsp)
    }

    def toAvalon = {
      val ret = AvalonMM(c.getAvalonConfig)
      ret.read := cmd.valid
      ret.address := cmd.address
      ret.burstCount := cmd.length
      cmd.ready := ret.waitRequestn


      rsp.valid := ret.readDataValid
      rsp.last := False
      rsp.fragment := ret.readData
      val rspCounter = Reg(UInt(c.burstWidth bits)) init(1)
      when(rsp.valid){
        rspCounter := rspCounter + 1
        when(rspCounter === cmd.length){
          rspCounter := 1
          rsp.last := True
        }
      }

      ret
    }
  }

  class Block(c: Config) extends Component {
    val io = new Bundle {
      val ctrl = slave(Ctrl(c))
      val mem = master(Mem(c))
    }

    val pendingMemCmd = CounterMultiRequest(
      width=log2Up(c.pendingRequetMax + 1),
      io.mem.cmd.fire -> (_ + 1),
      (io.mem.rsp.fire && io.mem.rsp.last) -> (_ - 1)
    )

    val pendingMemRsp = CounterMultiRequest(
      width=log2Up(c.pendingRequetMax*c.burstLengthMax + 1),
      io.mem.cmd.fire -> (_ + io.mem.cmd.length),
      io.mem.rsp.fire -> (_ - 1)
    )

    val isActive = RegInit(False)
    val addressCounter = Reg(UInt(c.addressWidth bit))
    val memCmdCounter = Reg(UInt(c.memCmdCountWidth bit))
    
    io.ctrl.cmd.ready := False
    io.mem.cmd.valid := False
    when(!isActive) {
      when(io.ctrl.cmd.valid) {
        addressCounter := io.ctrl.cmd.startAt
        memCmdCounter := io.ctrl.cmd.memCmdCount
        isActive := True
      }
    } otherwise {
      when(memCmdCounter =/= 0){
        io.mem.cmd.valid := True
      } otherwise {
        when(pendingMemRsp <= 1 && io.mem.rsp.fire) {
          isActive := False
          io.ctrl.cmd.ready := True
        }
      }
    }

    when(io.mem.cmd.fire) {
      addressCounter := addressCounter + io.ctrl.cmd.burstLength
      memCmdCounter := memCmdCounter - 1
    }

    val memRsp = cloneOf(io.mem.rsp)
    memRsp.valid := io.mem.rsp.valid
    memRsp.last := io.ctrl.cmd.ready
    memRsp.fragment := io.mem.rsp.fragment

    val toManyPendingCmd = pendingMemCmd > c.pendingRequetMax-1
    val toManyPendingRsp = Bool
    val rspArea = if(c.ctrlRspClock == null || this.clockDomain == c.ctrlRspClock) new Area{
      val pendingMemToFifo = CounterMultiRequest(
        width=log2Up(c.fifoSize + 1),
        io.mem.cmd.fire -> (_ + io.mem.cmd.length),
        io.ctrl.rsp.fire -> (_ - 1)
      )
      toManyPendingRsp := pendingMemToFifo > c.fifoSize-io.ctrl.cmd.burstLength
      io.ctrl.rsp << memRsp.toStream.queue(c.fifoSize)
    } else new Area{
      val fifo = new StreamFifoCC(Fragment(Bits(c.dataWidth bit)),c.fifoSize,pushClock = ClockDomain.current,popClock = c.ctrlRspClock)
      fifo.io.push << memRsp.toStream
      fifo.io.pop >> io.ctrl.rsp

      toManyPendingRsp := RegNext(fifo.io.pushOccupancy) + pendingMemRsp > c.fifoSize-io.ctrl.cmd.burstLength-1    //-1 because of regnext fifo occupancy
    }

    when(toManyPendingCmd || toManyPendingRsp) {
      io.mem.cmd.valid := False
    }
    io.mem.cmd.address := addressCounter
    io.mem.cmd.length := io.ctrl.cmd.burstLength
  }


  def main(args: Array[String]) {
    SpinalVhdl({
      val rspClock = null //ClockDomain.external("rspClock")
      new Block(Config(
        addressWidth = 32,
        dataWidth = 32,
        memCmdCountMax = 1<<24,
        burstLengthMax = 8,
        fifoSize = 128,
        pendingRequetMax = 4,
        ctrlRspClock = rspClock
      )).setDefinitionName("TopLevel")
    })
  }
}


