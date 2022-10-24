package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.sim.BmbBridgeTester
import spinal.lib.sim.{StreamDriver, StreamMonitor}

import scala.collection.mutable
import scala.util.Random



object BmbSyncRemover{
  def outputConfig(p : BmbAccessParameter) = p.sourcesTransform(s => s.copy(canSync = true, contextWidth = s.contextWidth + 1))
}


case class BmbSyncRemover(p : BmbParameter,rspQueueSize : Int = 8, pendingMax : Int = 16) extends Component{
  val io = new Bundle{
    val input = slave(Bmb(p))
    val output = master(Bmb(BmbSyncRemover.outputConfig(p.access)))
  }

  assert(pendingMax >= 4 && isPow2(pendingMax))

  case class Context() extends Bundle{
    val withSync = Bool()
    val input = Bits(p.access.contextWidth bits)
  }

  val cmdContext = Context()
  cmdContext.withSync := io.input.cmd.isWrite
  cmdContext.input := io.input.cmd.context

  val rspBuffered = io.output.rsp.queueLowLatency(size = rspQueueSize, latency = 1)
  val rspBufferedContext = rspBuffered.context.as(Context())

  val syncCounters = for(sourceId <- p.access.sources.keys) yield new Area {
    val rspBufferedHit = rspBuffered.source === sourceId

    val value = Reg(UInt(log2Up(pendingMax)+1 bits)) init(0)
    value := value + U(io.output.sync.fire && io.output.sync.source === sourceId) - U(rspBuffered.lastFire & rspBufferedHit && rspBufferedContext.withSync)

    val ok = value =/= 0 && rspBufferedHit
    val full = value.msb
  }


  io.output.cmd << io.input.cmd
  io.output.cmd.context.removeAssignments() := B(cmdContext)

  io.input.rsp << rspBuffered.continueWhen(!rspBufferedContext.withSync || syncCounters.map(_.ok).toSeq.orR)
  io.input.rsp.context.removeAssignments() := rspBufferedContext.input

  io.output.sync.ready := Delay(!(syncCounters.map(_.full).toList.orR), 2)
}


object BmbSyncRemoverTester extends App{
  import spinal.core.sim._
  SimConfig.compile{
    BmbSyncRemover(
      p = BmbParameter(
        access = BmbAccessParameter(
          addressWidth = 16,
          dataWidth = 32
        ) .addSources(4, BmbSourceParameter(
          lengthWidth = 5,
          contextWidth = 3,
          canRead =  true,
          canWrite = true,
          alignment = BmbParameter.BurstAlignement.BYTE
        ))
      )
    )
  }.doSimUntilVoid("test", seed = 42) { dut =>

    val pendingSync = Array.fill(1 << dut.p.access.sourceWidth) (0)

    StreamMonitor(dut.io.output.cmd, dut.clockDomain){ p =>
      if(p.last.toBoolean && p.opcode.toInt == Bmb.Cmd.Opcode.WRITE){
        pendingSync(p.source.toInt) += 1;
      }
    }

    StreamDriver(dut.io.output.sync, dut.clockDomain){ p =>
      var source = Random.nextInt(1 << dut.p.access.sourceWidth)
      if(pendingSync(source) != 0){
        pendingSync(source) -= 1
        p.source #= source
        true
      }else{
        false
      }

    }.transactionDelay = () => Random.nextInt(20)

    new BmbBridgeTester(
      master = dut.io.input,
      masterCd = dut.clockDomain,
      slave = dut.io.output,
      slaveCd = dut.clockDomain,
      rspCountTarget = 10000
    )
  }
}