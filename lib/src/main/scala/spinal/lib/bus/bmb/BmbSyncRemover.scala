package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

object BmbSyncRemover{

}

//TODO improve that crapy design XD
case class BmbSyncRemover(p : BmbParameter,rspQueueSize : Int = 8, pendingMax : Int = 16) extends Component{
  val io = new Bundle{
    val input = slave(Bmb(p.copy(invalidation = p.invalidation.copy(canSync = false))))
    val output = master(Bmb(p))
  }
  assert(p.access.sources.size == 1)

  val (cmdFork, rspFork) = StreamFork2(io.input.cmd)
  cmdFork >> io.output.cmd

  val rspIsWrite = rspFork.translateWith(rspFork.isWrite).takeWhen(rspFork.first).queueLowLatency(pendingMax, latency = 1)
  val rspBuffered = io.output.rsp.queueLowLatency(size = rspQueueSize, latency = 1)

  val syncCounters = for(sourceId <- p.access.sources.keys) yield  new Area {
    val rspBufferedHit = rspBuffered.source === sourceId

    val value = Reg(UInt(log2Up(pendingMax + 1) bits)) init(0)
    value := value + U(io.output.sync.fire && io.output.sync.source === sourceId) - U(rspBuffered.lastFire & rspBufferedHit && rspIsWrite.payload)

    val ok = value =/= 0 && rspBufferedHit
  }


  io.input.rsp << rspBuffered.continueWhen(!rspIsWrite.payload || syncCounters.map(_.ok).toSeq.orR)
  rspIsWrite.ready := rspBuffered.lastFire

  io.output.sync.ready := True
}
