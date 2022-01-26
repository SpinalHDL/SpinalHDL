package spinal.lib.bus.bsb.sim

import spinal.core.ClockDomain
import spinal.lib.bus.bsb.Bsb
import spinal.lib.sim.{StreamDriver, StreamMonitor}
import spinal.core.sim._

import scala.collection.mutable
import scala.util.Random



abstract class BsbMonitor(bsb : Bsb, cd : ClockDomain) extends StreamMonitor(bsb, cd){
  addCallback{ p =>
    val mask = p.mask.toBigInt
    val data = p.data.toBigInt
    val source = p.source.toInt
    val sink = p.sink.toInt
    val withLast = bsb.last.toBoolean
    for(i <- 0 until bsb.p.byteCount){
      if(mask.testBit(i)) onByte((data >> i*8).toByte, source, sink)
    }
    if(withLast) onLast(source,sink)
  }

  def onByte(value : Byte, source : Int, sink : Int): Unit
  def onLast(source : Int, sink : Int): Unit
}

case class BsbPacket(source : Int, sink : Int, data : Seq[Byte])

case class BsbDriver(bsb : Bsb, cd : ClockDomain) {
  val sources = Array.fill(1 << bsb.p.sourceWidth)(mutable.Queue[BsbPacket]())
  val progresses = Array.fill(1 << bsb.p.sourceWidth)(0)
  def sourcesHeadEmpty(source : Int) = sources(source).head.data.size == progresses(source)

  def push(packet : BsbPacket) = sources(packet.source).enqueue(packet)

  val sd = StreamDriver(bsb, cd) { p =>
    val sources = this.sources.filter(_.nonEmpty).toSeq
    if (sources.isEmpty) {
      false
    } else {
      val packets = sources.randomPick()
      val packet = packets.head
      var data = BigInt(0)
      var mask = BigInt(0)
      for (byteId <- 0 until bsb.p.byteCount) if (!sourcesHeadEmpty(packet.source) && Random.nextBoolean()) {
        data |= BigInt(packet.data(progresses(packet.source)).toInt & 0xFF) << byteId * 8
        mask |= 1 << byteId
        progresses(packet.source) = progresses(packet.source) + 1
      }
      p.data #= data
      p.mask #= mask
      p.source #= packet.source
      p.sink #= packet.sink
      if (sourcesHeadEmpty(packet.source)  && Random.nextBoolean()) {
        p.last #= true
        progresses(packet.source) = 0
        packets.dequeue()
      } else {
        p.last #= false
      }
      true
    }
  }
}
