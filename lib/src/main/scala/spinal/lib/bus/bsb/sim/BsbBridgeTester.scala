package spinal.lib.bus.bsb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bsb.Bsb
import spinal.lib.sim.StreamReadyRandomizer

import scala.collection.mutable
import scala.util.Random

class BsbBridgeTester(input : Bsb,
                      output : Bsb,
                      inputCd : ClockDomain,
                      outputCd : ClockDomain) {

  val inputDriver = new BsbDriver(input, inputCd)
  val outputDriver = StreamReadyRandomizer(output, outputCd)


  val ref = Array.fill(1 << input.p.sourceWidth)(mutable.Queue[BsbPacket]())
  val progresses = Array.fill(1 << input.p.sourceWidth)(0)

  val outputMonitor = new BsbMonitor(output, outputCd) {
    override def onByte(value: Byte, source: Int, sink: Int): Unit = {
      val packet = ref(source).head
      assert(packet.sink == sink)
      assert(packet.data(progresses(source)) == value)
      progresses(source) = progresses(source) + 1
    }
    override def onLast(source: Int, sink: Int): Unit = {
      assert(ref(source).head.data.size == progresses(source))
      progresses(source) = 0
      ref(source).dequeue()
    }
  }


  val agents = for(sourceId <- 0 until 1 << input.p.sourceWidth){
    for(_ <- 0 until 100){
      val packet = new BsbPacket(source = input.source.randomizedInt(), sink = input.sink.randomizedInt(), (0 until Random.nextInt(100) + 1).map(_ => Random.nextInt.toByte).toArray)
      ref(packet.source).enqueue(packet)
      inputDriver.push(packet)
    }
  }

  waitUntil(ref.forall(_.isEmpty))
}
