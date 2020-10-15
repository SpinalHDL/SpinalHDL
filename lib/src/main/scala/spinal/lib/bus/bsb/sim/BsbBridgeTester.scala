package spinal.lib.bus.bsb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bsb.Bsb
import spinal.lib.sim.{StreamDriver, StreamReadyRandomizer}

import scala.collection.mutable
import scala.util.Random

class BsbBridgeTester(input : Bsb,
                      output : Bsb,
                      inputCd : ClockDomain,
                      outputCd : ClockDomain) {

  var lastSource = input.source.randomizedInt()
  var lastSink   = input.sink.randomizedInt()
  val inputDriver = StreamDriver(input, inputCd){ p =>
    if(Random.nextFloat() < 0.1){
      lastSource  = input.source.randomizedInt()
      lastSink    = input.sink.randomizedInt()
    }
    p.data.randomize()
    p.mask.randomize()
    p.source #= lastSource
    p.sink #= lastSink
    p.last #= Random.nextFloat() < 0.2
    true
  }
  val outputDriver = StreamReadyRandomizer(output, outputCd)

  case class ByteEvent(value: Byte, source: Int, sink: Int)
  case class LastEvent(source: Int, sink: Int)
  val ref = mutable.Queue[Any]()

  val inputMonitor = new BsbMonitor(input, inputCd) {
    override def onByte(value: Byte, source: Int, sink: Int): Unit = {
      ref += ByteEvent(value, source, sink)
    }
    override def onLast(source: Int, sink: Int): Unit = {
      ref += LastEvent(source, sink)
    }
  }

  var progress = 0
  val outputMonitor = new BsbMonitor(output, outputCd) {
    override def onByte(value: Byte, source: Int, sink: Int): Unit = {
      assert(ref.dequeue() ==  ByteEvent(value, source, sink))
      progress += 1
    }
    override def onLast(source: Int, sink: Int): Unit = {
      assert(ref.dequeue() == LastEvent(source, sink))
      progress += 1
    }
  }


  waitUntil(progress > 100000)
}
