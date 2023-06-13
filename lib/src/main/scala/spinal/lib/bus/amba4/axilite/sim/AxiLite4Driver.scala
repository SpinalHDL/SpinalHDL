package spinal.lib.bus.amba4.axilite.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.Stream
import spinal.lib.bus.amba4.axilite._
import spinal.lib.sim.{StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable

case class AxiLite4Driver(axi : AxiLite4, clockDomain : ClockDomain) {
  def reset() : Unit = {
    axi.aw.valid #= false
    axi.w.valid #= false
    axi.ar.valid #= false
    axi.r.ready #= true
    axi.b.ready #= true
  }

  reset()

  def read(address : BigInt) : BigInt = {
    axi.ar.payload.prot.assignBigInt(6)
    
    axi.ar.valid #= true
    axi.ar.payload.addr #= address
    axi.ar.prot #= 0

    axi.r.ready #= true

    clockDomain.waitSamplingWhere(axi.ar.ready.toBoolean)

    axi.ar.valid #= false

    clockDomain.waitSamplingWhere(axi.r.valid.toBoolean)

    axi.r.ready #= false

    axi.r.payload.data.toBigInt
  }

  val awQueue = mutable.Queue[() => Unit]()
  val awDriver = StreamDriver(axi.aw, clockDomain) { _ =>
    if (awQueue.nonEmpty) {
      awQueue.dequeue().apply()
      true
    } else {
      false
    }
  }

  val wQueue = mutable.Queue[() => Unit]()
  val wDriver = StreamDriver(axi.w, clockDomain) { _ =>
    if (wQueue.nonEmpty) {
      wQueue.dequeue().apply()
      true
    } else {
      false
    }
  }

  def write(address : BigInt, data : BigInt) : Unit = {
    awQueue.enqueue { () =>
      axi.aw.addr #= address
      axi.aw.prot #= 0
    }

    wQueue.enqueue { () =>
      axi.w.data #= data
      axi.w.strb #= (BigInt(1) << axi.config.bytePerWord) - 1
    }

    clockDomain.waitSamplingWhere(wQueue.isEmpty && awQueue.isEmpty)
    clockDomain.waitSamplingWhere(axi.b.ready.toBoolean && axi.b.valid.toBoolean)
  }

  def writeRandom(address: BigInt): Unit = {
    awQueue.enqueue { () =>
      axi.aw.addr #= address
      axi.aw.prot #= 0
    }

    wQueue.enqueue { () =>
      axi.w.data.randomize()
      axi.w.strb #= (BigInt(1) << axi.config.bytePerWord) - 1
    }

    clockDomain.waitSamplingWhere(wQueue.isEmpty && awQueue.isEmpty)
    clockDomain.waitSamplingWhere(axi.b.ready.toBoolean && axi.b.valid.toBoolean)
  }
}

class AxiLite4ReadOnlySlaveAgent(ar: Stream[AxiLite4Ax], r: Stream[AxiLite4R], clockDomain: ClockDomain) {

  val busConfig = ar.config

  val arQueueDepth = 1

  def doRead(addr: BigInt): Unit = {
    r.payload.data.randomize()

    val aligned = (addr & busConfig.bytePerWord-1) == 0
    if (aligned) {
      r.payload.resp #= 0
    } else {
      r.payload.resp #= 2
    }
  }

  var arQueue = mutable.Queue[BigInt]()

  val arMonitor = StreamMonitor(ar, clockDomain){_ =>
    val beatAddr = ar.addr.toBigInt

    arQueue += beatAddr
  }

  val rDriver = StreamDriver(r, clockDomain){_ =>
    if (arQueue.nonEmpty) {
      doRead(arQueue.dequeue())
      true
    } else {
      false
    }
  }

  val arRandomizer = StreamReadyRandomizer(ar, clockDomain, () => arQueue.size < arQueueDepth)
}

class AxiLite4WriteOnlySlaveAgent(aw: Stream[AxiLite4Ax], w: Stream[AxiLite4W], b: Stream[AxiLite4B], clockDomain: ClockDomain) {

  val busConfig = aw.config

  val awQueueDepth = 1

  val awQueue = mutable.Queue[BigInt]()
  val wQueue = mutable.Queue[(BigInt, BigInt)]()

  def onWrite(addr: BigInt, data: BigInt, strb: BigInt): Unit = {
    val aligned = (addr & busConfig.bytePerWord - 1) == 0
    if (aligned) {
      b.payload.resp #= 0
    } else {
      b.payload.resp #= 2
    }
  }

  val awMonitor = StreamMonitor(aw, clockDomain) { _ =>
    val beatAddr = aw.addr.toBigInt

    awQueue += beatAddr
  }

  val wMonitor = StreamMonitor(w, clockDomain) { _ =>
    val strb = w.strb.toBigInt
    val data = w.data.toBigInt
    wQueue += ((data, strb))
  }

  val bDriver = StreamDriver(b, clockDomain){_ =>
    if (awQueue.nonEmpty && wQueue.nonEmpty) {
      val addr = awQueue.dequeue()
      val (data, resp) = wQueue.dequeue()
      onWrite(addr, data, resp)
      true
    } else {
      false
    }
  }

  val awRandomizer = StreamReadyRandomizer(aw, clockDomain, () => awQueue.size < awQueueDepth)
  val wRandomizer = StreamReadyRandomizer(w, clockDomain)
}
