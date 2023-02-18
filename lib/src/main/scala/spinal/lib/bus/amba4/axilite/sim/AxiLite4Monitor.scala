package spinal.lib.bus.amba4.axilite.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.sim._

import scala.collection.mutable


abstract class AxiLite4ReadOnlyMonitor(ar: Stream[AxiLite4Ax], r: Stream[AxiLite4R], clockDomain: ClockDomain) {

  val busConfig = ar.config

  def onReadStart(addr: BigInt): Unit = {}
  def onReadByte(addr: BigInt, data: Byte): Unit = {}
  def onResponse(addr: BigInt, resp: Byte): Unit = {}

  val arQueue = mutable.Queue[BigInt]()
  val rQueue = mutable.Queue[(BigInt, Byte)]()

  def update(): Unit = {
    while(arQueue.nonEmpty && rQueue.nonEmpty) {
      val addr = arQueue.dequeue()

      val (data, resp) = rQueue.dequeue()
      for(i <- 0 until busConfig.bytePerWord) {
        onReadByte(addr+i, ((data >> i*8) & 0xFF).toByte)
      }
      onResponse(addr, resp)
    }
  }

  val arMonitor = StreamMonitor(ar, clockDomain){_ =>
    val size = busConfig.bytePerWord
    val beatAddr = ar.addr.toBigInt
    val start = beatAddr & ~BigInt(size - 1)
    onReadStart(start)
    arQueue += start
    update()
  }

  val rMonitor = StreamMonitor(r, clockDomain){_ =>
    val data = r.data.toBigInt
    val resp = r.resp.toInt.toByte
    rQueue += ((data, resp))
    update()
  }

  def reset(): Unit = {
    arQueue.clear()
    rQueue.clear()
  }
}

abstract class AxiLite4WriteOnlyMonitor(aw: Stream[AxiLite4Ax], w: Stream[AxiLite4W], b: Stream[AxiLite4B], clockDomain: ClockDomain) {

  val busConfig = aw.config

  def onWriteStart(addr: BigInt): Unit = {}
  def onWriteByteAlways(addr: BigInt, data: Byte, strobe: Boolean): Unit = {}
  def onWriteByte(addr: BigInt, data: Byte): Unit = {}
  def onResponse(addr: BigInt, resp: Byte): Unit = {}

  val awQueue = mutable.Queue[BigInt]()
  val wQueue = mutable.Queue[() => Unit]()
  val bAddrQueue = mutable.Queue[BigInt]()
  val bQueue = mutable.Queue[Byte]()

  def update(): Unit = {
    while(awQueue.nonEmpty && wQueue.nonEmpty) {
      wQueue.dequeue().apply()
    }
    while(bAddrQueue.nonEmpty && bQueue.nonEmpty) {
      onResponse(bAddrQueue.dequeue(), bQueue.dequeue())
    }
  }

  val awMonitor = StreamMonitor(aw, clockDomain){_ =>
    val size = busConfig.bytePerWord
    val beatAddr = aw.addr.toBigInt

    val start = beatAddr & ~BigInt(size-1)

    awQueue += start
    update()
  }

  val wMonitor = StreamMonitor(w, clockDomain){_ =>
    val strb = w.strb.toBigInt
    val data = w.data.toBigInt
    wQueue += { () =>
      val addr = awQueue.dequeue()
      onWriteStart(addr)
      val strobes = (0 until busConfig.bytePerWord).map(strb.testBit).toArray
      for (i <- 0 until busConfig.bytePerWord) {
        val _byte = ((data >> (8*i)).toInt & 0xFF).toByte
        onWriteByteAlways(addr+i, _byte, strobes(i))
        if (strobes(i))
          onWriteByte(addr+i, _byte)
      }
      bAddrQueue += addr
    }
    update()
  }

  val bMonitor = StreamMonitor(b, clockDomain){_ =>
    bQueue += b.resp.toInt.toByte
    update()
  }

  def reset(): Unit = {
    awQueue.clear()
    wQueue.clear()
    bQueue.clear()
  }
}
