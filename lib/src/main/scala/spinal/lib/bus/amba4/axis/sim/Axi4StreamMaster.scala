package spinal.lib.bus.amba4.axis.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axis.Axi4Stream._
import spinal.lib.sim._

import scala.collection.mutable

/**
 * Simulation master for the Axi4Stream bus protocol [[Axi4Stream]].
 *
 * @constructor create a simulation master with the given bus instance and clock domain
 * @param axis bus master to drive
 * @param clockDomain clock domain to sample data on
 * @example
 * {{{
 *   SimConfig.compile(new Component {
 *     val io = new Bundle {
 *       val axisSlave = slave(Axi4Stream(Axi4StreamConfig(32)))
 *     }
 *     io.axisSlave.assignDontCare
 *   }).doSim("sample") { dut =>
 *     val master = Axi4StreamMaster(dut.io.axisSlave, dut.clockDomain)
 *     master.send(Random.nextBytes(256).toList)
 *   }
 * }}}
 */
case class Axi4StreamMaster(axis: Axi4Stream, clockDomain: ClockDomain) {
  private val busConfig = axis.config
  private val queue = mutable.Queue[Axi4StreamBundle => Unit]()

  private def log(msg: String): Unit = {
    println(s"Axi4StreamMaster: $msg")
  }

  /** Send synchronously a full transaction to the bus. */
  def send(data: List[Byte]): Unit = {
    val mtx = SimMutex().lock()
    sendCB(data) {
      mtx.unlock()
    }
    mtx.await()
  }

  /** Send asynchronously; same as {@link send}, but completion is delivered in a callback */
  def sendCB(data: List[Byte])(callback: => Unit): Unit = {
    val fullLength = roundUp(data.length, busConfig.dataWidth).toInt
    if (fullLength != data.length && !busConfig.useStrb && !busConfig.useKeep) {
      log(s"not using strb or keep but length not multiple of data width; data will be zero padded")
    }

    val beats = (data.map { byte => (byte, 1) } padTo(fullLength, (0.toByte, 0)) grouped busConfig.dataWidth).toList
    log(s"initiating send, ${beats.length} beats in total")
    beats.zipWithIndex.foreach { case (dataWithStrb, idx) =>
      val (data, strbBinInts) = dataWithStrb.unzip
      val strb = strbBinInts.binIntsToBigInt
      queue += { bundle =>
        val isLast = idx + 1 == beats.length

        bundle.data #= data.toArray
        if (busConfig.useId) bundle.id.randomize()
        if (busConfig.useStrb) bundle.strb #= strb
        if (busConfig.useLast) bundle.last #= isLast
        if (busConfig.useKeep) bundle.keep #= strb

        log(f"beat #$idx: data ${data.bytesToHex} strb ${strb.hexString()} last $isLast")

        if (isLast) callback
      }
    }
  }

  private val driver = StreamDriver(axis, clockDomain) { b =>
    if (queue.isEmpty) false else {
      queue.dequeue()(b)
      true
    }
  }

  /** Reset bus slave (dropping all pending transactions) */
  def reset(): Unit = {
    queue.clear

    driver.reset
  }
}
