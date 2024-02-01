package spinal.lib.bus.amba4.axis.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.sim._
import spinal.lib.bus.amba4.axis.Axi4Stream._

import scala.collection.mutable

/**
 * Simulation slave for the Axi4Stream bus protocol [[Axi4Stream]].
 *
 * @constructor create a simulation slave with the given bus instance and clock domain
 * @param axis bus slave to drive
 * @param clockDomain clock domain to sample data on
 * @example
 * {{{
 *   SimConfig.compile(new Component {
 *     val io = new Bundle {
 *       val axisMaster = master(Axi4Stream(Axi4StreamConfig(32)))
 *     }
 *     io.axisMaster.assignDontCare
 *   }).doSim("sample") { dut =>
 *     val slave = Axi4StreamSlave(dut.io.axisMaster, dut.clockDomain)
 *     val data = slave.recv()
 *   }
 * }}}
 *
 * @note The current implementation does not buffer unexpected transactions (i.e. bus activity when no
 * {@link recv} has been issued).  In some race conditions, this may result in incomplete captures due to the first
 * beats being lost.  Consider enqueuing a asynchronous request with the callback interface ({@link recvCB}) before
 * issuing the triggering action.
 */
case class Axi4StreamSlave(axis: Axi4Stream, clockDomain: ClockDomain) {
  private val busConfig = axis.config
  private val queue = mutable.Queue[Axi4StreamBundle => Unit]()

  private def log(msg: String) = {
    println(s"Axi4StreamSlave\t: $msg")
  }

  /** Receive synchronously a full transaction from the bus. */
  def recv(): List[Byte] = {
    var result: List[Byte] = null
    val mtx = SimMutex().lock()
    recvCB() { data =>
      result = data
      mtx.unlock()
    }
    mtx.await()
    result
  }

  /** Receive asynchronously; same as {@link recv}, but result is delivered in a callback */
  def recvCB()(callback: List[Byte] => Unit): Unit = {
    val builder = new mutable.ArrayBuilder.ofByte

    log(s"initiating recv")

    def handleBeat(bundle: Axi4StreamBundle): Unit = {
      // XXX: keep + strb has a special meaning, but we are not handling that
      val strb = if (busConfig.useKeep) {
        bundle.keep.toBooleans
      } else if (busConfig.useStrb) {
        bundle.strb.toBooleans
      } else {
        Array.fill(busConfig.dataWidth)(true)
      }

      builder ++= bundle.data.toBytes.zip(strb).filter(_._2).map(_._1)

      if (bundle.last.toBoolean) {
        callback(builder.result.toList)
      } else {
        queue += handleBeat
      }
    }

    queue += handleBeat
  }

  StreamReadyRandomizer(axis, clockDomain)
  StreamMonitor(axis, clockDomain) { b =>
    if (queue.nonEmpty) {
      queue.dequeue()(b)
    }
  }

  /** Reset bus slave (dropping all pending transactions) */
  def reset(): Unit = {
    queue.clear
  }
}
