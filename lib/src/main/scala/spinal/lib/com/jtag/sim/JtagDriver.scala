package spinal.lib.com.jtag.sim

import scala.collection.mutable
import spinal.core.TimeNumber
import spinal.core.sim._
import spinal.lib.com.jtag.Jtag

/*
 * Jtag Driver tool
 * Author: Alexis Marquet
 */
case class JtagDriver(jtag: Jtag, clockPeriod: TimeNumber) {

  /*
   * Reset the JTAG tap into Test-Logic-Reset state
   */
  def doResetTap(): Unit = {
    doTmsSeq(Seq(true, true, true, true, true))
  }

  /*
   * Send a sequence of TMS bits to move the JTAG tap into a specific state
   */
  def doTmsSeq(tmsSeq: Seq[Boolean]): Unit = {
    for (tms <- tmsSeq) {
      jtag.tms #= tms
      doClockCycles(1)
    }
  }

  /*
   * Send a sequence of TDI bits to the JTAG tap and return the TDO sequence
   * param: flipTms is used to flip the last TMS bit to 1 when the scan chain is finished
   * This allows to write longer sequences of bits from multiple calls to this function
   */
  def doScanChain(tdiSeq: Seq[Boolean], flipTms: Boolean): Seq[Boolean] = {
    // use mutable seq
    val tdoSeq = mutable.Seq.fill(tdiSeq.length)(false)
    for (i <- tdiSeq.indices) {
      jtag.tdi #= tdiSeq(i)
      jtag.tms #= (i == tdiSeq.length - 1 && flipTms)
      tdoSeq(i) = jtag.tdo.toBoolean
      doClockCycles(1)
    }
    tdoSeq.toSeq
  }

  /*
   * Send n clock cycle to the JTAG tap
   */
  def doClockCycles(n: Long): Unit = {
    for (_ <- 0 until n.toInt) {
      jtag.tck #= true
      sleep(timeToLong(clockPeriod / 2))
      jtag.tck #= false
      sleep(timeToLong(clockPeriod / 2))
    }
  }
}
