package spinal.lib.bus.pcie.sim

import spinal.core._
import spinal.lib._
import spinal.lib.bus.pcie.Tlp

object PcieCompleterReqArbiter {
  def apply(read: Stream[Fragment[Tlp]], write: Stream[Fragment[Tlp]]): Stream[Fragment[Tlp]] = {
    assert(read.config == write.config)
    val arbiter = new StreamArbiterFactory().fragmentLock.roundRobin.build(read.payload, 2)
    Vec(read, write) <> arbiter.io.inputs
    arbiter.io.output
  }
}

object Util {
  def take(bi: BigInt, range: Range): BigInt = {
    assert(range.step == 1)
    val len = range.end - range.start + range.isInclusive.toInt
    return ((bi >> range.start) & ((1 << len)-1))
  }
  
}

