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
  
  def notCrossAlignment(addr: BigInt, len: BigInt, align: BigInt): Boolean = {
    (addr/align) == ((addr+len-1)/align)
  }


  def countOne(bi: BigInt) = {
    assert(bi >= 0)
    var count = 0
    var bits = bi
    while(bits != 0) {
      if((bits & 1) == 1) {
        count = count + 1
      }
      bits = bits >> 1
    }
    count
  }

}

