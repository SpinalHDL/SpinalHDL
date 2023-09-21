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
  // def calcBe(addr: Int, len: Int): (Int, Int) = {
  //   if((addr+len-1)/4 == addr/4) {
  //     ((((1<<len)-1) << (addr&3))&15, 0)
  //   } else {
  //     val lastBe = (addr+len)&3 match {
  //       case 0 => 15
  //       case 1 => 1
  //       case 2 => 3
  //       case 3 => 7
  //     }
  //     val firstBe = addr & 3 match {
  //       case 0 => 15
  //       case 1 => 14
  //       case 2 => 12
  //       case 3 => 8
  //     }
  //     (firstBe, lastBe)
  //   }
  // }

  // def calcDwCount(addr: Int, len: Int): Int = {
  //   if(len == 0) {
  //     1
  //   } else {
  //     val first = addr >> 2
  //     val last = if(((addr + len) & 3) == 0) ((addr+len) >> 2)-1 else (addr+len) >> 2
  //     val dw = last-first+1
  //     assert(dw<=4096)
  //     if(dw==4096) {
  //       0
  //     } else {
  //       dw
  //     }
  //   }
  // }

  def take(bi: BigInt, range: Range): BigInt = {
    assert(range.step == 1)
    val len = range.end - range.start + range.isInclusive.toInt
    return ((bi >> range.start) & ((1 << len)-1))
  }
  
  def toBoolean(bits: BigInt): Boolean = {
    if(bits == 0) {
      false
    } else if(bits == 1) {
      true
    } else {
      SpinalError("cast error")
    }
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

