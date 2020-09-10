package spinal.lib.bus
import spinal.core._
import spinal.lib.{Fragment, Stream}

import spinal.core._

package object bsb {
  type Bsb = spinal.lib.Stream[BsbTransaction]

  implicit class BsbPimper(pimped : Bsb) {
    def firstSink = {
      val regs = (0 until 1 << pimped.p.sinkWidth).map(sinkId => RegNextWhen(pimped.last, pimped.fire && pimped.sink === sinkId, True))
      B(regs)
    }

    def toStreamFragment(omitMask : Boolean = false) = {
      assert(omitMask || !pimped.p.withMask)
      val s = Stream(Fragment(Bits(pimped.p.byteCount*8 bits)))
      s.arbitrationFrom(pimped)
      s.fragment := pimped.data
      s.last := pimped.last
      s
    }
    def toStream(omitMask : Boolean = false) = {
      assert(omitMask || !pimped.p.withMask)
      val s = Stream(Bits(pimped.p.byteCount*8 bits))
      s.arbitrationFrom(pimped)
      s.payload := pimped.data
      s
    }
  }
}
