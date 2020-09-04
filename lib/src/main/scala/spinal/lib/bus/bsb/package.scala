package spinal.lib.bus

import spinal.core._

package object bsb {
  type Bsb = spinal.lib.Stream[BsbTransaction]

  implicit class BsbPimper[T <: Data](pimped : Bsb){
    def firstSink = {
      val regs = (0 until 1 << pimped.p.sinkWidth).map(sinkId => RegNextWhen(pimped.last, pimped.fire && pimped.sink === sinkId, True))
      B(regs)
    }
  }
}
