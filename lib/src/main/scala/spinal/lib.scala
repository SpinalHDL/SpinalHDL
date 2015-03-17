package spinal

import spinal.core._

package object lib {
  implicit def flowFragmentPimped[T <: Data](that : Flow[Fragment[T]]) = new FlowFragmentPimped[T](that)
  implicit def handshakeFragmentPimped[T <: Data](that : Handshake[Fragment[T]]) = new HandshakeFragmentPimped[T](that)

  implicit def dataCarrierFragmentPimped[T <: Data](that : DataCarrier[Fragment[T]]) = new DataCarrierFragmentPimped[T](that)
  implicit def dataCarrierFragmentBitsPimped(that : DataCarrier[Fragment[Bits]]) = new DataCarrierFragmentBitsPimped(that)

  implicit def memPimped[T <: Data](mem : Mem[T]) = new MemPimped(mem)
}