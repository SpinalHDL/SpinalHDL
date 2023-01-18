package spinal.lib.ethernet

import spinal.core._
import spinal.lib.Stream
import spinal.lib.bus.amba4.axis.Axi4Stream.Axi4StreamBundle

object Axi4StreamConditionalJoin {

  /** Convert a tuple of streams into a stream of tuples
    * source stream1 has higher priority
    */
  def apply[T1 <: Axi4StreamBundle, T2 <: Axi4StreamBundle](
      source1: Stream[T1], source2: Stream[T2],
      source1Select: Bool, source2Select: Bool
  ): Stream[TupleBundle2[T1, T2]] = {
    val sources = Seq(source1, source2)
    val combined = Stream(
      TupleBundle2(source1.payloadType, source2.payloadType)
    )
    combined.valid := (sources(0).valid && sources(1).valid) |
      (source1Select && sources(0).valid)
    sources(0).ready := combined.fire
    sources(1).ready := source2Select & combined.fire
    //    if (useKeep/useLast/useUser)
    combined.payload._1.keep := source1.fire ? source1.payload.keep | 0
    combined.payload._2.keep := source2.fire ? source2.payload.keep | 0
    if (source1.user != null & source2.user != null) {
      combined.payload._1.user := source1.fire ? source1.payload.user | 0
      combined.payload._2.user := source2.fire ? source2.payload.user | 0
    }
    combined.payload._1.data := source1.fire ? source1.payload.data | 0
    combined.payload._2.data := source2.fire ? source2.payload.data | 0
    combined.payload._1.last := source1.fire ? source1.payload.last | False
    combined.payload._2.last := source2.fire ? source2.payload.last | False
    combined
  }
}
