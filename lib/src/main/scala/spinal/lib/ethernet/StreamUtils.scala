package spinal.lib.ethernet

import spinal.core._
import spinal.core.internals.TypeBool
import spinal.lib._

object SubStreamJoin {
  def apply[T1 <: Data, T2 <: Data](main: Stream[T1], sub: Stream[T2], withSub: Bool): Stream[TupleBundle2[T1, T2]] = {
    val combined = Stream(TupleBundle2(
      main.payloadType,
      sub.payloadType
    ))
    combined.valid := withSub ? (main.valid && sub.valid) | main.valid
    main.ready := combined.fire
    sub.ready := withSub ? combined.fire | False
    combined.payload._1 := main.payload
    combined.payload._2 := sub.payload
    combined
  }
}
object StreamPayloadResetIfInvalid {
  def apply[T <: Data](s: Stream[T]): Stream[T] = {
    val resetStream = s.clone()
    resetStream.arbitrationFrom(s)
    resetStream.payload.flatten.zipWithIndex.map { case (payload, idx) =>
      when(!s.valid) {
        if (payload.getTypeObject == TypeBool) {
          payload := False
        } else {
          payload := B(0)
        }
      }.otherwise {
        payload := s.payload.flatten(idx)
      }
    }
    resetStream
  }
}