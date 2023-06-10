package spinal.lib.bus

import spinal.core._
import spinal.lib._

package object tilelink {
  def sizeToBeatMinusOne(p : BusParameter, size : UInt) : UInt = signalCache(p, size){
    (0 to p.sizeMax).map(s => U(((1 << s)+p.dataBytes-1)/p.dataBytes-1, p.beatWidth bits)).read(size)
  }
  implicit class BusFragmentPimper(ch : BusFragment){
    def sizeToBeatMinusOne = tilelink.sizeToBeatMinusOne(ch.p, ch.size)
  }

  implicit class TilelinkBusFragmentPimper[T <: BusFragment] (ch : Stream[T]){
    def fillBeatCache() = signalCache(ch -> "fillBeatCache") (new Composite(ch, "tracker"){
      val beat = Reg(UInt(ch.p.beatWidth bits)) init(0)
      val last = !ch.withBeats || beat === sizeToBeatMinusOne(ch.p, ch.size)
      when(ch.fire){
        beat := (beat + 1).resized
        when(last){
          beat := 0
        }
      }
    })

    def isLast() : Bool = {
      ch.withData match {
        case false => True
        case true => fillBeatCache().last
      }
    }

    def isFirst() : Bool = ch.withData match {
      case false => True
      case true => fillBeatCache().beat === 0
    }

    def beatCounter() = fillBeatCache().beat
  }
}
