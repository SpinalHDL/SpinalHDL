package spinal.core

import scala.collection.mutable.ArrayBuffer
import internals._
import spinal.idslplugin._

object UnionElement {
  implicit def wrapped[T <: Data](ue: UnionElement[T]) = ue.get()
}

class UnionElement[T <: Data](val t: HardType[T], host: Union) {
  def get(): T = signalCache(this, "get") {
    val wrap = host.raw.resized.as(t)
    var offsetCounter = 0
    for (e <- wrap.flatten) {
      val eWidth = e.getBitsWidth

      e.compositeAssign = new Assignable {
        val offset = offsetCounter

        override protected def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
          def getBits(w: Int) = that match {
            case that: BitVector if widthOf(that) != w => {
              val tmp = cloneOf(that).setWidth(w)
              tmp := that
              tmp.asBits
            }
            case that: Data => that.asBits
          }

          target match {
            case x: BaseType => host.raw.compositAssignFrom(getBits(eWidth), RangedAssignmentFixed(host.raw, offset + eWidth - 1, offset), kind)
            case x: BitAssignmentFixed => host.raw(offset + x.bitId).compositAssignFrom(that, host.raw, kind)
            case x: BitAssignmentFloating => host.raw(offset + x.bitId.asInstanceOf[UInt]).compositAssignFrom(that, host.raw, kind)
            case x: RangedAssignmentFixed => host.raw(offset + x.hi downto offset + x.lo).compositAssignFrom(getBits(x.getWidth), host.raw, kind)
            case x: RangedAssignmentFloating => host.raw(offset + x.offset.asInstanceOf[UInt], x.bitCount bits).compositAssignFrom(getBits(x.getWidth), host.raw, kind)
          }
        }

        override def getRealSourceNoRec: Any = host.raw
      }

      offsetCounter += eWidth
    }
    wrap
  }
}


class Union extends MultiData with PostInitCallback {
  val uTypes = ArrayBuffer[UnionElement[Data]]()
  var raw: Bits = null

  def newElement[T <: Data](t: HardType[T]) = {
    val e = new UnionElement(t, this)
    uTypes += e.asInstanceOf[UnionElement[Data]]
    e
  }

  def build(): Unit = {
    raw = Bits(uTypes.map(e => widthOf(e.t)).max bits)
    raw.setRefOwner(this)
    raw.setPartialName("", weak = true)
  }


  override protected def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
    that match {
      case from: Union => {
        this.raw := from.raw
      }
    }
  }

  override def elements: ArrayBuffer[(String, Data)] = {
    ArrayBuffer("" -> raw)
  }

  override def postInitCallback() = {
    build()
    this
  }
}