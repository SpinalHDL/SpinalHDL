package spinal.core

import scala.collection.mutable.ArrayBuffer
import internals._
import spinal.idslplugin._

object UnionElement {
  implicit def wrapped[T <: Data](ue: UnionElement[T]) = ue.get()
}

class UnionElement[T <: Data](val t: HardType[T], host: Union) {
  def get(): T = signalCache(this, "get") (host.raw.resized.aliasAs(t))
}

class Union(selfBuild : Boolean = true) extends MultiData with PostInitCallback {
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
    if(selfBuild) build()
    this
  }
}