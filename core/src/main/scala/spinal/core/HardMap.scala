package spinal.core

import spinal.idslplugin.Location

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object HardMap{
  def apply(content : Seq[NamedType[_ <: Data]]) : HardMap = {
    val ret = new HardMap()
    content.foreach(e => ret.add(e))
    ret
  }
}

class HardMap extends MultiData {
  val storage = mutable.LinkedHashMap[NamedType[Data], Data]()
  var elementsCache : ArrayBuffer[(String, Data)] = null

  def keyToName(key : Any) = key match {
    case n: Nameable if n.isNamed => n.getName()
  }

  def update[T <: Data](key : NamedType[T], value : T): Unit = {
    assert(elementsCache == null)
    assert(!storage.contains(key.asInstanceOf[NamedType[Data]]))
    storage(key.asInstanceOf[NamedType[Data]]) = value
    if(OwnableRef.proposal(value, this)) value.setPartialName(keyToName(key), Nameable.DATAMODEL_WEAK)
  }

  def add[T <: Data](key: NamedType[T]) : Unit = {
    this(key) = key()
  }

  def apply[T <: Data](key: NamedType[T]): T = {
    storage(key.asInstanceOf[NamedType[Data]]).asInstanceOf[T]
  }

  override def elements: ArrayBuffer[(String, Data)] = {
    if(elementsCache == null) {
      elementsCache = ArrayBuffer[(String, Data)]()
      for ((k, d) <- storage) {
        val name = keyToName(k)
        elementsCache += name -> d
      }
    }
    elementsCache
  }

  def hardMapAssign(that: HardMap)(f: (Data, Data) => Unit): Unit = {
    for ((name, element) <- elements) {
      val other = that.find(name)
      if (other == null) {
        LocatedPendingError(s"Bundle assignment is not complete. $this need '$name' but $that doesn't provide it.")
      }
      else {
        f(element, other)
      }
    }
  }

  protected override def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
    that match {
      case that: HardMap =>
        if (!this.getClass.isAssignableFrom(that.getClass)) SpinalError("HardMap must have the same final class to" +
          " be assigned. Either use assignByName or assignSomeByName at \n" + ScalaLocated.long)
        hardMapAssign(that)((to, from) => to.compositAssignFrom(from, to, kind))
      case _ => throw new Exception("Undefined assignment")
    }
  }
}
