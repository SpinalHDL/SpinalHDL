package spinal.lib.generator

import spinal.core.{Nameable, SpinalTag}
import spinal.core.fiber._

import scala.collection.mutable




class MemoryMaster[T <: Nameable](node : Handle[T]) extends SpinalTag
class MemorySlave[T <: Nameable](node : Handle[T]) extends SpinalTag
class SimpleBus[T <: Nameable](val node : Handle[T], val size : BigInt) extends SpinalTag
class MemoryConnection[T <: Nameable, T2 <: Nameable](val input : Handle[T], val output : Handle[T2], val address :  Handle[BigInt]) extends SpinalTag

object Export{
  def unapply(x: Export): Option[(String, Any)] = Some((x.name, x.value))
}

class Export(val name : String, val value : Any) extends SpinalTag
class Dts[T <: Nameable](val node : Handle[T], val value : String) extends SpinalTag




//trait TagContainer{
//  val tags = mutable.LinkedHashSet[Tag]()
//}
