package spinal.lib.generator_backup

import spinal.core.Nameable

import scala.collection.mutable


trait Tag{

}

class MemoryMaster[T <: Nameable](node : Handle[T]) extends Tag
class MemorySlave[T <: Nameable](node : Handle[T]) extends Tag
class SimpleBus[T <: Nameable](val node : Handle[T], val size : BigInt) extends Tag
class MemoryConnection[T <: Nameable, T2 <: Nameable](val input : Handle[T], val output : Handle[T2], val address :  Handle[BigInt]) extends Tag

object Export{
  def unapply(x: Export): Option[(String, Any)] = Some((x.name, x.value))
}

class Export(val name : String, val value : Any) extends Tag
class Dts[T <: Nameable](val node : Handle[T], val value : String) extends Tag




trait TagContainer{
  val tags = mutable.LinkedHashSet[Tag]()
}
