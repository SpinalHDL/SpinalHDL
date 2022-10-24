package spinal.lib.logic

import spinal.core._
import spinal.core.{BaseType, Bits, Component, EnumLiteral, False, HardType, RegInit, RegNext, SpinalEnumCraft, out}
import spinal.lib.KeepAttribute

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class DecodingSpec[T <: BaseType](key : HardType[T]){
  var default : Option[Masked] = None
  val needs = mutable.LinkedHashMap[Masked, Masked]() //key, value

  def setDefault(value : Masked) = {
    default match {
      case Some(x) => ???
      case None => default = Some(value)
    }
    this
  }
  def addNeeds(key : Masked, value : Masked): this.type = {
    needs.get(key) match {
      case Some(x) => assert(needs(key) == value)
      case None => needs(key) = value
    }
    this
  }

  def addNeeds(keys : Seq[Masked], value : Masked): this.type = {
    keys.foreach(addNeeds(_, value))
    this
  }

  def build(sel : Bits, coverAll : Iterable[Masked]) : T = {
    val defaultsKeys = mutable.LinkedHashSet[Masked]()
    defaultsKeys ++= coverAll
    defaultsKeys --= needs.keys
    val defaultNeeds = default match {
      case Some(x) => defaultsKeys.map(_ -> x)
      case None => Nil
    }
    val finalSpec = needs ++ defaultNeeds
    Symplify(sel, finalSpec, key.getBitsWidth).as(key)
  }
}

object DecodingSpecExample extends App{
  SpinalVerilog(new Component{
    val spec = new DecodingSpec(HardType(UInt(4 bits)))
    
    spec.setDefault(Masked(U"0011"))
    spec.addNeeds(Masked(B"000"), Masked(U"1000"))
    spec.addNeeds(Masked(B"100"), Masked(U"1100"))

    val sel = in Bits(3 bits)
    val result = out UInt(4 bits)
    result := spec.build(sel, (0 to 7).map(Masked(_, 0x7)))
  })
}