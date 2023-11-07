package spinal.lib.misc.pipeline

import spinal.core.{Data, HardType, Nameable}

object Stageable{
  def apply[T <: Data](gen : => T) = new Stageable(gen)
  def apply[T <: Data](gen : HardType[T]) = new Stageable(gen.craft())
}

class Stageable[T <: Data](gen : => T) extends HardType(gen) with Nameable {

}
case class StageableOffset(val value : Any)
object StageableOffsetNone extends StageableOffset(null)
case class StageableKey(stageable: Stageable[Data], subKey : Any){
  override def toString = {
    var name = stageable.getName()
    if(subKey != null) name = name + "_" + subKey
    name
  }
}

