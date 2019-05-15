package spinal.core

import spinal.core.internals.Operator

object Formal {
  def past[T <: Data](that : T, delay : Int) : T = that.formalPast(delay)
  def past[T <: Data](that : T) : T = past(that, 1)

  def rose(that : Bool) : Bool = that.wrapUnaryOperator(new Operator.Formal.Rose)
  def fell(that : Bool) : Bool = that.wrapUnaryOperator(new Operator.Formal.Fell)
  def changed[T <: BaseType](that : T) : Bool = that.wrapUnaryWithBool(new Operator.Formal.Changed)
  def stable[T <: BaseType](that : T) : Bool = that.wrapUnaryWithBool(new Operator.Formal.Stable)
  def initstate() : Bool = {
    val ret = Bool()
    ret.assignFrom(new Operator.Formal.InitState)
    ret
  }

//  def enabled[T](block : => T) : T = GenerationFlags.formal(block)
}
