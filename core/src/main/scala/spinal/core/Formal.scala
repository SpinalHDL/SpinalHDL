package spinal.core

import spinal.core.internals.Operator

object Formal {
  def past(that : Bool) : Bool = that.wrapUnaryOperator(new Operator.Formal.PastBool)
  def past(that : Bits) : Bits = that.wrapUnaryOperator(new Operator.Formal.PastBits)
  def past(that : UInt) : UInt = that.wrapUnaryOperator(new Operator.Formal.PastUInt)
  def past(that : SInt) : SInt = that.wrapUnaryOperator(new Operator.Formal.PastSInt)
  def past[T <: SpinalEnum](that : SpinalEnumCraft[T]) : SpinalEnumCraft[T] = that.wrapUnaryOperator(new Operator.Formal.PastEnum(that.spinalEnum))

  def rise(that : Bool) : Bool = that.wrapUnaryOperator(new Operator.Formal.Rise)
  def fall(that : Bool) : Bool = that.wrapUnaryOperator(new Operator.Formal.Fall)
  def changed[T <: BaseType](that : T) : Bool = that.wrapUnaryWithBool(new Operator.Formal.Changed)
  def stable[T <: BaseType](that : T) : Bool = that.wrapUnaryWithBool(new Operator.Formal.Stable)
  def initstate() : Bool = {
    val ret = Bool()
    ret.assignFrom(new Operator.Formal.InitState)
    ret
  }

//  def enabled[T](block : => T) : T = GenerationFlags.formal(block)
}
