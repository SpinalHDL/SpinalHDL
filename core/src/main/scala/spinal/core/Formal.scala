package spinal.core

import spinal.core.internals.Operator

object Formal {
  def past(that : Bool, delay : Int) : Bool = that.wrapUnaryOperator(new Operator.Formal.PastBool(delay))
  def past(that : Bits, delay : Int) : Bits = that.wrapUnaryOperator(new Operator.Formal.PastBits(delay))
  def past(that : UInt, delay : Int) : UInt = that.wrapUnaryOperator(new Operator.Formal.PastUInt(delay))
  def past(that : SInt, delay : Int) : SInt = that.wrapUnaryOperator(new Operator.Formal.PastSInt(delay))
  def past[T <: SpinalEnum](that : SpinalEnumCraft[T], delay : Int) : SpinalEnumCraft[T] = that.wrapUnaryOperator(new Operator.Formal.PastEnum(that.spinalEnum, delay))

  def past(that : Bool) : Bool = past(that, 1)
  def past(that : Bits) : Bits = past(that, 1)
  def past(that : UInt) : UInt = past(that, 1)
  def past(that : SInt) : SInt = past(that, 1)
  def past[T <: SpinalEnum](that : SpinalEnumCraft[T]) : SpinalEnumCraft[T] = past(that, 1)


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
