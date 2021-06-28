package spinal.core

import spinal.core.internals.Operator

object Formal {

  //that.formalPast(delay) replacement
  def past[T <: Data](that : T, delay : Int) : T = {
    require(delay >= 0,"Negative cycleCount is not allowed in Delay")
    var ptr = that
    for(i <- 0 until delay) {
      ptr = RegNext(ptr)
      ptr.unsetName().setCompositeName(that, "past_" + (i + 1), true)
    }
    ptr
  }
  def past[T <: Data](that : T) : T = past(that, 1)

  def rose(that : Bool) : Bool = that.rise(True)
  def fell(that : Bool) : Bool = that.fall(False)
  def changed[T <: BaseType](that : T, init : T = null) : Bool = RegNext(that, init = init) =/= that
  def stable[T <: BaseType](that : T, init : T = null) : Bool = RegNext(that, init = init) === that
  def initstate() : Bool = {
    val ret = Bool()
    ret.assignFrom(new Operator.Formal.InitState)
    ret
  }

//  def past[T <: Data](that : T, delay : Int) : T = that.formalPast(delay)
//  def rose(that : Bool) : Bool = that.wrapUnaryOperator(new Operator.Formal.Rose)
//  def fell(that : Bool) : Bool = that.wrapUnaryOperator(new Operator.Formal.Fell)
//  def changed[T <: BaseType](that : T) : Bool = that.wrapUnaryWithBool(new Operator.Formal.Changed)
//  def stable[T <: BaseType](that : T) : Bool = that.wrapUnaryWithBool(new Operator.Formal.Stable)
  //  def enabled[T](block : => T) : T = GenerationFlags.formal(block)
}
