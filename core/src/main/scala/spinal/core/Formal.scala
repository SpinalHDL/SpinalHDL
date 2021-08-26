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

  /** past implementation optimizied for multiple delays
   *
   * Instead of giving a delay in cycles, you can provide a sequence of delays and will
   * get a Vec containing the history of your value at the given time points.
   *
   * Examples:
   *
   * A signal should have been held for the last four cycles: `past(signal, 1 to 4).reduce(_ && _)` (start
   * the range at 0 if the signal should still be high in the current cycle.
   *
   * A signal should have been raised in the last four cycles: `past(signal, 1 to 4).reduce(_ || _)`.
   *
   * Some data should have been stable: `past(data, 0 to 4).reduce(_ === _)`.
   *
   * Some data should have had a specific value: `past(data, 0 to 4).fold(True)(_ && (_ === correctValue))`
   * or `past(data, 0 to 4).map(_ === correctValue).reduce(_ && _)`.
   *
   * Running average of a value: `past(value, (0 to 8)).sum() >>> 3`
   *
   * You can also give a specific number of delays: `past(data, Seq(0, 1, 3, 5))`.
   *
   * Check some condition every n cycles: `past(data, (0 to 2).map(_ * n))`
   */
  def past[T <: Data](that : T, delays : scala.collection.Iterable[Int]) : Vec[T] = {
    val delays_ = delays.toSeq

    var ptr = that
    val vec = Vec(null.asInstanceOf[T], delays_.max)
    if (delays_.contains(0)) {
      vec(0) := ptr
    }
    for(i <- 1 to delays_.max) {
      ptr = RegNext(ptr)
      ptr.unsetName().setCompositeName(that, "past_" + i, true)
      if (delays_.contains(i)) {
        vec(i) := ptr
      }
    }

    vec
  }

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
