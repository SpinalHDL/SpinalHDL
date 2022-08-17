package spinal.core

import spinal.core.internals.Operator

package object formal {
  def FormalConfig: SpinalFormalConfig = new SpinalFormalConfig()

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
  def pastValid() : Bool = signalCache("formal.pastValid")(RegNext(True) initial(False)  setWeakName("f_past_valid"))

  def rose(that : Bool) : Bool = that.rise(True)
  def fell(that : Bool) : Bool = that.fall(False)
  def changed[T <: Data](that : T, init : T = null.asInstanceOf[T]) : Bool = RegNext(that, init = init) =/= that
  def stable[T <: Data](that : T, init : T = null.asInstanceOf[T]) : Bool = RegNext(that, init = init) === that
  def initstate() : Bool = {
    val ret = Bool()
    ret.assignFrom(new Operator.Formal.InitState)
    ret
  }

  def anyseq[T <: Data](that : T) : T = {
    that.assignFormalRandom(Operator.Formal.RANDOM_ANY_SEQ)
    that
  }
  def anyconst[T <: Data](that : T) : T = {
    that.assignFormalRandom(Operator.Formal.RANDOM_ANY_CONST)
    that
  }
  def allseq[T <: Data](that : T) : T = {
    that.assignFormalRandom(Operator.Formal.RANDOM_ALL_SEQ)
    that
  }
  def allconst[T <: Data](that : T) : T = {
    that.assignFormalRandom(Operator.Formal.RANDOM_ALL_CONST)
    that
  }
}
