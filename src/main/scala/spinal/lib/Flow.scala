package spinal.lib

import spinal._


object Flow {
  def apply[T <: Data](dataType: T) = new Flow(dataType)
}

class Flow[T <: Data](dataType: T) extends Bundle with Interface {
  val valid = Bool()
  val data = dataType.clone()

  override def clone: this.type = Flow(dataType).asInstanceOf[this.type]

  override def asMaster: this.type = asOutput
  override def asSlave: this.type = asInput

  def <<(that: Flow[T]): Unit = this connectFrom that
  def <-<(that: Flow[T]): Unit = this m2sPipeFrom that

  def fire: Bool = valid

  def connectFrom(that: Flow[T]): Unit = {
    valid := that.valid
    data := that.data
  }

  def takeIf(cond: Bool): Flow[T] = {
    val next = new Flow(dataType)
    next.valid := this.valid && cond
    next.data := this.data
    return next
  }

  def throwIt(cond: Bool): Flow[T] = {
    this takeIf (!cond)
  }

  def translateWith[T2 <: Data](that: T2): Flow[T2] = {
    val next = new Flow(that)
    next.valid := this.valid
    next.data := that
    next
  }

  def m2sPipeFrom(that: Flow[T]): Flow[T] = {
    val reg = RegNext(that)
    reg.valid.setRegInit(Bool(false))
    this connectFrom reg
    this
  }

}
