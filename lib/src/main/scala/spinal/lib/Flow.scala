package spinal.lib

import spinal.core._


class FlowFactory extends MSFactory{
  object Fragment extends FlowFragmentFactory

  def apply[T <: Data](dataType: T) = {
    val ret = new Flow(dataType)
    postApply(ret)
    ret
  }


}

object Flow extends FlowFactory

class Flow[T <: Data](_dataType: T) extends Bundle with IMasterSlave with DataCarrier[T]{
  val valid = Bool
  val data : T = _dataType.clone()

  def dataType = cloneOf(_dataType)
  override def clone: this.type = Flow(_dataType).asInstanceOf[this.type]

  override def asMaster: this.type = out(this)
  override def asSlave: this.type = in(this)


  override def freeRun: this.type = this


  def toReg : T = toReg(null.asInstanceOf[T])
  def toReg(init: T): T = RegNextWhen(this.data,this.fire,init)


  def <<(that: Flow[T]): Flow[T] = this connectFrom that
  def >>(into: Flow[T]): Flow[T] = {
    into << this;
    into
  }


  def <-<(that: Flow[T]): Flow[T] = {
    this << that.m2sPipe
    that
  }

  override def fire: Bool = valid

  def toStream  : Stream[T] = toStream(null)
  def toStream(overflow : Bool) : Stream[T] = {
    val ret = Stream(dataType)
    ret.valid := this.valid
    ret.data := this.data
    if(overflow != null) overflow := ret.valid && !ret.ready
    ret
  }

  def connectFrom(that: Flow[T]): Flow[T] = {
    valid := that.valid
    data := that.data
    that
  }

  def takeWhen(cond: Bool): Flow[T] = {
    val next = new Flow(_dataType)
    next.valid := this.valid && cond
    next.data := this.data
    return next
  }

  def throwWhen(cond: Bool): Flow[T] = {
    this takeWhen (!cond)
  }

  def translateWith[T2 <: Data](that: T2): Flow[T2] = {
    val next = new Flow(that)
    next.valid := this.valid
    next.data := that
    next
  }

  def m2sPipe(): Flow[T] = {
    val ret = RegNext(this)
    ret.valid.init(False)
    ret connectFrom this
    ret
  }

  def push(that : T): Unit ={
    valid := True
    data := that
  }

  def default(that : T): Unit ={
    valid := False
    data := that
  }
}



object RegFlow{
  def apply[T <: Data](dataType : T) = {
    val reg = Reg(Flow(dataType))
    reg.valid init(False)
    reg
  }
}

