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
  val payload : T = _dataType.clone()

  def dataType = cloneOf(_dataType)
  override def clone: this.type = Flow(_dataType).asInstanceOf[this.type]

  override def asMaster(): this.type = out(this)
  override def asSlave(): this.type = in(this)


  override def freeRun(): this.type = this


  def toReg() : T = toReg(null.asInstanceOf[T])
  def toReg(init: T): T = RegNextWhen(this.payload,this.fire,init)


  def <<(that: Flow[T]): Flow[T] = this connectFrom that
  def >>(into: Flow[T]): Flow[T] = {
    into << this;
    into
  }


  def <-<(that: Flow[T]): Flow[T] = {
    this << that.m2sPipe
    that
  }
  def >->(that: Flow[T]): Flow[T] = that <-< this

  override def fire: Bool = valid

  def toStream  : Stream[T] = toStream(null)
  def toStream(overflow : Bool) : Stream[T] = {
    val ret = Stream(dataType)
    ret.valid := this.valid
    ret.payload := this.payload
    if(overflow != null) overflow := ret.valid && !ret.ready
    ret
  }

  def toStream(overflow : Bool,fifoSize: Int, overflowOccupancyAt : Int) : Stream[T] = {
    val (ret,occupancy) = this.toStream.queueWithOccupancy(fifoSize)
    overflow := occupancy >= overflowOccupancyAt
    ret
  }


  def connectFrom(that: Flow[T]): Flow[T] = {
    valid := that.valid
    payload := that.payload
    that
  }

  def takeWhen(cond: Bool): Flow[T] = {
    val next = new Flow(_dataType)
    next.valid := this.valid && cond
    next.payload := this.payload
    return next
  }

  def throwWhen(cond: Bool): Flow[T] = {
    this takeWhen (!cond)
  }

  def translateWith[T2 <: Data](that: T2): Flow[T2] = {
    val next = new Flow(that)
    next.valid := this.valid
    next.payload := that
    next
  }

  def translateFrom[T2 <: Data](that: Flow[T2])(dataAssignement: (T, that.payload.type) => Unit): Flow[T] = {
    this.valid := that.valid
    dataAssignement(this.payload, that.payload)
    this
  }

  def m2sPipe(): Flow[T] = {
    val ret = RegNext(this)
    ret.valid.init(False)
    ret
  }

  def stage() : Flow[T] = this.m2sPipe()

  def push(that : T): Unit ={
    valid := True
    payload := that
  }

  def default(that : T): Unit ={
    valid := False
    payload := that
  }
}



object RegFlow{
  def apply[T <: Data](dataType : T) = {
    val reg = Reg(Flow(dataType))
    reg.valid init(False)
    reg
  }
}

object FlowCCByToggle {
  def apply[T <: Data](input: Flow[T], clockIn: ClockDomain = ClockDomain.current, clockOut: ClockDomain = ClockDomain.current): Flow[T] = {
    val c = new FlowCCByToggle[T](input.payload, clockIn, clockOut)
    c.io.input connectFrom input
    return c.io.output
  }
}


class FlowCCByToggle[T <: Data](dataType: T, clockIn: ClockDomain, clockOut: ClockDomain) extends Component {
  val io = new Bundle {
    val input = slave Flow (dataType)
    val output = master Flow (dataType)
  }

  val outHitSignal = Bool

  val inputArea = new ClockingArea(clockIn) {
    val target = Reg(Bool)
    val data = Reg(io.input.payload)
    when(io.input.valid) {
      target := !target
      data := io.input.payload
    }
  }


  val outputArea = new ClockingArea(clockOut) {
    val target = BufferCC(inputArea.target, if(clockIn.hasResetSignal) False else null)
    val hit = RegNext(target)

    val flow = io.input.clone
    flow.valid := (target =/= hit)
    flow.payload := inputArea.data
    flow.payload.addTag(crossClockDomain)

    io.output <-< flow
  }

  if(clockIn.hasResetSignal){
    inputArea.target init(False)
    outputArea.hit init(False)
  }else{
    inputArea.target.randBoot()
  }
}
