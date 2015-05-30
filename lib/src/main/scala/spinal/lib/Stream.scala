package spinal.lib

import spinal.core._


class StreamFactory extends MSFactory {
  object Fragment extends StreamFragmentFactory

  def apply[T <: Data](dataType: T) = {
    val ret = new Stream(dataType)
    postApply(ret)
    ret
  }
}
object Stream extends StreamFactory


class EventFactory extends MSFactory {
  def apply = {
    val ret = new Stream(new NoData)
    postApply(ret)
    ret
  }
}


class Stream[T <: Data](_dataType: T) extends Bundle with IMasterSlave with DataCarrier[T] {
  val valid = Bool
  val ready = Bool
  val data: T = _dataType.clone()


  def dataType = cloneOf(_dataType)
  override def clone: this.type = Stream(_dataType).asInstanceOf[this.type]

  override def asMaster: this.type = {
    out(valid)
    in(ready)
    out(data)
    this
  }

  override def asSlave: this.type = asMaster.flip

  override def freeRun: this.type = {
    ready := True
    this
  }

  def toFlow: Flow[T] = {
    freeRun
    val ret = Flow(_dataType)
    ret.valid := this.valid
    ret.data := this.data
    ret
  }

  def <<(that: Stream[T]): Stream[T] = connectFrom(that)
  //def <<[T2 <: Data](that : Stream[T2])(dataAssignement : (T,T2) => Unit): Stream[T2]  = connectFrom(that)(dataAssignement)

  def >>(into: Stream[T]): Stream[T] = {
    into << this;
    into
  }

  def <-<(that: Stream[T]): Stream[T] = {
    this << that.m2sPipe
    that
  }
  def >->(into: Stream[T]): Stream[T] = {
    into <-< this;
    into
  }

  def </<(that: Stream[T]): Stream[T] = {
    this << that.s2mPipe
    that
  }
  def >/>(that: Stream[T]): Stream[T] = {
    that </< this
    that
  }
  def <-/<(that: Stream[T]): Stream[T] = {
    this << that.s2mPipe.m2sPipe
    that
  }
  def >/->(into: Stream[T]): Stream[T] = {
    into <-/< this;
    into
  }

  def &(cond: Bool): Stream[T] = continueWhen(cond)
  def ~[T2 <: Data](that: T2): Stream[T2] = translateWith(that)
  def ~~[T2 <: Data](translate: (T) => T2): Stream[T2] = {
    (this ~ translate(this.data))
  }


  def queue(size: Int): Stream[T] = {
    val fifo = new StreamFifo(dataType, size)
    fifo.io.push << this
    return fifo.io.pop
  }

  override def fire: Bool = valid & ready
  def isFree: Bool = !valid || ready
  def connectFrom(that: Stream[T]): Stream[T] = {
    this.valid := that.valid
    that.ready := this.ready
    this.data := that.data
    that
  }

  def translateFrom[T2 <: Data](that: Stream[T2])(dataAssignement: (T, that.data.type) => Unit): Stream[T] = {
    this.valid := that.valid
    that.ready := this.ready
    dataAssignement(this.data, that.data)
    this
  }

  def translateInto[T2 <: Data](into: Stream[T2])(dataAssignement: (T2, T) => Unit): Stream[T2] = {
    into.translateFrom(this)(dataAssignement)
    into
  }

  def m2sPipe: Stream[T] = m2sPipe(false)


  def m2sPipe(crossClockData: Boolean): Stream[T] = {
    val ret = Stream(_dataType)

    val rValid = RegInit(False)
    val rData = Reg(_dataType)
    if (crossClockData) rData.addTag(crossClockDomain)

    this.ready := (!ret.valid) || ret.ready

    when(this.ready) {
      rValid := this.valid
      rData := this.data
    }

    ret.valid := rValid
    ret.data := rData


    ret
  }

  def s2mPipe: Stream[T] = {
    val ret = Stream(_dataType)

    val rValid = RegInit(False)
    val rBits = Reg(_dataType)

    ret.valid := this.valid || rValid
    this.ready := !rValid
    ret.data := Mux(rValid, rBits, this.data)

    when(ret.ready) {
      rValid := False
    }

    when(this.ready && (!ret.ready)) {
      rValid := this.valid
      rBits := this.data
    }
    ret
  }

  def translateWith[T2 <: Data](that: T2): Stream[T2] = {
    val next = new Stream(that)
    next.valid := this.valid
    this.ready := next.ready
    next.data := that
    next
  }

  def continueWhen(cond: Bool): Stream[T] = {
    val next = new Stream(_dataType)
    next.valid := this.valid && cond
    this.ready := next.ready && cond
    next.data := this.data
    return next
  }

  def throwWhen(cond: Bool): Stream[T] = {
    val next = this.clone

    next connectFrom this
    when(cond) {
      next.valid := False
      this.ready := True
    }
    next
  }

  def haltWhen(cond: Bool): Stream[T] = continueWhen(!cond)
  def takeWhen(cond: Bool): Stream[T] = throwWhen(!cond)


  def fragmentTransaction(bitsWidth: Int): Stream[Fragment[Bits]] = {
    val converter = new StreamToStreamFragmentBits(data, bitsWidth)
    converter.io.input << this
    return converter.io.output
  }
}


class StreamArbiterCore[T <: Data](dataType: T, val portCount: Int)(arbitrationLogic: (StreamArbiterCore[T]) => Area, lockLogic: (StreamArbiterCore[T]) => Area) extends Component {
  val io = new Bundle {
    val inputs = Vec(portCount, slave Stream (dataType))
    val output = master Stream (dataType)
    val chosen = out UInt (log2Up(portCount) bit)
  }

  val locked = RegInit(False)

  val maskProposal = Vec(portCount, Bool)
  val maskLocked = Reg(Vec(portCount, Bool))
  val maskRouted = Mux(locked, maskLocked, maskProposal)


  when(io.output.valid) {
    maskLocked := maskRouted
  }

  val arbitration = arbitrationLogic(this)
  val lock = lockLogic(this)

  //Route
  var outputValid = False
  var outputData = B(0)
  for ((input, mask) <- (io.inputs, maskRouted).zipped) {
    outputValid = outputValid | (mask & input.valid) //mask & is not mandatory for all kind of arbitration/lock
    outputData = outputData | Mux(mask, input.data.toBits, B(0))
    input.ready := mask & io.output.ready
  }
  io.output.valid := outputValid
  io.output.data.assignFromBits(outputData)

  io.chosen := OHToUInt(maskRouted)

}

class StreamArbiterCoreFactory{
  var arbitrationLogic : (StreamArbiterCore[_]) => Area = StreamArbiterCore.arbitration_lowIdPortFirst
  var lockLogic : (StreamArbiterCore[_]) => Area = StreamArbiterCore.lock_transactionLock

  def build[T <: Data](dataType: T, portCount : Int) : StreamArbiterCore[T] = {
    new StreamArbiterCore(dataType,portCount)(arbitrationLogic,lockLogic)
  }

  def build[T <: Data](input: Seq[Stream[T]]) : Stream[T] ={
    val arbiter = build(input(0).dataType,input.size)
    (arbiter.io.inputs,input).zipped.foreach(_ << _)
    return arbiter.io.output
  }

  def lowIdPortFirst : this.type = {
    arbitrationLogic = StreamArbiterCore.arbitration_lowIdPortFirst
    this
  }
  def inOrder : this.type = {
    arbitrationLogic = StreamArbiterCore.arbitration_InOrder
    this
  }
  def noLock : this.type = {
    lockLogic = StreamArbiterCore.lock_transactionLock
    this
  }
  def fragmentLock : this.type = {
    lockLogic = StreamArbiterCore.lock_fragmentLock
    this
  }
  def transactionLock : this.type = {
    lockLogic = StreamArbiterCore.lock_transactionLock
    this
  }
}

object StreamArbiterCore {
  def arbitration_lowIdPortFirst(core: StreamArbiterCore[_]) = new Area {
    import core._
    var search = True
    for (i <- 0 to portCount - 2) {
      maskProposal(i) := search & io.inputs(i).valid
      search = search & !io.inputs(i).valid
    }
    maskProposal(portCount - 1) := search
  }

  def arbitration_InOrder(core: StreamArbiterCore[_]) = new Area {
    import core._

    val counter = Counter(core.portCount,io.output.fire)

    for (i <- 0 to core.portCount - 1) {
      maskProposal(i) := False
    }

    maskProposal(counter) := True
  }

  def lock_none(core: StreamArbiterCore[_]) = new Area {

  }

  def lock_transactionLock(core: StreamArbiterCore[_]) = new Area {
    import core._

    when(io.output.valid) {
      locked := True
    }
    when(io.output.ready) {
      locked := False
    }
  }

  def lock_fragmentLock(core: StreamArbiterCore[_]) = new Area {
    val realCore = core.asInstanceOf[StreamArbiterCore[Fragment[_]]]
    import realCore._

    when(io.output.valid) {
      locked := True
    }
    when(io.output.ready && io.output.data.last) {
      locked := False
    }
  }
}

//object StreamArbiterPriorityToLow{
//  def apply[T <: Data](dataType: T, portCount : Int) : StreamArbiter[T] ={
//    new StreamArbiter(dataType,portCount)(StreamArbiter.arbitration_lowIdPortFirst,StreamArbiter.lock_none)
//  }
//
//  def apply[T <: Data](input: Vec[Stream[T]]) : Stream[T] ={
//    val arbiter = new StreamArbiter(input(0).dataType,input.size)(StreamArbiter.arbitration_lowIdPortFirst,StreamArbiter.lock_none)
//    (arbiter.io.inputs,input).zipped.foreach(_ << _)
//    return arbiter.io.output
//  }
//}

//TODOTEST
//class StreamArbiterPriorityImpl[T <: Data](dataType: T, portCount: Int, allowSwitchWithoutConsumption: Boolean = false) extends StreamArbiterCore(dataType, portCount, allowSwitchWithoutConsumption) {
//  var search = True
//  for (i <- 0 to portCount - 2) {
//    maskProposal(i) := search & io.inputs(i).valid
//    search = search & !io.inputs(i).valid
//  }
//  maskProposal(portCount - 1) := search
//}


object StreamFork{
  def apply[T <: Data](input : Stream[T], portCount: Int): Vec[Stream[T]] ={
    val fork = new StreamFork(input.dataType,portCount)
    fork.io.input << input
    return fork.io.output
  }
}

object StreamFork2{
  def apply[T <: Data](input : Stream[T]): (Stream[T],Stream[T])={
    val fork = new StreamFork(input.dataType,2)
    fork.io.input << input
    return (fork.io.output(0),fork.io.output(1))
  }
}


//TODOTEST
class StreamFork[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val input = slave Stream (dataType)
    val output = Vec(portCount, master Stream (dataType))
  }
  val linkEnable = Vec(portCount, RegInit(True))

  io.input.ready := True
  for (i <- 0 until portCount) {
    when(!io.output(i).ready && linkEnable(i)) {
      io.input.ready := False
    }
  }

  for (i <- 0 until portCount) {
    io.output(i).valid := io.input.valid && linkEnable(i)
    io.output(i).data := io.input.data
    when(io.output(i).fire) {
      linkEnable(i) := False
    }
  }

  when(io.input.ready) {
    linkEnable.foreach(_ := True)
  }
}

//TODOTEST
class StreamDemux[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val sel = in UInt (log2Up(portCount) bit)
    val input = slave Stream (dataType)
    val output = Vec(portCount, master Stream (dataType))
  }
  io.input.ready := False
  for (i <- 0 to portCount - 1) {
    io.output(i).data := io.input.data
    when(U(i) !== io.sel) {
      io.output(i).valid := False
    } otherwise {
      io.output(i).valid := io.input.valid
      io.input.ready := io.output(i).ready
    }
  }
}


class StreamFifo[T <: Data](dataType: T, depth: Int) extends Component {
  val io = new Bundle {
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
    val occupancy = out UInt (log2Up(depth + 1) bit)
  }

  val ram = Mem(dataType, depth)

  val pushPtr = Counter(depth)
  val popPtr = Counter(depth)
  val ptrMatch = pushPtr === popPtr
  val risingOccupancy = RegInit(False)
  val pushing = io.push.fire
  val popping = io.pop.fire
  val empty = ptrMatch & !risingOccupancy
  val full = ptrMatch & risingOccupancy

  io.push.ready := !full
  io.pop.valid := !empty & !(RegNext(popPtr.valueNext === pushPtr, False) & !full) //mem write to read propagation
  io.pop.data := ram.readSync(popPtr.valueNext)

  when(pushing !== popping) {
    risingOccupancy := pushing
  }
  when(pushing) {
    ram(pushPtr.value) := io.push.data
    pushPtr ++
  }
  when(popping) {
    popPtr ++
  }

  val ptrDif = pushPtr - popPtr
  if (isPow2(depth))
    io.occupancy := ((risingOccupancy && ptrMatch) ## ptrDif).toUInt
  else {
    when(ptrMatch) {
      io.occupancy := Mux(risingOccupancy, U(depth), U(0))
    } otherwise {
      io.occupancy := Mux(pushPtr > popPtr, ptrDif, U(depth) + ptrDif)
    }
  }
}



class StreamFifoCC[T <: Data](dataType: T, depth: Int, pushClockDomain: ClockDomain, popClockDomain: ClockDomain) extends Component {
  assert(isPow2(depth))
  assert(depth >= 2)

  val io = new Bundle {
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
    val pushOccupancy = out UInt (log2Up(depth) + 1 bit)
    val popOccupancy = out UInt (log2Up(depth) + 1 bit)
  }

  val ptrWidth = log2Up(depth) + 1
  def isFull(a: Bits, b: Bits) = a(ptrWidth - 1, ptrWidth - 2) === ~b(ptrWidth - 1, ptrWidth - 2) && a(ptrWidth - 3, 0) === b(ptrWidth - 3, 0)
  def isEmpty(a: Bits, b: Bits) = a === b

  val ram = Mem(dataType, depth)

  val popToPushGray = Bits(ptrWidth bit)
  val pushToPopGray = Bits(ptrWidth bit)

  val pushCC = new ClockingArea(pushClockDomain) {
    val pushPtr = Counter(depth << 1)
    val pushPtrGray = RegNext(toGray(pushPtr.valueNext))
    val popPtrGray = BufferCC(popToPushGray, B"0")
    val full = isFull(pushPtrGray, popPtrGray)

    io.push.ready := !full
    when(io.push.fire) {
      ram(pushPtr) := io.push.data
      pushPtr ++
    }

    io.pushOccupancy := pushPtr - fromGray(popPtrGray)
  }

  val popCC = new ClockingArea(popClockDomain) {
    val popPtr = Counter(depth << 1)
    val popPtrGray = RegNext(toGray(popPtr.valueNext))
    val pushPtrGray = BufferCC(pushToPopGray, B"0")
    val empty = isEmpty(popPtrGray, pushPtrGray)

    io.pop.valid := !empty
    io.pop.data := ram.readSyncCC(popPtr.valueNext)
    when(io.pop.fire) {
      popPtr ++
    }

    io.popOccupancy := fromGray(pushPtrGray) - popPtr
  }

  pushToPopGray := pushCC.pushPtrGray
  popToPushGray := popCC.popPtrGray
}

object StreamCCByToggle {
  def apply[T <: Data](input: Stream[T], clockIn: ClockDomain, clockOut: ClockDomain): Stream[T] = {
    val c = new StreamCCByToggle[T](input.data, clockIn, clockOut)
    c.io.input connectFrom input
    return c.io.output
  }
}


class StreamCCByToggle[T <: Data](dataType: T, clockIn: ClockDomain, clockOut: ClockDomain) extends Component {
  val io = new Bundle {
    val input = slave Stream (dataType)
    val output = master Stream (dataType)
  }

  val outHitSignal = Bool

  val inputArea = new ClockingArea(clockIn) {
    val hit = BufferCC(outHitSignal, False)
    val target = RegInit(False)
    val data = Reg(io.input.data)
    io.input.ready := False
    when(io.input.valid && hit === target) {
      target := !target
      data := io.input.data
      io.input.ready := True
    }
  }


  val outputArea = new ClockingArea(clockOut) {
    val target = BufferCC(inputArea.target, False)
    val hit = RegInit(False)
    outHitSignal := hit

    val stream = io.input.clone
    stream.valid := (target !== hit)
    stream.data := inputArea.data
    stream.data.addTag(crossClockDomain)

    when(stream.fire) {
      hit := !hit
    }

    io.output << stream.m2sPipe
  }
}




class DispatcherInOrder[T <: Data](gen: T, n: Int) extends Component {
  val io = new Bundle {
    val input = slave Stream(gen)
    val outputs = Vec(n,master Stream(gen))
  }
  val counter = Counter(n,io.input.fire)

  if (n == 1) {
    io.input >> io.outputs(0)
  } else {
    io.input.ready := False
    for (i <- 0 to n - 1) {
      io.outputs(i).data := io.input.data
      when(counter !== i) {
        io.outputs(i).valid := False
      } otherwise {
        io.outputs(i).valid := io.input.valid
        io.input.ready := io.outputs(i).ready
      }
    }
  }
}

object StreamFlowArbiter{
  def apply[T <: Data](inputStream : Stream[T],inputFlow : Flow[T]) : Flow[T] = {
    val output = cloneOf(inputFlow)

    output.valid := inputFlow.valid || inputStream.valid
    inputStream.ready := !inputFlow.valid
    output.data := Mux(inputFlow.valid, inputFlow.data, inputStream.data)

    output
  }
}

class StreamFlowArbiter[T <: Data](dataType : T) extends Area{
  val io = new Bundle{
    val inputFlow = slave Flow(dataType)
    val inputStream = slave Stream(dataType)
    val output = master Flow(dataType)
  }
  io.output.valid := io.inputFlow.valid || io.inputStream.valid
  io.inputStream.ready := !io.inputFlow.valid
  io.output.data := Mux(io.inputFlow.valid, io.inputFlow.data, io.inputStream.data)
}

