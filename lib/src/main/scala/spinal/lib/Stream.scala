package spinal.lib

import spinal.core._


class StreamFactory extends MSFactory {
  object Fragment extends StreamFragmentFactory

  def apply[T <: Data](dataType:  T) = {
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


class Stream[T <: Data](_dataType:  T) extends Bundle with IMasterSlave with DataCarrier[T] {
  val valid = Bool
  val ready = Bool
  val payload: T = _dataType.clone


  def dataType : T  = _dataType
  override def clone: this.type = Stream(_dataType).asInstanceOf[this.type]

  override def asMaster(): this.type = {
    out(valid)
    in(ready)
    out(payload)
    this
  }

  override def asSlave(): this.type = asMaster().flip()

  def asDataStream = this.asInstanceOf[Stream[Data]]
  override def freeRun(): this.type = {
    ready := True
    this
  }

  def toFlow: Flow[T] = {
    freeRun()
    val ret = Flow(_dataType)
    ret.valid := this.valid
    ret.payload := this.payload
    ret
  }

  def <<(that: Stream[T]): Stream[T] = connectFrom(that)
  //def <<[T2 <: Data](that : Stream[T2])(dataAssignement : (T,T2) => Unit): Stream[T2]  = connectFrom(that)(dataAssignement)

  def >>(into: Stream[T]): Stream[T] = {
    into << this;
    into
  }

  def <-<(that: Stream[T]): Stream[T] = {
    this << that.m2sPipe()
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
    this << that.s2mPipe.m2sPipe()
    that
  }
  def >/->(into: Stream[T]): Stream[T] = {
    into <-/< this;
    into
  }

  def &(cond: Bool): Stream[T] = continueWhen(cond)
  def ~[T2 <: Data](that: T2): Stream[T2] = translateWith(that)
  def ~~[T2 <: Data](translate: (T) => T2): Stream[T2] = {
    (this ~ translate(this.payload))
  }


  def queue(size: Int): Stream[T] = {
    val fifo = new StreamFifo(dataType, size)
    fifo.io.push << this
    return fifo.io.pop
  }

  def queue(size: Int, pushClock: ClockDomain, popClock: ClockDomain): Stream[T] = {
    val fifo = new StreamFifoCC(dataType, size, pushClock, popClock)
    fifo.io.push << this
    return fifo.io.pop
  }

  def queueWithOccupancy(size: Int): (Stream[T], UInt) = {
    val fifo = new StreamFifo(dataType, size)
    fifo.io.push << this
    return (fifo.io.pop, fifo.io.occupancy)
  }

  def queueWithPushOccupancy(size: Int, pushClock: ClockDomain, popClock: ClockDomain): (Stream[T], UInt) = {
    val fifo = new StreamFifoCC(dataType, size, pushClock, popClock)
    fifo.io.push << this
    return (fifo.io.pop, fifo.io.pushOccupancy)
  }

  def isStall : Bool = valid && !ready
  override def fire: Bool = valid & ready
  def isFree: Bool = !valid || ready
  def connectFrom(that: Stream[T]): Stream[T] = {
    this.valid := that.valid
    that.ready := this.ready
    this.payload := that.payload
    that
  }

  def arbitrationFrom[T2 <: Data](that : Stream[T2]) : Unit = {
    this.valid := that.valid
    that.ready := this.ready
  }

  def translateFrom[T2 <: Data](that: Stream[T2])(dataAssignement: (T, that.payload.type) => Unit): Stream[T] = {
    this.valid := that.valid
    that.ready := this.ready
    dataAssignement(this.payload, that.payload)
    this
  }



  def translateInto[T2 <: Data](into: Stream[T2])(dataAssignement: (T2, T) => Unit): Stream[T2] = {
    into.translateFrom(this)(dataAssignement)
    into
  }

  def stage() : Stream[T] = this.m2sPipe()

  //! if collapsBubble is enable then ready is not "don't care" during valid low !
  def m2sPipe(collapsBubble : Boolean = true,crossClockData: Boolean = false): Stream[T] = {
    val ret = Stream(_dataType)

    val rValid = RegInit(False)
    val rData = Reg(_dataType)
    if (crossClockData) rData.addTag(crossClockDomain)

    this.ready := (Bool(collapsBubble) && !ret.valid) || ret.ready

    when(this.ready) {
      rValid := this.valid
      rData := this.payload
    }

    ret.valid := rValid
    ret.payload := rData


    ret
  }

  def s2mPipe(): Stream[T] = {
    val ret = Stream(_dataType)

    val rValid = RegInit(False)
    val rBits = Reg(_dataType)

    ret.valid := this.valid || rValid
    this.ready := !rValid
    ret.payload := Mux(rValid, rBits, this.payload)

    when(ret.ready) {
      rValid := False
    }

    when(this.ready && (!ret.ready)) {
      rValid := this.valid
      rBits := this.payload
    }
    ret
  }

  // cut all path, but divide the bandwidth by 2, 1 cycle latency
  def halfPipe(): Stream[T] = {
    val ret = Stream(_dataType)

    val rValid = RegInit(False)
    val rReady = RegInit(True)
    val rPayload = Reg(dataType)

    when(!rValid){
      rValid := this.valid
      rReady := !this.valid
      rPayload := this.payload
    } otherwise {
      rValid := !ret.ready
      rReady := ret.ready
    }

    ret.valid := rValid
    ret.payload := rPayload
    this.ready := rReady
    ret
  }

  def translateWith[T2 <: Data](that: T2): Stream[T2] = {
    val next = new Stream(that)
    next.valid := this.valid
    this.ready := next.ready
    next.payload := that
    next
  }

  def continueWhen(cond: Bool): Stream[T] = {
    val next = new Stream(_dataType)
    next.valid := this.valid && cond
    this.ready := next.ready && cond
    next.payload := this.payload
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
    val converter = new StreamToStreamFragmentBits(payload, bitsWidth)
    converter.io.cmd << this
    return converter.io.rsp
  }
  def addFragmentLast(last : Bool) : Stream[Fragment[T]] = {
    val ret = Stream(Fragment(dataType))
    ret.valid := this.valid
    this.ready := ret.ready
    ret.last := last
    ret.fragment := this.payload
    return ret
  }
}


object StreamArbiter {
  def arbitration_lowIdPortFirst(core: StreamArbiter[_]) = new Area {
    import core._
    var search = True
    for (i <- 0 to portCount - 2) {
      maskProposal(i) := search & io.cmd(i).valid
      search = search & !io.cmd(i).valid
    }
    maskProposal(portCount - 1) := search
  }

  def arbitration_InOrder(core: StreamArbiter[_]) = new Area {
    import core._
    val counter = Counter(core.portCount, io.rsp.fire)

    for (i <- 0 to core.portCount - 1) {
      maskProposal(i) := False
    }

    maskProposal(counter) := True
  }

  def lock_none(core: StreamArbiter[_]) = new Area {

  }

  def lock_transactionLock(core: StreamArbiter[_]) = new Area {
    import core._
    when(io.rsp.valid) {
      locked := True
    }
    when(io.rsp.ready) {
      locked := False
    }
  }

  def lock_fragmentLock(core: StreamArbiter[_]) = new Area {
    val realCore = core.asInstanceOf[StreamArbiter[Fragment[_]]]
    import realCore._
    when(io.rsp.valid) {
      locked := True
    }
    when(io.rsp.ready && io.rsp.last) {
      locked := False
    }
  }
}

class StreamArbiter[T <: Data](dataType: T, val portCount: Int)(arbitrationLogic: (StreamArbiter[T]) => Area, lockLogic: (StreamArbiter[T]) => Area) extends Component {
  val io = new Bundle {
    val cmd = Vec(slave Stream (dataType),portCount)
    val rsp = master Stream (dataType)
    val chosen = out UInt (log2Up(portCount) bit)
  }

  val locked = RegInit(False)

  val maskProposal = Vec(Bool,portCount)
  val maskLocked = Reg(Vec(Bool,portCount))
  val maskRouted = Mux(locked, maskLocked, maskProposal)


  when(io.rsp.valid) {
    maskLocked := maskRouted
  }

  val arbitration = arbitrationLogic(this)
  val lock = lockLogic(this)

  //Route
//  var outputValid = False
//  var outputData = B(0)
//  for ((input, mask) <- (io.cmd, maskRouted).zipped) {
//    outputValid = outputValid | (mask & input.valid) //mask & is not mandatory for all kind of arbitration/lock
//    outputData = outputData | Mux(mask, input.payload.asBits, B(0))
//    input.ready := mask & io.rsp.ready
//  }
//  io.rsp.valid := outputValid
//  io.rsp.payload.assignFromBits(outputData)

  io.rsp.valid := (io.cmd, maskRouted).zipped.map(_.valid & _).reduce(_ | _)
  io.rsp.payload := MuxOH(maskRouted,Vec(io.cmd.map(_.payload)))
  (io.cmd, maskRouted).zipped.foreach(_.ready := _ & io.rsp.ready)

  io.chosen := OHToUInt(maskRouted)

}


class StreamArbiterFactory {
  var arbitrationLogic: (StreamArbiter[_]) => Area = StreamArbiter.arbitration_lowIdPortFirst
  var lockLogic: (StreamArbiter[_]) => Area = StreamArbiter.lock_transactionLock

  def build[T <: Data](dataType: T, portCount: Int): StreamArbiter[T] = {
    new StreamArbiter(dataType, portCount)(arbitrationLogic, lockLogic)
  }

  def build[T <: Data](input: Seq[Stream[T]]): Stream[T] = {
    val arbiter = build(input(0).dataType, input.size)
    (arbiter.io.cmd, input).zipped.foreach(_ << _)
    return arbiter.io.rsp
  }

  def lowIdPortFirst: this.type = {
    arbitrationLogic = StreamArbiter.arbitration_lowIdPortFirst
    this
  }
  def inOrder: this.type = {
    arbitrationLogic = StreamArbiter.arbitration_InOrder
    this
  }
  def noLock: this.type = {
    lockLogic = StreamArbiter.lock_none
    this
  }
  def fragmentLock: this.type = {
    lockLogic = StreamArbiter.lock_fragmentLock
    this
  }
  def transactionLock: this.type = {
    lockLogic = StreamArbiter.lock_transactionLock
    this
  }
}




object StreamFork {
  def apply[T <: Data](input: Stream[T], portCount: Int): Vec[Stream[T]] = {
    val fork = new StreamFork(input.dataType, portCount)
    fork.io.cmd << input
    return fork.io.rsp
  }
}

object StreamFork2 {
  def apply[T <: Data](input: Stream[T]): (Stream[T], Stream[T]) = {
    val fork = new StreamFork(input.dataType, 2)
    fork.io.cmd << input
    return (fork.io.rsp(0), fork.io.rsp(1))
  }
}


//TODOTEST
class StreamFork[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val cmd = slave Stream (dataType)
    val rsp = Vec(master Stream (dataType),portCount)
  }
  val linkEnable = Vec(RegInit(True),portCount)

  io.cmd.ready := True
  for (i <- 0 until portCount) {
    when(!io.rsp(i).ready && linkEnable(i)) {
      io.cmd.ready := False
    }
  }

  for (i <- 0 until portCount) {
    io.rsp(i).valid := io.cmd.valid && linkEnable(i)
    io.rsp(i).payload := io.cmd.payload
    when(io.rsp(i).fire) {
      linkEnable(i) := False
    }
  }

  when(io.cmd.ready) {
    linkEnable.foreach(_ := True)
  }
}

//TODOTEST
object StreamDemux{
  def apply[T <: Data](cmd: Stream[T],sel : UInt, portCount: Int) : Vec[Stream[T]] = {
    val c = new StreamDemux(cmd.payload,portCount)
    c.io.cmd << cmd
    c.io.sel := sel
    c.io.rsp
  }
}

class StreamDemux[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val sel = in UInt (log2Up(portCount) bit)
    val cmd = slave Stream (dataType)
    val rsp = Vec(master Stream (dataType),portCount)
  }
  io.cmd.ready := False
  for (i <- 0 to portCount - 1) {
    io.rsp(i).payload := io.cmd.payload
    when(i =/= io.sel) {
      io.rsp(i).valid := False
    } otherwise {
      io.rsp(i).valid := io.cmd.valid
      io.cmd.ready := io.rsp(i).ready
    }
  }
}


class StreamFifo[T <: Data](dataType: T, depth: Int) extends Component {
  val io = new Bundle {
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
    val flush = in Bool() default(False)
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
  io.pop.payload := ram.readSync(popPtr.valueNext)

  when(pushing =/= popping) {
    risingOccupancy := pushing
  }
  when(pushing) {
    ram(pushPtr.value) := io.push.payload
    pushPtr.increment()
  }
  when(popping) {
    popPtr.increment()
  }

  val ptrDif = pushPtr - popPtr
  if (isPow2(depth))
    io.occupancy := ((risingOccupancy && ptrMatch) ## ptrDif).asUInt
  else {
    when(ptrMatch) {
      io.occupancy := Mux(risingOccupancy, U(depth), U(0))
    } otherwise {
      io.occupancy := Mux(pushPtr > popPtr, ptrDif, U(depth) + ptrDif)
    }
  }

  when(io.flush){
    pushPtr.clear()
    popPtr.clear()
    risingOccupancy := False
  }
}


class StreamFifoCC[T <: Data](dataType: T, val depth: Int, pushClock: ClockDomain, popClock: ClockDomain) extends Component {
  assert(isPow2(depth))
  assert(depth >= 2)

  val io = new Bundle {
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
    val pushOccupancy = out UInt (log2Up(depth+1)  bit)
    val popOccupancy = out UInt (log2Up(depth+1) bit)
  }

  val ptrWidth = log2Up(depth) + 1
  def isFull(a: Bits, b: Bits) = a(ptrWidth - 1, ptrWidth - 2) === ~b(ptrWidth - 1, ptrWidth - 2) && a(ptrWidth - 3, 0) === b(ptrWidth - 3, 0)
  def isEmpty(a: Bits, b: Bits) = a === b

  val ram = Mem(dataType, depth)

  val popToPushGray = Bits(ptrWidth bit)
  val pushToPopGray = Bits(ptrWidth bit)

  val pushCC = new ClockingArea(pushClock) {
    val pushPtr = Counter(depth << 1)
    val pushPtrGray = RegNext(toGray(pushPtr.valueNext))
    val popPtrGray = BufferCC(popToPushGray, B(0,ptrWidth bit))
    val full = isFull(pushPtrGray, popPtrGray)

    io.push.ready := !full
    when(io.push.fire) {
      ram(pushPtr.resized) := io.push.payload
      pushPtr.increment()
    }

    io.pushOccupancy := (pushPtr - fromGray(popPtrGray)).resized
  }

  val popCC = new ClockingArea(popClock) {
    val popPtr = Counter(depth << 1)
    val popPtrGray = RegNext(toGray(popPtr.valueNext))
    val pushPtrGray = BufferCC(pushToPopGray, B(0,ptrWidth bit))
    val empty = isEmpty(popPtrGray, pushPtrGray)

    io.pop.valid := !empty
    io.pop.payload := ram.readSyncCC(popPtr.valueNext.resized)
    when(io.pop.fire) {
      popPtr.increment()
    }

    io.popOccupancy := (fromGray(pushPtrGray) - popPtr).resized
  }

  pushToPopGray := pushCC.pushPtrGray
  popToPushGray := popCC.popPtrGray
}

object StreamCCByToggle {
    def apply[T <: Data](push: Stream[T], pushClock: ClockDomain, popClock: ClockDomain): Stream[T] = {
    val c = new StreamCCByToggle[T](push.payload, pushClock, popClock)
    c.io.push << push
    return c.io.pop
  }
}


class StreamCCByToggle[T <: Data](dataType: T, pushClock: ClockDomain, popClock: ClockDomain) extends Component {
  val io = new Bundle {
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
  }

  val outHitSignal = Bool

  val cmdArea = new ClockingArea(pushClock) {
    val hit = BufferCC(outHitSignal, False)
    val target = RegInit(False)
    val data = Reg(io.push.payload)
    io.push.ready := False
    when(io.push.valid && hit === target) {
      target := !target
      data := io.push.payload
      io.push.ready := True
    }
  }


  val rspArea = new ClockingArea(popClock) {
    val target = BufferCC(cmdArea.target, False)
    val hit = RegInit(False)
    outHitSignal := hit

    val stream = io.push.clone
    stream.valid := (target =/= hit)
    stream.payload := cmdArea.data
    stream.payload.addTag(crossClockDomain)

    when(stream.fire) {
      hit := !hit
    }

    io.pop << stream.m2sPipe()
  }
}


class StreamDispatcherInOrder[T <: Data](gen: T, n: Int) extends Component {
  val io = new Bundle {
    val cmd = slave Stream (gen)
    val rsp = Vec(master Stream (gen),n)
  }
  val counter = Counter(n, io.cmd.fire)

  if (n == 1) {
    io.cmd >> io.rsp(0)
  } else {
    io.cmd.ready := False
    for (i <- 0 to n - 1) {
      io.rsp(i).payload := io.cmd.payload
      when(counter !== i) {
        io.rsp(i).valid := False
      } otherwise {
        io.rsp(i).valid := io.cmd.valid
        io.cmd.ready := io.rsp(i).ready
      }
    }
  }
}

object StreamFlowArbiter {
  def apply[T <: Data](cmdStream: Stream[T], cmdFlow: Flow[T]): Flow[T] = {
    val output = cloneOf(cmdFlow)

    output.valid := cmdFlow.valid || cmdStream.valid
    cmdStream.ready := !cmdFlow.valid
    output.payload := Mux(cmdFlow.valid, cmdFlow.payload, cmdStream.payload)

    output
  }
}

//Give priority to the cmdFlow
class StreamFlowArbiter[T <: Data](dataType: T) extends Area {
  val io = new Bundle {
    val cmdFlow = slave Flow (dataType)
    val cmdStream = slave Stream (dataType)
    val rsp = master Flow (dataType)
  }
  io.rsp.valid := io.cmdFlow.valid || io.cmdStream.valid
  io.cmdStream.ready := !io.cmdFlow.valid
  io.rsp.payload := Mux(io.cmdFlow.valid, io.cmdFlow.payload, io.cmdStream.payload)
}


object StreamMux {
  def apply[T <: Data](select: UInt, inputs: Seq[Stream[T]]): Stream[T] = {
    val vec = Vec(inputs)
    StreamMux(select, vec)
  }

  def apply[T <: Data](select: UInt, inputs: Vec[Stream[T]]): Stream[T] = {
    val ret = cloneOf(inputs(0))
    ret.valid := inputs(select).valid
    ret.payload := inputs(select).payload

    for ((input, index) <- inputs.zipWithIndex) {
      input.ready := select === index && ret.ready
    }

    ret
  }
}


case class EventEmitter(on : Event){
  val reg = RegInit(False)
  when(on.ready){
    reg := False
  }
  on.valid := reg

  def emit(): Unit ={
    reg := True
  }
}

object StreamJoin{
  def apply(sources : Stream[_]*) : Event = {
    val event = Event
    val eventFire = event.fire
    event.valid := sources.map(_.valid).reduce(_ && _)
    sources.foreach(_.ready := eventFire)
    event
  }
}