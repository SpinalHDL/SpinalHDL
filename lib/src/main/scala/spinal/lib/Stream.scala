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
  val valid   = Bool
  val ready   = Bool
  val payload = cloneOf(_dataType)


  def dataType : T  = _dataType
  override def clone: Stream[T] =  Stream(_dataType)

  override def asMaster(): Unit = {
    out(valid)
    in(ready)
    out(payload)
  }


  def asDataStream = this.asInstanceOf[Stream[Data]]
  override def freeRun(): this.type = {
    ready := True
    this
  }

/** @return Return a flow drived by this stream. Ready of ths stream is always high
  */
  def toFlow: Flow[T] = {
    freeRun()
    val ret = Flow(_dataType)
    ret.valid := this.valid
    ret.payload := this.payload
    ret
  }

/** Connect that to this
  */
  def <<(that: Stream[T]): Stream[T] = connectFrom(that)

/** Connect this to that
  */
  def >>(into: Stream[T]): Stream[T] = {
    into << this
    into
  }

/** Connect that to this. The valid/payload path are cut by an register stage
  */
  def <-<(that: Stream[T]): Stream[T] = {
    this << that.stage()
    that
  }

/** Connect this to that. The valid/payload path are cut by an register stage
  */
  def >->(into: Stream[T]): Stream[T] = {
    into <-< this
    into
  }

/** Connect that to this. The ready path is cut by an register stage
  */
  def </<(that: Stream[T]): Stream[T] = {
    this << that.s2mPipe
    that
  }

/** Connect this to that. The ready path is cut by an register stage
  */
  def >/>(that: Stream[T]): Stream[T] = {
    that </< this
    that
  }

/** Connect that to this. The valid/payload/ready path are cut by an register stage
  */
  def <-/<(that: Stream[T]): Stream[T] = {
    this << that.s2mPipe.m2sPipe()
    that
  }

/** Connect that to this. The valid/payload/ready path are cut by an register stage
  */
  def >/->(into: Stream[T]): Stream[T] = {
    into <-/< this;
    into
  }

  def &(cond: Bool): Stream[T] = continueWhen(cond)
  def ~[T2 <: Data](that: T2): Stream[T2] = translateWith(that)
  def ~~[T2 <: Data](translate: (T) => T2): Stream[T2] = {
    (this ~ translate(this.payload))
  }

/** Connect this to a fifo and return its pop stream
  */
  def queue(size: Int): Stream[T] = {
    val fifo = new StreamFifo(dataType, size)
    fifo.setPartialName(this,"fifo")
    fifo.io.push << this
    fifo.io.pop
  }

/** Connect this to an clock crossing fifo and return its pop stream
  */
  def queue(size: Int, pushClock: ClockDomain, popClock: ClockDomain): Stream[T] = {
    val fifo = new StreamFifoCC(dataType, size, pushClock, popClock)
    fifo.io.push << this
    return fifo.io.pop
  }

/** Connect this to a fifo and return its pop stream and its occupancy
  */
  def queueWithOccupancy(size: Int): (Stream[T], UInt) = {
    val fifo = new StreamFifo(dataType, size)
    fifo.io.push << this
    return (fifo.io.pop, fifo.io.occupancy)
  }

/** Connect this to a cross clock domain fifo and return its pop stream and its push side occupancy
  */
  def queueWithPushOccupancy(size: Int, pushClock: ClockDomain, popClock: ClockDomain): (Stream[T], UInt) = {
    val fifo = new StreamFifoCC(dataType, size, pushClock, popClock)
    fifo.io.push << this
    return (fifo.io.pop, fifo.io.pushOccupancy)
  }


  /** Connect this to a zero latency fifo and return its pop stream
    */
  def queueLowLatency(size: Int, latency : Int = 0): Stream[T] = {
    val fifo = new StreamFifoLowLatency(dataType, size, latency)
    fifo.setPartialName(this,"fifo")
    fifo.io.push << this
    fifo.io.pop
  }

/** Return True when a transaction is present on the bus but the ready is low
    */
  def isStall : Bool = valid && !ready

  /** Return True when a transaction is appear (first cycle)
    */
  def isNew : Bool = valid && !(RegNext(isStall) init(False))

/** Return True when a transaction occure on the bus (Valid && ready)
  */
  override def fire: Bool = valid & ready

  def isFree: Bool = !valid || ready
  def connectFrom(that: Stream[T]): Stream[T] = {
    this.valid := that.valid
    that.ready := this.ready
    this.payload := that.payload
    that
  }

  /** Drive arbitration signals of this from that
    */
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

/** Connect this to a valid/payload register stage and return its output stream
  */
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

  def s2mPipe(stagesCount : Int): Stream[T] = {
    stagesCount match {
      case 0 => this
      case _ => this.s2mPipe().s2mPipe(stagesCount-1)
    }
  }

/** cut all path, but divide the bandwidth by 2, 1 cycle latency
  */
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

/** Block this when cond is False. Return the resulting stream
  */
  def continueWhen(cond: Bool): Stream[T] = {
    val next = new Stream(_dataType)
    next.valid := this.valid && cond
    this.ready := next.ready && cond
    next.payload := this.payload
    return next
  }

/** Drop transactions of this when cond is True. Return the resulting stream
  */
  def throwWhen(cond: Bool): Stream[T] = {
    val next = Stream(dataType)

    next << this
    when(cond) {
      next.valid := False
      this.ready := True
    }
    next
  }

  /** Stop transactions on this when cond is True. Return the resulting stream
    */
  def haltWhen(cond: Bool): Stream[T] = continueWhen(!cond)

/** Drop transaction of this when cond is False. Return the resulting stream
  */
  def takeWhen(cond: Bool): Stream[T] = throwWhen(!cond)


  def fragmentTransaction(bitsWidth: Int): Stream[Fragment[Bits]] = {
    val converter = new StreamToStreamFragmentBits(payload, bitsWidth)
    converter.io.input << this
    return converter.io.output
  }
  def addFragmentLast(last : Bool) : Stream[Fragment[T]] = {
    val ret = Stream(Fragment(dataType))
    ret.valid := this.valid
    this.ready := ret.ready
    ret.last := last
    ret.fragment := this.payload
    return ret
  }

  override def getTypeString = getClass.getSimpleName + "[" + this.payload.getClass.getSimpleName + "]"
}


object StreamArbiter {
  object Arbitration{
    def lowerFirst(core: StreamArbiter[_ <: Data]) = new Area {
      import core._
      maskProposal := OHMasking.first(Vec(io.inputs.map(_.valid)))
    }

    def roundRobin(core: StreamArbiter[_ <: Data]) = new Area {
      import core._
      for(bitId  <- maskLocked.range){
        maskLocked(bitId) init(Bool(bitId == maskLocked.length-1))
      }
      //maskProposal := maskLocked
      maskProposal := OHMasking.roundRobin(Vec(io.inputs.map(_.valid)),Vec(maskLocked.last +: maskLocked.take(maskLocked.length-1)))
    }

    def sequentialOrder(core: StreamArbiter[_]) = new Area {
      import core._
      val counter = Counter(core.portCount, io.output.fire)
      for (i <- 0 to core.portCount - 1) {
        maskProposal(i) := False
      }
      maskProposal(counter) := True
    }
  }

  object Lock{
    def none(core: StreamArbiter[_]) = new Area {

    }

    def transactionLock(core: StreamArbiter[_]) = new Area {
      import core._
      locked setWhen(io.output.valid)
      locked.clearWhen(io.output.ready)
    }

    def fragmentLock(core: StreamArbiter[_]) = new Area {
      val realCore = core.asInstanceOf[StreamArbiter[Fragment[_]]]
      import realCore._
      locked setWhen(io.output.valid)
      locked.clearWhen(io.output.ready && io.output.last)
    }
  }
}

class StreamArbiter[T <: Data](dataType: T, val portCount: Int)(val arbitrationFactory: (StreamArbiter[T]) => Area,val lockFactory: (StreamArbiter[T]) => Area) extends Component {
  val io = new Bundle {
    val inputs = Vec(slave Stream (dataType),portCount)
    val output = master Stream (dataType)
    val chosen = out UInt (log2Up(portCount) bit)
    val chosenOH = out Bits (portCount bit)
  }

  val locked = RegInit(False)

  val maskProposal = Vec(Bool,portCount)
  val maskLocked = Reg(Vec(Bool,portCount))
  val maskRouted = Mux(locked, maskLocked, maskProposal)


  when(io.output.valid) {
    maskLocked := maskRouted
  }

  val arbitration = arbitrationFactory(this)
  val lock = lockFactory(this)

  io.output.valid := (io.inputs, maskRouted).zipped.map(_.valid & _).reduce(_ | _)
  io.output.payload := MuxOH(maskRouted,Vec(io.inputs.map(_.payload)))
  (io.inputs, maskRouted).zipped.foreach(_.ready := _ & io.output.ready)

  io.chosenOH := maskRouted.asBits
  io.chosen := OHToUInt(io.chosenOH)
}


class StreamArbiterFactory {
  var arbitrationLogic: (StreamArbiter[_ <: Data]) => Area = StreamArbiter.Arbitration.lowerFirst
  var lockLogic: (StreamArbiter[_ <: Data]) => Area = StreamArbiter.Lock.transactionLock

  def build[T <: Data](dataType: T, portCount: Int): StreamArbiter[T] = {
    new StreamArbiter(dataType, portCount)(arbitrationLogic, lockLogic)
  }

  def onArgs[T <: Data](inputs: Stream[T]*): Stream[T] = on(inputs.seq)
  def on[T <: Data](inputs: Seq[Stream[T]]): Stream[T] = {
    val arbiter = build(inputs(0).dataType, inputs.size)
    (arbiter.io.inputs, inputs).zipped.foreach(_ << _)
    return arbiter.io.output
  }

  def lowerFirst: this.type = {
    arbitrationLogic = StreamArbiter.Arbitration.lowerFirst
    this
  }
  def roundRobin: this.type = {
    arbitrationLogic = StreamArbiter.Arbitration.roundRobin
    this
  }
  def sequentialOrder: this.type = {
    arbitrationLogic = StreamArbiter.Arbitration.sequentialOrder
    this
  }
  def noLock: this.type = {
    lockLogic = StreamArbiter.Lock.none
    this
  }
  def fragmentLock: this.type = {
    lockLogic = StreamArbiter.Lock.fragmentLock
    this
  }
  def transactionLock: this.type = {
    lockLogic = StreamArbiter.Lock.transactionLock
    this
  }
}




object StreamFork {
  def apply[T <: Data](input: Stream[T], portCount: Int): Vec[Stream[T]] = {
    val fork = new StreamFork(input.dataType, portCount)
    fork.io.input << input
    return fork.io.outputs
  }
}

object StreamFork2 {
  def apply[T <: Data](input: Stream[T]): (Stream[T], Stream[T]) = {
    val fork = new StreamFork(input.dataType, 2)
    fork.io.input << input
    return (fork.io.outputs(0), fork.io.outputs(1))
  }
}


//TODOTEST
class StreamFork[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val input = slave Stream (dataType)
    val outputs = Vec(master Stream (dataType),portCount)
  }
  val linkEnable = Vec(RegInit(True),portCount)

  io.input.ready := True
  for (i <- 0 until portCount) {
    when(!io.outputs(i).ready && linkEnable(i)) {
      io.input.ready := False
    }
  }

  for (i <- 0 until portCount) {
    io.outputs(i).valid := io.input.valid && linkEnable(i)
    io.outputs(i).payload := io.input.payload
    when(io.outputs(i).fire) {
      linkEnable(i) := False
    }
  }

  when(io.input.ready) {
    linkEnable.foreach(_ := True)
  }
}

//TODOTEST
object StreamDemux{
  def apply[T <: Data](input: Stream[T],select : UInt, portCount: Int) : Vec[Stream[T]] = {
    val c = new StreamDemux(input.payload,portCount)
    c.io.input << input
    c.io.select := select
    c.io.outputs
  }
}

class StreamDemux[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val select = in UInt (log2Up(portCount) bit)
    val input = slave Stream (dataType)
    val outputs = Vec(master Stream (dataType),portCount)
  }
  io.input.ready := False
  for (i <- 0 to portCount - 1) {
    io.outputs(i).payload := io.input.payload
    when(i =/= io.select) {
      io.outputs(i).valid := False
    } otherwise {
      io.outputs(i).valid := io.input.valid
      io.input.ready := io.outputs(i).ready
    }
  }
}


object StreamFifo{
  def apply[T <: Data](dataType: T, depth: Int) = new StreamFifo(dataType,depth)
}

class StreamFifo[T <: Data](dataType: T, depth: Int) extends Component {
  require(depth >= 1)
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

object StreamFifoLowLatency{
  def apply[T <: Data](dataType: T, depth: Int) = new StreamFifoLowLatency(dataType,depth)
}

class StreamFifoLowLatency[T <: Data](dataType: T, depth: Int, latency : Int = 0) extends Component {
  require(depth >= 1)
  val io = new Bundle {
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
    val flush = in Bool() default (False)
    val occupancy = out UInt (log2Up(depth + 1) bit)
  }
  val ram = Mem(dataType, depth)
  val pushPtr = Counter(depth)
  val popPtr = Counter(depth)
  val ptrMatch = pushPtr === popPtr
  val risingOccupancy = RegInit(False)
  val empty = ptrMatch & !risingOccupancy
  val full = ptrMatch & risingOccupancy

  val pushing = io.push.fire
  val popping = io.pop.fire

  io.push.ready := !full

  latency match{
    case 0 => {
      when(!empty){
        io.pop.valid := True
        io.pop.payload := ram.readAsync(popPtr.value)
      } otherwise{
        io.pop.valid := io.push.valid
        io.pop.payload := io.push.payload
      }
    }
    case 1 => {
      io.pop.valid := !empty
      io.pop.payload := ram.readAsync(popPtr.value)
    }
  }
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

object StreamFifoCC{
  def apply[T <: Data](dataType: T, depth: Int, pushClock: ClockDomain, popClock: ClockDomain) = new StreamFifoCC(dataType,depth,pushClock,popClock)
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
    val pushPtrGray = RegNext(toGray(pushPtr.valueNext)) init(0)
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
    val popPtrGray = RegNext(toGray(popPtr.valueNext)) init(0)
    val pushPtrGray = BufferCC(pushToPopGray, B(0,ptrWidth bit))
    val empty = isEmpty(popPtrGray, pushPtrGray)

    io.pop.valid := !empty
    io.pop.payload := ram.readSync(popPtr.valueNext.resized,clockCrossing = true)
    when(io.pop.fire) {
      popPtr.increment()
    }

    io.popOccupancy := (fromGray(pushPtrGray) - popPtr).resized
  }

  pushToPopGray := pushCC.pushPtrGray
  popToPushGray := popCC.popPtrGray
}

object StreamCCByToggle {
  def apply[T <: Data](input: Stream[T], inputClock: ClockDomain, outputClock: ClockDomain): Stream[T] = {
    val c = new StreamCCByToggle[T](input.payload, inputClock, outputClock)
    c.io.input << input
    return c.io.output
  }

  def apply[T <: Data](dataType: T, inputClock: ClockDomain, outputClock: ClockDomain): StreamCCByToggle[T] = {
    new StreamCCByToggle[T](dataType, inputClock, outputClock)
  }
}




class StreamCCByToggle[T <: Data](dataType: T, inputClock: ClockDomain, outputClock: ClockDomain) extends Component {
  val io = new Bundle {
    val input = slave Stream (dataType)
    val output = master Stream (dataType)
  }

  val outHitSignal = Bool

  val pushArea = new ClockingArea(inputClock) {
    val hit = BufferCC(outHitSignal, False)
    val target = RegInit(False)
    val data = Reg(io.input.payload)
    io.input.ready := False
    when(io.input.valid && hit === target) {
      target := !target
      data := io.input.payload
      io.input.ready := True
    }
  }


  val popArea = new ClockingArea(outputClock) {
    val target = BufferCC(pushArea.target, False)
    val hit = RegInit(False)
    outHitSignal := hit

    val stream = cloneOf(io.input)
    stream.valid := (target =/= hit)
    stream.payload := pushArea.data
    stream.payload.addTag(crossClockDomain)

    when(stream.fire) {
      hit := !hit
    }

    io.output << stream.m2sPipe()
  }
}

object StreamDispatcherSequencial{
  def apply[T <: Data](input: Stream[T], outputCount: Int): Vec[Stream[T]] = {
    val dispatcher = new StreamDispatcherSequencial(input.dataType, outputCount)
    dispatcher.io.input << input
    return dispatcher.io.outputs
  }
}
class StreamDispatcherSequencial[T <: Data](gen: T, n: Int) extends Component {
  val io = new Bundle {
    val input = slave Stream (gen)
    val outputs = Vec(master Stream (gen),n)
  }
  val counter = Counter(n, io.input.fire)

  if (n == 1) {
    io.input >> io.outputs(0)
  } else {
    io.input.ready := False
    for (i <- 0 to n - 1) {
      io.outputs(i).payload := io.input.payload
      when(counter =/= i) {
        io.outputs(i).valid := False
      } otherwise {
        io.outputs(i).valid := io.input.valid
        io.input.ready := io.outputs(i).ready
      }
    }
  }
}

object StreamFlowArbiter {
  def apply[T <: Data](inputStream: Stream[T], inputFlow: Flow[T]): Flow[T] = {
    val output = cloneOf(inputFlow)

    output.valid := inputFlow.valid || inputStream.valid
    inputStream.ready := !inputFlow.valid
    output.payload := Mux(inputFlow.valid, inputFlow.payload, inputStream.payload)

    output
  }
}

//Give priority to the inputFlow
class StreamFlowArbiter[T <: Data](dataType: T) extends Area {
  val io = new Bundle {
    val inputFlow = slave Flow (dataType)
    val inputStream = slave Stream (dataType)
    val output = master Flow (dataType)
  }
  io.output.valid := io.inputFlow.valid || io.inputStream.valid
  io.inputStream.ready := !io.inputFlow.valid
  io.output.payload := Mux(io.inputFlow.valid, io.inputFlow.payload, io.inputStream.payload)
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
  def arg(sources : Stream[_]*) : Event = apply(sources.seq)
  def apply(sources : Seq[Stream[_]]) : Event = {
    val event = Event
    val eventFire = event.fire
    event.valid := sources.map(_.valid).reduce(_ && _)
    sources.foreach(_.ready := eventFire)
    event
  }
  def apply[T <: Data](sources : Seq[Stream[_]],payload : T) : Stream[T] = StreamJoin(sources).translateWith(payload)
}





object StreamWidthAdapter{
  def apply[T <: Data,T2 <: Data](input : Stream[T],output : Stream[T2]): Unit = {
    val inputWidth = widthOf(input)
    val outputWidth = widthOf(output)
    if(inputWidth == outputWidth){
      output.arbitrationFrom(input)
      output.assignFromBits(input.asBits)
    } else if(inputWidth > outputWidth){
      val factor = inputWidth / outputWidth
      val counter = Counter(factor,inc = output.fire)
      output.valid := input.valid
      output.payload.assignFromBits(input.payload.asBits.subdivideIn(factor slices).read(counter))
      input.ready := output.ready && counter.willOverflowIfInc
    } else{
      SpinalError("Currently not implemented")
    }
  }
}

object StreamFragmentWidthAdapter{
  def apply[T <: Data,T2 <: Data](input : Stream[Fragment[T]],output : Stream[Fragment[T2]]): Unit = {
    val inputWidth = widthOf(input.fragment)
    val outputWidth = widthOf(output.fragment)
    if(inputWidth == outputWidth){
      output.arbitrationFrom(input)
      output.assignFromBits(input.asBits)
    } else if(inputWidth > outputWidth){
      require(inputWidth % outputWidth == 0)
      val factor = inputWidth / outputWidth
      val counter = Counter(factor,inc = output.fire)
      output.valid := input.valid
      output.fragment.assignFromBits(input.fragment.asBits.subdivideIn(factor slices).read(counter))
      output.last := input.last && counter.willOverflowIfInc
      input.ready := output.ready && counter.willOverflowIfInc
    } else{
      SpinalError("Currently not implemented")
    }
  }
}
