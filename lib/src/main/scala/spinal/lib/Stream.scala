package spinal.lib

import spinal.core._
import spinal.idslplugin.Location
import spinal.lib.eda.bench.{AlteraStdTargets, Bench, Rtl, XilinxStdTargets}

import scala.collection.Seq

trait StreamPipe {
  def apply[T <: Data](m: Stream[T]): Stream[T]
}

object StreamPipe {
  val NONE = new StreamPipe {
    override def apply[T <: Data](m: Stream[T]) = m.combStage()
  }
  val M2S = new StreamPipe {
    override def apply[T <: Data](m: Stream[T]) = m.m2sPipe()
  }
  val S2M = new StreamPipe {
    override def apply[T <: Data](m: Stream[T]) = m.s2mPipe()
  }
  val FULL = new StreamPipe {
    override def apply[T <: Data](m: Stream[T]) = m.s2mPipe().m2sPipe()
  }
  val HALF = new StreamPipe {
    override def apply[T <: Data](m: Stream[T]) = m.halfPipe()
  }
}

class StreamFactory extends MSFactory {
  object Fragment extends StreamFragmentFactory

  def apply[T <: Data](hardType: HardType[T]) = {
    val ret = new Stream(hardType)
    postApply(ret)
    ret
  }

  def apply[T <: Data](hardType: => T) : Stream[T] = apply(HardType(hardType))
}

object Stream extends StreamFactory

class EventFactory extends MSFactory {
  def apply = {
    val ret = new Stream(new NoData)
    postApply(ret)
    ret
  }
}

class Stream[T <: Data](val payloadType :  HardType[T]) extends Bundle with IMasterSlave with DataCarrier[T] {
  val valid   = Bool()
  val ready   = Bool()
  val payload = payloadType()

  override def clone: Stream[T] =  Stream(payloadType)

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
    val ret = Flow(payloadType)
    ret.valid := this.valid
    ret.payload := this.payload
    ret
  }

  def toFlowFire: Flow[T] = {
    val ret = Flow(payloadType)
    ret.valid := this.fire
    ret.payload := this.payload
    ret
  }

  def asFlow: Flow[T] = {
    val ret = Flow(payloadType)
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
    this << that.s2mPipe()
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
    this << that.s2mPipe().m2sPipe()
    that
  }

/** Connect that to this. The valid/payload/ready path are cut by an register stage
  */
  def >/->(into: Stream[T]): Stream[T] = {
    into <-/< this;
    into
  }

  def pipelined(pipe: StreamPipe) = pipe(this)
  def pipelined(m2s : Boolean = false,
                s2m : Boolean = false,
                halfRate : Boolean = false) : Stream[T] = {
    (m2s, s2m, halfRate) match {
      case (false,false,false) => StreamPipe.NONE(this)
      case (true,false,false) =>  StreamPipe.M2S(this)
      case (false,true,false) =>  StreamPipe.S2M(this)
      case (true,true,false) =>   StreamPipe.FULL(this)
      case (false,false,true) =>  StreamPipe.HALF(this)
    }
  }

  def &(cond: Bool): Stream[T] = continueWhen(cond)
  def ~[T2 <: Data](that: T2): Stream[T2] = translateWith(that)
  def ~~[T2 <: Data](translate: (T) => T2): Stream[T2] = map(translate)
  def map[T2 <: Data](translate: (T) => T2): Stream[T2] = {
    (this ~ translate(this.payload))
  }

/** Ignore the payload */
  def toEvent() : Event = {
    val ret = Event
    ret.arbitrationFrom(this)
    ret
  }

/** Connect this to a fifo and return its pop stream
  */
  def queue(size: Int): Stream[T] = new Composite(this){
    val fifo = new StreamFifo(payloadType, size)
    fifo.io.push << self
  }.fifo.io.pop

/** Connect this to an clock crossing fifo and return its pop stream
  */
  def queue(size: Int, pushClock: ClockDomain, popClock: ClockDomain): Stream[T] = {
    val fifo = new StreamFifoCC(payloadType, size, pushClock, popClock).setCompositeName(this,"queue", true)
    fifo.io.push << this
    return fifo.io.pop
  }

/** Connect this to a fifo and return its pop stream and its occupancy
  */
  def queueWithOccupancy(size: Int): (Stream[T], UInt) = {
    val fifo = new StreamFifo(payloadType, size).setCompositeName(this,"queueWithOccupancy", true)
    fifo.io.push << this
    return (fifo.io.pop, fifo.io.occupancy)
  }

  def queueWithAvailability(size: Int): (Stream[T], UInt) = {
    val fifo = new StreamFifo(payloadType, size).setCompositeName(this,"queueWithAvailability", true)
    fifo.io.push << this
    return (fifo.io.pop, fifo.io.availability)
  }

/** Connect this to a cross clock domain fifo and return its pop stream and its push side occupancy
  */
  def queueWithPushOccupancy(size: Int, pushClock: ClockDomain, popClock: ClockDomain): (Stream[T], UInt) = {
    val fifo = new StreamFifoCC(payloadType, size, pushClock, popClock).setCompositeName(this,"queueWithPushOccupancy", true)
    fifo.io.push << this
    return (fifo.io.pop, fifo.io.pushOccupancy)
  }


  /** Connect this to a zero latency fifo and return its pop stream
    */
  def queueLowLatency(size: Int, latency : Int = 0): Stream[T] = {
    val fifo = new StreamFifoLowLatency(payloadType, size, latency)
    fifo.setPartialName(this, "fifo", true)
    fifo.io.push << this
    fifo.io.pop
  }

  def ccToggle(pushClock: ClockDomain, popClock: ClockDomain): Stream[T] = {
    val cc = new StreamCCByToggle(payloadType, pushClock, popClock).setCompositeName(this,"ccToggle", true)
    cc.io.input << this
    cc.io.output
  }

  def ccToggleWithoutBuffer(pushClock: ClockDomain, popClock: ClockDomain): Stream[T] = {
    val cc = new StreamCCByToggle(payloadType, pushClock, popClock, withOutputBuffer=false, withInputWait=true).setCompositeName(this,"ccToggle", true)
    cc.io.input << this
    cc.io.output
  }


  /**
   * Connect this to a new stream that only advances every n elements, thus repeating the input several times.
   * @return A tuple with the resulting stream that duplicates the items and the counter, indicating how many
   *				 times the current element has been repeated.
   */
  def repeat(times: Int): (Stream[T], UInt) = {
    val ret = Stream(payloadType)
    val counter = Counter(times, ret.fire)
    ret.valid := this.valid
    ret.payload := this.payload
    this.ready := ret.ready && counter.willOverflowIfInc
    (ret, counter)
  }
  
  /**
   * Connect this to a new stream whose payload is n times as wide, but that only fires every n cycles.
   * It introduces 0 to factor-1 cycles of latency. Mapping a stream into memory and mapping a slowed
   * down stream into memory should yield the same result, thus the elements of the input will be
   * written from high bits to low bits.
   */
  def slowdown(factor: Int): Stream[Vec[T]] = {
    val next = new Stream(Vec(payloadType(), factor)).setCompositeName(this, "slowdown_x" + factor, true)
    next.payload(0) := this.payload
    for (i <- 1 until factor) {
      next.payload(i) := RegNextWhen(next.payload(i - 1), this.fire)
    }
    val counter = Counter(factor)
    when(this.fire) {
      counter.increment()
    }
    when(counter.willOverflowIfInc) {
      this.ready := next.ready
      next.valid := this.valid
    } otherwise {
      this.ready := True
      next.valid := False
    }
    next
  }

/** Return True when a transaction is present on the bus but the ready signal is low
    */
  def isStall : Bool = (valid && !ready).setCompositeName(this, "isStall", true)

  /** Return True when a transaction has appeared (first cycle)
    */
  def isNew : Bool = (valid && !(RegNext(isStall) init(False))).setCompositeName(this, "isNew", true)

  /** Return True when a transaction occurs on the bus (valid && ready)
  */
  override def fire: Bool = (valid & ready).setCompositeName(this, "fire", true)

/** Return True when the bus is ready, but no data is present
  */
  def isFree: Bool = (!valid || ready).setCompositeName(this, "isFree", true)
  
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

  def translateFrom[T2 <: Data](that: Stream[T2])(dataAssignment: (T, that.payload.type) => Unit): Stream[T] = {
    this.valid := that.valid
    that.ready := this.ready
    dataAssignment(this.payload, that.payload)
    this
  }

  def translateInto[T2 <: Data](into: Stream[T2])(dataAssignment: (T2, T) => Unit): Stream[T2] = {
    into.translateFrom(this)(dataAssignment)
    into
  }

/** Replace this stream's payload with another one
  */
  def translateWith[T2 <: Data](that: T2): Stream[T2] = {
    val next = new Stream(that).setCompositeName(this, "translated", true)
    next.arbitrationFrom(this)
    next.payload := that
    next
  }

/** Change the payload's content type. The new type must have the same bit length as the current one.
  */
  def transmuteWith[T2 <: Data](that: HardType[T2]) = {
    val next = new Stream(that).setCompositeName(this, "transmuted", true)
    next.arbitrationFrom(this)
    next.payload.assignFromBits(this.payload.asBits)
    next
  }


  def swapPayload[T2 <: Data](that: HardType[T2]) = {
    val next = new Stream(that).setCompositeName(this, "swap", true)
    next.arbitrationFrom(this)
    next
  }


  /** A combinatorial stage doesn't do anything, but it is nice to separate signals for combinatorial transformations.
  */
  def combStage() : Stream[T] = {
    val ret = Stream(payloadType).setCompositeName(this, "combStage", true)
    ret << this
    ret
  }

  /** Connect this to a valid/payload register stage and return its output stream
  */
  def stage() : Stream[T] = this.m2sPipe()

  //! if collapsBubble is enable then ready is not "don't care" during valid low !
  def m2sPipe(collapsBubble : Boolean = true, crossClockData: Boolean = false, flush : Bool = null, holdPayload : Boolean = false): Stream[T] = new Composite(this) {
    val m2sPipe = Stream(payloadType)

    val rValid = RegNextWhen(self.valid, self.ready) init(False)
    val rData = RegNextWhen(self.payload, if(holdPayload) self.fire else self.ready)

    if (crossClockData) rData.addTag(crossClockDomain)
    if (flush != null) rValid clearWhen(flush)

    self.ready := m2sPipe.ready
    if (collapsBubble) self.ready setWhen(!m2sPipe.valid)

    m2sPipe.valid := rValid
    m2sPipe.payload := rData
  }.m2sPipe

  def s2mPipe(flush : Bool = null): Stream[T] = new Composite(this) {
    val s2mPipe = Stream(payloadType)

    val rValid = RegInit(False) setWhen(self.valid) clearWhen(s2mPipe.ready)
    val rData = RegNextWhen(self.payload, self.ready)

    self.ready := !rValid

    s2mPipe.valid := self.valid || rValid
    s2mPipe.payload := Mux(rValid, rData, self.payload)

    if(flush != null) rValid.clearWhen(flush)
  }.s2mPipe

  def s2mPipe(stagesCount : Int): Stream[T] = {
    stagesCount match {
      case 0 => this
      case _ => this.s2mPipe().s2mPipe(stagesCount-1)
    }
  }

  def validPipe() : Stream[T] = new Composite(this) {
    val validPipe = Stream(payloadType)

    val rValid = RegInit(False) setWhen(self.valid) clearWhen(validPipe.fire)

    self.ready := validPipe.fire

    validPipe.valid := rValid
    validPipe.payload := self.payload
  }.validPipe

/** cut all path, but divide the bandwidth by 2, 1 cycle latency
  */
  def halfPipe(flush : Bool = null): Stream[T] = new Composite(this) {
    val halfPipe = Stream(payloadType)

    val rValid = RegInit(False) setWhen(self.valid) clearWhen(halfPipe.fire)
    val rData = RegNextWhen(self.payload, self.ready)

    self.ready := !rValid

    halfPipe.valid := rValid
    halfPipe.payload := rData

    if(flush != null) rValid clearWhen(flush)
  }.halfPipe

/** Block this when cond is False. Return the resulting stream
  */
  def continueWhen(cond: Bool): Stream[T] = {
    val next = new Stream(payloadType)
    next.valid := this.valid && cond
    this.ready := next.ready && cond
    next.payload := this.payload
    return next
  }

/** Drop transactions of this when cond is True. Return the resulting stream
  */
  def throwWhen(cond: Bool): Stream[T] = {
    val next = Stream(payloadType).setCompositeName(this, "thrown", true)

    next << this
    when(cond) {
      next.valid := False
      this.ready := True
    }
    next
  }

  def clearValidWhen(cond : Bool): Stream[T] = {
    val next = Stream(payloadType).setCompositeName(this, "clearValidWhen", true)
    next.valid := this.valid && !cond
    next.payload := this.payload
    this.ready := next.ready
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
  
  /**
   * Convert this stream to a fragmented stream by adding a last bit. To view it from
   * another perspective, bundle together successive events as fragments of a larger whole.
   * You can then use enhanced operations on fragmented streams, like reducing of elements.
   */
  def addFragmentLast(last : Bool) : Stream[Fragment[T]] = {
    val ret = Stream(Fragment(payloadType))
    ret.arbitrationFrom(this)
    ret.last := last
    ret.fragment := this.payload
    return ret
  }
  
  /** 
   *  Like addFragmentLast(Bool), but instead of manually telling which values go together,
   *  let a counter do the job. The counter will increment for each passing element. Last
   *  will be set high at the end of each revolution.
	 * @example {{{ outStream = inStream.addFragmentLast(new Counter(5)) }}}
   */
  def addFragmentLast(counter: Counter) : Stream[Fragment[T]] = {
    when (this.fire) {
      counter.increment()
    }
    val last = counter.willOverflowIfInc
    return addFragmentLast(last)
  }
  
  def setIdle(): this.type = {
    this.valid := False
    this.payload.assignDontCare()
    this
  }
  
  def setBlocked(): this.type = {
    this.ready := False
    this
  }

  def forkSerial(cond : Bool): Stream[T] = new Composite(this, "forkSerial"){
    val next = Stream(payloadType)
    next.valid := self.valid
    next.payload := self.payload
    self.ready := next.ready && cond
  }.next

  override def getTypeString = getClass.getSimpleName + "[" + this.payload.getClass.getSimpleName + "]"

  /**
   * Assert that this stream conforms to the stream semantics:
   * https://spinalhdl.github.io/SpinalDoc-RTD/dev/SpinalHDL/Libraries/stream.html#semantics
   * - After being asserted, valid may only be deasserted once the current payload was acknowleged.
   *
   * @param payloadInvariance Check that the payload does not change when valid is high and ready is low.
   */
  def withAsserts(payloadInvariance : Boolean = true)(implicit loc : Location) : this.type = {
    import spinal.core.formal._
    val stack = ScalaLocated.long
    when(past(this.isStall) init(False)) {
      assert(this.valid,  "Stream transaction disappeared:\n" + stack)
      if(payloadInvariance) assert(stable(this.payload), "Stream transaction payload changed:\n" + stack)
    }
    this
  }

  def withAssumes(payloadInvariance : Boolean = true)(implicit loc : Location): this.type  = {
    import spinal.core.formal._
    when(past(this.isStall) init (False)) {
      assume(this.valid)
      if(payloadInvariance) assume(stable(this.payload))
    }
    this
  }

  def withCovers(back2BackCycles: Int = 1): this.type  = {
    import spinal.core.formal._
    val hist = History(this.fire, back2BackCycles).reduce(_ && _)
    cover(hist)
    cover(this.isStall)
    // doubt that if this is required in generic scenario.
    // cover(this.ready && !this.valid)
    this
  }

  def withOrderAsserts(dataAhead : T, dataBehind : T)(implicit loc : Location) : Tuple2[Bool, Bool] = {
    import spinal.core.formal._
    val aheadOut = RegInit(False) setWhen (this.fire && dataAhead === this.payload)
    val behindOut = RegInit(False) setWhen (this.fire && dataBehind === this.payload)

    when(!aheadOut){ assert(!behindOut) }
    when(behindOut){ assert(aheadOut) }

    cover(aheadOut)
    cover(behindOut)
    
    (aheadOut, behindOut)
  }

  def withOrderAssumes(dataAhead : T, dataBehind : T)(implicit loc : Location) : Tuple2[Bool, Bool] = {
    import spinal.core.formal._
    val aheadIn = RegInit(False) setWhen (this.fire && dataAhead === this.payload)
    val behindIn = RegInit(False) setWhen (this.fire && dataBehind === this.payload)
    when(aheadIn) { assume(this.payload =/= dataAhead) }
    when(behindIn) { assume(this.payload =/= dataBehind) }
    
    assume(dataAhead =/= dataBehind)
    when(!aheadIn) { assume(!behindIn) }
    when(behindIn) { assume(aheadIn) }

    (aheadIn, behindIn)
  }

  /** Assert that this stream conforms to the stream semantics:
    * https://spinalhdl.github.io/SpinalDoc-RTD/dev/SpinalHDL/Libraries/stream.html#semantics
    * - After being asserted, valid should be acknowledged in limited cycles.
    *
    * @param maxStallCycles Check that the max cycles the interface would hold in stall.
    */
  def withTimeoutAsserts(maxStallCycles: Int = 0) = new Area {
    import spinal.core.formal._
    val logic = (maxStallCycles > 0) generate new Area {
      val counter = Counter(maxStallCycles, isStall).setCompositeName(this, "timeoutCounter", true)
      when(!isStall) { counter.clear() }
        .otherwise { assert(!counter.willOverflow) }
    }
  }

  def withTimeoutAssumes(maxStallCycles: Int = 0) = new Area {
    import spinal.core.formal._
    val logic = (maxStallCycles > 0) generate new Area {
      val counter = Counter(maxStallCycles, isStall).setCompositeName(this, "timeoutCounter", true)
      when(!isStall) { counter.clear() }
        .elsewhen(counter.willOverflow) { assume(ready === True) }
    }
  }
}

object StreamArbiter {
  /** An Arbitration will choose which input stream to take at any moment. */
  object Arbitration{
    def lowerFirst(core: StreamArbiter[_ <: Data]) = new Area {
      import core._
      maskProposal := OHMasking.first(Vec(io.inputs.map(_.valid)))
    }

    /** This arbiter contains an implicit transactionLock */
    def sequentialOrder(core: StreamArbiter[_]) = new Area {
      import core._
      val counter = Counter(core.portCount, io.output.fire)
      for (i <- 0 to core.portCount - 1) {
        maskProposal(i) := False
      }
      maskProposal(counter) := True
    }

    def roundRobin(core: StreamArbiter[_ <: Data]) = new Area {
      import core._
      for(bitId  <- maskLocked.range){
        maskLocked(bitId) init(Bool(bitId == maskLocked.length-1))
      }
      //maskProposal := maskLocked
      maskProposal := OHMasking.roundRobin(Vec(io.inputs.map(_.valid)),Vec(maskLocked.last +: maskLocked.take(maskLocked.length-1)))
    }
  }

  /** When a lock activates, the currently chosen input won't change until it is released. */
  object Lock {
    def none(core: StreamArbiter[_]) = new Area {

    }

    /**
     * Many handshaking protocols require that once valid is set, it must stay asserted and the payload
     *  must not changed until the transaction fires, e.g. until ready is set as well. Since some arbitrations
     *  may change their chosen input at any moment in time (which is not wrong), this may violate such
     *  handshake protocols. Use this lock to be compliant in those cases.
     */
    def transactionLock(core: StreamArbiter[_]) = new Area {
      import core._
      locked setWhen(io.output.valid)
      locked.clearWhen(io.output.fire)
    }

    /**
     * This lock ensures that once a fragmented transaction is started, it will be finished without
     * interruptions from other streams. Without this, fragments of different streams will get intermingled.
     * This is only relevant for fragmented streams.
     */
    def fragmentLock(core: StreamArbiter[_]) = new Area {
      val realCore = core.asInstanceOf[StreamArbiter[Fragment[_]]]
      import realCore._
      locked setWhen(io.output.valid)
      locked.clearWhen(io.output.fire && io.output.last)
    }
  }
}

/**
 *  A StreamArbiter is like a StreamMux, but with built-in complex selection logic that can arbitrate input
 *  streams based on a schedule or handle fragmented streams. Use a StreamArbiterFactory to create instances of this class.
 */
class StreamArbiter[T <: Data](dataType: HardType[T], val portCount: Int)(val arbitrationFactory: (StreamArbiter[T]) => Area, val lockFactory: (StreamArbiter[T]) => Area) extends Component {
  val io = new Bundle {
    val inputs = Vec(slave Stream (dataType),portCount)
    val output = master Stream (dataType)
    val chosen = out UInt (log2Up(portCount) bit)
    val chosenOH = out Bits (portCount bit)
  }

  val locked = RegInit(False).allowUnsetRegToAvoidLatch

  val maskProposal = Vec(Bool(),portCount)
  val maskLocked = Reg(Vec(Bool(),portCount))
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

  def build[T <: Data](dataType: HardType[T], portCount: Int): StreamArbiter[T] = {
    new StreamArbiter(dataType, portCount)(arbitrationLogic, lockLogic)
  }

  def onArgs[T <: Data](inputs: Stream[T]*): Stream[T] = on(inputs.seq)
  def on[T <: Data](inputs: Seq[Stream[T]]): Stream[T] = {
    val arbiter = build(inputs(0).payloadType, inputs.size)
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

/**
 * This is equivalent to a StreamDemux, but with a counter attached to the port selector.
 */
// TODOTEST
object StreamDispatcherSequential {
  def apply[T <: Data](input: Stream[T], outputCount: Int): Vec[Stream[T]] = {
    val select = Counter(outputCount)
    when (input.fire) {
    select.increment()
    }
    StreamDemux(input, select, outputCount)
  }
}

/**
 * @deprecated Do not use
 */
// TODOTEST
object StreamDispatcherSequencial {
  def apply[T <: Data](input: Stream[T], outputCount: Int): Vec[Stream[T]] = {
    StreamDispatcherSequential(input, outputCount)
  }
}

/**
 * @deprecated Do not use. Use the companion object or a normal regular StreamMux instead.
 */
class StreamDispatcherSequencial[T <: Data](gen: HardType[T], n: Int) extends Component {
  val io = new Bundle {
    val input = slave Stream (gen)
    val outputs = Vec(master Stream (gen), n)
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

/**
 * This is equivalent to a StreamMux, but with a counter attached to the port selector.
 */
// TODOTEST
object StreamCombinerSequential {
  def apply[T <: Data](inputs: Seq[Stream[T]]): Stream[T] = {
    val select = Counter(inputs.length)
    val stream = StreamMux(select, inputs)
    when (stream.fire) {
      select.increment()
    }
    stream
  }
}

/** Combine a stream and a flow to a new stream. If both input sources fire, the flow will be preferred. */
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

/**
 *  Multiplex multiple streams into a single one, always only processing one at a time.
 */
object StreamMux {
  def apply[T <: Data](select: UInt, inputs: Seq[Stream[T]]): Stream[T] = {
    val vec = Vec(inputs)
    StreamMux(select, vec)
  }

  def apply[T <: Data](select: UInt, inputs: Vec[Stream[T]]): Stream[T] = {
    val c = new StreamMux(inputs(0).payload, inputs.length)
    (c.io.inputs, inputs).zipped.foreach(_ << _)
    c.io.select := select
    c.io.output
  }
}

class StreamMux[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val select = in UInt (log2Up(portCount) bit)
    val inputs = Vec(slave Stream (dataType), portCount)
    val output = master Stream (dataType)
  }
  for ((input, index) <- io.inputs.zipWithIndex) {
    input.ready := io.select === index && io.output.ready
  }
  io.output.valid := io.inputs(io.select).valid
  io.output.payload := io.inputs(io.select).payload
}

//TODOTEST
/** 
 *  Demultiplex one stream into multiple output streams, always selecting only one at a time.
 */
object StreamDemux{
  def apply[T <: Data](input: Stream[T], select : UInt, portCount: Int) : Vec[Stream[T]] = {
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

object StreamFork {
  def apply[T <: Data](input: Stream[T], portCount: Int, synchronous: Boolean = false): Vec[Stream[T]] = {
    val fork = new StreamFork(input.payloadType, portCount, synchronous).setCompositeName(input, "fork", true)
    fork.io.input << input
    return fork.io.outputs
  }
}

object StreamFork2 {
  def apply[T <: Data](input: Stream[T], synchronous: Boolean = false): (Stream[T], Stream[T]) = new Composite(input, "fork2"){
    val outputs = (cloneOf(input), cloneOf(input))
    val logic = new StreamForkArea(input, List(outputs._1, outputs._2), synchronous)
  }.outputs
}

object StreamFork3 {
  def apply[T <: Data](input: Stream[T], synchronous: Boolean = false): (Stream[T], Stream[T], Stream[T]) = new Composite(input, "fork3"){
    val outputs = (cloneOf(input), cloneOf(input), cloneOf(input))
    val logic = new StreamForkArea(input, List(outputs._1, outputs._2, outputs._3), synchronous)
  }.outputs
}

/**
 * A StreamFork will clone each incoming data to all its output streams. If synchronous is true,
 *  all output streams will always fire together, which means that the stream will halt until all 
 *  output streams are ready. If synchronous is false, output streams may be ready one at a time,
 *  at the cost of an additional flip flop (1 bit per output). The input stream will block until
 *  all output streams have processed each item regardlessly.
 *  
 *  Note that this means that when synchronous is true, the valid signal of the outputs depends on
 *  their inputs, which may lead to dead locks when used in combination with systems that have it the
 *  other way around. It also violates the handshake of the AXI specification (section A3.3.1).
 */
//TODOTEST
class StreamFork[T <: Data](dataType: HardType[T], portCount: Int, synchronous: Boolean = false) extends Component {
  val io = new Bundle {
    val input = slave Stream (dataType)
    val outputs = Vec(master Stream (dataType), portCount)
  }
  val logic = new StreamForkArea(io.input, io.outputs, synchronous)
}

class StreamForkArea[T <: Data](input : Stream[T], outputs : Seq[Stream[T]], synchronous: Boolean = false) extends Area {
  val portCount = outputs.size
  if (synchronous) {
    input.ready := outputs.map(_.ready).reduce(_ && _)
    outputs.foreach(_.valid := input.valid && input.ready)
    outputs.foreach(_.payload := input.payload)
  } else {
    /* Store if an output stream already has taken its value or not */
    val linkEnable = Vec(RegInit(True),portCount)

    /* Ready is true when every output stream takes or has taken its value */
    input.ready := True
    for (i <- 0 until portCount) {
      when(!outputs(i).ready && linkEnable(i)) {
        input.ready := False
      }
    }

    /* Outputs are valid if the input is valid and they haven't taken their value yet.
     * When an output fires, mark its value as taken. */
    for (i <- 0 until portCount) {
      outputs(i).valid := input.valid && linkEnable(i)
      outputs(i).payload := input.payload
      when(outputs(i).fire) {
        linkEnable(i) := False
      }
    }

    /* Reset the storage for each new value */
    when(input.ready) {
      linkEnable.foreach(_ := True)
    }
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

/** Join multiple streams into one. The resulting stream will only fire if all of them fire, so you may want to buffer the inputs. */
object StreamJoin {
  
  /**
   * Convert a tuple of streams into a stream of tuples
   */
  def apply[T1 <: Data,T2 <: Data](source1: Stream[T1], source2: Stream[T2]): Stream[TupleBundle2[T1, T2]] = {
    val sources = Seq(source1, source2)
    val combined = Stream(TupleBundle2(
        source1.payloadType,
        source2.payloadType
    ))
    combined.valid := sources.map(_.valid).reduce(_ && _)
    sources.foreach(_.ready := combined.fire)
    combined.payload._1 := source1.payload
    combined.payload._2 := source2.payload
    combined
  }

  /**
   * Convert a vector of streams into a stream of vectors.
   */
  def vec[T <: Data](sources: Seq[Stream[T]]): Stream[Vec[T]] = {
    val payload = Vec(sources.map(_.payload))
    val combined = Stream(payload)
    combined.payload := payload
    combined.valid := sources.map(_.valid).reduce(_ && _)
    sources.foreach(_.ready := combined.fire)
    combined
  }
  
  def arg(sources : Stream[_]*) : Event = apply(sources.seq)

  /** Join streams, but ignore the payload of the input streams. */
  def apply(sources: Seq[Stream[_]]): Event = {
    val event = Event
    val eventFire = event.fire
    event.valid := sources.map(_.valid).reduce(_ && _)
    sources.foreach(_.ready := eventFire)
    event
  }
  
  /**
   * Join streams, but ignore the payload and replace it with a custom one.
   * @param payload The payload of the resulting stream
   */
  def fixedPayload[T <: Data](sources: Seq[Stream[_]], payload: T): Stream[T] = StreamJoin(sources).translateWith(payload)
}

trait StreamFifoInterface[T <: Data]{
  def push          : Stream[T]
  def pop           : Stream[T]
  def pushOccupancy : UInt
  def popOccupancy  : UInt
}

object StreamFifo{
  def apply[T <: Data](dataType: T, depth: Int) = new StreamFifo(dataType,depth)
}

class StreamFifo[T <: Data](dataType: HardType[T], depth: Int) extends Component {
  require(depth >= 0)
  val io = new Bundle {
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
    val flush = in Bool() default(False)
    val occupancy    = out UInt (log2Up(depth + 1) bits)
    val availability = out UInt (log2Up(depth + 1) bits)
  }

  val bypass = (depth == 0) generate new Area {
      io.push >> io.pop
      io.occupancy := 0
      io.availability := 0
    }
  val oneStage = (depth == 1) generate new Area{
      io.push.m2sPipe(flush = io.flush) >> io.pop
      io.occupancy := U(io.pop.valid)
      io.availability := U(!io.pop.valid)
    }
  val logic = (depth > 1) generate new Area {
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
    if (isPow2(depth)) {
      io.occupancy := ((risingOccupancy && ptrMatch) ## ptrDif).asUInt
      io.availability := ((!risingOccupancy && ptrMatch) ## (popPtr - pushPtr)).asUInt
    } else {
      when(ptrMatch) {
        io.occupancy    := Mux(risingOccupancy, U(depth), U(0))
        io.availability := Mux(risingOccupancy, U(0), U(depth))
      } otherwise {
        io.occupancy := Mux(pushPtr > popPtr, ptrDif, U(depth) + ptrDif)
        io.availability := Mux(pushPtr > popPtr, U(depth) + (popPtr - pushPtr), (popPtr - pushPtr))
      }
    }

    when(io.flush){
      pushPtr.clear()
      popPtr.clear()
      risingOccupancy := False
    }
  }
  
  def withAssumes() = this.rework {
    import spinal.core.formal._
    assume(io.pop.payload === past(logic.ram(logic.popPtr)))
  }

  def formalCheck(cond: T => Bool): Vec[Bool] = this.rework {
    val pushBound = logic.pushPtr.value + depth
    val check = Vec(False, depth)
    for (i <- 0 until depth) {
      val popIndex = logic.popPtr.resize(log2Up(depth) + 1 bits) + i
      when(logic.popPtr < logic.pushPtr) {
        when(popIndex < logic.pushPtr) { check(i) := cond(logic.ram(popIndex.resized)) }
      }.elsewhen(logic.popPtr > logic.pushPtr) {
        when(popIndex < pushBound) { check(i) := cond(logic.ram(popIndex.resized)) }
      }.elsewhen(logic.popPtr === logic.pushPtr && io.pop.valid) {
        check(i) := cond(logic.ram(i))
      }
    }
    check
  }

  def formalContains(word: T): Bool = this.rework {
    formalCheck(_ === word.pull()).reduce(_ || _)
  }
  def formalContains(cond: T => Bool): Bool = this.rework {
    formalCheck(cond).reduce(_ || _)
  }

  def formalCount(word: T): UInt = this.rework {
    CountOne(formalCheck(_ === word.pull()))
  }
  def formalCount(cond: T => Bool): UInt = this.rework {
    CountOne(formalCheck(cond))
  }
}

object StreamFifoLowLatency{
  def apply[T <: Data](dataType: T, depth: Int) = new StreamFifoLowLatency(dataType,depth)
}

class StreamFifoLowLatency[T <: Data](val dataType: HardType[T],val depth: Int,val latency : Int = 0, useVec : Boolean = false) extends Component {
  require(depth >= 1)
  val io = new Bundle with StreamFifoInterface[T] {
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
    val flush = in Bool() default (False)
    val occupancy = out UInt (log2Up(depth + 1) bit)
    override def pushOccupancy: UInt = occupancy
    override def popOccupancy: UInt = occupancy
  }
  val vec = useVec generate Vec(Reg(dataType), depth)
  val ram = !useVec generate Mem(dataType, depth)
  val pushPtr = Counter(depth)
  val popPtr = Counter(depth)
  val ptrMatch = pushPtr === popPtr
  val risingOccupancy = RegInit(False)
  val empty = ptrMatch & !risingOccupancy
  val full = ptrMatch & risingOccupancy

  val pushing = io.push.fire
  val popping = io.pop.fire

  io.push.ready := !full

  val readed = if(useVec) vec(popPtr.value) else ram(popPtr.value)

  latency match{
    case 0 => {
      when(!empty){
        io.pop.valid := True
        io.pop.payload := readed
      } otherwise{
        io.pop.valid := io.push.valid
        io.pop.payload := io.push.payload
      }
    }
    case 1 => {
      io.pop.valid := !empty
      io.pop.payload := readed
    }
  }
  when(pushing =/= popping) {
    risingOccupancy := pushing
  }
  when(pushing) {
    if(useVec)
      vec.write(pushPtr.value, io.push.payload)
    else
      ram.write(pushPtr.value, io.push.payload)
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
  def apply[T <: Data](dataType: HardType[T], depth: Int, pushClock: ClockDomain, popClock: ClockDomain) = new StreamFifoCC(dataType, depth, pushClock, popClock)
}

//class   StreamFifoCC[T <: Data](dataType: HardType[T], val depth: Int, val pushClock: ClockDomain,val popClock: ClockDomain) extends Component {
//
//  assert(isPow2(depth) & depth >= 2, "The depth of the StreamFifoCC must be a power of 2 and equal or bigger than 2")
//
//  val io = new Bundle with StreamFifoInterface[T]{
//    val push          = slave  Stream(dataType)
//    val pop           = master Stream(dataType)
//    val pushOccupancy = out UInt(log2Up(depth + 1) bits)
//    val popOccupancy  = out UInt(log2Up(depth + 1) bits)
//  }
//
//  val ptrWidth = log2Up(depth) + 1
//  def isFull(a: Bits, b: Bits) = a(ptrWidth - 1 downto ptrWidth - 2) === ~b(ptrWidth - 1 downto ptrWidth - 2) && a(ptrWidth - 3 downto 0) === b(ptrWidth - 3 downto 0)
//  def isEmpty(a: Bits, b: Bits) = a === b
//
//  val ram = Mem(dataType, depth)
//
//  val popToPushGray = Bits(ptrWidth bits)
//  val pushToPopGray = Bits(ptrWidth bits)
//
//  val pushCC = new ClockingArea(pushClock) {
//    val pushPtr     = Counter(depth << 1)
//    val pushPtrGray = RegNext(toGray(pushPtr.valueNext)) init(0)
//    val popPtrGray  = BufferCC(popToPushGray, B(0, ptrWidth bits))
//    val full        = isFull(pushPtrGray, popPtrGray)
//
//    io.push.ready := !full
//
//    when(io.push.fire) {
//      ram(pushPtr.resized) := io.push.payload
//      pushPtr.increment()
//    }
//
//    io.pushOccupancy := (pushPtr - fromGray(popPtrGray)).resized
//  }
//
//  val popCC = new ClockingArea(popClock) {
//    val popPtr      = Counter(depth << 1)
//    val popPtrGray  = RegNext(toGray(popPtr.valueNext)) init(0)
//    val pushPtrGray = BufferCC(pushToPopGray, B(0, ptrWidth bit))
//    val empty       = isEmpty(popPtrGray, pushPtrGray)
//
//    io.pop.valid   := !empty
//    io.pop.payload := ram.readSync(popPtr.valueNext.resized, clockCrossing = true)
//
//    when(io.pop.fire) {
//      popPtr.increment()
//    }
//
//    io.popOccupancy := (fromGray(pushPtrGray) - popPtr).resized
//  }
//
//  pushToPopGray := pushCC.pushPtrGray
//  popToPushGray := popCC.popPtrGray
//}



class StreamFifoCC[T <: Data](val dataType: HardType[T],
                              val depth: Int,
                              val pushClock: ClockDomain,
                              val popClock: ClockDomain,
                              val withPopBufferedReset : Boolean = true) extends Component {

  assert(isPow2(depth) & depth >= 2, "The depth of the StreamFifoCC must be a power of 2 and equal or bigger than 2")

  val io = new Bundle with StreamFifoInterface[T]{
    val push          = slave  Stream(dataType)
    val pop           = master Stream(dataType)
    val pushOccupancy = out UInt(log2Up(depth + 1) bits)
    val popOccupancy  = out UInt(log2Up(depth + 1) bits)
  }

  val ptrWidth = log2Up(depth) + 1
  def isFull(a: Bits, b: Bits) = a(ptrWidth - 1 downto ptrWidth - 2) === ~b(ptrWidth - 1 downto ptrWidth - 2) && a(ptrWidth - 3 downto 0) === b(ptrWidth - 3 downto 0)
  def isEmpty(a: Bits, b: Bits) = a === b

  val ram = Mem(dataType, depth)

  val popToPushGray = Bits(ptrWidth bits)
  val pushToPopGray = Bits(ptrWidth bits)

  val pushCC = new ClockingArea(pushClock) {
    val pushPtr     = Reg(UInt(log2Up(2*depth) bits)) init(0)
    val pushPtrPlus = pushPtr + 1
    val pushPtrGray = RegNextWhen(toGray(pushPtrPlus), io.push.fire) init(0)
    val popPtrGray  = BufferCC(popToPushGray, B(0, ptrWidth bits))
    val full        = isFull(pushPtrGray, popPtrGray)

    io.push.ready := !full

    when(io.push.fire) {
      ram(pushPtr.resized) := io.push.payload
      pushPtr := pushPtrPlus
    }

    io.pushOccupancy := (pushPtr - fromGray(popPtrGray)).resized
  }

  val finalPopCd = popClock.withOptionalBufferedResetFrom(withPopBufferedReset)(pushClock)
  val popCC = new ClockingArea(finalPopCd) {
    val popPtr      = Reg(UInt(log2Up(2*depth) bits)) init(0)
    val popPtrPlus  = popPtr + 1
    val popPtrGray  = RegNextWhen(toGray(popPtrPlus), io.pop.fire) init(0)
    val pushPtrGray = BufferCC(pushToPopGray, B(0, ptrWidth bit))
    val empty       = isEmpty(popPtrGray, pushPtrGray)

    io.pop.valid   := !empty
    io.pop.payload := ram.readSync((io.pop.fire ? popPtrPlus | popPtr).resized, clockCrossing = true)

    when(io.pop.fire) {
      popPtr := popPtrPlus
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

class StreamCCByToggle[T <: Data](dataType: HardType[T], 
                                  inputClock: ClockDomain, 
                                  outputClock: ClockDomain, 
                                  withOutputBuffer : Boolean = true,
                                  withInputWait : Boolean = false,
                                  withOutputBufferedReset : Boolean = true) extends Component {
  val io = new Bundle {
    val input = slave Stream (dataType())
    val output = master Stream (dataType())
  }

  val outHitSignal = Bool()

  val pushArea = inputClock on new Area {
    val hit = BufferCC(outHitSignal, False)
    val accept = Bool()
    val target = RegInit(False) toggleWhen(accept)
    val data = RegNextWhen(io.input.payload, accept)

    if (!withInputWait) {
      accept := io.input.fire
      io.input.ready := (hit === target)
    } else {
      val busy = RegInit(False) setWhen(accept) clearWhen(io.input.ready)
      accept := (!busy) && io.input.valid
      io.input.ready := busy && (hit === target)
    }
  }

  val finalOutputClock = outputClock.withOptionalBufferedResetFrom(withOutputBufferedReset)(inputClock)
  val popArea = finalOutputClock on new Area {
    val stream = cloneOf(io.input)

    val target = BufferCC(pushArea.target, False)
    val hit = RegNextWhen(target, stream.fire) init(False)
    outHitSignal := hit

    stream.valid := (target =/= hit)

    stream.payload := pushArea.data
    stream.payload.addTag(crossClockDomain)

    io.output << (if(withOutputBuffer) stream.m2sPipe(holdPayload = true) else stream)
  }
}

/**
 * Enumeration to present order of slices.
 */
sealed trait SlicesOrder
/** Slice with lower bits process first */
object LOWER_FIRST extends SlicesOrder
/** Slice with higher bits process first */
object HIGHER_FIRST extends SlicesOrder

object StreamWidthAdapter {
  def apply[T <: Data,T2 <: Data](input : Stream[T],output : Stream[T2], endianness: Endianness = LITTLE, padding : Boolean = false): Unit = {
    val inputWidth = widthOf(input.payload)
    val outputWidth = widthOf(output.payload)
    if(inputWidth == outputWidth){
      output.arbitrationFrom(input)
      output.payload.assignFromBits(input.payload.asBits)
    } else if(inputWidth > outputWidth){
      require(inputWidth % outputWidth == 0 || padding)
      val factor = (inputWidth + outputWidth - 1) / outputWidth
      val paddedInputWidth = factor * outputWidth
      val counter = Counter(factor,inc = output.fire)
      output.valid := input.valid
      endianness match {
        case `LITTLE` => output.payload.assignFromBits(input.payload.asBits.resize(paddedInputWidth).subdivideIn(factor slices).read(counter))
        case `BIG`    => output.payload.assignFromBits(input.payload.asBits.resize(paddedInputWidth).subdivideIn(factor slices).reverse.read(counter))
      }
      input.ready := output.ready && counter.willOverflowIfInc
    } else{
      require(outputWidth % inputWidth == 0 || padding)
      val factor  = (outputWidth + inputWidth - 1) / inputWidth
      val paddedOutputWidth = factor * inputWidth
      val counter = Counter(factor,inc = input.fire)
      val buffer  = Reg(Bits(paddedOutputWidth - inputWidth bits))
      when(input.fire){
        buffer := input.payload ## (buffer >> inputWidth)
      }
      output.valid := input.valid && counter.willOverflowIfInc
      endianness match {
        case `LITTLE` => output.payload.assignFromBits((input.payload ## buffer).resize(outputWidth))
        case `BIG`    => output.payload.assignFromBits((input.payload ## buffer).subdivideIn(factor slices).reverse.asBits().resize(outputWidth))
      }
      input.ready := !(!output.ready && counter.willOverflowIfInc)
    }
  }

  def apply[T <: Data,T2 <: Data](input : Stream[T],output : Stream[T2], order : SlicesOrder): Unit = {
    StreamWidthAdapter(input, output, order, false)
  }

  def apply[T <: Data,T2 <: Data](input : Stream[T],output : Stream[T2], order : SlicesOrder, padding : Boolean): Unit = {
    val endianness = order match {
      case HIGHER_FIRST => BIG
      case LOWER_FIRST => LITTLE
    }
    StreamWidthAdapter(input, output, endianness, padding)
  }

  def make[T <: Data, T2 <: Data](input : Stream[T], outputPayloadType : HardType[T2], order : SlicesOrder) : Stream[T2] = {
    val ret = Stream(outputPayloadType())
    StreamWidthAdapter(input,ret,order,false)
    ret
  }

  def make[T <: Data, T2 <: Data](input : Stream[T], outputPayloadType : HardType[T2], order : SlicesOrder, padding : Boolean) : Stream[T2] = {
    val ret = Stream(outputPayloadType())
    StreamWidthAdapter(input,ret,order,padding)
    ret
  }

  def make[T <: Data, T2 <: Data](input : Stream[T], outputPayloadType : HardType[T2], endianness: Endianness = LITTLE, padding : Boolean = false) : Stream[T2] = {
    val ret = Stream(outputPayloadType())
    StreamWidthAdapter(input,ret,endianness,padding)
    ret
  }

  def main(args: Array[String]) : Unit = {
    SpinalVhdl(new Component{
      val input = slave(Stream(Bits(4 bits)))
      val output = master(Stream(Bits(32 bits)))
      StreamWidthAdapter(input,output)
    })
  }
}

object StreamFragmentWidthAdapter {
  def apply[T <: Data,T2 <: Data](input : Stream[Fragment[T]],output : Stream[Fragment[T2]], endianness: Endianness = LITTLE, padding : Boolean = false): Unit = {
    val inputWidth = widthOf(input.fragment)
    val outputWidth = widthOf(output.fragment)
    if(inputWidth == outputWidth){
      output.arbitrationFrom(input)
      output.payload.assignFromBits(input.payload.asBits)
    } else if(inputWidth > outputWidth){
      require(inputWidth % outputWidth == 0 || padding)
      val factor = (inputWidth + outputWidth - 1) / outputWidth
      val paddedInputWidth = factor * outputWidth
      val counter = Counter(factor,inc = output.fire)
      output.valid := input.valid
      endianness match {
        case `LITTLE` => output.fragment.assignFromBits(input.fragment.asBits.resize(paddedInputWidth).subdivideIn(factor slices).read(counter))
        case `BIG`    => output.fragment.assignFromBits(input.fragment.asBits.resize(paddedInputWidth).subdivideIn(factor slices).reverse.read(counter))
      }
      output.last := input.last && counter.willOverflowIfInc
      input.ready := output.ready && counter.willOverflowIfInc
    } else{
      require(outputWidth % inputWidth == 0 || padding)
      val factor  = (outputWidth + inputWidth - 1) / inputWidth
      val paddedOutputWidth = factor * inputWidth
      val counter = Counter(factor,inc = input.fire)
      val buffer  = Reg(Bits(paddedOutputWidth - inputWidth bits))
      when(input.fire){
        buffer := input.fragment ## (buffer >> inputWidth)
      }
      output.valid := input.valid && counter.willOverflowIfInc
      endianness match {
        case `LITTLE` => output.fragment.assignFromBits((input.fragment ## buffer).resize(outputWidth))
        case `BIG`    => output.fragment.assignFromBits((input.fragment ## buffer).subdivideIn(factor slices).reverse.asBits().resize(outputWidth))
      }
      output.last := input.last
      input.ready := !(!output.ready && counter.willOverflowIfInc)
    }
  }

  def apply[T <: Data,T2 <: Data](input : Stream[Fragment[T]],output : Stream[Fragment[T2]], order : SlicesOrder): Unit = {
    StreamFragmentWidthAdapter(input, output, order, false)
  }

  def apply[T <: Data,T2 <: Data](input : Stream[Fragment[T]],output : Stream[Fragment[T2]], order : SlicesOrder, padding : Boolean): Unit = {
    val endianness = order match {
      case HIGHER_FIRST => BIG
      case LOWER_FIRST => LITTLE
    }
    StreamFragmentWidthAdapter(input, output, endianness, padding)
  }

  def make[T <: Data, T2 <: Data](input : Stream[Fragment[T]], outputPayloadType : HardType[T2], order : SlicesOrder) : Stream[Fragment[T2]] = {
    val ret = Stream(Fragment(outputPayloadType()))
    StreamFragmentWidthAdapter(input,ret,order,false)
    ret
  }

  def make[T <: Data, T2 <: Data](input : Stream[Fragment[T]], outputPayloadType : HardType[T2], order : SlicesOrder, padding : Boolean) : Stream[Fragment[T2]] = {
    val ret = Stream(Fragment(outputPayloadType()))
    StreamFragmentWidthAdapter(input,ret,order,padding)
    ret
  }

  def make[T <: Data, T2 <: Data](input : Stream[Fragment[T]], outputPayloadType : HardType[T2], endianness: Endianness = LITTLE, padding : Boolean = false) : Stream[Fragment[T2]] = {
    val ret = Stream(Fragment(outputPayloadType()))
    StreamFragmentWidthAdapter(input,ret,endianness,padding)
    ret
  }
}

case class StreamFifoMultiChannelPush[T <: Data](payloadType : HardType[T], channelCount : Int) extends Bundle with IMasterSlave {
  val channel = Bits(channelCount bits)
  val full = Bool()
  val stream = Stream(payloadType)

  override def asMaster(): Unit = {
    out(channel)
    master(stream)
    in(full)
  }
}

case class StreamFifoMultiChannelPop[T <: Data](payloadType : HardType[T], channelCount : Int) extends Bundle with IMasterSlave {
  val channel = Bits(channelCount bits)
  val empty   = Bits(channelCount bits)
  val stream  = Stream(payloadType)

  override def asMaster(): Unit = {
    out(channel)
    slave(stream)
    in(empty)
  }

  def toStreams(withCombinatorialBuffer : Boolean) = new Area{
    val bufferIn, bufferOut = Vec(Stream(payloadType), channelCount)
    (bufferOut, bufferIn).zipped.foreach((s, m) => if(withCombinatorialBuffer) s </< m else s <-< m)

    val needRefill = B(bufferIn.map(_.ready))
    val selOh = OHMasking.first(needRefill & ~empty) //TODO
    val nonEmpty = (~empty).orR
    channel := selOh
    for((feed, sel) <- (bufferIn, selOh.asBools).zipped){
      feed.valid := sel && nonEmpty
      feed.payload := stream.payload
    }
    stream.ready := (selOh & B(bufferIn.map(_.ready))).orR
  }.setCompositeName(this,"toStreams", true).bufferOut

}

//Emulate multiple fifo but with one push,one pop port and a shared storage
//io.availability has one cycle latency
case class StreamFifoMultiChannelSharedSpace[T <: Data](payloadType : HardType[T], channelCount : Int, depth : Int, withAllocationFifo : Boolean = false) extends Component{
  assert(isPow2(depth))
  val io = new Bundle {
    val push = slave(StreamFifoMultiChannelPush(payloadType, channelCount))
    val pop  = slave(StreamFifoMultiChannelPop(payloadType, channelCount))
    val availability = out UInt(log2Up(depth) + 1 bits)
  }
  val ptrWidth = log2Up(depth)

  val payloadRam = Mem(payloadType(), depth)
  val nextRam = Mem(UInt(ptrWidth bits), depth)

  val full = False
  io.push.full := full
  io.push.stream.ready := !full

  val pushNextEntry = UInt(ptrWidth bits)
  val popNextEntry = nextRam.wordType()



  val channels = for (channelId <- 0 until channelCount) yield new Area {
    val valid = RegInit(False)
    val headPtr = Reg(UInt(ptrWidth bits))
    val lastPtr = Reg(UInt(ptrWidth bits))
    val lastFire = False
    when(io.pop.stream.fire && io.pop.channel(channelId)) {
      headPtr := popNextEntry
      when(headPtr === lastPtr){
        lastFire := True
        valid := False
      }
    }

    when(!valid || lastFire){
      headPtr := pushNextEntry
    }

    when(io.push.stream.fire && io.push.channel(channelId)) {
      lastPtr := pushNextEntry
      valid := True
    }
    io.pop.empty(channelId) := !valid
  }

  val pushLogic = new Area{
    val previousAddress = MuxOH(io.push.channel, channels.map(_.lastPtr))
    when(io.push.stream.fire) {
      payloadRam.write(pushNextEntry, io.push.stream.payload)
      when((channels.map(_.valid).asBits() & io.push.channel).orR) {
        nextRam.write(previousAddress, pushNextEntry)
      }
    }
  }

  val popLogic = new Area {
    val readAddress = channels.map(_.headPtr).read(OHToUInt(io.pop.channel))
    io.pop.stream.valid := (io.pop.channel & ~io.pop.empty).orR
    io.pop.stream.payload := payloadRam.readAsync(readAddress)
    popNextEntry := nextRam.readAsync(readAddress)
  }

  val allocationByCounter = !withAllocationFifo generate new Area{
    val allocationPtr = Reg(UInt(ptrWidth bits)) init(0)

    when(io.push.stream.fire) {
      allocationPtr := allocationPtr + 1
    }

    val onChannels = for(c <- channels) yield new Area{
      full setWhen(c.valid && allocationPtr === c.headPtr)
      val wasValid = RegNext(c.valid) init(False)
      val availability = RegNext(c.headPtr-allocationPtr)
    }

    val (availabilityValid, availabilityValue) = onChannels.map(c => (c.wasValid, c.availability)).reduceBalancedTree{case (a,b) => (a._1 || b._1, (a._1 && (!b._1 || a._2 < b._2)) ? a._2 | b._2)}
    io.availability := (availabilityValid ? availabilityValue | depth)

    pushNextEntry := allocationPtr
  }


  val allocationByFifo = withAllocationFifo generate new Area{
    ???
  }


}

object StreamFifoMultiChannelBench extends App{
  val payloadType = HardType(Bits(8 bits))
  class BenchFpga(channelCount : Int) extends Rtl{
    override def getName(): String = "Bench" + channelCount
    override def getRtlPath(): String = getName() + ".v"
    SpinalVerilog(new Component{
      val push = slave(StreamFifoMultiChannelPush(payloadType, channelCount))
      val pop  = slave(StreamFifoMultiChannelPop(payloadType, channelCount))
      val fifo = StreamFifoMultiChannelSharedSpace(payloadType, channelCount, 32)

      fifo.io.push.channel := RegNext(push.channel)
      push.full := RegNext(fifo.io.push.full)
      fifo.io.push.stream  <-/< push.stream

      fifo.io.pop.channel := RegNext(pop.channel)
      pop.empty := RegNext(fifo.io.pop.empty)
      pop.stream  <-/<  fifo.io.pop.stream

      setDefinitionName(BenchFpga.this.getName())
    })
  }
  class BenchFpga2(channelCount : Int) extends Rtl{
    override def getName(): String = "BenchToStream" + channelCount
    override def getRtlPath(): String = getName() + ".v"
    SpinalVerilog(new Component{
      val push = slave(StreamFifoMultiChannelPush(payloadType, channelCount))
      val fifo = StreamFifoMultiChannelSharedSpace(payloadType, channelCount, 32)

      fifo.io.push.channel := RegNext(push.channel)
      push.full := RegNext(fifo.io.push.full)
      fifo.io.push.stream  <-/< push.stream

      setDefinitionName(BenchFpga2.this.getName())

      val outputs = fifo.io.pop.toStreams(false).map(_.s2mPipe().asMaster())
    })
  }


  val rtls = List(2,4,8).map(width => new BenchFpga(width)) ++ List(2,4,8).map(width => new BenchFpga2(width))

  val targets = XilinxStdTargets()// ++ AlteraStdTargets()


  Bench(rtls, targets)
}

object StreamTransactionCounter {
    def apply[T <: Data, T2 <: Data](
        trigger: Stream[T],
        target: Stream[T2],
        count: UInt,
        noDelay: Boolean = false
    ): StreamTransactionCounter = {
        val inst = new StreamTransactionCounter(count.getWidth, noDelay)
        inst.io.ctrlFire := trigger.fire
        inst.io.targetFire := target.fire
        inst.io.count := count
        inst
    }
}

class StreamTransactionCounter(
    countWidth: Int,
    noDelay: Boolean = false
) extends Component {
    val io = new Bundle {
        val ctrlFire   = in Bool ()
        val targetFire = in Bool ()
        val available  = out Bool ()
        val count      = in UInt (countWidth bits)
        val working    = out Bool ()
        val last       = out Bool ()
        val done       = out Bool ()
        val value      = out UInt (countWidth bit)
    }

    val countReg = RegNextWhen(io.count, io.ctrlFire)
    val counter  = Counter(io.count.getBitsWidth bits)
    val expected = if(noDelay) { countReg.getAheadValue() } else { CombInit(countReg) }

    val lastOne = counter >= expected
    val running = Reg(Bool()) init False
    val working = CombInit(running)

    val done         = lastOne && io.targetFire
    if(noDelay){
      when(io.ctrlFire) { working := True }
      when(done) { running := False }
      .otherwise { running := working }
    } else {
      when (io.ctrlFire) { running := True } 
      .elsewhen(done) { running := False }
    }

    when(done) {
        counter.clear()
    } elsewhen (io.targetFire & working) {
        counter.increment()
    }

    io.working := working
    io.last := lastOne & working
    io.done := done & working
    io.value := counter
    if(noDelay) { io.available := !running } else { io.available := !working | io.done }

    def withAsserts() = new Area {
      val startedReg = Reg(Bool()) init False
      val started = CombInit(startedReg)
      val waiting = io.working & !started
      when(io.targetFire & io.working) {
        started := True
        startedReg := True
      }
      when(done) { startedReg := False }

      when(startedReg) { assert(io.working && counter.value > 0 && counter.value <= expected) }
      when(counter.value > 0) { assert(started) }
//      when(!io.working) { assert(counter.value === 0) }
//      assert(counter.value <= expected)
    }
}

object StreamTransactionExtender {
    def apply[T <: Data](input: Stream[T], count: UInt, noDelay: Boolean = false)(
        implicit driver: (UInt, T, Bool) => T = (_: UInt, p: T, _: Bool) => p
    ): Stream[T] = {
        val c = new StreamTransactionExtender(input.payloadType, input.payloadType, count.getBitsWidth, noDelay, driver)
        c.io.input << input
        c.io.count := count
        c.io.output
    }

    def apply[T <: Data, T2 <: Data](input: Stream[T], output: Stream[T2], count: UInt)(
        driver: (UInt, T, Bool) => T2
    ): StreamTransactionExtender[T, T2] = StreamTransactionExtender(input, output, count, false)(driver)

    def apply[T <: Data, T2 <: Data](input: Stream[T], output: Stream[T2], count: UInt, noDelay: Boolean)(
        driver: (UInt, T, Bool) => T2
    ): StreamTransactionExtender[T, T2] = {
        val c = new StreamTransactionExtender(input.payloadType, output.payloadType, count.getBitsWidth, noDelay, driver)
        c.io.input << input
        c.io.count := count
        output << c.io.output
        c
    }
}

/* Extend one input transfer into serveral outputs, io.count represent delivering output (count + 1) times. */
class StreamTransactionExtender[T <: Data, T2 <: Data](
    dataType: HardType[T],
    outDataType: HardType[T2],
    countWidth: Int,
    noDelay: Boolean,
    driver: (UInt, T, Bool) => T2
) extends Component {
    val io = new Bundle {
        val count   = in UInt (countWidth bit)
        val input   = slave Stream dataType
        val output  = master Stream outDataType
        val working = out Bool ()
        val first   = out Bool ()
        val last    = out Bool ()
        val done    = out Bool ()
    }

    val counter  = StreamTransactionCounter(io.input, io.output, io.count, noDelay)
    val payloadReg  = Reg(io.input.payloadType)
    val lastOne  = counter.io.last
    val count = counter.io.value
    val payload = if(noDelay) CombInit(payloadReg.getAheadValue) else CombInit(payloadReg)

    when(io.input.fire) {
        payloadReg := io.input.payload
    }

    io.output.payload := driver(count, payload, lastOne)
    io.output.valid := counter.io.working
    io.input.ready := counter.io.available
    io.last := lastOne
    io.done := counter.io.done
    io.first := (counter.io.value === 0) && counter.io.working
    io.working := counter.io.working
}
