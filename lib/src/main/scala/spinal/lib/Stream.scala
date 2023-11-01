package spinal.lib

import spinal.core._
import spinal.idslplugin.Location
import spinal.lib.eda.bench.{AlteraStdTargets, Bench, Rtl, XilinxStdTargets}

import scala.collection.Seq
import scala.collection.mutable

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
  def queue(size: Int, latency : Int = 2, forFMax : Boolean = false): Stream[T] = new Composite(this){
    val fifo = StreamFifo(payloadType, size, latency = latency, forFMax = forFMax)
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
  def queueWithOccupancy(size: Int, latency : Int = 2, forFMax : Boolean = false): (Stream[T], UInt) = {
    val fifo = StreamFifo(payloadType, size, latency = latency, forFMax = forFMax).setCompositeName(this,"queueWithOccupancy", true)
    fifo.io.push << this
    return (fifo.io.pop, fifo.io.occupancy)
  }

  def queueWithAvailability(size: Int, latency : Int = 2, forFMax : Boolean = false): (Stream[T], UInt) = {
    val fifo = StreamFifo(payloadType, size, latency = latency, forFMax = forFMax).setCompositeName(this,"queueWithAvailability", true)
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
  def isStall : Bool = signalCache(this ->"isStall")((valid && !ready).setCompositeName(this, "isStall", true))

  /** Return True when a transaction has appeared (first cycle)
    */
  def isNew : Bool = signalCache(this ->"isNew")((valid && !(RegNext(isStall) init(False))).setCompositeName(this, "isNew", true))

  /** Return True when a transaction occurs on the bus (valid && ready)
  */
  override def fire: Bool = signalCache(this ->"fire")((valid & ready).setCompositeName(this, "fire", true))

/** Return True when the bus is ready, but no data is present
  */
  def isFree: Bool = signalCache(this ->"isFree")((!valid || ready).setCompositeName(this, "isFree", true))
  
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

    val rValidN = RegInit(True) clearWhen(self.valid) setWhen(s2mPipe.ready)
    val rData = RegNextWhen(self.payload, self.ready)

    self.ready := rValidN

    s2mPipe.valid := self.valid || !rValidN
    s2mPipe.payload := Mux(rValidN, self.payload, rData)

    if(flush != null) rValidN.setWhen(flush)
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
  def formalAssertsMaster(payloadInvariance : Boolean = true)(implicit loc : Location) = new Composite(this, "asserts") {
    import spinal.core.formal._
    val stack = ScalaLocated.long
    when(past(isStall) init(False)) {
      assert(valid,  "Stream transaction disappeared:\n" + stack)
      if(payloadInvariance) assert(stable(payload), "Stream transaction payload changed:\n" + stack)
    }
  }

  def formalAssumesSlave(payloadInvariance : Boolean = true)(implicit loc : Location) = new Composite(this, "assumes") {
    import spinal.core.formal._
    when(past(isStall) init (False)) {
      assume(valid)
      if(payloadInvariance) assume(stable(payload))
    }
  }

  def formalCovers(back2BackCycles: Int = 1) = new Composite(this, "covers") {
    import spinal.core.formal._
    val hist = History(fire, back2BackCycles).reduce(_ && _)
    cover(hist)
    cover(isStall)
    // doubt that if this is required in generic scenario.
    // cover(this.ready && !this.valid)
  }

  def formalAssertsOrder(dataAhead : T, dataBehind : T)(implicit loc : Location) : Tuple2[Bool, Bool] = new Composite(this, "orders")  {
    import spinal.core.formal._
    val aheadOut = RegInit(False) setWhen (fire && dataAhead === payload)
    val behindOut = RegInit(False) setWhen (fire && dataBehind === payload)

    when(!aheadOut){ assert(!behindOut) }
    when(behindOut){ assert(aheadOut) }

    cover(aheadOut)
    cover(behindOut)
    
    val out = (aheadOut, behindOut)
  }.out

  // flags if subjects have entered the StreamFifo
  def formalAssumesOrder(dataAhead : T, dataBehind : T)(implicit loc : Location) : Tuple2[Bool, Bool] = new Composite(this, "orders") {
    import spinal.core.formal._
    // flags indicates if the subjects went in the StreamFIfo
    val aheadIn = RegInit(False) setWhen (fire && dataAhead === payload)
    val behindIn = RegInit(False) setWhen (fire && dataBehind === payload)
    // once subject entered, prevent duplicate payloads from entering the StreamFifo
    when(aheadIn) { assume(payload =/= dataAhead) }
    when(behindIn) { assume(payload =/= dataBehind) }
    
    // make sure our two subjects are distinguishable (different)
    assume(dataAhead =/= dataBehind)
    // assume our subjects go inside the StreamFifo in correct order
    when(!aheadIn) { assume(!behindIn) }
    when(behindIn) { assume(aheadIn) }
    // return which subjects went in the StreamFifo
    val out = (aheadIn, behindIn)
  }.out

  /** Assert that this stream conforms to the stream semantics:
    * https://spinalhdl.github.io/SpinalDoc-RTD/dev/SpinalHDL/Libraries/stream.html#semantics
    * - After being asserted, valid should be acknowledged in limited cycles.
    *
    * @param maxStallCycles Check that the max cycles the interface would hold in stall.
    */
  def formalAssertsTimeout(maxStallCycles: Int = 0) = new Composite(this, "timeout") {
    import spinal.core.formal._
    val logic = (maxStallCycles > 0) generate new Area {
      val counter = Counter(maxStallCycles, isStall)
      when(!isStall) { counter.clear() }
        .otherwise { assert(!counter.willOverflow) }
    }
  }

  def formalAssumesTimeout(maxStallCycles: Int = 0) = new Composite(this, "timeout") {
    import spinal.core.formal._
    val logic = (maxStallCycles > 0) generate new Area {
      val counter = Counter(maxStallCycles, isStall)
      when(!isStall) { counter.clear() }
        .elsewhen(counter.willOverflow) { assume(ready === True) }
    }
  }

  def toReg() : T = toReg(null.asInstanceOf[T])
  def toReg(init: T): T = {
    this.ready := True
    RegNextWhen(this.payload,this.fire,init)
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

  def buildOn[T <: Data](inputs : Seq[Stream[T]]): StreamArbiter[T] = {
    val a = new StreamArbiter(inputs.head.payloadType, inputs.size)(arbitrationLogic, lockLogic)
    (a.io.inputs, inputs).zipped.foreach(_ << _)
    a
  }

  def buildOn[T <: Data](first : Stream[T], others : Stream[T]*): StreamArbiter[T] = {
    buildOn(first :: others.toList)
  }

  def onArgs[T <: Data](inputs: Stream[T]*): Stream[T] = on(inputs.seq)
  def on[T <: Data](inputs: Seq[Stream[T]]): Stream[T] = {
    val arbiter = build(inputs(0).payloadType, inputs.size)
    (arbiter.io.inputs, inputs).zipped.foreach(_ << _)
    val ret = arbiter.io.output.combStage()
//    arbiter.setCompositeName(ret, "arbiter")
    ret
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

  def setLock(body : (StreamArbiter[_ <: Data]) => Area) : this.type = {
    lockLogic = body
    this
  }
  def noLock: this.type = setLock(StreamArbiter.Lock.none)
  def fragmentLock: this.type = setLock(StreamArbiter.Lock.fragmentLock)
  def transactionLock: this.type = setLock(StreamArbiter.Lock.transactionLock)
  def lambdaLock[T <: Data](unlock: Stream[T] => Bool) : this.type = setLock{
    case c : StreamArbiter[T] => new Area {
      import c._
      locked setWhen(io.output.valid)
      locked.clearWhen(io.output.fire && unlock(io.output))
    }
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

  def apply[T <: Data](select: Stream[UInt], inputs: Vec[Stream[T]]): Stream[T] = {
    val c = new StreamMux(inputs(0).payload, inputs.length)
    (c.io.inputs, inputs).zipped.foreach(_ << _)
    select >> c.io.createSelector()
    c.io.output
  }
}

class StreamMux[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val select = in UInt (log2Up(portCount) bit)
    val inputs = Vec(slave Stream (dataType), portCount)
    val output = master Stream (dataType)
    def createSelector(): Stream[UInt] = new Composite(this, "selector") {
      val stream = Stream(cloneOf(select))
      val reg = stream.haltWhen(output.isStall).toReg(U(0))
      select := reg
    }.stream
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

  def apply[T <: Data](input: Stream[T], select : Stream[UInt], portCount: Int) : Vec[Stream[T]] = {
    val c = new StreamDemux(input.payload,portCount)
    c.io.input << input
    select >> c.io.createSelector()
    c.io.outputs
  }

  def two[T <: Data](input: Stream[T], select : UInt) : (Stream[T], Stream[T]) = {
    val demux = apply(input, select, 2)
    (demux(0).combStage(), demux(1).combStage())
  }
  def two[T <: Data](input: Stream[T], select : Bool) : (Stream[T], Stream[T]) = two(input, select.asUInt)
}

class StreamDemux[T <: Data](dataType: T, portCount: Int) extends Component {
  val io = new Bundle {
    val select = in UInt (log2Up(portCount) bit)
    val input = slave Stream (dataType)
    val outputs = Vec(master Stream (dataType),portCount)
    def createSelector(): Stream[UInt] = new Composite(this, "selector") {
      val stream = Stream(cloneOf(select))
      val reg = stream.haltWhen(input.isStall).toReg(U(0))
      select := reg
    }.stream
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

object StreamDemuxOh{
  def apply[T <: Data](input : Stream[T], oh : Seq[Bool]) : Vec[Stream[T]] = oh.size match {
    case 1 => Vec(input.combStage())
    case _ => {
      val ret = Vec(oh.map{sel =>
        val output = cloneOf(input)
        output.valid   := input.valid && sel
        output.payload := input.payload
        output
      })
      input.ready    := (ret, oh).zipped.map(_.ready && _).orR
      ret
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

  def takes[T <: Data](input: Stream[T],take0 : Bool, take1 : Bool, synchronous: Boolean = false): (Stream[T], Stream[T]) = new Composite(input, "fork2") {
    val forks = (cloneOf(input), cloneOf(input))
    val logic = new StreamForkArea(input, List(forks._1, forks._2), synchronous)
    val outputs = (forks._1.takeWhen(take0), forks._1.takeWhen(take1))
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
  /*Used for async, Store if an output stream already has taken its value or not */
  val linkEnable = if(!synchronous)Vec(RegInit(True),portCount)else null
  if (synchronous) {
    input.ready := outputs.map(_.ready).reduce(_ && _)
    outputs.foreach(_.valid := input.valid && input.ready)
    outputs.foreach(_.payload := input.payload)
  } else {
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
  def apply[T <: Data](dataType: HardType[T],
                       depth: Int,
                       latency : Int = 2,
                       forFMax : Boolean = false) = {
    assert(latency >= 0 && latency <= 2)
    new StreamFifo(
      dataType,
      depth,
      withAsyncRead = latency < 2,
      withBypass = latency == 0,
      forFMax = forFMax
    )
  }
}

/**
  * Fully redesigned in release 1.8.2 allowing improved timing closure.
  * - latency of 0, 1, 2 cycles
  *
  * @param dataType
  * @param depth Number of element stored in the fifo, Note that if withAsyncRead==false, then one extra transaction can be stored
  * @param withAsyncRead Read the memory using asyncronous read port (ex distributed ram). If false, add 1 cycle latency
  * @param withBypass Bypass the push port to the pop port when the fifo is empty. If false, add 1 cycle latency
  *                   Only available if withAsyncRead == true
  * @param forFMax Tune the design to get the maximal clock frequency
  * @param useVec Use an Vec of register instead of a Mem to store the content
  *               Only available if withAsyncRead == true
  */
class StreamFifo[T <: Data](val dataType: HardType[T],
                            val depth: Int,
                            val withAsyncRead : Boolean = false,
                            val withBypass : Boolean = false,
                            val allowExtraMsb : Boolean = true,
                            val forFMax : Boolean = false,
                            val useVec : Boolean = false) extends Component {
  require(depth >= 0)

  if(withBypass) require(withAsyncRead)
  if(useVec) require (withAsyncRead)

  val io = new Bundle with StreamFifoInterface[T]{
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
    val flush = in Bool() default(False)
    val occupancy    = out UInt (log2Up(depth + 1) bits)
    val availability = out UInt (log2Up(depth + 1) bits)
    override def pushOccupancy = occupancy
    override def popOccupancy = occupancy
  }

  class CounterUpDownFmax(states : BigInt, init : BigInt) extends Area{
    val incr, decr = Bool()
    val value = Reg(UInt(log2Up(states) bits)) init(init)
    val plusOne = KeepAttribute(value + 1)
    val minusOne = KeepAttribute(value - 1)
    when(incr =/= decr){
      value := incr.mux(plusOne, minusOne)
    }
    when(io.flush) { value := init }
  }

  val withExtraMsb = allowExtraMsb && isPow2(depth)
  val bypass = (depth == 0) generate new Area {
    io.push >> io.pop
    io.occupancy := 0
    io.availability := 0
  }
  val oneStage = (depth == 1) generate new Area {
    val doFlush = CombInit(io.flush)
    val buffer = io.push.m2sPipe(flush = doFlush)
    io.pop << buffer
    io.occupancy := U(buffer.valid)
    io.availability := U(!buffer.valid)

    if(withBypass){
      when(!buffer.valid){
        io.pop.valid := io.push.valid
        io.pop.payload := io.push.payload
        doFlush setWhen(io.pop.ready)
      }
    }
  }
  val logic = (depth > 1) generate new Area {
    val vec = useVec generate Vec(Reg(dataType), depth)
    val ram = !useVec generate Mem(dataType, depth)

    val ptr = new Area{
      val doPush, doPop = Bool()
      val full, empty = Bool()
      val push = Reg(UInt(log2Up(depth) + withExtraMsb.toInt bits)) init(0)
      val pop  = Reg(UInt(log2Up(depth) + withExtraMsb.toInt bits)) init(0)
      val occupancy = cloneOf(io.occupancy)
      val popOnIo = cloneOf(pop) // Used to track the global occupancy of the fifo (the extra buffer of !withAsyncRead)
      val wentUp = RegNextWhen(doPush, doPush =/= doPop) init(False) clearWhen (io.flush)

      val arb = new Area {
        val area = !forFMax generate {
          withExtraMsb match {
            case true => { //as we have extra MSB, we don't need the "wentUp"
              full := (push ^ popOnIo ^ depth) === 0
              empty := push === pop
            }
            case false => {
              full := push === popOnIo && wentUp
              empty := push === pop && !wentUp
            }
          }
        }

        val fmax = forFMax generate new Area {
          val counterWidth = log2Up(depth) + 1
          val emptyTracker = new CounterUpDownFmax(1 << counterWidth, 1 << (counterWidth - 1)) {
            incr := doPop
            decr := doPush
            empty := value.msb
          }

          val fullTracker = new CounterUpDownFmax(1 << counterWidth, (1 << (counterWidth - 1)) - depth) {
            incr := io.push.fire
            decr := io.pop.fire
            full := value.msb
          }
        }
      }


      when(doPush){
        push := push + 1
        if(!isPow2(depth)) when(push === depth - 1){ push := 0 }
      }
      when(doPop){
        pop := pop + 1
        if(!isPow2(depth)) when(pop === depth - 1){ pop := 0 }
      }

      when(io.flush){
        push := 0
        pop := 0
      }


      val forPow2 = (withExtraMsb && !forFMax) generate new Area{
        occupancy := push - popOnIo  //if no extra msb, could be U(full ## (push - popOnIo))
      }

      val notPow2 = (!withExtraMsb && !forFMax) generate new Area{
        val counter = Reg(UInt(log2Up(depth + 1) bits)) init(0)
        counter := counter + U(io.push.fire) - U(io.pop.fire)
        occupancy := counter

        when(io.flush) { counter := 0 }
      }
      val fmax = forFMax generate new CounterUpDownFmax(depth + 1, 0){
        incr := io.push.fire
        decr := io.pop.fire
        occupancy := value
      }
    }

    val push = new Area {
      io.push.ready := !ptr.full
      ptr.doPush := io.push.fire
      val onRam = !useVec generate new Area {
        val write = ram.writePort()
        write.valid := io.push.fire
        write.address := ptr.push.resized
        write.data := io.push.payload
      }
      val onVec = useVec generate new Area {
        when(io.push.fire){
          vec.write(ptr.push.resized, io.push.payload)
        }
      }
    }

    val pop = new Area{
      val addressGen = Stream(UInt(log2Up(depth) bits))
      addressGen.valid := !ptr.empty
      addressGen.payload := ptr.pop.resized
      ptr.doPop := addressGen.fire

      val sync = !withAsyncRead generate new Area{
        assert(!useVec)
        val readArbitation = addressGen.m2sPipe(flush = io.flush)
        val readPort = ram.readSyncPort
        readPort.cmd := addressGen.toFlowFire
        io.pop << readArbitation.translateWith(readPort.rsp)

        val popReg = RegNextWhen(ptr.pop, readArbitation.fire) init(0)
        ptr.popOnIo := popReg
        when(io.flush){ popReg := 0 }
      }

      val async = withAsyncRead generate new Area{
        val readed = useVec match {
          case true => vec.read(addressGen.payload)
          case false => ram.readAsync(addressGen.payload)
        }
        io.pop << addressGen.translateWith(readed)
        ptr.popOnIo := ptr.pop

        if(withBypass){
          when(ptr.empty){
            io.pop.valid := io.push.valid
            io.pop.payload := io.push.payload
            ptr.doPush clearWhen(io.pop.ready)
          }
        }
      }
    }

    io.occupancy := ptr.occupancy
    if(!forFMax) io.availability := depth - ptr.occupancy
    val fmaxAvail = forFMax generate new CounterUpDownFmax(depth + 1, depth){
      incr := io.pop.fire
      decr := io.push.fire
      io.availability := value
    }
  }



  // check a condition against all valid payloads in the FIFO RAM
  def formalCheckRam(cond: T => Bool): Vec[Bool] = this rework new Composite(this){
    val condition = (0 until depth).map(x => cond(if (useVec) logic.vec(x) else logic.ram(x)))
    // create mask for all valid payloads in FIFO RAM
    // inclusive [popd_idx, push_idx) exclusive
    // assume FIFO RAM is full with valid payloads
    //           [ ...  push_idx ... ]
    //           [ ...  pop_idx  ... ]
    // mask      [ 1 1 1 1 1 1 1 1 1 ]
    val mask = Vec(True, depth)
    val push_idx = logic.ptr.push.resize(log2Up(depth))
    val pop_idx = logic.ptr.pop.resize(log2Up(depth))
    // pushMask(i)==0 indicates location i was popped
    val popMask = (~((U(1) << pop_idx) - 1)).asBits
    // pushMask(i)==1 indicates location i was pushed
    val pushMask = ((U(1) << push_idx) - 1).asBits
    // no wrap   [ ... popd_idx ... push_idx ... ]
    // popMask   [ 0 0 1 1 1 1  1 1 1 1 1 1 1 1 1]
    // pushpMask [ 1 1 1 1 1 1  1 1 0 0 0 0 0 0 0] &
    // mask      [ 0 0 1 1 1 1  1 1 0 0 0 0 0 0 0]
    when(pop_idx < push_idx) {
      mask.assignFromBits(pushMask & popMask)
      // wrapped   [ ... push_idx ... popd_idx ... ]
      // popMask   [ 0 0 0 0 0 0  0 0 1 1 1 1 1 1 1]
      // pushpMask [ 1 1 0 0 0 0  0 0 0 0 0 0 0 0 0] |
      // mask      [ 1 1 0 0 0 0  0 0 1 1 1 1 1 1 1]
    }.elsewhen(pop_idx > push_idx) {
      mask.assignFromBits(pushMask | popMask)
      // empty?
      //           [ ...  push_idx ... ]
      //           [ ...  pop_idx  ... ]
      // mask      [ 0 0 0 0 0 0 0 0 0 ]
    }.elsewhen(logic.ptr.empty) {
      mask := mask.getZero
    }
    val check = mask.zipWithIndex.map{case (x, id) => x & condition(id)}
    val vec = Vec(check)
  }.vec

  def formalCheckOutputStage(cond: T => Bool): Bool = this.rework {
    // only with sync RAM read, io.pop is directly connected to the m2sPipe() stage
    Bool(!withAsyncRead) & io.pop.valid & cond(io.pop.payload)
  }

  // verify this works, then we can simplify below
  //def formalCheck(cond: T => Bool): Vec[Bool] = this.rework {
  //  Vec(formalCheckOutputStage(cond) +: formalCheckRam(cond))
  //}

  def formalContains(word: T): Bool = this.rework {
    formalCheckRam(_ === word.pull()).reduce(_ || _) || formalCheckOutputStage(_ === word.pull())
  }
  def formalContains(cond: T => Bool): Bool = this.rework {
    formalCheckRam(cond).reduce(_ || _) || formalCheckOutputStage(cond)
  }

  def formalCount(word: T): UInt = this.rework {
    // occurance count in RAM and in m2sPipe()
    CountOne(formalCheckRam(_ === word.pull())) +^ U(formalCheckOutputStage(_ === word.pull()))
  }
  def formalCount(cond: T => Bool): UInt = this.rework {
    // occurance count in RAM and in m2sPipe()
    CountOne(formalCheckRam(cond)) +^ U(formalCheckOutputStage(cond))
  }

  def formalFullToEmpty() = this.rework {
    val was_full = RegInit(False) setWhen(!io.push.ready)
    cover(was_full && logic.ptr.empty)
  }
}

object StreamFifoLowLatency{
  def apply[T <: Data](dataType: T, depth: Int) = new StreamFifoLowLatency(dataType,depth)
}

class StreamFifoLowLatency[T <: Data](val dataType: HardType[T],val depth: Int,val latency : Int = 0, useVec : Boolean = false) extends Component {
  assert(latency == 0 || latency == 1)

  val io = new Bundle with StreamFifoInterface[T]{
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
    val flush = in Bool() default(False)
    val occupancy    = out UInt (log2Up(depth + 1) bits)
    val availability = out UInt (log2Up(depth + 1) bits)
    override def pushOccupancy = occupancy
    override def popOccupancy = occupancy
  }

  val fifo = new StreamFifo(
    dataType = dataType,
    depth = depth,
    withAsyncRead = true,
    withBypass = latency == 0,
    useVec = useVec
  )

  io.push <> fifo.io.push
  io.pop <> fifo.io.pop
  io.flush <> fifo.io.flush
  io.occupancy <> fifo.io.occupancy
  io.availability <> fifo.io.availability
}

object StreamFifoCC{
  def apply[T <: Data](dataType: HardType[T], depth: Int, pushClock: ClockDomain, popClock: ClockDomain) = new StreamFifoCC(dataType, depth, pushClock, popClock)
  def apply[T <: Data](push : Stream[T], pop : Stream[T], depth: Int, pushClock: ClockDomain, popClock: ClockDomain) = {
    val fifo = new StreamFifoCC(push.payloadType, depth, pushClock, popClock)
    fifo.io.push << push
    fifo.io.pop >> pop
    fifo
  }
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
                              val withPopBufferedReset : Boolean = ClockDomain.crossClockBufferPushToPopResetGen.get) extends Component {

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
    val popPtrPlus  = KeepAttribute(popPtr + 1)
    val popPtrGray  = toGray(popPtr)
    val pushPtrGray = BufferCC(pushToPopGray, B(0, ptrWidth bit))
    val addressGen = Stream(UInt(log2Up(depth) bits))
    val empty = isEmpty(popPtrGray, pushPtrGray)
    addressGen.valid := !empty
    addressGen.payload := popPtr.resized

    when(addressGen.fire){
      popPtr := popPtrPlus
    }

    val readArbitation = addressGen.m2sPipe()
    val readPort = ram.readSyncPort(clockCrossing = true)
    readPort.cmd := addressGen.toFlowFire
    io.pop << readArbitation.translateWith(readPort.rsp)

    val ptrToPush = RegNextWhen(popPtrGray, readArbitation.fire) init(0)
    val ptrToOccupancy = RegNextWhen(popPtr, readArbitation.fire) init(0)
    io.popOccupancy := (fromGray(pushPtrGray) - ptrToOccupancy).resized
  }

  pushToPopGray := pushCC.pushPtrGray
  popToPushGray := popCC.ptrToPush

  def formalAsserts(gclk: ClockDomain) = new Composite(this, "asserts") {
    import spinal.core.formal._
    val pushArea = new ClockingArea(pushClock) {
      when(pastValid & changed(pushCC.popPtrGray)) {
        assert(fromGray(pushCC.popPtrGray) - past(fromGray(pushCC.popPtrGray)) <= depth)
      }
      assert(pushCC.pushPtrGray === toGray(pushCC.pushPtr))
      assert(pushCC.pushPtr - fromGray(pushCC.popPtrGray) <= depth)
    }

    val popCheckClock = if (withPopBufferedReset) popClock.copy(reset = pushClock.isResetActive) else popClock
    val popArea = new ClockingArea(popCheckClock) {
      when(pastValid & changed(popCC.pushPtrGray)) {
        assert(fromGray(popCC.pushPtrGray) - past(fromGray(popCC.pushPtrGray)) <= depth)
      }
      assert(popCC.popPtrGray === toGray(popCC.popPtr))
      assert(fromGray(popCC.pushPtrGray) - popCC.popPtr <= depth)
      assert(popCC.popPtr === fromGray(popCC.ptrToPush) + io.pop.valid.asUInt)
    }

    val globalArea = new ClockingArea(gclk) {
      when(io.push.ready) { assert(pushCC.pushPtr - popCC.popPtr <= depth - 1) }
        .otherwise { assert(pushCC.pushPtr - popCC.popPtr <= depth) }
    }
  }
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
                                  withOutputBufferedReset : Boolean = ClockDomain.crossClockBufferPushToPopResetGen.get) extends Component {
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

    io.output << (if(withOutputBuffer) stream.m2sPipe(holdPayload = true, crossClockData = true) else stream)
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

//padding=true allow having the input output width modulo not being 0
//earlyLast=true add the hardware required to handle sizer where the last input transaction come before the fullness of the output buffer
//Return an area with an dataMask signal specifying which chunk of the output stream is loaded with data, when the output stream is valid. (outputWidth > inputWidth && earlyLast)
object StreamFragmentWidthAdapter {
  def apply[T <: Data,T2 <: Data](input : Stream[Fragment[T]],
                                  output : Stream[Fragment[T2]],
                                  endianness: Endianness = LITTLE,
                                  padding : Boolean = false,
                                  earlyLast : Boolean = false) = new Area{
    val inputWidth = widthOf(input.fragment)
    val outputWidth = widthOf(output.fragment)
    val dataMask = Bits((outputWidth+inputWidth-1)/inputWidth bits)
    if(inputWidth == outputWidth){
      output.arbitrationFrom(input)
      output.payload.assignFromBits(input.payload.asBits)
      dataMask.setAll()
    } else if(inputWidth > outputWidth) new Composite(input, "widthAdapter") {
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
      dataMask.setAll()
    } else new Composite(input, "widthAdapter"){
      require(outputWidth % inputWidth == 0 || padding)
      val factor  = (outputWidth + inputWidth - 1) / inputWidth
      val paddedOutputWidth = factor * inputWidth
      val counter = Counter(factor,inc = input.fire)
      val buffer  = Reg(Bits(paddedOutputWidth - inputWidth bits))
      val sendIt = CombInit(counter.willOverflowIfInc)
      output.valid := input.valid && sendIt
      output.last := input.last
      input.ready := output.ready || !sendIt

      if(earlyLast){
        sendIt setWhen(input.last)
        when(input.valid && input.last && output.ready) {
          counter.clear()
        }
      }

      val data = CombInit(input.fragment ## buffer)
      endianness match {
        case `LITTLE` => output.fragment.assignFromBits(data.resize(outputWidth))
        case `BIG`    => output.fragment.assignFromBits(data.subdivideIn(factor slices).reverse.asBits().resize(outputWidth))
      }

      earlyLast match {
        case false => {
          dataMask.setAll()
          when(input.fire) {
            buffer := input.fragment ## (buffer >> inputWidth)
          }
        }
        case true  => {
          endianness match {
            case `LITTLE` => for((bit, id) <- dataMask.asBools.zipWithIndex) bit := counter >= id
            case `BIG`    => for((bit, id) <- dataMask.asBools.reverse.zipWithIndex) bit := counter >= id
          }
          for((bit, id) <- dataMask.asBools.zipWithIndex) bit := counter >= id

          when(input.fire) {
            whenIndexed(buffer.subdivideIn(inputWidth bits), counter, relaxedWidth = true) {
              _ := input.fragment.asBits
            }
          }
          whenIndexed(data.subdivideIn(inputWidth bits).dropRight(1), counter, relaxedWidth = true) {
            _ := input.fragment.asBits
          }
        }
      }
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

    def formalAsserts() = new Composite(this, "asserts") {
      val startedReg = Reg(Bool()) init False
      when(io.targetFire & io.working) {
        startedReg := True
      }
      when(done) { startedReg := False }
      assert(startedReg === (counter.value > 0))

      when(!io.working) { assert(counter.value === 0) }
      assert(counter.value <= expected)
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
    
    def formalAsserts() = counter.formalAsserts()
}

object StreamUnpacker {

  /** Decomposes a Data field into a map of words to Word-relative range -> Field-relative range. The starting bit
    * is any absolute position within some set of words,
    *
    * For example, a word with 16 bits starting at bit 4 decomposed into 8 bit words would result in:
    * {
    *   0 -> ((4 to 7) -> (0 to 3)),
    *   1 -> ((0 to 7) -> (4 to 11)),
    *   2 -> ((0 to 3) -> (12 to 15))
    * }
    *
    * @param wordWidth Word width to decompose into it
    * @param field Data to decompose
    * @param startBit Bit to start at, as absolute position (may be greater than `wordWidth`)
    * @return Map of word index to Word-relative range -> Field-relative range
    */
  def decomposeField(field: Data, startBit: Int, wordWidth: Int): Map[Int, (Range, Range)] = {
    val lastBit = startBit + field.getBitsWidth - 1
    // Determine which words the field falls into
    val firstWord = startBit / wordWidth
    val lastWord = (field.getBitsWidth + startBit - 1) / wordWidth

    (firstWord to lastWord).map { wordInd =>
      // Make the current word's range
      val curWord = (wordInd * wordWidth) until ((wordInd + 1) * wordWidth)

      // Find the largest range of the field that fits into the word, in absolute bits
      // This is merely clipping the field first and last bits by the current word's min and max
      val absWordRange = startBit.max(curWord.min) to lastBit.min(curWord.max)

      // Find the range that the field's word-indexed range maps to in the field itself
      // Just back off the starting bit from the word-indexed range
      val relFieldRange = absWordRange.min - startBit to absWordRange.max - startBit

      // Convert the absolute word range into a relative one
      val relWordRange = absWordRange.min - curWord.min to absWordRange.max - curWord.min

      wordInd -> (relWordRange -> relFieldRange)
    }.toMap
  }

  /** Converts a layout of Data and starting bit pairs into a map of word range to Data range slices for each word
    * that the Data spans, indexed by each Data. The return type is a 2D map relating each Data to each word index.
    * The range pairs for each word index represent which bits of the word (local to the width of the word) map to the
    * bits of Data that lie within the word.
    *
    * @param wordWidth Width of the Stream's words
    * @param layout List of Data to starting bit pairs
    * @return Map of Data, Map of word index to word range, Data range pair
    */
  def layoutToWordMap(
      wordWidth: Int,
      layout: List[(Data, Int)]
  ): mutable.LinkedHashMap[Data, Map[Int, (Range, Range)]] = {
    layout.map { case (data, startBit) =>
      data -> decomposeField(data, startBit, wordWidth)
    }.toMapLinked
  }

  /** Unpacks a Stream given a layout of Data fields.
    * Field layout is accepted as pairs of Data and their start bits. Starting bits are interpreted as absolute bit
    * positions within a multi-word layout. The StreamUnpacker will read as many words from `input` as necessary to
    * unpack all fields. Fields that exceed a word width will be wrapped into as many subsequent words needed.
    *
    * @param input Stream to read from
    * @param layout List of Data fields and their start bits
    * @tparam T Stream Data type
    * @return Unpacker instance
    */
  def apply[T <: Data](input: Stream[T], layout: List[(Data, Int)]): StreamUnpacker[T] = {
    require(layout.nonEmpty)

    new StreamUnpacker[T](input, layoutToWordMap(input.payloadType.getBitsWidth, layout))
  }

  /** Unpacks a Stream into a given PackedBundle
    * The StreamUnpacker will read as many words from `input` as necessary to unpack all fields. Fields that exceed a
    * word width will be wrapped into as many subsequent words needed.
    *
    * @param input Stream to read from
    * @param packedbundle PackedBundle to unpack into
    * @tparam T Stream Data type
    * @tparam B PackedBundle type
    * @return Unpacker instance
    */
  def apply[T <: Data, B <: PackedBundle](input: Stream[T], packedbundle: B): StreamUnpacker[T] = {
    // Defer to the other `apply` method with a layout derived from the PackedBundle's mappings
    StreamUnpacker(
      input,
      packedbundle.mappings.map { case (range, data) =>
        data -> range.min
      }.toList
    )
  }
}

/** Unpacks `stream`'s words into the given `layout`'s Data.
  * `stream` is directly driven by this area.
  * `layout` Data are driven through a register.
  *
  * `io.start` starts unpacking
  * `io.dones` is set of bits indicating when the associated Data in `layout` is unpacked.
  * `io.allDone` indicates when the last word has been unpacked.
  *
  * Use the companion object `StreamUnpacker` to create an instance.
  */
class StreamUnpacker[T <: Data](
    stream: Stream[T],
    layout: mutable.LinkedHashMap[Data, Map[Int, (Range, Range)]]
) extends Area {

  val io = new Bundle {
    val start = Bool()
    val dones = Bits(layout.keys.size bits)
    val allDone = Bool()
  }

  private val fields = layout.keys.toList

  // Make output registers, as bits
  private val rData = fields.map { d =>
    val regData = Reg(cloneOf(d.asBits)) init B(0)
    d.assignFromBits(regData)
    regData
  }

  private val running = Reg(Bool()) init False
  private val dones = Reg(Bits(fields.length bits)) init B(0)
  private val allDone = Reg(Bool()) init False
  private val counter = Counter(layout.values.flatMap(_.keys).max + 1)

  private val inFlow = stream.takeWhen(running).toFlow

  when(io.start) {
    counter.clear()
    running := True
  }

  // Dones are only asserted for a single cycle
  dones.clearAll()
  allDone.clear()

  when(inFlow.valid & running) {
    counter.increment()

    // Latch any data in the current word
    layout.foreach { case (layoutData, wordMap) =>
      wordMap.foreach { case (wordInd, (wordRange, dataRange)) =>
        when(counter.value === wordInd) {
          rData(fields.indexOf(layoutData))(dataRange) := inFlow.payload.asBits(wordRange)
        }
      }

      // Flag done at the last word of the data
      dones(fields.indexOf(layoutData)).setWhen(counter.value === wordMap.keys.max)
    }

    when(counter.willOverflowIfInc) {
      running.clear()
      allDone.set()
    }
  }

  // Output mapping
  io.dones := dones
  io.allDone := allDone
}

object StreamPacker {

  /** Packs a given layout of Data fields into a Stream.
    * Field layout is accepted as pairs of Data and their start bits. Starting bits are interpreted as absolute bit
    * positions within a multi-word layout. The StreamPacker will write as many words to `output` as necessary to pack
    * all fields. Fields that exceed a word width will be wrapped into as many subsequent words needed.
    *
    * Note, no overlap checking is performed.
    *
    * @param output Stream to write to
    * @param layout List of Data fields and their start bits
    * @tparam T Stream Data type
    * @return StreamPacker instance
    */
  def apply[T <: Data](output: Stream[T], layout: List[(Data, Int)]): StreamPacker[T] = {
    require(layout.nonEmpty)

    new StreamPacker[T](output, StreamUnpacker.layoutToWordMap(output.payloadType.getBitsWidth, layout))
  }

  /** Packs a given PackedBundle into a Stream.
    * The StreamPacker will write as many words to `output` as necessary to pack
    * all fields. Fields that exceed a word width will be wrapped into as many subsequent words needed.
    *
    * Note, no overlap checking is performed.
    *
    * @param output Stream to write to
    * @param packedbundle PackedBundle to pack from
    * @tparam T Stream Data type
    * @tparam B PackedBundel type
    * @return StreamPacker instance
    */
  def apply[T <: Data, B <: PackedBundle](output: Stream[T], packedbundle: B): StreamPacker[T] = {
    // Defer to the other `apply` method with a layout derived from the PackedBundle's mappings
    StreamPacker(
      output,
      packedbundle.mappings.map { case (range, data) =>
        data -> range.min
      }.toList
    )
  }
}

/** Packs `layout`'s Data into the given `stream`
  *
  * `stream` is directly driven by this area.
  *
  * `layout` Data is read directly
  *
  * `io.start` indicates when to start packing. All `layout`'s Data is registered before packing.
  *
  * `io.done` indicates when the last word has been packed.
  *
  * Use the companion object `StreamPapcker` to create an instance.
  */
class StreamPacker[T <: Data](
    stream: Stream[T],
    layout: mutable.LinkedHashMap[Data, Map[Int, (Range, Range)]]
) extends Area {

  require(layout.nonEmpty)

  private val dataIn = layout.keys.toList

  val io = new Bundle {
    val start = Bool()
    val done = Bool()
  }

  private val counter = Counter(layout.values.flatMap(_.keys).max + 1)
  private val running = RegInit(False)

  private val outValid = RegInit(False)
  private val outDone = RegInit(False)
  private val nextWord = Reg(stream.payloadType)

  private val buffer = RegNextWhen(Vec(dataIn.map(_.asBits)), io.start)

  when(io.start) {
    running.set()
    counter.clear()
  }

  when(stream.fire) {
    outValid.clear()
    outDone.clear()
  }

  when(running && stream.isFree) {
    when(counter.willOverflowIfInc) {
      running.clear()
      outDone.set()
    } otherwise {
      counter.increment()
    }

    // Generate the word
    nextWord := nextWord.getZero
    outValid := True

    layout.foreach { case (layoutData, wordMap) =>
      wordMap.foreach { case (wordInd, (wordRange, dataRange)) =>
        when(counter.value === wordInd) {
          nextWord.assignFromBits(
            buffer(dataIn.indexOf(layoutData)).asBits(dataRange),
            wordRange.max,
            wordRange.min
          )
        }
      }
    }
  }

  // Connect the outputs
  stream.payload := nextWord
  stream.valid := outValid
  io.done := outDone
}
