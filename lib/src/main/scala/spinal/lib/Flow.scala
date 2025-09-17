package spinal.lib

import spinal.core._

import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer


class FlowFactory extends MSFactory{
  object Fragment extends FlowFragmentFactory

  def apply[T <: Data](payloadType: HardType[T]) = {
    val ret = new Flow(payloadType)
    postApply(ret)
    ret
  }

  def apply[T <: Data](payloadType: => T) : Flow[T] = apply(HardType(payloadType))
}

object Flow extends FlowFactory

class Flow[T <: Data](val payloadType: HardType[T]) extends Bundle with IMasterSlave with DataCarrier[T]{
  val valid = Bool()
  val payload : T = payloadType()

  override def clone: Flow[T] = Flow(payloadType).asInstanceOf[this.type]

  override def asMaster(): Unit = out(this)
  override def asSlave() : Unit  = in(this)


  override def freeRun(): this.type = this
  def setIdle(): this.type = {
    valid := False
    payload.assignDontCare()
    this
  }


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

  def combStage() : Flow[T] = {
    val ret = Flow(payloadType).setCompositeName(this, "combStage", true)
    ret << this
    ret
  }
  def swapPayload[T2 <: Data](that: HardType[T2]) = {
    val next = new Flow(that).setCompositeName(this, "swap", true)
    next.valid := this.valid
    next
  }


  def toStream  : Stream[T] = toStream(null)
  def toStream(overflow : Bool) : Stream[T] = {
    val ret = Stream(payloadType).setCompositeName(this, "toStream", true)
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

  def ccToggle(pushClock: ClockDomain,
               popClock: ClockDomain,
               withOutputBufferedReset : Boolean = ClockDomain.crossClockBufferPushToPopResetGen.get,
               withOutputM2sPipe : Boolean = true) : Flow[T] = {
    val cc = new FlowCCByToggle(payloadType, pushClock, popClock, withOutputBufferedReset=withOutputBufferedReset, withOutputM2sPipe=withOutputM2sPipe).setCompositeName(this,"ccToggle", true)
    cc.io.input << this
    cc.io.output
  }

  def connectFrom(that: Flow[T]): Flow[T] = {
    valid := that.valid
    payload := that.payload
    that
  }

  def takeWhen(cond: Bool): Flow[T] = {
    val next = new Flow(payloadType).setCompositeName(this, "takeWhen", true)
    next.valid := this.valid && cond
    next.payload := this.payload
    return next
  }

  /**
   * Discard transactions when cond is true.
   *
   * This is the same as throwWhen() but with a semantically clearer function name.
   * Prefer discardWhen() over throwWhen() for new designs.
   *
   * @param cond Condition
   *
   * @return The resulting Flow
   */
  def discardWhen(cond: Bool): Flow[T] = {
    this throwWhen(cond)
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

  def translateFrom[T2 <: Data](that: Flow[T2])(dataAssignment: (T, that.payload.type) => Unit): Flow[T] = {
    this.valid := that.valid
    dataAssignment(this.payload, that.payload)
    this
  }

  def ~[T2 <: Data](that: T2): Flow[T2] = translateWith(that)
  def ~~[T2 <: Data](translate: (T) => T2): Flow[T2] = map(translate)
  def map[T2 <: Data](translate: (T) => T2): Flow[T2] = (this ~ translate(this.payload)).setCompositeName(this, "map", true)

  def m2sPipe : Flow[T] = m2sPipe()
  def m2sPipe(holdPayload : Boolean = false,
              flush : Bool = null,
              crossClockData : Boolean = false): Flow[T] = {
    if(!holdPayload) {
      val ret = RegNext(this)
      ret.valid.init(False)
      if(flush != null) when(flush){ ret.valid := False }
      if(crossClockData) {
        ret.payload.addTag(crossClockDomain)
        ret.addTag(crossClockMaxDelay(1, useTargetClock = true))
      }
      ret
    } else {
      val ret = Reg(this)
      ret.valid.init(False)
      ret.valid := this.valid
      when(this.valid){
        ret.payload := this.payload
      }
      if(flush != null) when(flush){ ret.valid := False }
      if(crossClockData) {
        ret.payload.addTag(crossClockDomain)
        ret.addTag(crossClockMaxDelay(1, useTargetClock = true))
      }
      ret
    }.setCompositeName(this, "m2sPipe", true)
  }

  def stage() : Flow[T] = this.m2sPipe().setCompositeName(this, "stage", true)

  /**
   * Delay the flow by a given number of cycles
   * @param cycleCount Number of cycles to delay the flow
   * @return Delayed flow
   */
  def delay(cycleCount : Int) : Flow[T] = {
    cycleCount match {
      case 0 => this
      case _ => this.stage().delay(cycleCount - 1)
    }
  }

  def push(that : T): Unit ={
    valid := True
    payload := that
  }

  def default(that : T): Unit ={
    valid := False
    payload := that
  }

  def queueWithOccupancy(size: Int): (Stream[T], UInt) = {
    val fifo = new StreamFifo(payloadType, size).setCompositeName(this,"queueWithOccupancy", true)
    fifo.io.push << this.toStream
    fifo.io.push.ready.allowPruning()
    return (fifo.io.pop, fifo.io.occupancy)
  }

  def queueWithAvailability(size: Int): (Stream[T], UInt) = {
    val fifo = new StreamFifo(payloadType, size).setCompositeName(this,"queueWithAvailability", true)
    fifo.io.push << this.toStream
    fifo.io.push.ready.allowPruning()
    return (fifo.io.pop, fifo.io.availability)
  }
}

/** Create a new Flow that is always valid, with a given payload */
object ValidFlow {
  def apply[T <: Data](payload: T): Flow[T] = {
    val flow = Flow(payload)
    flow.push(payload)
    flow
  }
}

object RegFlow{
  def apply[T <: Data](dataType : T) : Flow[T] = {
    val reg = Reg(Flow(dataType))
    reg.valid init(False)
    reg
  }
}

@deprecated("Renamed to 'UnsafeFlowCCByToggle' because it's not safe in many cases, see its comments")
object FlowCCByToggle {
  def apply[T <: Data](input: Flow[T],
                       inputClock: ClockDomain = ClockDomain.current,
                       outputClock: ClockDomain = ClockDomain.current,
                       withOutputBufferedReset: Boolean = ClockDomain.crossClockBufferPushToPopResetGen.get,
                       withOutputM2sPipe: Boolean = true): Flow[T] = FlowCCUnsafeByToggle(input, inputClock, outputClock, withOutputBufferedReset, withOutputM2sPipe)
}

@deprecated("Renamed to 'UnsafeFlowCCByToggle' because it's not safe in many cases, see its comments")
class FlowCCByToggle[T <: Data](dataType: HardType[T],
                                inputClock: ClockDomain,
                                outputClock: ClockDomain,
                                withOutputBufferedReset: Boolean = ClockDomain.crossClockBufferPushToPopResetGen.get,
                                withOutputM2sPipe: Boolean = true) extends FlowCCUnsafeByToggle(dataType, inputClock, outputClock, withOutputBufferedReset, withOutputM2sPipe)

object FlowCCUnsafeByToggle {
  /** CDC for a Flow
    *
    * This component is not a safe CDC structure for the general use-case.
    * The transmitting side MUST be slower then the receiving side e.g. if:
    * - transmitting clock is appropriately faster than the receiving one
    * - datarate from the transmitter is slow enough
    * - the component is used as part of a larger CDC component with handshaking
    *
    * Do NOT use this component if you are unsure how it works or whether it is
    * applicable.
    * A safe alternative (that also shows data loss is):
    * <pre>
    * val overflow = Bool()
    * val crossed = myFlow.toStream(overflow).ccToggle(inputClock, outputClock).toFlow
    * </pre>
    */
  def apply[T <: Data](input: Flow[T],
                       inputClock: ClockDomain = ClockDomain.current,
                       outputClock: ClockDomain = ClockDomain.current,
                       withOutputBufferedReset : Boolean = ClockDomain.crossClockBufferPushToPopResetGen.get,
                       withOutputM2sPipe : Boolean = true): Flow[T] = {
    val c = new FlowCCUnsafeByToggle[T](input.payload, inputClock, outputClock, withOutputBufferedReset, withOutputM2sPipe)
    c.io.input connectFrom input
    c.io.output
  }
}

/** CDC for a Flow, NOT SAFE for general use
  * @see UnsafeFlowCCByToggle.apply
  */
class FlowCCUnsafeByToggle[T <: Data](dataType: HardType[T],
                                inputClock: ClockDomain,
                                outputClock: ClockDomain,
                                withOutputBufferedReset : Boolean = ClockDomain.crossClockBufferPushToPopResetGen.get,
                                withOutputM2sPipe : Boolean = true) extends Component {
  val io = new Bundle {
    val input = slave  Flow (dataType)
    val output = master Flow (dataType)
  }

  val finalOutputClock = outputClock.withOptionalBufferedResetFrom(withOutputBufferedReset && inputClock.hasResetSignal)(inputClock)
  val doInit = inputClock.canInit && finalOutputClock.canInit

  val inputArea = new ClockingArea(inputClock) {
    val target = Reg(Bool())
    val data = Reg(io.input.payload)
    when(io.input.valid) {
      target := !target
      data := io.input.payload
    }
  }

  val outputArea = new ClockingArea(finalOutputClock) {
    val target = BufferCC(inputArea.target, doInit generate False, randBoot = !doInit, inputAttributes = Seq(crossClockMaxDelay(1, useTargetClock = true)))
    val hit = RegNext(target).addTag(noInit)

    val flow = cloneOf(io.input)
    flow.valid := (target =/= hit)
    flow.payload := inputArea.data

    io.output << (if(withOutputM2sPipe) flow.m2sPipe(holdPayload = true, crossClockData = true) else flow)
  }


  if(doInit){
    inputArea.target init(False)
    outputArea.hit init(False)
  }else{
    inputArea.target.randBoot()
    outputArea.hit.randBoot()
  }
}

object FlowArbiter{
  def apply[T <: Data](inputs : Seq[Flow[T]], output : Flow[T]) = {
    assert(OH.isLegal(inputs.map(_.valid).asBits))
    output.valid := inputs.map(_.valid).orR
    output.payload := OHMux.or(inputs.map(_.valid).toIndexedSeq, inputs.map(_.payload), true)
  }
}

class FlowArbiterBuilder[T <: Data](output : Flow[T]){
  val inputs = ArrayBuffer[Flow[T]]()
  def create() = inputs.addRet(cloneOf(output))
  def build() = FlowArbiter(inputs, output)
}
