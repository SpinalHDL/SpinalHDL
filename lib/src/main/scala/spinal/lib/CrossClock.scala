package spinal.lib

import spinal.core._
import spinal.core.fiber._
import spinal.core.internals.{Literal, PhaseContext, PhaseNetlist, UIntLiteral}
import spinal.core.sim.SimDataPimper

import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

object BufferCC {
  def apply[T <: Data](input: T, init: => T = null, bufferDepth: Option[Int] = None, randBoot : Boolean = false, inputAttributes: Seq[SpinalTag] = List(), allBufAttributes: Seq[SpinalTag] = List()): T = {
    val c = new BufferCC(input, init, bufferDepth, randBoot, inputAttributes, allBufAttributes)
    c.setCompositeName(input, "buffercc", true)
    // keep hierarchy for timing constraint generation
    c.io.dataIn := input

    val ret = cloneOf(c.io.dataOut)
    ret := c.io.dataOut
    return ret
  }

  def apply[T <: Data](input: T, init: => T, bufferDepth: Int, randBoot : Boolean): T =
    apply(input, init, Some(bufferDepth), randBoot)
  def apply[T <: Data](input: T, init: => T, bufferDepth: Int): T =
    apply(input, init, Some(bufferDepth))
  def apply[T <: Data](input: T, bufferDepth: Int, randBoot : Boolean): T =
    apply(input, null.asInstanceOf[T], Some(bufferDepth), randBoot)
  def apply[T <: Data](input: T, bufferDepth: Int): T =
    apply(input, null.asInstanceOf[T], Some(bufferDepth))

  def withTag[T <: Data](input: T): T =
    apply(input, null.asInstanceOf[T], inputAttributes = List(crossClockMaxDelay(1, true)))
  def withTag[T <: Data](input: T, init: => T): T =
    apply(input, init, inputAttributes = List(crossClockMaxDelay(1, true)))
  def falsePath[T <: Data](input: T): T =
    apply(input, null.asInstanceOf[T], inputAttributes = List(crossClockFalsePath()))

  val defaultDepth = ScopeProperty(2)
  def defaultDepthOptioned(cd : ClockDomain, option : Option[Int]) : Int = {
    option match {
      case Some(x) => return x
      case None =>
    }
    ClockDomain.current.getTags().foreach{
      case t : CrossClockBufferDepth => {
        return t.value
      }
      case _ =>
    }
    ClockDomain.current.clock.getTags().foreach{
      case t : CrossClockBufferDepth => {
        return t.value
      }
      case _ =>
    }
    return defaultDepth.get
  }
}

class BufferCC[T <: Data](val dataType: T,
                          init :  => T,
                          val bufferDepth: Option[Int],
                          val randBoot : Boolean = false,
                          val inputAttributes: Seq[SpinalTag] = List(),
                          val allBufAttributes: Seq[SpinalTag] = List()) extends Component {
  def getInit() : T = init
  val finalBufferDepth = BufferCC.defaultDepthOptioned(ClockDomain.current, bufferDepth)
  assert(finalBufferDepth >= 1)

  val io = new Bundle {
    val dataIn = in(cloneOf(dataType))
    val dataOut = out(cloneOf(dataType))
  }

  val buffers = Vec(Reg(dataType, init),finalBufferDepth)
  if(randBoot) buffers.foreach(_.randBoot())

  buffers(0) := io.dataIn
  buffers(0).addTag(crossClockDomain)
  inputAttributes.foreach(buffers(0).addTag)
  for (i <- 1 until finalBufferDepth) {
    buffers(i) := buffers(i - 1)
    buffers(i).addTag(crossClockBuffer)
  }
  buffers.map(b => allBufAttributes.foreach(b.addTag))

  io.dataOut := buffers.last

  addAttribute("keep_hierarchy", "TRUE")
}

class BufferCCBlackBox(width : Int,
                       initValue : BigInt,
                       depth : Int,
                       val randBoot : Boolean,
                       val inputAttributes: Seq[SpinalTag],
                       val allBufAttributes: Seq[SpinalTag]) extends BlackBox {
  val io = new Bundle {
    val clk = in Bool()
    val reset = in Bool()
    val dataIn = in(Bits(width bits))
    val dataOut = out(Bits(width bits))
  }

  addGenerics(
    "WIDTH" -> width,
    "DEPTH" -> depth
  )
  val withInit = initValue != null
  addGeneric("INIT_ENABLE", withInit)
  addGeneric("INIT_VALUE", B((withInit).mux(initValue, BigInt(0)), width bits))
  addGeneric("INIT_KIND", clockDomain.config.resetKind.getName())
  addGeneric("INIT_POLARITY", clockDomain.config.resetActiveLevel.getName())

  val cd = withInit.mux(clockDomain.get, parent.rework(clockDomain.get.copy(reset = False)))
  cd.on {
    mapCurrentClockDomain(
      io.clk,
      io.reset
    )
  }

  //Model
  val buffers = Vec(Reg(Bits(width bits), B(initValue)), depth)
  if(randBoot) buffers.foreach(_.randBoot())

  buffers(0) := io.dataIn
  buffers(0).addTag(crossClockDomain)
  inputAttributes.foreach(buffers(0).addTag)
  for (i <- 1 until depth) {
    buffers(i) := buffers(i - 1)
    buffers(i).addTag(crossClockBuffer)
  }
  buffers.map(b => allBufAttributes.foreach(b.addTag))

  io.dataOut := buffers.last
}


class PhaseBufferCCBB extends PhaseNetlist{
  override def impl(pc: PhaseContext): Unit = {
    pc.walkComponents {
      case c: BufferCC[Data] => {
        c.buffers.foreach(_.flattenForeach { s =>
          s.removeAssignments()
          s.removeStatement()
        })
        c.io.dataOut.removeAssignments()
        c.rework {
          val cd = ClockDomain.current
          val initBt = c.getInit()
          val initValue = (initBt != null).mux(Literal(initBt), BigInt(0))
          val bb = new BufferCCBlackBox(
            width = c.io.dataIn.getBitsWidth,
            initValue = initValue,
            depth = c.finalBufferDepth,
            randBoot = c.randBoot,
            inputAttributes = c.inputAttributes,
            allBufAttributes = c.allBufAttributes
          )
          bb.setName("cc")
          bb.io.dataIn := c.io.dataIn.asBits
          c.io.dataOut := bb.io.dataOut.as(c.io.dataOut)
        }
      }
      case _ =>
    }
  }
}

//SamplerCC works mostly like a BufferCC but integrate an additional Flow input with a register on the push clock domain
class SamplerCC[T <: Data](val dataType : HardType[T],
                           init : => T,
                           val depth : Int,
                           val pushCd : ClockDomain,
                           val popCd : ClockDomain,
                           val bufferDepth: Option[Int],
                           val randBoot : Boolean = false,
                           val inputAttributes: Seq[SpinalTag] = List(),
                           val allBufAttributes: Seq[SpinalTag] = List()) extends BlackBox {
  val io = new Bundle {
    val push = slave Flow(dataType)
    val pushReg = out(dataType)
    val popReg = out(dataType)
  }

  val cc = popCd on new BufferCC(
    dataType = dataType(),
    init = init,
    bufferDepth = bufferDepth,
    randBoot = randBoot,
    inputAttributes = inputAttributes,
    allBufAttributes = allBufAttributes
  )

  val push = new ClockingArea(pushCd){
    val reg = Reg(dataType)
    when(io.push.valid){
      reg := io.push.payload
    }

    cc.io.dataIn := reg
  }

  io.popReg := cc.io.dataOut
}




object PulseCCByToggle {
  def apply(input: Bool,
            clockIn: ClockDomain,
            clockOut: ClockDomain): Bool = {
    val c = new PulseCCByToggle(clockIn,clockOut).setCompositeName(input, "pulseCCByToggle")
    c.io.pulseIn := input
    return c.io.pulseOut
  }
}


class PulseCCByToggle(clockIn: ClockDomain, clockOut: ClockDomain, withOutputBufferedReset : Boolean = ClockDomain.crossClockBufferPushToPopResetGen.get) extends Component{
  val io = new Bundle{
    val pulseIn = in Bool()
    val pulseOut = out Bool()
  }

  val inArea = clockIn on new Area {
    val target = RegInit(False) toggleWhen(io.pulseIn)
  }


  val finalOutputClock = clockOut.withOptionalBufferedResetFrom(withOutputBufferedReset)(clockIn)
  val outArea = finalOutputClock on new Area {
    val target = BufferCC.withTag(inArea.target, False)

    io.pulseOut := target.edge(False)
  }
}


object ResetCtrl{
  /**
   *
   * @param input Input reset signal
   * @param clockDomain ClockDomain which will use the synchronised reset.
   * @param inputPolarity (HIGH/LOW)
   * @param outputPolarity (HIGH/LOW)
   * @param bufferDepth Number of register stages used to avoid metastability (default=2)
   * @param inputSync Active high, will set reset high in a sync way
   * @return Filtered Bool which is asynchronously asserted synchronously deasserted
   */
  def asyncAssertSyncDeassert(input : Bool,
                              clockDomain : ClockDomain,
                              inputPolarity : Polarity = HIGH,
                              outputPolarity : Polarity = null, //null => inferred from the clockDomain
                              bufferDepth : Option[Int] = None,
                              inputSync : Bool = False) : Bool = {
    val samplerCD = clockDomain.copy(
      reset = input,
      config = clockDomain.config.copy(
        resetKind = ASYNC,
        resetActiveLevel = inputPolarity
      )
    )

    val solvedOutputPolarity = if(outputPolarity == null) clockDomain.config.resetActiveLevel else outputPolarity
    val falsePathAttrs = List(crossClockFalsePath(Some(input), destType = TimingEndpointType.RESET))
    samplerCD(BufferCC(
      input       = ((if(solvedOutputPolarity == HIGH) False else True) ^ inputSync).setCompositeName(input, "asyncAssertSyncDeassert", true),
      init        = if(solvedOutputPolarity == HIGH) True  else False,
      bufferDepth = bufferDepth,
      allBufAttributes = falsePathAttrs)
    )
  }

  /**
   * As asyncAssertSyncDeassert but directly drive the clockDomain reset with the synchronised signal.
   */
  def asyncAssertSyncDeassertDrive(input : Bool,
                                   clockDomain : ClockDomain,
                                   inputPolarity : Polarity = HIGH,
                                   outputPolarity : Polarity = null, //null => inferred from the clockDomain
                                   bufferDepth : Option[Int] = None) : Unit = clockDomain.reset := asyncAssertSyncDeassert(
    input = input ,
    clockDomain = clockDomain ,
    inputPolarity = inputPolarity ,
    outputPolarity = outputPolarity ,
    bufferDepth = bufferDepth
  )

  //Return a new clockdomain which use all the properties of clockCd but use as reset source a syncronized value from resetCd
  def asyncAssertSyncDeassertCreateCd(resetCd : ClockDomain,
                                      clockCd : ClockDomain = ClockDomain.current,
                                      bufferDepth : Option[Int] = None) : ClockDomain = {
    clockCd.copy(
      clock = clockCd.clock,
      reset = ResetCtrl.asyncAssertSyncDeassert(
        input = resetCd.reset,
        clockDomain = clockCd,
        inputPolarity = resetCd.config.resetActiveLevel,
        outputPolarity = clockCd.config.resetActiveLevel,
        bufferDepth = bufferDepth
      ).setCompositeName(resetCd.reset, "synchronized", true)
    )
  }
}


case class ResetAggregatorSource(pin: Bool, sync: Boolean, pol: Polarity)
class ResetAggregator(sources: Seq[ResetAggregatorSource], pol: Polarity) extends Area {
  def asyncBuffer(s : ResetAggregatorSource) : BufferCC[Bool] = {
    val samplerCd = ClockDomain.current.copy(
      reset = s.pin,
      config = ClockDomain.current.config.copy(
        resetKind = ASYNC,
        resetActiveLevel = s.pol
      )
    )

    val cc = samplerCd on new BufferCC(
      dataType = Bool(),
      init = pol.assertedBool,
      bufferDepth = None,
      inputAttributes = List(crossClockFalsePath(Some(s.pin), destType = TimingEndpointType.RESET))
    )
    cc.io.dataIn := pol.deassertedBool
    cc
  }

  val asyncBuffers = for (s <- sources if !s.sync) yield asyncBuffer(s)
  val syncBuffers = for (s <- sources if s.sync) yield (if (s.pol == pol) s.pin else !s.pin)
  val resets = asyncBuffers.map(_.io.dataOut) ++ syncBuffers
  val reset = if (pol == HIGH) resets.orR else resets.norR
}

class ResetHolder(cycles: Int, pol: Polarity) extends Area {
  val counter = Reg(UInt(log2Up(cycles + 1) bits)) init (0)
  val reset = (counter =/= cycles) ^ pol.deassertedBool
  when(reset === pol.assertedBool) {
    counter := counter + 1
  }

  def doSyncReset(): Unit = {
    counter := 0
  }
}

class ResetCtrlFiber(val config: ClockDomainConfig = ClockDomain.defaultConfig) extends Area {
  val lock = spinal.core.fiber.Lock()

  val reset = Bool().simPublic()
  val cd = ClockDomain.current.copy(reset = reset, config = config)
  var holdCycles = 64
  var withBootReset = false

  val resets = ArrayBuffer[ResetAggregatorSource]()
  val syncRelaxedResets = ArrayBuffer[ResetAggregatorSource]()

  def addAsyncReset(pin: Bool, pol: Polarity): this.type = {
    resets += ResetAggregatorSource(pin, false, pol)
    this
  }

  def addSyncReset(pin: Bool, pol: Polarity): this.type = {
    resets += ResetAggregatorSource(pin, true, pol)
    this
  }

  def addSyncRelaxedReset(pin: Bool, pol: Polarity): this.type = {
    syncRelaxedResets += ResetAggregatorSource(pin, true, pol)
    this
  }

  def addAsyncReset(ctrl: ResetCtrlFiber): this.type = {
    addAsyncReset(ctrl.reset, ctrl.config.resetActiveLevel)
    this
  }
  def addReset(ctrl: ResetCtrlFiber): this.type = {
    addAsyncReset(ctrl.reset, ctrl.config.resetActiveLevel)
    this
  }

  def enableBootReset(): this.type = {
    withBootReset = true
    this
  }

  def createAsyncReset(pol: Polarity): Bool = {
    val pin = Bool()
    addAsyncReset(pin, pol)
    pin
  }

  def createSyncReset(pol: Polarity): Bool = {
    val pin = Bool()
    addSyncReset(pin, pol)
    pin
  }

  def createSyncRelaxedReset(pol: Polarity): Bool = {
    val pin = Bool()
    addSyncRelaxedReset(pin, pol)
    pin
  }

  val fiber = Fiber build new Area {
    lock.await()
    val ccd = ClockDomain.current

    val bootReset = withBootReset generate new Area {
      val cd = ccd.withBootReset()
      val reg = cd(RegNext(True) init (False))
      addSyncReset(reg, LOW)
    }

    val aggregator = new ResetAggregator(resets, config.resetActiveLevel)

    val holderCd = ClockDomain(
      clock = ccd.clock,
      reset = aggregator.reset,
      config = ccd.config.copy(
        resetKind = ASYNC,
        resetActiveLevel = config.resetActiveLevel
      )
    )
    val holder = holderCd on new ResetHolder(holdCycles, config.resetActiveLevel)
    if (syncRelaxedResets.nonEmpty) holderCd on {
      when(syncRelaxedResets.map(e => e.pin === e.pol.assertedBool).orR) {
        holder.doSyncReset()
      }
    }

    val bufferCd = ClockDomain(
      clock = ccd.clock,
      reset = holder.reset,
      config = ccd.config.copy(
        resetKind = ASYNC,
        resetActiveLevel = config.resetActiveLevel
      )
    )
    val buffer = bufferCd on new BufferCC(
      dataType = Bool(),
      init = config.resetActiveLevel.assertedBool,
      bufferDepth = None,
      inputAttributes = List(crossClockFalsePath(Some(holder.reset), destType = TimingEndpointType.RESET))
    )
    buffer.io.dataIn := config.resetActiveLevel.deassertedBool

    reset := buffer.io.dataOut
  }
}
