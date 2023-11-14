package spinal.lib

import spinal.core._
import spinal.core.fiber._
import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

object BufferCC {
  def apply[T <: Data](input: T, init: => T = null, bufferDepth: Option[Int] = None, randBoot : Boolean = false): T = {
    val c = new BufferCC(input, init, bufferDepth, randBoot)
    c.setCompositeName(input, "buffercc", true)
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

class BufferCC[T <: Data](val dataType: T, init :  => T, val bufferDepth: Option[Int], val  randBoot : Boolean = false) extends Component {
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
  for (i <- 1 until finalBufferDepth) {
    buffers(i) := buffers(i - 1)
    buffers(i).addTag(crossClockBuffer)
  }

  io.dataOut := buffers.last
}


object PulseCCByToggle {
  def apply(input: Bool,
            clockIn: ClockDomain,
            clockOut: ClockDomain): Bool = {
    val c = new PulseCCByToggle(clockIn,clockOut)
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
    val target = BufferCC(inArea.target, False)

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
   * @return Filtred Bool which is asynchronously asserted synchronously deaserted
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
    samplerCD(BufferCC(
      input       = (if(solvedOutputPolarity == HIGH) False else True) ^ inputSync,
      init        = (if(solvedOutputPolarity == HIGH) True  else False),
      bufferDepth = bufferDepth)
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
      ).setCompositeName(resetCd.reset, "syncronized", true)
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
      bufferDepth = None
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

  val reset = Bool()
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
      bufferDepth = None
    )
    buffer.io.dataIn := config.resetActiveLevel.deassertedBool

    reset := buffer.io.dataOut
  }
}