package spinal.lib.generator

import spinal.core._
import spinal.lib._
import spinal.core.fiber._
import spinal.lib.BufferCC
import spinal.lib.blackbox.xilinx.s7.BUFG

import scala.collection.mutable.ArrayBuffer


trait ResetSensitivity
object ResetSensitivity{
  object NONE extends ResetSensitivity
  object HIGH extends ResetSensitivity
  object LOW extends ResetSensitivity
  object RISE extends ResetSensitivity
  object FALL extends ResetSensitivity
}

abstract class ClockDomainResetGeneratorIf extends Area{
  def inputClockDomain = Handle[ClockDomain]
  def outputClockDomain = Handle[ClockDomain]
  def asyncReset(reset : Handle[Bool], sensitivity : ResetSensitivity)
  def relaxedReset(reset : Handle[Bool], sensitivity : ResetSensitivity)
}

case class ClockDomainResetGeneratorV2() extends ClockDomainResetGeneratorIf {
  override val inputClockDomain = Handle[ClockDomain]
  val holdDuration = Handle[Int]()
  val powerOnReset = Handle.sync(false)

  val outputClockDomainConfig = Handle(GlobalData.get.commonClockConfig)

  override val outputClockDomain = Handle(
    ClockDomain(
      clock = inputClockDomain.clock,
      reset = logic.outputReset,
      frequency = inputClockDomain.frequency,
      config = outputClockDomainConfig
    )
  )

  def enablePowerOnReset() = powerOnReset.load(true)
  override def asyncReset(reset: Handle[Bool], sensitivity: ResetSensitivity): Unit = {
    hardFork {
      logic.doAsyncReset setWhen (sensitivity match {
        case ResetSensitivity.HIGH => reset
        case ResetSensitivity.LOW => !reset
      })
    }
  }
  override def relaxedReset(reset: Handle[Bool], sensitivity: ResetSensitivity): Unit = {
    hardFork {
      logic.doHoldReset setWhen (sensitivity match {
        case ResetSensitivity.HIGH => reset
        case ResetSensitivity.LOW => !reset
      })
    }
  }

  def asyncReset(reset : Handle[ClockDomain], hold : Handle[ClockDomain]) : Unit = {
    reset.derivate(logic.doAsyncReset setWhen _.isResetActive)
    hold.derivate(logic.doHoldReset setWhen _.isResetActive)
  }
  def asyncReset(reset : ClockDomainResetGeneratorIf, hold : ClockDomainResetGeneratorIf) : Unit = {
    asyncReset(reset.inputClockDomain, hold.outputClockDomain)
  }

  def clockedFrom(input : Handle[ClockDomain]) : Unit = hardFork(
    inputClockDomain.load(input.withoutReset())
  )

  def makeExternal(frequency : IClockDomainFrequency = UnknownFrequency,
                   withResetPin : Boolean = true,
                   resetActiveLevel : Polarity = HIGH,
                   crossClockBufferDepth : Option[Int] = None): this.type = {
    hardFork{
      val clock = in Bool() setCompositeName(ClockDomainResetGeneratorV2.this, "external_clk")
      val reset = withResetPin generate (in Bool()  setCompositeName(ClockDomainResetGeneratorV2.this, "external_reset"))
      crossClockBufferDepth.foreach(v => clock.addTag(new CrossClockBufferDepth(v)))
      if(withResetPin) asyncReset(reset, resetActiveLevel match {
        case HIGH => ResetSensitivity.HIGH
        case LOW => ResetSensitivity.LOW
      })
      inputClockDomain.load(
        ClockDomain(
          clock = clock,
          reset = reset,
          frequency = frequency,
          config = ClockDomainConfig(
            resetKind = ASYNC,
            resetActiveLevel = resetActiveLevel
          )
        )
      )
    }

    this
  }

  val logic = Handle(new Area{
    val doAsyncReset = False
    val doHoldReset = False

    val powerOnLogic = powerOnReset.get generate new ClockingArea(inputClockDomain.withBootReset()){
      val resetCounter = Reg(UInt(4 bits)) init(0)
      val doIt = !resetCounter.msb
      when(doIt) {
        resetCounter := resetCounter + 1
      }
      doAsyncReset setWhen doIt
    }


    val duration = holdDuration.get
    val holdingLogic = (duration != 0) generate new ClockingArea(inputClockDomain.withoutReset().copy(
      reset = spinal.lib.ResetCtrl.asyncAssertSyncDeassert(
        doAsyncReset,
        inputClockDomain,
        inputPolarity = spinal.core.HIGH,
        outputPolarity = spinal.core.HIGH
      ),
      config = inputClockDomain.config.copy(resetKind = ASYNC, resetActiveLevel = HIGH)
    )){
      val resetCounter = Reg(UInt(log2Up(duration) + 1 bits)) init(0)
      val doIt = !resetCounter.msb

      when(doIt) {
        resetCounter := resetCounter + 1
      }

      val clear = inputClockDomain on BufferCC(doHoldReset)
      when(clear){
        resetCounter := 0
      }
    }

    val outputReset = spinal.lib.ResetCtrl.asyncAssertSyncDeassert(
      doAsyncReset,
      inputClockDomain,
      inputPolarity = spinal.core.HIGH,
      outputPolarity = outputClockDomainConfig.resetActiveLevel,
      inputSync = if(holdingLogic != null) holdingLogic.doIt else doHoldReset
    )
  })
}

case class ClockDomainResetGenerator() extends ClockDomainResetGeneratorIf {
  override val inputClockDomain = Handle[ClockDomain]
  val holdDuration = Handle[Int]
  val powerOnReset = Handle.sync(false)


  def setInput(input : Handle[ClockDomain]) = inputClockDomain.load(input)

  def setInput(input : Handle[ClockDomain], omitReset : Boolean) : Unit = hardFork(
    inputClockDomain.load(input.copy(reset = if(omitReset) null else input.reset))
  )

  def setInput(input : ClockDomainResetGenerator) = inputClockDomain.load(input.outputClockDomain)

  def setInput(clock : Bool,
               frequency : IClockDomainFrequency = UnknownFrequency,
               powerOnReset : Boolean = false) = inputClockDomain.load(
    ClockDomain(
      clock = clock,
      frequency = frequency,
      config = ClockDomainConfig(
        resetKind = BOOT
      )
    )
  )
  val outputClockDomainConfig = Handle(GlobalData.get.commonClockConfig)

  override val outputClockDomain = Handle(
    ClockDomain(
      clock = inputClockDomain.clock,
      reset = logic.outputReset,
      frequency = inputClockDomain.frequency,
      config = outputClockDomainConfig
//      config = ClockDomainConfig(
//        resetKind = spinal.core.SYNC
//      )
    )
  )

  val logic = Handle{
    new ClockingArea(
      inputClockDomain.copy(
        reset = null,
        config = inputClockDomain.config.copy(
          resetKind = if(GlobalData.get.config.device.supportBootResetKind)
            BOOT
          else
            inputClockDomain.config.resetKind
        )
      )
    ) {
      val inputResetTrigger = False
      val outputResetUnbuffered = False

      val inputResetAdapter = (inputClockDomain.reset != null) generate {
        val generator = ResetGenerator(ClockDomainResetGenerator.this)
        generator.reset.load(inputClockDomain.reset)
        generator.kind.load(inputClockDomain.config.resetKind)
        generator.sensitivity.load(inputClockDomain.config.resetActiveLevel match {
          case HIGH => ResetSensitivity.HIGH
          case LOW => ResetSensitivity.LOW
        })
        generator
      }

      //Keep reset active for a while
      val duration = holdDuration.get
      val noHold = (duration == 0) generate when(inputResetTrigger){
        outputResetUnbuffered := outputClockDomainConfig.resetAssertValue
      }
      val holdingLogic = (duration != 0) generate new Area{
        val resetCounter = Reg(UInt(log2Up(duration + 1) bits))

        when(resetCounter =/= duration) {
          resetCounter := resetCounter + 1
          outputResetUnbuffered := outputClockDomainConfig.resetAssertValue
        }
        when(inputResetTrigger) {
          resetCounter := 0
        }
      }

      //Create all reset used later in the design
      val outputReset = RegNext(outputResetUnbuffered)

      if(inputClockDomain.config.resetKind == BOOT || powerOnReset.get){
        outputReset init(outputClockDomainConfig.resetAssertValue)
        holdingLogic.resetCounter init(0)
      }
    }
  }

  case class ResetGenerator(dady : ClockDomainResetGenerator) extends Area{
    val reset = Handle[Bool]
    val kind = Handle[ResetKind]
    val sensitivity = Handle[ResetSensitivity]

    val stuff = Handle(new ClockingArea(dady.inputClockDomain){
      val syncTrigger = kind.get match {
        case SYNC => {
          RegNext(reset.get)
        }
        case ASYNC => {
          sensitivity.get match {
            case ResetSensitivity.NONE => ???
            case ResetSensitivity.HIGH => spinal.lib.ResetCtrl.asyncAssertSyncDeassert(reset.get, dady.inputClockDomain, inputPolarity = spinal.core.HIGH, spinal.core.HIGH)
            case ResetSensitivity.LOW => spinal.lib.ResetCtrl.asyncAssertSyncDeassert(reset.get, dady.inputClockDomain, inputPolarity = spinal.core.LOW, spinal.core.HIGH)
            case ResetSensitivity.RISE => BufferCC(reset.get).rise
            case ResetSensitivity.FALL => BufferCC(reset.get).fall
          }
        }
        case BOOT => ???
      }
      dady.logic.inputResetTrigger setWhen(syncTrigger)
    })
  }


  override def asyncReset(reset : Handle[Bool], sensitivity : ResetSensitivity) = {
    val generator = ResetGenerator(this)
    generator.reset.load(reset)
    generator.sensitivity.load(sensitivity)
    generator.kind.load(ASYNC)
    generator
  }


  override def relaxedReset(reset: Handle[Bool], sensitivity: ResetSensitivity): Unit = asyncReset(reset, sensitivity)

  def asyncReset(reset : Handle[ClockDomain]) : Handle[ResetGenerator] = reset.produce{
    val generator = ResetGenerator(this)
    generator.reset.load(reset.isResetActive)
    generator.sensitivity.load(ResetSensitivity.HIGH)
    generator.kind.load(ASYNC)
    generator
  }

  def asyncReset(reset : ClockDomainResetGenerator) : Handle[ResetGenerator] = asyncReset(reset.outputClockDomain)


  def makeExternal(frequency : IClockDomainFrequency = UnknownFrequency,
                   withResetPin : Boolean = true,
                   resetKind: ResetKind = ASYNC,
                   resetActiveLevel : Polarity = HIGH,
                   crossClockBufferDepth : Option[Int] = None): this.type = {
    hardFork{
      val clock = in Bool() setCompositeName(ClockDomainResetGenerator.this, "external_clk")
      val reset = withResetPin generate (in Bool()  setCompositeName(ClockDomainResetGenerator.this, "external_reset"))
      crossClockBufferDepth.foreach(v => clock.addTag(new CrossClockBufferDepth(v)))
      inputClockDomain.load(
        ClockDomain(
          clock = clock,
          reset = reset,
          frequency = frequency,
          config = ClockDomainConfig(
            resetKind = resetKind,
            resetActiveLevel = resetActiveLevel
          )
        )
      )
    }

    this
  }

  def enablePowerOnReset() = powerOnReset.load(true)
}


case class Arty7BufgGenerator() extends Area{
  val input = Handle[ClockDomain]
  val output = Handle{
    input.copy(
      clock = if(input.clock != null) BUFG.on(input.clock ) else input.clock ,
      reset = if(input.reset != null) BUFG.on(input.reset ) else input.reset ,
      clockEnable = if(input.clockEnable != null) BUFG.on(input.clockEnable ) else input.clockEnable ,
      softReset = if(input.softReset != null) BUFG.on(input.softReset ) else input.softReset
    )//.setSynchronousWith(input)
  }
}