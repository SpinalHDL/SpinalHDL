package spinal.lib.generator_backup

import spinal.core._
import spinal.lib.BufferCC
import spinal.lib.blackbox.xilinx.s7.BUFG


trait ResetSensitivity
object ResetSensitivity{
  object NONE extends ResetSensitivity
  object HIGH extends ResetSensitivity
  object LOW extends ResetSensitivity
  object RISE extends ResetSensitivity
  object FALL extends ResetSensitivity
}

case class ClockDomainResetGenerator() extends Generator {
  noClockDomain()

  val inputClockDomain = createDependency[ClockDomain]
  val holdDuration = createDependency[Int]
  val powerOnReset = createDependency[Boolean]
  powerOnReset.load(false)

  def setInput(input : Handle[ClockDomain]) = inputClockDomain.merge(input)

  def setInput(input : Handle[ClockDomain], omitReset : Boolean) : Unit = input.produce(
    inputClockDomain.load(input.get.copy(reset = if(omitReset) null else input.reset))
  )

  def setInput(input : ClockDomainResetGenerator) = inputClockDomain.merge(input.outputClockDomain)

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

  val outputClockDomain = produce(
    ClockDomain(
      clock = inputClockDomain.clock,
      reset = logic.outputReset,
      frequency = inputClockDomain.frequency,
      config = ClockDomainConfig(
        resetKind = spinal.core.SYNC
      )
    )
  )

  val logic = add task new ClockingArea(inputClockDomain.copy(reset = null, config = inputClockDomain.config.copy(resetKind = BOOT))) {
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
    val noHold = (duration == 0) generate outputResetUnbuffered.setWhen(inputResetTrigger)
    val holdingLogic = (duration != 0) generate new Area{
      val resetCounter = Reg(UInt(log2Up(duration + 1) bits))

      when(resetCounter =/= duration) {
        resetCounter := resetCounter + 1
        outputResetUnbuffered := True
      }
      when(inputResetTrigger) {
        resetCounter := 0
      }
    }

    //Create all reset used later in the design
    val outputReset = RegNext(outputResetUnbuffered)

    if(inputClockDomain.config.resetKind == BOOT || powerOnReset.get){
      outputReset init(True)
      holdingLogic.resetCounter init(0)
    }
  }

  case class ResetGenerator(dady : ClockDomainResetGenerator) extends Generator{
    val reset = createDependency[Bool]
    val kind = createDependency[ResetKind]
    val sensitivity = createDependency[ResetSensitivity]
    dependencies += dady.logic

    val stuff = add task new ClockingArea(dady.inputClockDomain){
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
    }
  }


  def asyncReset(reset : Handle[Bool], sensitivity : ResetSensitivity) = {
    val generator = ResetGenerator(this)
    generator.reset.merge(reset)
    generator.sensitivity.load(sensitivity)
    generator.kind.load(ASYNC)
    generator
  }

  def asyncReset(reset : Handle[ClockDomain]) : Handle[ResetGenerator] = reset.produce{
    val generator = ResetGenerator(this)
    generator.reset.load(reset.isResetActive)
    generator.sensitivity.load(ResetSensitivity.HIGH)
    generator.kind.load(ASYNC)
    generator
  }

  def asyncReset(reset : ClockDomainResetGenerator) : Handle[ResetGenerator] = asyncReset(reset.outputClockDomain)


  def makeExternal(frequency : IClockDomainFrequency = UnknownFrequency, withResetPin : Boolean = true): this.type = {
    this(Dependable(){
      val clock = in Bool() setCompositeName(ClockDomainResetGenerator.this, "external_clk")
      val reset = withResetPin generate (in Bool()  setCompositeName(ClockDomainResetGenerator.this, "external_reset"))

      inputClockDomain.load(
        ClockDomain(
          clock = clock,
          reset = reset,
          frequency = frequency,
          config = ClockDomainConfig(
            resetKind = ASYNC,
            resetActiveLevel = HIGH
          )
        )
      )
    })

    this
  }

  def enablePowerOnReset() = powerOnReset.load(true)
}


case class Arty7BufgGenerator() extends Generator{
  val input = createDependency[ClockDomain]
  val output = produce{
    input.copy(
      clock = if(input.clock != null) BUFG.on(input.clock ) else input.clock ,
      reset = if(input.reset != null) BUFG.on(input.reset ) else input.reset ,
      clockEnable = if(input.clockEnable != null) BUFG.on(input.clockEnable ) else input.clockEnable ,
      softReset = if(input.softReset != null) BUFG.on(input.softReset ) else input.softReset
    )//.setSynchronousWith(input)
  }
}