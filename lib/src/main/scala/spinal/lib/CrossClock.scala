package spinal.lib

import spinal.core._


object BufferCC {
  def apply[T <: Data](input: T, init: => T = null, bufferDepth: Int = defaultDepth.get, randBoot : Boolean = false): T = {
    val c = new BufferCC(input, init, bufferDepth, randBoot)
    c.setCompositeName(input, "buffercc", true)
    c.io.dataIn := input

    val ret = cloneOf(c.io.dataOut)
    ret := c.io.dataOut
    return ret
  }

  val defaultDepth = ScopeProperty(2)
}

class BufferCC[T <: Data](dataType: T, init :  => T, bufferDepth: Int = BufferCC.defaultDepth.get, randBoot : Boolean = false) extends Component {
  assert(bufferDepth >= 1)

  val io = new Bundle {
    val dataIn = in(cloneOf(dataType))
    val dataOut = out(cloneOf(dataType))
  }

  val buffers = Vec(Reg(dataType, init),bufferDepth)
  if(randBoot) buffers.foreach(_.randBoot())

  buffers(0) := io.dataIn
  buffers(0).addTag(crossClockDomain)
  for (i <- 1 until bufferDepth) {
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


class PulseCCByToggle(clockIn: ClockDomain, clockOut: ClockDomain, withOutputBufferedReset : Boolean = true) extends Component{
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
   * @return Filtred Bool which is asynchronously asserted synchronously deaserted
   */
  def asyncAssertSyncDeassert(input : Bool,
                              clockDomain : ClockDomain,
                              inputPolarity : Polarity = HIGH,
                              outputPolarity : Polarity = null, //null => inferred from the clockDomain
                              bufferDepth : Int = BufferCC.defaultDepth.get) : Bool = {
    val samplerCD = clockDomain.copy(
      reset = input,
      config = clockDomain.config.copy(
        resetKind = ASYNC,
        resetActiveLevel = inputPolarity
      )
    )

    val solvedOutputPolarity = if(outputPolarity == null) clockDomain.config.resetActiveLevel else outputPolarity
    samplerCD(BufferCC(
      input       = if(solvedOutputPolarity == HIGH) False else True,
      init        = if(solvedOutputPolarity == HIGH) True  else False,
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
                                   bufferDepth : Int = BufferCC.defaultDepth.get) : Unit = clockDomain.reset := asyncAssertSyncDeassert(
    input = input ,
    clockDomain = clockDomain ,
    inputPolarity = inputPolarity ,
    outputPolarity = outputPolarity ,
    bufferDepth = bufferDepth
  )

  //Return a new clockdomain which use all the properties of clockCd but use as reset source a syncronized value from resetCd
  def asyncAssertSyncDeassertCreateCd(resetCd : ClockDomain,
                                      clockCd : ClockDomain = ClockDomain.current,
                                      bufferDepth : Int = BufferCC.defaultDepth.get) : ClockDomain = {
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
