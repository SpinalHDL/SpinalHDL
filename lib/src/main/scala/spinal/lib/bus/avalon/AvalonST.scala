package spinal.lib.bus.avalon

import spinal.core._
import spinal.lib._

case class AvalonSTConfig(dataWidth: Int,
                          useData: Boolean = true,
                          useChannels: Boolean = false,
                          channelWidth: Int = -1,
                          useError: Boolean = false,
                          errorWidth: Int = -1,
                          useReady: Boolean = true,
                          useValid: Boolean = true,
                          useEmpty: Boolean = false,
                          emptyWidth: Int = -1,
                          useEOP: Boolean = false,
                          useSOP: Boolean = false,
                          /* Meta data values */
                          maxChannels: Int = -1,
                          beatsPerCycle: Int = -1,
                          dataBitsPerSymbol: Int = -1,
                          emptyWithinPacket: Boolean = false,
                          errorDescriptor: Array[String] = Array(),
                          symbolEndianness: Endianness = LITTLE,
                          readyLatency: Int = 0,
                          readyAllowance: Int = 0) {
  assert(!(useReady || useValid), "AvalonST must have at least ready OR valid enabled!")
}

case class AvalonSTPayload(config: AvalonSTConfig) extends Bundle {
  val data: Bits    = if (config.useData) Bits(config.dataWidth bit) else null
  val channel: UInt = if (config.useChannels) UInt(config.channelWidth bit) else null
  val error: Bits   = if (config.useError) Bits(config.errorWidth bit) else null
  val empty: UInt   = if (config.useEmpty) UInt(config.emptyWidth bit) else null
  val eop: Bool     = if (config.useEOP) Bool() else null
  val sop: Bool     = if (config.useSOP) Bool() else null
}

case class AvalonST(config: AvalonSTConfig) extends Bundle with IMasterSlave {

  val ready: Bool = if (config.useReady) Bool() else null
  val valid: Bool = if (config.useValid) Bool() else null
  val payload: AvalonSTPayload = AvalonSTPayload(config)

  val latencyDelay: Bool = Delay(this.ready, config.readyLatency)
  val latencyReady: Bool = ready && latencyDelay
  val allowanceDelay: Bool = Delay(this.ready, config.readyAllowance)
  val allowanceReady: Bool = !ready && allowanceDelay

  // Logical ready accounting for ready latency and allowance
  val logicalReady: Bool = latencyReady || allowanceReady

  val fire: Bool = valid && logicalReady

  override def clone: AvalonST = AvalonST(config)

  override def asMaster(): Unit = {
    if (config.useReady) { in(this.ready) }
    if (config.useValid) { out(this.valid) }
    out(this.payload)
  }

  private def driveWeak[T <: Data](sink: T, src: T) = {
    if (sink != null && src != null) {
      sink := src
    } else if (sink != null || src != null) {
      LocatedPendingError(s"Can't drive ${sink} with ${src} because sink or source is null!")
    }
  }

  private def arbitrateFrom(that: AvalonST) = {
    driveWeak(this.ready, that.ready)
    driveWeak(that.valid, this.valid)
  }

  def <<(that: AvalonST): AvalonST = {
    arbitrateFrom(that)
    this.payload := that.payload

    this
  }

  def >>(that: AvalonST): AvalonST = {
    that << this
    that
  }

  def <-<(that: AvalonST): AvalonST = {
    this << that.stage()
    that
  }

  def >->(that: AvalonST): AvalonST = {
    that <-< this
    that
  }

  def </<(that: AvalonST): AvalonST = {
    this << that.m2sPipe()
    that
  }

  def >/>(that: AvalonST): AvalonST = {
    that </< this
    that
  }

  def </-<(that: AvalonST): AvalonST = {
    this << that.s2mPipe().m2sPipe()
    that
  }

  def >-/>(that: AvalonST): AvalonST = {
    that </-< this
    that
  }

  def stage(): AvalonST = this.m2sPipe()

  /**
   * Cuts the valid/payload data path with a register and increments required ready allowance
   */
  def m2sPipe(flush: Bool = null, holdPayload : Boolean = false): AvalonST = new Composite(this) {
      val m2sPipe = AvalonST(config.copy(readyAllowance = config.readyAllowance+1))

      val rPayload = RegNextWhen(self.payload, if (holdPayload && config.useValid) self.valid else True)

      config.useValid generate {
        val rValid = RegNext(self.valid) init(False)
        if (flush != null) rValid clearWhen(flush)
        m2sPipe.valid := rValid
      }

      m2sPipe.payload := rPayload

      driveWeak(self.ready, m2sPipe.ready)
    }.m2sPipe

  /**
   * Cuts the ready data path with a register and decrements the required ready latency
   */
  def s2mPipe(): AvalonST = {
    if (!config.useReady) {
      this
    } else {
      new Composite(this) {
        val s2mPipe = AvalonST(config.copy(readyLatency = 0.max(config.readyLatency-1)))

        val rReady = RegNext(s2mPipe.ready) init(False)

        self.ready := rReady

        driveWeak(self.valid, s2mPipe.valid)
        s2mPipe.payload := self.payload
      }.s2mPipe
    }
  }
}
