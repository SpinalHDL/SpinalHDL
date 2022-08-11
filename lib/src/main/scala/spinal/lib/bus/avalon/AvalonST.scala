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
  assert(useReady || useValid, "AvalonST must have at least ready OR valid enabled!")
}

case class AvalonSTPayload(config: AvalonSTConfig) extends Bundle {
  val data: Bits    = config.useData generate Bits(config.dataWidth bit)
  val channel: UInt = config.useChannels generate UInt(config.channelWidth bit)
  val error: Bits   = config.useError generate Bits(config.errorWidth bit)
  val empty: UInt   = config.useEmpty generate UInt(config.emptyWidth bit)
  val eop: Bool     = config.useEOP generate Bool()
  val sop: Bool     = config.useSOP generate Bool()
}

case class AvalonST(config: AvalonSTConfig) extends Bundle with IMasterSlave {

  val ready: Bool = config.useReady generate Bool()
  val valid: Bool = config.useValid generate Bool()
  val payload: AvalonSTPayload = AvalonSTPayload(config)

  def latencyDelay: Bool = signalCache.apply(this, "latencyDelay") {
    Delay(this.ready, config.readyLatency)
  }
  def allowanceDelay: Bool = signalCache.apply(this, "allowanceDelay") {
    Delay(latencyDelay, config.readyAllowance)
  }

  // Logical ready accounting for ready latency and allowance
  def logicalReady: Bool = latencyDelay || allowanceDelay

  def fire: Bool = valid && logicalReady

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
    driveWeak(that.ready, this.ready)
    driveWeak(this.valid, that.valid)
  }

  def <<(that: AvalonST): AvalonST = {
    assert(that.config.readyAllowance <= config.readyAllowance, "Sink stream ready allowance must be <= than that of the source stream")
    assert(that.config.readyLatency >= config.readyLatency, "Sink stream ready latency must be >= than that of the source stream")
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

  def pipelined(m2s: Boolean,
                s2m: Boolean): AvalonST = {
    (m2s, s2m) match {
      case (false, false) => this.combStage()
      case (true, false) => this.m2sPipe()
      case (false, true) => this.s2mPipe()
      case (true, true) => this.s2mPipe().m2sPipe()
    }
  }

  def stage(): AvalonST = this.m2sPipe()

  def combStage(): AvalonST = {
    val ret = AvalonST(config).setCompositeName(this, "combStage", true)
    ret << this
    ret
  }

  /**
   * Cuts the valid/payload data path with a register
   */
  def m2sPipe(flush: Bool = null, holdPayload : Boolean = false): AvalonST = new Composite(this) {
      val m2sPipe = AvalonST(config)

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

        driveWeak(s2mPipe.valid, self.valid)
        s2mPipe.payload := self.payload
      }.s2mPipe
    }
  }
}

object AvalonSTDelayAdapter {
  def apply(inStream: AvalonST, outStream: AvalonST, depth: Int = 31): AvalonSTDelayAdapter = {
    val adapter = new AvalonSTDelayAdapter(inStream.config,
      outStream.config.readyLatency,
      outStream.config.readyAllowance,
      depth = depth)
    adapter.io.s << inStream
    adapter.io.m >> outStream
    adapter
  }
}

class AvalonSTDelayAdapter(config: AvalonSTConfig,
                           newReadyLatency: Int,
                           newReadyAllowance: Int,
                           depth: Int = 31) extends Component {
  val io = new Bundle {
    val m = master(AvalonST(config.copy(readyLatency = newReadyLatency, readyAllowance = newReadyAllowance)))
    val s = slave(AvalonST(config))
  }

  val fifo = new StreamFifo(AvalonSTPayload(config), depth)

  fifo.io.push.payload := io.s.payload
  fifo.io.push.valid := io.s.valid && Delay(io.s.ready, config.readyLatency)
  val nearFull = fifo.io.occupancy < depth-config.readyAllowance
  io.s.ready := fifo.io.push.ready && nearFull

  io.m.payload := fifo.io.pop.payload
  io.m.valid := fifo.io.pop.valid
  fifo.io.pop.ready := io.m.logicalReady
}
