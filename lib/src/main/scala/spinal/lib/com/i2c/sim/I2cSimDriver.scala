package spinal.lib.com.i2c.sim

import spinal.core.sim._
import spinal.core._
import spinal.lib.io.ReadableOpenDrain

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * One soft (simulation-side) endpoint of an open-drain wire. Writing a level
  * re-evaluates the interconnect immediately; `read()` returns the ANDed level.
  */
class OpenDrainSoftConnection(interconnect: OpenDrainInterconnect) {
  var value = true

  def write(value: Boolean): Unit = {
    if (this.value != value) {
      this.value = value
      interconnect.evaluate()
    }
  }

  def read() = interconnect.value
}

/**
  * Simulated open-drain wire: several soft endpoints and hard DUT pins
  * AND together. `value` is the current wire level (true = High/released).
  *
  * Level changes always flow through [[evaluate]], which also notifies
  * `onChange` (a single consumer per interconnect — the [[I2cSimDriver]]).
  */
class OpenDrainInterconnect {
  val softConnections = ArrayBuffer[OpenDrainSoftConnection]()
  val hardWriters = ArrayBuffer[Bool]()
  val hardReaders = ArrayBuffer[Bool]()
  var value = true
  /** Called with the new level on every actual change (raw, unfiltered). */
  var onChange: (Boolean => Unit) = _

  def newSoftConnection(): OpenDrainSoftConnection = {
    val endpoint = new OpenDrainSoftConnection(this)
    softConnections.append(endpoint)
    endpoint
  }

  private def pinWatcher(driver: Bool): Unit = {
    sim.forkSensitive {
      if (driver.toBoolean != value) {
        evaluate()
      }
    }
  }

  /** Master-role DUT pin: drives Low via `.write`, reads the wire via `.read`. */
  def addHardMaster(pin: ReadableOpenDrain[Bool]): Unit = {
    addHardDriver(pin.write)
    addHardReader(pin.read)
  }

  /** Slave-role DUT pin: drives Low via `.read`, reads the wire via `.write`. */
  def addHardSlave(pin: ReadableOpenDrain[Bool]): Unit = {
    addHardDriver(pin.read)
    addHardReader(pin.write)
  }

  /** Master convention (kept for compatibility). */
  def addHard(pin: ReadableOpenDrain[Bool]): Unit = addHardMaster(pin)

  def addHardDriver(driver: Bool): Unit = {
    hardWriters += driver
    pinWatcher(driver)
  }

  def addHardReader(reader: Bool): Unit = {
    hardReaders += reader
    reader #= value
  }

  /** Re-AND all drivers and publish the level to the hard readers. */
  def evaluate(): Unit = {
    var newValue = true
    for (soft <- softConnections) newValue &= soft.value
    for (hard <- hardWriters) newValue &= hard.toBoolean
    if (newValue != value) {
      value = newValue
      for (reader <- hardReaders) reader #= newValue
      if (onChange != null) onChange(newValue)
    }
  }
}

/** I2C bus events as decoded by the soft host. */
sealed trait I2cSimEvent
object I2cSimEvent {
  case object Start extends I2cSimEvent
  case object Stop extends I2cSimEvent
  case object Ack extends I2cSimEvent
  case object Nack extends I2cSimEvent
  case class Data(value: Int) extends I2cSimEvent
}

/**
  * Soft-host I2C master / bus oracle on the [[OpenDrainInterconnect]] wires
  * (UM10204 conformance map F-T2 / F-T3).
  *
  * Drive side: queued feed operations executed by a background thread with a
  * proper START / Repeated-START / STOP timing sequence and arbitration
  * detection (on a lost bit the host releases both lines, stops clocking,
  * and never issues STOP — UM10204 §3.1.8 loser behaviour). `feedContestFrame`
  * never drives SCL: it follows the DUT's clock so a DUT master and the soft
  * host can genuinely contend on SDA.
  *
  * Decode side: raw wire levels pass a sub-threshold-pulse filter (Table 9
  * spike suppression: a level change must persist for `spikeFilter` to be
  * committed) and a bit counter that emits Start / Stop / Data / Ack / Nack
  * events from the committed levels.
  *
  * One driver owns the `onChange` callback of each interconnect it is given.
  */
class I2cSimDriver(
    scl: OpenDrainInterconnect,
    sda: OpenDrainInterconnect,
    bitPeriod: TimeNumber = 400 ns,
    spikeFilter: TimeNumber = 50 ns
) {
  import I2cSimEvent._  // event terms; shadow spinal.core.Data in term position
  val sclConn = scl.newSoftConnection()
  val sdaConn = sda.newSoftConnection()

  val tLow: TimeNumber = bitPeriod / 2
  val tHigh: TimeNumber = bitPeriod / 2
  val tSample: TimeNumber = 60 ns
  val tSuSta: TimeNumber = 200 ns
  val tHdSto: TimeNumber = 50 ns
  val tBuf: TimeNumber = 100 ns
  val tBound: TimeNumber = 5000 ns

  // ---- raw level tracking with spike (Table 9) filtering ----
  // interconnect.value is a plain Scala var, so changes arrive through the
  // onChange callback; a raw change only commits after it persisted for
  // `spikeFilter`, which kills sub-threshold pulses on the wire.
  private class FilteredWire(inter: OpenDrainInterconnect, onCommit: Boolean => Unit) {
    var level: Boolean = inter.value
    private var checkPending = false
    inter.onChange = { raw =>
      if (raw != level && !checkPending) {
        checkPending = true
        delayed(spikeFilter) {
          checkPending = false
          if (inter.value == raw) {
            level = raw
            onCommit(raw)
          }
        }
      }
    }
  }

  // ---- event decoder (commit-driven; lambdas run at commit time, by which
  // point both wires exist) ----
  private val events = mutable.Queue[I2cSimEvent]()
  private var inFrame = false
  private var bitCnt = 0
  private var buffer = 0

  private var sdaWire: FilteredWire = _  // assigned below; lambdas run at commit time
  private val sclWire = new FilteredWire(scl, hi => {
    if (hi && inFrame) {
      bitCnt += 1
      buffer = (buffer << 1) | (if (sdaWire.level) 1 else 0)
      if (bitCnt == 8) events.enqueue(Data(buffer))
      if (bitCnt == 9) {
        events.enqueue(if (sdaWire.level) Nack else Ack)
        bitCnt = 0
        buffer = 0
      }
    }
  })
  sdaWire = new FilteredWire(sda, hi => {
    if (sclWire.level) {
      if (hi) {
        inFrame = false
        events.enqueue(Stop)
      } else {
        inFrame = true
        bitCnt = 0
        buffer = 0
        events.enqueue(Start)
      }
    }
  })

  // ---- feed queue + background engine ----
  private sealed trait FeedOp
  private object OpStart extends FeedOp
  private object OpContestStart extends FeedOp
  private object OpStop extends FeedOp
  private object OpRestart extends FeedOp
  // value: SDA level to drive; drive: false = release (ACK slot / read bits);
  // follow: true = follow the wire's SCL and never drive it (arbitration).
  private case class OpBit(value: Boolean, drive: Boolean, follow: Boolean) extends FeedOp

  private val feed = mutable.Queue[FeedOp]()
  private var pending = 0
  private var arbLostPrivate = false
  private var stopCountPrivate = 0

  def arbLost: Boolean = arbLostPrivate
  def stopCount: Int = stopCountPrivate
  def sdaReleased: Boolean = sdaConn.value
  def sclReleased: Boolean = sclConn.value
  def sdaHigh: Boolean = sdaWire.level
  def sclHigh: Boolean = sclWire.level

  def takeEvents(): List[I2cSimEvent] = {
    val seen = events.toList
    events.clear()
    seen
  }

  def waitDrained(): Unit = {
    while (pending != 0) sleep(10 ns)
  }

  def feedStart(): Unit = { feed.enqueue(OpStart); pending += 1 }
  def feedContestStart(): Unit = { feed.enqueue(OpContestStart); pending += 1 }
  def feedStop(): Unit = { feed.enqueue(OpStop); pending += 1 }
  def feedRestart(): Unit = { feed.enqueue(OpRestart); pending += 1 }

  def feedByte(value: Int): Unit = {
    for (i <- 7 downto 0) {
      feed.enqueue(OpBit(((value >> i) & 1) == 1, drive = true, follow = false))
      pending += 1
    }
  }

  /** Release SDA for one bit slot so the peer's ACK/NACK shows on the wire. */
  def feedAckSlot(): Unit = {
    feed.enqueue(OpBit(true, drive = false, follow = false))
    pending += 1
  }

  /** Read phase: release during the 8 data bits, then drive the ACK/NACK. */
  def feedReadByte(ack: Boolean): Unit = {
    for (_ <- 0 until 8) {
      feed.enqueue(OpBit(true, drive = false, follow = false))
      pending += 1
    }
    feed.enqueue(OpBit(!ack, drive = true, follow = false))
    pending += 1
  }

  def feedAddrWrite(addr: Int): Unit = { feedByte(addr << 1); feedAckSlot() }
  def feedAddrRead(addr: Int): Unit = { feedByte((addr << 1) | 1); feedAckSlot() }

  def feedWriteFrame(addr: Int, data: Seq[Int]): Unit = {
    feedStart()
    feedAddrWrite(addr)
    for (d <- data) {
      feedByte(d)
      feedAckSlot()
    }
    feedStop()
  }

  /**
    * Arbitration contest: drop SDA for a joint START (never drive SCL), then
    * follow the wire's SCL for every bit, driving SDA per our own frame. On
    * the first bit where the wire disagrees, [[arbLost]] is set, both lines
    * are released and the remaining ops are skipped — no STOP is issued.
    */
  def feedContestFrame(addr: Int, data: Seq[Int]): Unit = {
    feedContestStart()
    val all = (addr << 1) +: data
    for (v <- all) {
      for (i <- 7 downto 0) {
        feed.enqueue(OpBit((((v >> i) & 1)) == 1, drive = true, follow = true))
        pending += 1
      }
      feed.enqueue(OpBit(true, drive = false, follow = true))
      pending += 1
    }
  }

  private def waitWire(cond: () => Boolean, what: String): Unit = {
    var n = 0
    while (!cond() && n < 500) { sleep(10 ns); n += 1 } // 500 x 10 ns = tBound
    assert(cond(), s"I2cSimDriver: timeout waiting for $what")
  }

  private def onArbLoss(): Unit = {
    arbLostPrivate = true
    sdaConn.write(true)
    sclConn.write(true)
  }

  private def execute(op: FeedOp): Unit = op match {
    case OpStart =>
      waitWire(() => sclWire.level && sdaWire.level, "bus idle before START")
      sdaConn.write(false)
      sleep(tSuSta)
      sclConn.write(false)
      waitWire(() => !sclWire.level, "SCL fall for START")

    case OpContestStart =>
      // Joint START: SDA fall only; the SCL fall comes from the DUT.
      sdaConn.write(false)

    case OpStop =>
      assert(!arbLostPrivate, "I2cSimDriver: an arbitration loser must not issue STOP")
      sdaConn.write(false)
      waitWire(() => !sdaWire.level, "SDA fall for STOP")
      sclConn.write(false)
      waitWire(() => !sclWire.level, "SCL fall for STOP")
      sleep(tHdSto)
      sclConn.write(true)
      waitWire(() => sclWire.level, "SCL rise for STOP")
      sdaConn.write(true)
      waitWire(() => sdaWire.level, "SDA rise for STOP")
      sleep(tBuf)
      stopCountPrivate += 1

    case OpRestart =>
      // SCL is already Low (previous bit's low phase). Hold it Low for a full
      // tLow so the peer commits the SCL fall and releases SDA, then issue
      // the Repeated START: SCL rises, SDA falls while SCL High, SCL falls.
      // (Pulsing SDA Low->High->Low while SCL High, as the old sequence did,
      // is a STOP+START on the wire, not a Repeated START.)
      sclConn.write(false)
      waitWire(() => !sclWire.level, "SCL fall for Repeated START")
      sleep(tLow)
      waitWire(() => sdaWire.level, "SDA release before Repeated START")
      sdaConn.write(true)
      sclConn.write(true)
      waitWire(() => sclWire.level, "SCL rise for Repeated START")
      sleep(tSuSta)
      sdaConn.write(false)  // Sr: SDA falls while SCL High
      waitWire(() => !sdaWire.level, "SDA fall for Repeated START")
      sleep(tSuSta)
      sclConn.write(false)
      waitWire(() => !sclWire.level, "SCL fall after Repeated START")

    case OpBit(value, drive, follow) =>
      if (follow) {
        // Never drive SCL: follow the wire's clock (UM10204 arbitration).
        waitWire(() => !sclWire.level, "SCL fall (follow)")
        sdaConn.write(if (drive) value else true)
        waitWire(() => sclWire.level, "SCL rise (follow)")
        sleep(tSample)
        val seen = sdaWire.level
        if (drive && value && !seen) onArbLoss()
      } else {
        sclConn.write(false)
        waitWire(() => !sclWire.level, "SCL fall")
        sdaConn.write(if (drive) value else true)
        sleep(tLow)
        sclConn.write(true)
        waitWire(() => sclWire.level, "SCL rise")
        sleep(tSample)
        val seen = sdaWire.level
        sleep(tHigh - tSample)
        sclConn.write(false)
        if (drive && value && !seen) onArbLoss()
      }
  }

  private val engine = fork {
    while (true) {
      while (feed.isEmpty) sleep(10 ns)
      val op = feed.dequeue()
      val doExec = op match {
        case OpStart | OpContestStart => true
        case _ => !arbLostPrivate
      }
      if (doExec) execute(op)
      pending -= 1
    }
  }
}
