package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, BusSlaveFactoryAddressWrapper}

/**
  * Fully-automatic I2C master controller (CSR host adapter).
  *
  * Architecture: this component is a register-bus host adapter around
  * [[I2cAutoController]], which owns all I2C protocol logic (bus
  * conditioning, timeout watchdog, external-bus tracking, transfer FSM).
  * The adapter owns the host-side plumbing — TX/RX byte-storage FIFOs and
  * the interrupt controller (sticky flags, mask, W1C, IRQ line) — and
  * translates the firmware-facing register map into the controller's host
  * API (transaction requests, byte seams, sidebands), including the
  * busy-reject policy. The external CSR contract is unchanged by this
  * split.
  *
  * Software writes the CMD register to initiate a transfer. The hardware
  * (via the controller) autonomously generates START, address, data bytes,
  * optional RESTART + phase-2, and STOP. Completion or error is signalled
  * via the IRQ output.
  *
  * The register map is register-bus agnostic: [[I2cAutoMaster.driveFrom]]
  * binds a caller-supplied [[BusSlaveFactory]] to parent-side registers
  * wired into the io. Any bus family whose slave factory extends
  * BusSlaveFactory (Bmb, Apb3, TileLink, ...) can serve the same register
  * map.
  *
  * Register map (byte addresses, 32-bit data width; bit offsets shift with addrW=7/10):
  *   0x00  CMD          W: {nackLast[addrW+2], hasPhase2[addrW+1], rw[addrW], addr[addrW-1:0]}
  *                       (fields + trigger land atomically on the bus write handshake)
  *   0x04  BYTE_COUNT   W: phase-1 data byte count
  *   0x08  CMD_P2       RW: {valid[addrW], p2Addr[addrW-1:0]} — optional phase-2
  *                       address: when phase2=1 and valid, phase 2 is addressed
  *                       to p2Addr instead of repeating the phase-1 address
  *                       (cross-slave RESTART). valid=0 keeps the legacy behaviour.
  *   0x0C  P2_BYTE_COUNT W: phase-2 data byte count
  *   0x10  TX_DATA      W: push one byte into TX FIFO
  *   0x14  RX_DATA      R: {valid[8], data[7:0]}
  *   0x18  STATUS       R: {txFull[7], aborted[6], err[5:3], rxAvail[2], txAvail[1], busy[0]}
  *   0x1C  INT_EN       RW: {reserved[2], error[1], complete[0]}   (bit2 reserved, never asserted)
  *   0x20  INT_FLAG     R:  interrupt flags
  *   0x24  INT_CLR      W:  W1C interrupt flags
  *   0x28  CFG_SAMPDIV  W: sampling clock divider
  *   0x2C  CFG_TIMEOUT  W: timeout counter (0 = disabled); also bounds slave
  *                        clock-stretch -- a live slave holding SCL Low longer than
  *                        this trips the 9-pulse recovery + STOP.
  *   0x30  CFG_TLOW     W: SCL low period
  *   0x34  CFG_THIGH    W: SCL high period
  *   0x38  CFG_TBUF     W: post-STOP idle time
  *   0x3C  CTRL         W: {abrt[0]}
  */
object I2cAutoMaster {
  case class Config(
    fifoDepth    : Int     = 64,
    support10bit : Boolean = false,
    timerWidth   : Int     = 12,
    sampWinSize  : Int     = 3,
    sampDivWidth : Int     = 10
  )

  // Protocol constants live with the engine; re-exported here for
  // backwards compatibility with existing references.
  val Err                 = I2cAutoController.Err
  val BUS_RECOVERY_PULSES = I2cAutoController.BUS_RECOVERY_PULSES
  val TIMEOUT_W           = I2cAutoController.TIMEOUT_W
  val DEFAULT_SEG         = I2cAutoController.DEFAULT_SEG
  val BITS_PER_BYTE       = I2cAutoController.BITS_PER_BYTE

  def apply(c: Config = Config()): I2cAutoMaster = new I2cAutoMaster(c)

  /**
   * Instantiate the register map on a caller-supplied bus factory and wire it
   * to the controller's io. Must be elaborated in a scope that can see both
   * the factory's bus and the io (typically the parent component).
   *
   * Event registers (CMD trigger, TX_DATA push, RX_DATA pop, CTRL abort) are
   * one-cycle pulses derived from bus write/read handshakes; value registers
   * are plain registers. Pulse timing matches the legacy internal
   * BmbSlaveFactory implementation (pulse high one cycle after the bus
   * handshake).
   */
  def driveFrom(io: I2cAutoMasterIo, busCtrl: BusSlaveFactory, baseAddress: BigInt): Unit = new Area {
    import io._
    val factory = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)
    val addrW   = cmd.addr.getWidth

    // CMD (0x00): fields + trigger. Field regs update at the write edge; the
    // trigger pulse goes high the following cycle, so the FSM latches stable
    // field values.
    val cmdAddr     = Reg(Bits(addrW bits)) init(0)
    val cmdRw       = RegInit(False)
    val cmdPhase2   = RegInit(False)
    val cmdNackLast = RegInit(False)
    val cmdTrigger  = RegInit(False)
    factory.write(cmdAddr, 0x00, 0)
    factory.write(cmdRw, 0x00, addrW)
    factory.write(cmdPhase2, 0x00, addrW + 1)
    factory.write(cmdNackLast, 0x00, addrW + 2)
    cmdTrigger := False
    factory.onWrite(0x00) { cmdTrigger := True }
    cmd.addr     := cmdAddr
    cmd.rw       := cmdRw
    cmd.phase2   := cmdPhase2
    cmd.nackLast := cmdNackLast
    cmd.trigger  := cmdTrigger

    // BYTE_COUNT / P2_BYTE_COUNT
    val byteCount = Reg(UInt(8 bits)) init(0)
    val p2ByteCount = Reg(UInt(8 bits)) init(0)
    factory.write(byteCount, 0x04)
    factory.write(p2ByteCount, 0x0C)
    cmd.byteCount := byteCount
    cmd.p2ByteCount := p2ByteCount

    // CMD_P2 (0x08): optional phase-2 address {valid[addrW], p2Addr[addrW-1:0]}.
    // Separate register (not part of the CMD word) so the packed CMD layout is
    // unchanged; writing it does NOT trigger a transfer.
    val cmdP2Addr      = Reg(Bits(addrW bits)) init(0)
    val cmdP2AddrValid = RegInit(False)
    factory.write(cmdP2Addr, 0x08, 0)
    factory.write(cmdP2AddrValid, 0x08, addrW)
    cmd.p2Addr      := cmdP2Addr
    cmd.p2AddrValid := cmdP2AddrValid

    // TX_DATA (0x10): push one byte into the TX FIFO
    val txData = Reg(Bits(8 bits)) init(0)
    val txPush = RegInit(False)
    factory.write(txData, 0x10)
    txPush := False
    factory.onWrite(0x10) { txPush := True }
    tx.payload := txData
    tx.valid   := txPush

    // RX_DATA (0x14): pop-on-read via the io.rx Stream. The read returns the
    // pre-pop head combinationally; the pop fires on the accepted-read cycle
    // itself, gated on valid. An empty-FIFO read is therefore a no-op (no
    // underflow), and back-to-back reads advance one entry each (no
    // duplicated head, no silently-swallowed entry).
    factory.read(rx.payload, 0x14, 0)
    factory.read(rx.valid, 0x14, 8)
    rx.ready := factory.isReading(0x14) && rx.valid

    // STATUS (0x18): {aborted[6], err[5:3], rxAvail[2], txAvail[1], busy[0]}
    factory.read(0x18, 0 -> status.busy, 1 -> status.txAvail, 2 -> status.rxAvail,
                 3 -> status.errCode, 6 -> status.aborted, 7 -> status.txFull)

    // Interrupts
    val intEn  = RegInit(B(0, 3 bits))
    val intClr = RegInit(B(0, 3 bits))
    factory.write(intEn, 0x1C)
    factory.read(intEn, 0x1C)  // INT_EN is documented RW: allow readback
    factory.read(intr.flags, 0x20)
    // INT_CLR is a W1C pulse: default to 0 every cycle; the register-map
    // write materializes after this default (build runs in addPrePopTask),
    // so the clear lasts exactly one cycle instead of sticking. A stuck
    // clear would silently swallow every later occurrence of that flag.
    intClr := B(0, 3 bits)
    factory.write(intClr, 0x24)
    intr.en  := intEn
    intr.clr := intClr

    // Timing configuration
    val sampDiv = Reg(UInt(cfg.sampDiv.getWidth bits)) init(0)
    val timeout = Reg(UInt(TIMEOUT_W bits)) init(0)
    val tLow    = Reg(UInt(cfg.tLow.getWidth bits)) init(DEFAULT_SEG)
    val tHigh   = Reg(UInt(cfg.tHigh.getWidth bits)) init(DEFAULT_SEG)
    val tBuf    = Reg(UInt(cfg.tBuf.getWidth bits)) init(DEFAULT_SEG)
    factory.write(sampDiv, 0x28)
    factory.write(timeout, 0x2C)
    factory.write(tLow, 0x30)
    factory.write(tHigh, 0x34)
    factory.write(tBuf, 0x38)
    cfg.sampDiv := sampDiv
    cfg.timeout := timeout
    cfg.tLow    := tLow
    cfg.tHigh   := tHigh
    cfg.tBuf    := tBuf

    // CTRL (0x3C): abort pulse. Fired only when the WRITTEN VALUE's bit0
    // is 1 (nonStopWrite taps the bus write data combinationally), so a
    // read-modify-write that leaves the bit clear does not abort the bus.
    val ctrlAbBit = Bool()
    factory.nonStopWrite(ctrlAbBit, 0)
    val abort = RegInit(False)
    abort := False
    factory.onWrite(0x3C) { abort := ctrlAbBit }
    cfg.abort := abort
  }
}

/** Controller io: I2C pins, IRQ, and the firmware-facing control/status signals bound by driveFrom. */
case class I2cAutoMasterIo(c: I2cAutoMaster.Config) extends Bundle {
  val i2c = master(I2c())
  val irq = out Bool()

  val cmd = new Bundle {
    val addr     = in Bits((if (c.support10bit) 10 else 7) bits)
    val rw       = in Bool()
    val phase2   = in Bool()
    val nackLast = in Bool()
    val byteCount     = in UInt(8 bits)
    val p2ByteCount     = in UInt(8 bits)
    val p2Addr      = in Bits((if (c.support10bit) 10 else 7) bits)
    val p2AddrValid = in Bool()
    val trigger  = in Bool()  // one-cycle pulse from the CMD register write
  }

  // TX byte feed, typed as a Flow (unidirectional, fire-and-forget): the
  // register map pulses valid for one cycle per TX_DATA write and never
  // holds it. See [[I2cAutoController]] for the overflow contract.
  val tx = slave(Flow(Bits(8 bits)))

  // RX_DATA pop-on-read modeled as a real Stream: valid = head available,
  // payload = pre-pop head, ready = accepted register read. ready is driven
  // combinationally by driveFrom (see there), so the returned head is atomic
  // with the pop and back-to-back reads 1 cycle apart advance one entry
  // each. An empty-FIFO read is a no-op (ready is gated on valid).
  val rx = master(Stream(Bits(8 bits)))

  val status = new Bundle {
    val busy    = out Bool()
    val errCode = out UInt(3 bits)
    val aborted = out Bool()
    val txAvail = out Bool()
    val rxAvail = out Bool()
    val txFull  = out Bool()
  }

  val intr = new Bundle {
    val en  = in Bits(3 bits)
    val flags  = out Bits(3 bits)
    val clr = in Bits(3 bits)  // W1C
  }

  val cfg = new Bundle {
    val sampDiv = in UInt(c.sampDivWidth bits)
    val timeout = in UInt(I2cAutoMaster.TIMEOUT_W bits)
    val tLow    = in UInt(c.timerWidth bits)
    val tHigh   = in UInt(c.timerWidth bits)
    val tBuf    = in UInt(c.timerWidth bits)
    val abort   = in Bool()  // one-cycle pulse from the CTRL register write
  }

  /** Bind the register map to a bus factory (see [[I2cAutoMaster.driveFrom]]). */
  def driveFrom(busCtrl: BusSlaveFactory, baseAddress: BigInt): Unit =
    I2cAutoMaster.driveFrom(this, busCtrl, baseAddress)
}

/**
  * CSR host adapter for [[I2cAutoController]].
  *
  * Owns the host-side plumbing the controller deliberately lacks: the
  * TX/RX byte-storage FIFOs, the interrupt controller (sticky flags, mask,
  * W1C, IRQ line), and the STATUS availability/fullness bits derived from
  * FIFO occupancy. Behaviour is identical to the former monolithic
  * I2cAutoMaster: the firmware contract (preload TX bytes before CMD, poll
  * txFull, busy-reject with errCode = Err.BUSY_REJECT) is documented on
  * [[I2cAutoController]] and enforced jointly by this adapter (reject
  * policy, FIFOs) and the controller (error recording).
  */
class I2cAutoMaster(c: I2cAutoMaster.Config) extends Component {
  require(c.sampWinSize >= 2,
    s"I2cAutoMaster.Config.sampWinSize must be >= 2 (got ${c.sampWinSize}): " +
    "the glitch-filter history window needs at least 2 samples")

  val io = I2cAutoMasterIo(c)

  // The controller owns all I2C protocol logic. This adapter owns the
  // host-side plumbing (FIFOs, interrupts, status assembly) and translates
  // the CSR-facing io into the controller's host API.
  val engine = new I2cAutoController(I2cAutoController.Config(
    support10bit = c.support10bit,
    timerWidth   = c.timerWidth,
    sampWinSize  = c.sampWinSize,
    sampDivWidth = c.sampDivWidth
  ))
  io.i2c <> engine.io.i2c

  // CMD register write (one-cycle trigger + latched field regs) ->
  // transaction-level request. req.valid is the trigger pulse itself; the
  // engine latches the payload on fire. If the engine is not ready on that
  // cycle (busy, or external bus occupied) the request is DROPPED and we
  // raise engine.io.reject for that cycle -> errCode = BUSY_REJECT. This
  // reproduces the pre-split firmware-visible behaviour exactly (no
  // queueing: a CMD write is an event, not a held request).
  val req = engine.io.req
  req.valid := io.cmd.trigger
  req.payload.addr        := io.cmd.addr
  req.payload.rw          := io.cmd.rw
  req.payload.phase2      := io.cmd.phase2
  req.payload.nackLast    := io.cmd.nackLast
  req.payload.byteCount   := io.cmd.byteCount
  req.payload.p2ByteCount := io.cmd.p2ByteCount
  req.payload.p2Addr      := io.cmd.p2Addr
  req.payload.p2AddrValid := io.cmd.p2AddrValid
  engine.io.reject := io.cmd.trigger && !req.ready

  // === Byte-storage FIFOs (host buffering) ===
  // The controller pulls/pushes bytes over the io.txData / io.rxData seams;
  // these FIFOs decouple the bursty register bus from the bus-rate engine.
  // latency = 0: pop.payload (the pre-pop head) is combinationally
  // available, which the RX_DATA pop-on-read relies on.
  val fifoStage = new Area {
    val txFifo = StreamFifo(Bits(8 bits), c.fifoDepth, latency = 0)
    val rxFifo = StreamFifo(Bits(8 bits), c.fifoDepth, latency = 0)
    // TX_DATA register pulse -> txFifo push (Flow: fire-and-forget; the
    // overflow assert below is the loud guard).
    txFifo.io.push.valid   := io.tx.valid
    txFifo.io.push.payload := io.tx.payload

    // txFifo pop side <> controller's pull seam (one byte per fire).
    txFifo.io.pop <> engine.io.txData

    // controller's push seam <> rxFifo push side.
    engine.io.rxData <> rxFifo.io.push

    // RX_DATA pop-on-read (io.rx Stream, see driveFrom): ready is
    // combinationally from the register-bus read acceptance, so the
    // returned head is atomic with the pop.
    io.rx.valid   := rxFifo.io.pop.valid
    io.rx.payload := rxFifo.io.pop.payload
    rxFifo.io.pop.ready := io.rx.ready

    assert(!(io.tx.valid && !txFifo.io.push.ready), "txFifo overflow: push on full FIFO")
    assert(!(engine.io.txData.fire && !txFifo.io.pop.valid), "txFifo underflow: pop on empty FIFO")
  }
  val txFifo      = fifoStage.txFifo
  val rxFifo      = fifoStage.rxFifo

  // === Interrupt controller ===
  // Sticky flags latched from the controller's one-cycle event pulses
  // (bit2 reserved, never asserted). The clearWhen loop precedes the event
  // latches in source order, so a set and a clear on the same cycle resolve
  // set-wins — the pre-move semantics (the old flag-set sites sat later in
  // the elaboration order than the old clearWhen loop).
  val intrFlags = RegInit(B(0, 3 bits))
  for (i <- 0 until 3) { intrFlags(i) clearWhen(io.intr.clr(i)) }
  when(engine.io.evtComplete) { intrFlags(0) := True }
  when(engine.io.evtError)    { intrFlags(1) := True }
  io.irq        := (io.intr.en & intrFlags).orR
  io.intr.flags := intrFlags

  // === Status assembly ===
  // Protocol status comes from the controller; availability/fullness from
  // the host FIFOs (firmware polls txFull before writing TX_DATA).
  io.status.busy    := engine.io.status.busy
  io.status.errCode := engine.io.status.errCode
  io.status.aborted := engine.io.status.aborted
  io.status.txAvail := txFifo.io.pop.valid
  io.status.rxAvail := rxFifo.io.pop.valid
  io.status.txFull  := !txFifo.io.push.ready

  engine.io.cfg <> io.cfg

  // Member aliases kept for white-box testbenches (I2cAutoSim / I2cAutoFuzz)
  // that reached into the former monolith via instance paths (dut.master.*).
  // Scala imports are lexical only; these top-level vals are what make the
  // signals reachable as members from outside.
  val fsm          = engine.fsm
  val busBusyExt   = engine.busBusyExt
  val status       = engine.status
}
