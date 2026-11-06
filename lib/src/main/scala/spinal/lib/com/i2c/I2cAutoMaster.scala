package spinal.lib.com.i2c

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, BusSlaveFactoryAddressWrapper}
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

/**
  * Fully-automatic I2C master controller.
  *
  * Software writes the CMD register to initiate a transfer. The hardware
  * autonomously generates START, address, data bytes, optional RESTART +
  * phase-2, and STOP. Completion or error is signalled via the IRQ output.
  *
  * The controller core is register-bus agnostic: it exposes the signals it
  * needs ([[I2cAutoMasterIo]]) and leaves register-map instantiation to
  * [[I2cAutoMaster.driveFrom]], which binds a caller-supplied
  * [[BusSlaveFactory]] to parent-side registers wired into the io. Any bus
  * family whose slave factory extends BusSlaveFactory (Bmb, Apb3, TileLink, ...)
  * can serve the same register map.
  *
  * Register map (byte addresses, 32-bit data width; bit offsets shift with addrW=7/10):
  *   0x00  CMD          W: {nackLast[addrW+2], hasPhase2[addrW+1], rw[addrW], addr[addrW-1:0]}
  *                       (fields + trigger land atomically on the bus write handshake)
  *   0x04  LEN1         W: phase-1 data byte count
  *   0x08  LEN2         W: phase-2 data byte count
  *   0x0C  TX_DATA      W: push one byte into TX FIFO
  *   0x10  RX_DATA      R: {valid[8], data[7:0]}
  *   0x14  STATUS       R: {txFull[7], aborted[6], err[5:3], rxAvail[2], txAvail[1], busy[0]}
  *   0x18  INT_EN       RW: {reserved[2], error[1], complete[0]}   (bit2 reserved, never asserted)
  *   0x1C  INT_FLAG     R:  interrupt flags
  *   0x20  INT_CLR      W:  W1C interrupt flags
  *   0x24  CFG_SAMPDIV  W: sampling clock divider
  *   0x28  CFG_TIMEOUT  W: timeout counter (0 = disabled); also bounds slave
  *                        clock-stretch -- a live slave holding SCL Low longer than
  *                        this trips the 9-pulse recovery + STOP.
  *   0x2C  CFG_TLOW     W: SCL low period
  *   0x30  CFG_THIGH    W: SCL high period
  *   0x34  CFG_TBUF     W: post-STOP idle time
  *   0x38  CTRL         W: {abrt[0]}
  *   0x3C  CMD_P2       RW: {valid[addrW], p2Addr[addrW-1:0]} — optional phase-2
  *                       address: when phase2=1 and valid, phase 2 is addressed
  *                       to p2Addr instead of repeating the phase-1 address
  *                       (cross-slave RESTART). valid=0 keeps the legacy behaviour.
  */
object I2cAutoMaster {


  case class Config(
    fifoDepth    : Int     = 64,
    support10bit : Boolean = false,
    timerWidth   : Int     = 12,
    sampWinSize  : Int     = 3,
    sampDivWidth : Int     = 10
  )

  /** Named STATUS.err / error-source codes (3-bit). */
  object Err {
    val NONE        = 0
    val ADDR_NACK   = 1
    val DATA_NACK   = 2
    val TIMEOUT     = 3
    val ARB_LOSS    = 4
    val BUSY_REJECT = 5
  }
  /** I2C bus-recovery pulse count (UM10204 §3.4.2). */
  val BUS_RECOVERY_PULSES = 9
  /** Bit-width of the runtime-configurable timeout counter. */
  val TIMEOUT_W = 20
  /** Default SCL segment length (filter-ticks) for tLow/tHigh/tBuf. */
  val DEFAULT_SEG = 10

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

    // LEN1 / LEN2
    val len1 = Reg(UInt(8 bits)) init(0)
    val len2 = Reg(UInt(8 bits)) init(0)
    factory.write(len1, 0x04)
    factory.write(len2, 0x08)
    cmd.len1 := len1
    cmd.len2 := len2

    // CMD_P2 (0x3C): optional phase-2 address {valid[addrW], p2Addr[addrW-1:0]}.
    // Separate register (not part of the CMD word) so the packed CMD layout is
    // unchanged; writing it does NOT trigger a transfer.
    val cmdP2Addr      = Reg(Bits(addrW bits)) init(0)
    val cmdP2AddrValid = RegInit(False)
    factory.write(cmdP2Addr, 0x3C, 0)
    factory.write(cmdP2AddrValid, 0x3C, addrW)
    cmd.p2Addr      := cmdP2Addr
    cmd.p2AddrValid := cmdP2AddrValid

    // TX_DATA (0x0C): push one byte into the TX FIFO
    val txData = Reg(Bits(8 bits)) init(0)
    val txPush = RegInit(False)
    factory.write(txData, 0x0C)
    txPush := False
    factory.onWrite(0x0C) { txPush := True }
    tx.data := txData
    tx.push := txPush

    // RX_DATA (0x10): pop-on-read. The read returns the pre-pop head
    // combinationally; the pop fires on the accepted-read cycle itself,
    // gated on notEmpty. An empty-FIFO read is therefore a no-op (no
    // underflow), and back-to-back reads advance one entry each (no
    // duplicated head, no silently-swallowed entry).
    factory.read(rx.head, 0x10, 0)
    factory.read(rx.notEmpty, 0x10, 8)
    rx.pop := factory.isReading(0x10) && rx.notEmpty

    // STATUS (0x14): {aborted[6], err[5:3], rxAvail[2], txAvail[1], busy[0]}
    factory.read(0x14, 0 -> status.busy, 1 -> status.txAvail, 2 -> status.rxAvail,
                 3 -> status.errCode, 6 -> status.aborted, 7 -> status.txFull)

    // Interrupts
    val intEn  = RegInit(B(0, 3 bits))
    val intClr = RegInit(B(0, 3 bits))
    factory.write(intEn, 0x18)
    factory.read(intEn, 0x18)  // INT_EN is documented RW: allow readback
    factory.read(intr.fl, 0x1C)
    // INT_CLR is a W1C pulse: default to 0 every cycle; the register-map
    // write materializes after this default (build runs in addPrePopTask),
    // so the clear lasts exactly one cycle instead of sticking. A stuck
    // clear would silently swallow every later occurrence of that flag.
    intClr := B(0, 3 bits)
    factory.write(intClr, 0x20)
    intr.en  := intEn
    intr.clr := intClr

    // Timing configuration
    val sampDiv = Reg(UInt(cfg.sampDiv.getWidth bits)) init(0)
    val timeout = Reg(UInt(TIMEOUT_W bits)) init(0)
    val tLow    = Reg(UInt(cfg.tLow.getWidth bits)) init(DEFAULT_SEG)
    val tHigh   = Reg(UInt(cfg.tHigh.getWidth bits)) init(DEFAULT_SEG)
    val tBuf    = Reg(UInt(cfg.tBuf.getWidth bits)) init(DEFAULT_SEG)
    factory.write(sampDiv, 0x24)
    factory.write(timeout, 0x28)
    factory.write(tLow, 0x2C)
    factory.write(tHigh, 0x30)
    factory.write(tBuf, 0x34)
    cfg.sampDiv := sampDiv
    cfg.timeout := timeout
    cfg.tLow    := tLow
    cfg.tHigh   := tHigh
    cfg.tBuf    := tBuf

    // CTRL (0x38): abort pulse. Fired only when the WRITTEN VALUE's bit0
    // is 1 (nonStopWrite taps the bus write data combinationally), so a
    // read-modify-write that leaves the bit clear does not abort the bus.
    val ctrlAbBit = Bool()
    factory.nonStopWrite(ctrlAbBit, 0)
    val abort = RegInit(False)
    abort := False
    factory.onWrite(0x38) { abort := ctrlAbBit }
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
    val len1     = in UInt(8 bits)
    val len2     = in UInt(8 bits)
    val p2Addr      = in Bits((if (c.support10bit) 10 else 7) bits)
    val p2AddrValid = in Bool()
    val trigger  = in Bool()  // one-cycle pulse from the CMD register write
  }

  val tx = new Bundle {
    val data = in Bits(8 bits)
    val push = in Bool()  // one-cycle pulse from the TX_DATA register write
  }

  val rx = new Bundle {
    val head     = out Bits(8 bits)  // pre-pop head
    val notEmpty = out Bool()
    val pop      = in Bool()  // one-cycle pulse from the RX_DATA register read
  }

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
    val fl  = out Bits(3 bits)
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

class I2cAutoMaster(c: I2cAutoMaster.Config) extends Component {
  require(c.sampWinSize >= 2,
    s"I2cAutoMaster.Config.sampWinSize must be >= 2 (got ${c.sampWinSize}): " +
    "the glitch-filter history window needs at least 2 samples")

  import I2cAutoMaster.{Config, Err, BUS_RECOVERY_PULSES, TIMEOUT_W, DEFAULT_SEG}

  val io = I2cAutoMasterIo(c)

  // === Input conditioning (#5, #6) ===
  // Two-FF synchronization of the open-drain bus wires: they originate in an
  // external clock domain (another chip's driver). Feeding raw metastable
  // signals into history/edge logic is the classic multi-chip hazard.
  // Precedent: I2cIoFilter in Misc.scala.
  val sclSync = BufferCC(io.i2c.scl.read, 2)
  val sdaSync = BufferCC(io.i2c.sda.read, 2)

  val filterTick = new Area {
    val counter = Reg(UInt(c.sampDivWidth bits)) init(0)
    val tick    = counter === U(0)
    counter := Mux(tick, io.cfg.sampDiv, counter - U(1))
  }

  // Glitch filter: sliding-window history; the filtered value only updates
  // when the ENTIRE window agrees (unanimous). The old condition
  // (andR || orR) reduced to orR (any non-zero pattern), letting a
  // single-sample glitch straight through. Windows start all-ones = released.
  val sclFilt = RegInit(True)
  val sdaFilt = RegInit(True)
  val sclHist = RegInit(~U(0, c.sampWinSize bits).asBits)
  val sdaHist = RegInit(~U(0, c.sampWinSize bits).asBits)

  when(filterTick.tick) {
    sclHist := Cat(sclSync, sclHist(c.sampWinSize - 2 downto 0))
    sdaHist := Cat(sdaSync, sdaHist(c.sampWinSize - 2 downto 0))
    when(sclHist.andR || sclHist.nandR) { sclFilt := sclSync }
    when(sdaHist.andR || sdaHist.nandR) { sdaFilt := sdaSync }
  }

  // === Timeout watchdog ===
  val timeoutFired = RegInit(False)
  val timeoutCtr   = Reg(UInt(TIMEOUT_W bits)) init(0)
  when(timeoutCtr =/= U(0)) {
    timeoutCtr := timeoutCtr - U(1)
    timeoutFired := timeoutCtr === U(1)
  } otherwise {
    timeoutFired := False
  }


  // === TX / RX FIFOs ===
  // Both FIFOs are now identical StreamFifo instances (the hand-rolled
  // Mem/wPtr/rPtr/cnt duplication is gone). Push/pop are handshakes.
  // latency = 0 enables the async read + bypass path, so pop.payload (the head)
  // is combinationally available -- exactly like the old `mem.readAsync(rPtr)`
  // peek that the FSM and the firmware both relied on.
  val txFifo = StreamFifo(Bits(8 bits), c.fifoDepth, latency = 0)
  val rxFifo = StreamFifo(Bits(8 bits), c.fifoDepth, latency = 0)

  // Pop/push intents: txPopIntent/rxPushIntent are one-cycle pulses the FSM
  // drives to the FIFOs. The RX pop is COMBINATIONAL from io.rx.pop (NOT a
  // register): the returned head (io.rx.head, below) must be atomic with the
  // pop so that back-to-back reads 1 cycle apart on a pipelined bus (BMB /
  // TileLink) advance one entry each instead of returning the same head twice.
  // (A registered pop lagged the read by a cycle and duplicated the head.)
  // rxPopIntent is a MANUAL pop register (held at 0 by the design): it gives
  // simulations a direct drain path (TB may #= force it, bypassing the
  // register bus). It is OR-ed into the FIFO pop below, alongside the
  // combinational CSR pop (io.rx.pop) that guarantees atomic pop-on-read.
  val txPopIntent  = RegInit(False)             // FSM -> txFifo pop
  val rxPushIntent = RegInit(False)             // FSM -> rxFifo push
  val rxPushData   = Reg(Bits(8 bits)) init(0)  // snapshot of rxByte at push time
  val rxPopIntent  = RegInit(False)             // manual/test pop hook (TB may #= force)
  // Received-byte accumulator (hoisted out of the FSM so the rxFifo push
  // payload can be driven from the wiring level below).
  val rxByte       = Reg(Bits(8 bits)) init(0)

  // Wire the firmware-facing FIFO interface (driven by driveFrom-bound registers)
  txFifo.io.push.valid   := io.tx.push
  txFifo.io.push.payload := io.tx.data
  txFifo.io.pop.ready    := txPopIntent
  txPopIntent := False  // default; the FSM sets True transiently on each pop

  rxFifo.io.push.valid   := rxPushIntent
  rxFifo.io.push.payload := rxPushData
  rxPushIntent := False  // default; the FSM sets True transiently on each push
  // Combinational: pop in the same cycle the register-bus read is accepted so
  // io.rx.head is the pre-pop head and back-to-back reads are atomic.
  // rxPopIntent is the undriven sim test hook (forced via #= in tests).
  rxFifo.io.pop.ready := io.rx.pop || rxPopIntent
  io.rx.head       := rxFifo.io.pop.payload
  io.rx.notEmpty   := rxFifo.io.pop.valid

  // Loud over/underflow guards (kept per Q3). The handshake makes these
  // structurally rare, but a bad FSM gate or a firmware push-while-full must
  // still blow up loudly in simulation (the old cnt-based asserts, re-expressed
  // against the handshake).
  // TX under/overflow are enforced by SIMULATION ASSERTS ONLY. In synthesis an
  // underflow (FSM pops a drained FIFO) would shift out garbage with no errCode
  // raised. CONTRACT: firmware MUST preload >= len1 data bytes into the TX FIFO
  // before triggering CMD (and keep it fed for phase-2 len2 bytes); poll
  // io.status.txAvail / io.status.txFull before pushing to avoid overflow.
  assert(!(io.tx.push   && !txFifo.io.push.ready), "txFifo overflow: push on full FIFO")
  assert(!(txPopIntent  && !txFifo.io.pop.valid),  "txFifo underflow: pop on empty FIFO")
  assert(!(rxPushIntent && !rxFifo.io.push.ready), "rxFifo overflow: push on full FIFO")
  assert(!(rxPopIntent  && !rxFifo.io.pop.valid),  "rxFifo underflow: pop on empty FIFO")

  /** Width of the address field in bits (7 or 10). */
  val addrW: Int = if (c.support10bit) 10 else 7
  /** Number of address bytes on the bus (1 for 7-bit, 2 for 10-bit). */
  val addrBytes: Int = if (c.support10bit) 2 else 1

  // === Status ===
  val status = new Area {
    val busy    = RegInit(False)
    val errCode = RegInit(U(0, 3 bits))  // 1=NACK 2=dataNACK 3=timeout 4=arbLoss 5=busyRejected
    val aborted = RegInit(False)
  }

  // Timeout reload on SCL edge or when not busy
  val sclPrev = RegInit(True)
  val sclEdge = sclFilt =/= sclPrev
  sclPrev := sclFilt
  when(sclEdge || !status.busy) {
    timeoutCtr := io.cfg.timeout
  }

  // === Interrupt controller ===
  val intr = new Area {
    val fl = RegInit(B(0, 3 bits))
    for (i <- 0 until 3) { fl(i) clearWhen(io.intr.clr(i)) }
  }
  io.irq := (io.intr.en & intr.fl).orR

  // Forward internal status to the io
  io.status.busy    := status.busy
  io.status.errCode := status.errCode
  io.status.aborted := status.aborted
  io.status.txAvail := txFifo.io.pop.valid
  io.status.rxAvail := rxFifo.io.pop.valid
  io.status.txFull  := !txFifo.io.push.ready  // Q5: firmware checks before writing TX_DATA
  io.intr.fl        := intr.fl

  // === Transfer FSM ===
  // §3.1.10 (UM10204): a master must not initiate a START while the bus is
  // busy. Track external occupancy from the filtered wires while we are NOT
  // driving (our own edges would otherwise feed back): a START sets busy,
  // a STOP clears it. While status.busy (our own transfer owns the wires)
  // the tracker is frozen.
  val busBusyExt = RegInit(False)
  // Armed only after the first accepted transfer: at power-up the wires can
  // show a spurious SDA-fall-while-SCL-High transient (driver init) that
  // would otherwise latch busBusyExt True forever and reject every CMD.
  val busSeenOnce = RegInit(False)
  // Consequence: the reset-to-first-CMD window does NOT reject an externally
  // busy bus (UM10204 sec 3.1.8). This assumes the bus is idle at reset, which
  // holds for a freshly-reset controller on a normally-idle bus. If the system
  // can power up mid-transfer on the bus, arm the tracker earlier (initialize
  // busSeenOnce := True) at the cost of the power-up transient guard above.
  val sdaPrev = RegNext(sdaFilt)
  val extStart = sclFilt && sdaPrev && !sdaFilt
  val extStop  = sclFilt && !sdaPrev && sdaFilt
  when(!status.busy && busSeenOnce) {
    when(extStart) { busBusyExt := True }
    when(extStop)  { busBusyExt := False }
  }

  val fsm = new StateMachine {

    // Timer with armed flag to avoid spurious timerDone on entry cycle
    val timer      = Reg(UInt(c.timerWidth bits)) init(0)
    val timerArmed = RegInit(False)
    val timerDone  = timerArmed && timer === U(0)

    // 9-pulse recovery counter
    val recovCtr   = Reg(UInt(4 bits)) init(0)

    // --- #10: START/STOP setup-hold segments derived from the bit timing ---
    // The I2C spec's fixed START/STOP setup/hold times (>=0.6 us in Standard
    // Mode, UM10204 Table 25) are approximated from the configured bit period:
    // a full bit (tLow + tHigh) conservatively covers TSU:STA / THD:STA /
    // TSU:STO / THD:STO; a quarter-low floor keeps the FSM snappy when tLow
    // is tiny (simulation). Sum wraps at 2^timerWidth, which merely caps the
    // segment at a plausible maximum.
    val tSegFull = Mux((io.cfg.tLow +^ io.cfg.tHigh) === U(0), U(1, c.timerWidth bits),
                       (io.cfg.tLow +^ io.cfg.tHigh).resize(c.timerWidth))
    val tSegQ    = Mux(io.cfg.tLow >= U(4), io.cfg.tLow >> 2, U(1, c.timerWidth bits))

    // Global timer decrement
    when(timerArmed && timer =/= U(0)) { timer := timer - U(1) }

    val phase   = RegInit(U(0, 1 bits))
    // 9 bits: 10-bit mode needs index 256 (final data byte when addrBytes=2
    // and len=255), and a wider counter keeps a wrapped index from ever
    // re-entering the address-byte range ([0, addrBytes-1]).
    val byteNum = RegInit(U(0, 9 bits))
    val bitPos  = RegInit(U(0, 4 bits))
    val curLen  = RegInit(U(0, 8 bits))
    val curRw   = RegInit(False)

    val txByte = Reg(Bits(8 bits)) init(0)

    // Set on a successful transfer end; the COMPLETE interrupt and the busy
    // deassertion are deferred to TBUF (bus truly idle), so firmware that
    // polls busy / waits for the IRQ and then programs the next CMD finds
    // the FSM back in IDLE and the command is not silently dropped.
    val transferOk = RegInit(False)

    // Drive signals
    val sdaDrive = RegInit(True)
    val sdaEn    = RegInit(False)
    // Combinational: True only while BIT_HIGH is active — the sole window
    // where arbitration can be judged (we own a transmitted bit and SCL is
    // High). STOP/RESTART sequences legitimately see residual slave ACK Low
    // on SDA; judging there would false-trigger.
    val inBitHigh = Bool()
    inBitHigh := False
    val sclDrive = RegInit(True)

    // Latched params
    val latAddr     = Reg(Bits(addrW bits)) init(0)
    val latRw       = RegInit(False)
    val latHasP2    = RegInit(False)
    val latNackLast = RegInit(False)
    val latLen2     = Reg(UInt(8 bits)) init(0)
    val latP2Addr      = Reg(Bits(addrW bits)) init(0)
    val latP2AddrValid = RegInit(False)

    // Derived
    val dataStart    = U(addrBytes, 9 bits)
    val isAddrByte   = byteNum < dataStart
    val isLastAddrByte = byteNum === U(addrBytes - 1, 8 bits)
    val isAckBit     = bitPos === U(8)
    val isLastByte   = byteNum === (dataStart - U(1)) + curLen

    val driveSda: Bool = Mux(isAckBit, Mux(isAddrByte, False, curRw), isAddrByte || !curRw)

    // Address drive logic (compile-time selected to avoid out-of-bounds in 7-bit mode)
    // 10-bit frame format per I2C spec (UM10204 §4.2.2 / IEC 60197):
    //   byte 0: 1 1 1 1 0 A9 A8 R/W
    //   byte 1: A7 A6 A5 A4 A3 A2 A1 A0
    // Cross-slave RESTART (CMD_P2): phase 2 may address a different slave;
    // p2AddrValid=0 repeats the phase-1 address (legacy behaviour).
    val effAddr = Mux(phase === U(1) && latP2AddrValid, latP2Addr, latAddr)
    val addrDrive: Bool = if (c.support10bit) {
      val tenBitByte0: Bool = Mux(bitPos(2 downto 0) <= U(3), True,           // b7..b4 = 1111
                                   Mux(bitPos(2 downto 0) === U(4), False,    // b3 = 0
                                     Mux(bitPos(2 downto 0) === U(5), effAddr(9),  // b2 = A9
                                       Mux(bitPos(2 downto 0) === U(6), effAddr(8), // b1 = A8
                                           curRw))))                          // b0 = R/W
      val tenBitByte1: Bool = effAddr(7 - bitPos(2 downto 0))                 // b7..b0 = A7..A0
      Mux(byteNum === U(0), tenBitByte0, tenBitByte1)
    } else {
      Mux(bitPos(2 downto 0) === U(7), curRw, effAddr(6 - bitPos(2 downto 0)))
    }

    val sdaValue: Bool = Mux(isAckBit,
      Mux(latNackLast && isLastByte && !(latHasP2 && !curRw), True, False),
      Mux(isAddrByte, addrDrive, txByte(7 - bitPos(2 downto 0)))
    )

    // States
    val IDLE: State = new State with EntryPoint {
      whenIsActive {
        when(io.cmd.trigger && !busBusyExt) {
          busSeenOnce := True  // arm external-bus tracker from now on
          latAddr     := io.cmd.addr
          latRw       := io.cmd.rw
          latHasP2    := io.cmd.phase2
          latNackLast := io.cmd.nackLast
          latLen2     := io.cmd.len2
          latP2Addr      := io.cmd.p2Addr
          latP2AddrValid := io.cmd.p2AddrValid
          phase   := U(0)
          byteNum := U(0)
          bitPos  := U(0)
          curLen  := io.cmd.len1
          curRw   := io.cmd.rw
          rxByte  := B(0)
          transferOk   := False
          status.busy    := True
          status.errCode := U(Err.NONE, 3 bits)
          status.aborted := False
          timeoutCtr     := io.cfg.timeout
          goto(START1)
        }
      }
    }

    val START1: State = new State {
      onEntry { timer := tSegQ; timerArmed := True }
      whenIsActive {
        sclDrive := True
        sdaEn    := True
        sdaDrive := True
        when(timerDone) { goto(START2) }
      }
    }

    val START2: State = new State {
      onEntry { timer := tSegFull; timerArmed := True }
      whenIsActive {
        sclDrive := True
        sdaEn    := True
        sdaDrive := False
        when(timerDone) { goto(START3) }
      }
    }

    val START3: State = new State {
      onEntry { timer := io.cfg.tLow; timerArmed := True }
      whenIsActive {
        sclDrive := False
        sdaEn    := True
        sdaDrive := False
        when(timerDone) { goto(BIT_LOW) }
      }
    }

    val BIT_LOW: State = new State {
      onEntry { timer := io.cfg.tLow; timerArmed := True }
      whenIsActive {
        sclDrive := False
        when(driveSda) {
          sdaEn    := True
          sdaDrive := sdaValue
        } otherwise {
          sdaEn := False
        }
        when(timerDone) { goto(BIT_HIGH) }
      }
    }

    val BIT_HIGH: State = new State {
      onEntry { timer := io.cfg.tHigh; timerArmed := True }
      whenIsActive {
        inBitHigh := True
        sclDrive := True
        when(driveSda) {
          sdaEn    := True
          sdaDrive := sdaValue
        } otherwise {
          sdaEn := False
        }
        // Clock stretching: reload timer if SCL held low
        when(!sclFilt) { timer := io.cfg.tHigh }

        when(timerDone && sclFilt) {
          when(!driveSda && !isAckBit) {
            // Accumulate received data bits; ACK bits are handled by the NACK
            // checks below (no write-only ackBit register is kept).
            rxByte(7 - bitPos(2 downto 0)) := sdaFilt
          }
          when(isAckBit) {
            when(isAddrByte) {
              when(isLastAddrByte) {
                when(sdaFilt) {  // NACK on last address byte
                  status.errCode := U(Err.ADDR_NACK, 3 bits)
                  intr.fl(1)     := True
                  // busy stays high through the STOP tail and is released in
                  // TBUF (bus truly idle), so a firmware re-issue is never
                  // silently dropped mid-cleanup (see transferOk / TBUF).
                  goto(STOP0)
                } otherwise {
                  when(curLen === U(0)) {
                    // Zero-length phase: the address phase is the whole
                    // transaction (e.g. pointer-only access). Completes
                    // after the STOP sequence like any other transfer;
                    // without this exit the FSM would walk into the data
                    // phase with a last-byte index pointing at the address
                    // byte and never terminate.
                    when(latHasP2 && phase === U(0)) {
                      // Phase 2 follows after a RESTART; its own address
                      // phase decides completion (a zero-length phase 2
                      // converges here with phase === U(1)).
                      phase   := U(1)
                      byteNum := U(0)
                      bitPos  := U(0)
                      curLen  := latLen2
                      curRw   := !latRw
                      rxByte  := B(0)
                      goto(RESTART0)
                    } otherwise {
                      transferOk := True
                      timeoutCtr := io.cfg.timeout  // disarm watchdog for the STOP tail
                      goto(STOP0)
                    }
                  } otherwise {
                    byteNum := dataStart
                    bitPos  := U(0)
                    when(!curRw && curLen > U(0)) {
                      txByte := txFifo.io.pop.payload
                      txPopIntent := True
                    }
                    goto(BIT_LOW)
                  }
                }
              } otherwise {
                // Intermediate address byte (10-bit mode): continue.
                // §3.1.11 A1/A3: a read-direction header (R/W=1) is NOT
                // acknowledged on this byte — the addressed slave answers
                // the SECOND byte after Sr. Aborting here (as a NACK) broke
                // every 10-bit read. The authoritative address ACK/NACK
                // check stays on the LAST address byte below.
                byteNum := byteNum + U(1)
                bitPos  := U(0)
                goto(BIT_LOW)
              }
            } otherwise {
              when(curRw) {
                rxPushIntent := True
                rxPushData   := rxByte
              }
              when(!curRw && sdaFilt) {  // Data NACK: use direct signal
                status.errCode := U(Err.DATA_NACK, 3 bits)
                intr.fl(1)     := True
                // busy stays high through the STOP tail; released in TBUF.
                goto(STOP0)
              } otherwise {
                when(isLastByte) {
                  when(latHasP2 && phase === U(0)) {
                    phase   := U(1)
                    byteNum := U(0)
                    bitPos  := U(0)
                    curLen  := latLen2
                    curRw   := !latRw
                    rxByte  := B(0)
                    // No TX prefetch here: if phase 2 is a write phase, its own
                    // address-ACK pop (below, gated on !curRw) loads the first
                    // byte; if phase 2 is a read phase, no TX bytes are needed.
                    // (An earlier `when(!latRw)` pop did a phantom pop on
                    // write->read composites, underflowing the FIFO counter and
                    // shifting the next write's data; a `when(latRw)` variant
                    // double-popped read->write composites. Both removed.)
                    goto(RESTART0)
                  } otherwise {
                    // Defer COMPLETE and the busy deassertion to TBUF
                    // (see transferOk): the bus is still occupied by the
                    // STOP sequence here.
                    transferOk := True
                    timeoutCtr := io.cfg.timeout  // disarm watchdog for the STOP tail
                    goto(STOP0)
                  }
                } otherwise {
                  byteNum := byteNum + U(1)
                  bitPos  := U(0)
                  rxByte  := B(0)
                  when(!curRw) {
                    txByte := txFifo.io.pop.payload
                    txPopIntent := True
                  }
                  goto(BIT_LOW)
                }
              }
            }
          } otherwise {
            bitPos := bitPos + U(1)
            goto(BIT_LOW)
          }
        }
      }
    }

    val RESTART0: State = new State {
      onEntry { timer := tSegQ; timerArmed := True }
      whenIsActive {
        sclDrive := False  // drop SCL to let slave release SDA
        sdaEn    := True
        sdaDrive := True
        when(timerDone) { goto(RESTART1) }
      }
    }

    val RESTART1: State = new State {
      onEntry { timer := tSegQ; timerArmed := True }
      whenIsActive {
        sclDrive := True
        sdaEn    := True
        sdaDrive := True
        // Hold SCL High only once the bus is actually High: a stretching
        // slave keeps the wire Low, and raising SDA while SCL is Low would
        // not register as a Repeated START. Bounded by the global timeout
        // watchdog (no SCL edges + busy => RECOV).
        when(timerDone && sclFilt) { goto(RESTART2) }
      }
    }

    val RESTART2: State = new State {
      onEntry { timer := tSegFull; timerArmed := True }
      whenIsActive {
        sclDrive := True
        sdaEn    := True
        sdaDrive := False
        when(timerDone) { goto(RESTART3) }
      }
    }

    val RESTART3: State = new State {
      onEntry { timer := io.cfg.tLow; timerArmed := True }
      whenIsActive {
        sclDrive := False
        sdaEn    := True
        sdaDrive := False
        when(timerDone) { goto(BIT_LOW) }
      }
    }

    // 9-pulse clock recovery (I2C spec §3.4.2: recover from bus hang)
    // Watchdog note: every exit-tail segment below reloads the watchdog on
    // entry (segments are far shorter than any sane timeout, so the tail
    // cannot self-starve), while STOP1 keeps its full watchdog protection:
    // a slave stretching SCL Low longer than `timeout` still triggers a
    // fresh recovery instead of an unbounded STOP1 stall.
    val RECOV_LO: State = new State {
      onEntry { timer := io.cfg.tLow; timerArmed := True; timeoutCtr := io.cfg.timeout }
      whenIsActive {
        sclDrive := False
        sdaEn    := False
        when(timerDone) { goto(RECOV_HI) }
      }
    }

    val RECOV_HI: State = new State {
      onEntry { timer := io.cfg.tHigh; timerArmed := True; timeoutCtr := io.cfg.timeout }
      whenIsActive {
        sclDrive := True
        sdaEn    := False
        when(timerDone) {
          when(recovCtr === U(1)) {
            recovCtr := U(0)
            goto(STOP0)  // after BUS_RECOVERY_PULSES, issue STOP
          } otherwise {
            recovCtr := recovCtr - U(1)
            goto(RECOV_LO)
          }
        }
      }
    }

    val STOP0: State = new State {
      onEntry { timer := tSegFull; timerArmed := True; timeoutCtr := io.cfg.timeout }
      whenIsActive {
        sclDrive := False  // drop SCL to let slave release SDA
        sdaEn    := True   // drive SDA Low (open-drain), NOT release:
        sdaDrive := False  // SDA must be Low before SCL rises in STOP1, else the
                           // SCL-rise + SDA-fall looks like a spurious START to the slave.
        when(timerDone) { goto(STOP1) }
      }
    }

    val STOP1: State = new State {
      onEntry { timer := tSegQ; timerArmed := True; timeoutCtr := io.cfg.timeout }
      whenIsActive {
        sclDrive := True   // SCL rises, master drives SDA Low
        sdaEn    := True
        sdaDrive := False
        // Wait for the bus to actually be High before releasing SDA: if the
        // slave stretched the last bit, the wire is still Low and SDA rising
        // over Low is not a STOP. Bounded by the timeout watchdog.
        when(timerDone && sclFilt) { goto(STOP2) }
      }
    }

    val STOP2: State = new State {
      onEntry { timer := tSegFull; timerArmed := True; timeoutCtr := io.cfg.timeout }
      whenIsActive {
        sclDrive := True
        sdaEn    := True
        sdaDrive := True   // SDA rises while SCL High = STOP
        when(timerDone) { goto(TBUF) }
      }
    }

    val TBUF: State = new State {
      onEntry { timer := io.cfg.tBuf; timerArmed := True; timeoutCtr := io.cfg.timeout }
      whenIsActive {
        sclDrive := True
        sdaEn    := False
        when(timerDone) {
          // Bus is now truly idle: raise COMPLETE and release busy. This is the
          // single point where busy deasserts for the normal/STOP/recovery tails
          // (success AND error paths alike), so busy-low always coincides with the
          // FSM entering IDLE and a firmware re-issue is never silently dropped.
          when(transferOk) { intr.fl(0) := True }
          transferOk  := False
          status.busy := False
          goto(IDLE)
        }
      }
    }

    // Arbitration loss: passive exit (no STOP - we're no longer bus master)
    val ARB_LOSS: State = new State {
      onEntry { timer := tSegQ; timerArmed := True }
      whenIsActive {
        sclDrive := True   // RELEASE SCL (open-drain): holding it Low here would
                           // brick the bus for the winning master, which could
                           // no longer clock the transfer until reset.
        sdaEn    := False  // release SDA immediately
        when(timerDone) {
          status.busy := False  // bus idle: release busy (mirror of TBUF for the passive-exit path)
          goto(IDLE)
        }
      }
    }

    always {
      // Arbitration loss (UM10204 §3.1.10): we placed a '1' on SDA (sdaEn
      // && sdaDrive => this controller drives the released/High level for a
      // bit it owns) yet the bus reads Low — another master pulled it down.
      // Judged only in the SCL-High sampling window (sclFilt) to skip
      // setup-time transients. Read-phase data bits are exempt by construction:
      // there sdaEn=False (the slave owns the bit), so a slave-driven Low is
      // normal data, not a competitor. A Low on the master's own ACK/NACK
      // placement (read-ACK NACK) is a genuine loss.
      when(status.busy && inBitHigh && sdaEn && sdaDrive && sclFilt && !sdaFilt) {
        status.errCode := U(Err.ARB_LOSS, 3 bits)
        intr.fl(1)     := True
        timeoutCtr     := io.cfg.timeout  // disarm watchdog for the passive-exit tail
        // busy stays high; released in ARB_LOSS (bus idle). Uses sdaFilt (the
        // glitch-filtered wire, consistent with all other SDA sampling here)
        // rather than the raw sdaSync so a single-tick glitch cannot false-fire.
        goto(ARB_LOSS)  // passive exit: no STOP
      }
      when(timeoutFired && status.busy) {
        status.errCode := U(Err.TIMEOUT, 3 bits)
        intr.fl(1)     := True
        recovCtr       := U(BUS_RECOVERY_PULSES)  // initiate 9-pulse recovery
        // busy stays high through the 9-pulse recovery + STOP tail; released
        // in TBUF. The recovery SCL edges keep the watchdog alive (sclEdge reload).
        goto(RECOV_LO)
      }
      when(io.cfg.abort) {
        status.aborted := True
        intr.fl(1)     := True
        goto(STOP0)  // abort: no recovery needed (local request); busy released in TBUF
      }
    }

    // RX clock-stretching (Q6a): while clocking out a received data bit and the
    // RX FIFO is full, hold SCL Low to stall the bus (legal I2C clock
    // stretching) until firmware drains the FIFO. `inBitHigh` is True only in
    // BIT_HIGH, so this never touches the START/STOP/RESTART tails or the
    // address phase. Holding SCL Low reuses the existing `when(!sclFilt)
    // { timer := tHigh }` reload in BIT_HIGH, which freezes the advance (it
    // requires sclFilt) while the wire is held Low.
    val rxStretch = Bool()
    rxStretch := inBitHigh && curRw && !isAddrByte && !rxFifo.io.push.ready

    this.build()
  }

  // Busy-reject (errCode=5): a CMD issued while the bus is occupied — by our
  // own unfinished transfer (FSM not in IDLE, incl. the recovery tail)
  // or by external START-without-STOP — is rejected with the error
  // interrupt instead of being silently dropped (previous behaviour: a
  // trigger arriving during the recovery tail vanished with no errCode and
  // no interrupt).
  when(io.cmd.trigger && (!fsm.isActive(fsm.IDLE) || busBusyExt)) {
    status.errCode := U(Err.BUSY_REJECT, 3 bits)
    intr.fl(1)     := True
  }

  // Drive pins. While clock-stretching (rxStretch) we force SCL Low to stall
  // the bus; otherwise SCL follows the FSM drive intent.
  io.i2c.scl.write := fsm.sclDrive && !fsm.rxStretch
  io.i2c.sda.write := Mux(fsm.sdaEn, fsm.sdaDrive, True)

  // Keep the timeout watchdog alive during our own clock-stretch (SCL held
  // Low, no edges): without this the watchdog would expire mid-stretch and
  // falsely recover/abort a healthy transfer.
  when(fsm.rxStretch) {
    timeoutCtr := io.cfg.timeout
  }

  // Expose internals for simulation
  status.busy.simPublic(); status.errCode.simPublic(); status.aborted.simPublic()
  io.status.txFull.simPublic()
  txFifo.io.pop.valid.simPublic(); txFifo.io.pop.payload.simPublic(); txFifo.io.occupancy.simPublic()
  rxFifo.io.pop.valid.simPublic(); rxFifo.io.pop.payload.simPublic(); rxFifo.io.occupancy.simPublic()
  txPopIntent.simPublic(); rxPushIntent.simPublic(); rxPushData.simPublic(); rxPopIntent.simPublic()
  fsm.bitPos.simPublic(); fsm.byteNum.simPublic()
  val rxStretchObs = fsm.rxStretch   // test/debug tap: RX clock-stretch condition
  rxStretchObs.simPublic()
}
