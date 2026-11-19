package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}

/**
  * I2C bus engine: turns a stream of transfer requests into I2C bus
  * waveforms and vice versa.
  *
  * This is the pure bus-facing half of the automatic I2C master. It knows
  * nothing about register maps or hosts: it consumes one transfer
  * descriptor per request (io.req), exchanges data bytes over the
  * io.txData / io.rxData seams, and reports protocol status plus
  * one-cycle evtComplete / evtError pulses. Byte storage and the interrupt
  * controller (mask / W1C / IRQ line) live in the host adapter
  * ([[I2cAutoMaster]]); the controller can equally be driven from any
  * other host logic or a testbench.
  *
  * Timing / electrical assumptions:
  *  - Single clock domain; the open-drain SCL/SDA wires are sampled through
  *    2-stage synchronizers (they originate in external chip domains).
  *  - Active-Low open-drain: `io.i2c.scl.write = False` pulls the wire Low,
  *    True releases it (external pull-up drives it High).
  *
  * Request semantics: io.req is a Stream, but the host drives req.valid as
  * a ONE-CYCLE pulse (an event, not a held request). If ready is low on
  * that cycle the request is dropped: the host must raise io.reject for
  * exactly that cycle and the engine records errCode = Err.BUSY_REJECT.
  * We deliberately do not hold valid until accepted — queueing a transfer
  * would break the "CMD write is an event" contract of the CSR front-end.
  *
  * Host data contract: present >= byteCount TX data bytes (plus
  * p2ByteCount more for phase 2) on io.txData before and during the
  * transfer — the controller pulls one byte at each data-byte boundary.
  * A TX underflow (demand while txData.valid is low) is caught by
  * simulation asserts only; in synthesis the last shifted-out byte
  * repeats without raising errCode. The host typically buffers the bytes
  * in a FIFO and polls its own occupancy for room.
  */
object I2cAutoController {


  case class Config(
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
  /** Bits per I2C data/address byte; the ACK/NACK bit is bit index BITS_PER_BYTE. */
  val BITS_PER_BYTE = 8

  def apply(c: Config = Config()): I2cAutoController = new I2cAutoController(c)

}

/**
  * One I2C transfer request from the host: the transaction-level view of a
  * CMD register write (address, direction, phase-2, byte counts).
  */
case class I2cAutoControllerReq(addrW: Int) extends Bundle {
  val addr        = Bits(addrW bits)
  val rw          = Bool()
  val phase2      = Bool()
  val nackLast    = Bool()
  val byteCount   = UInt(8 bits)
  val p2ByteCount = UInt(8 bits)
  val p2Addr      = Bits(addrW bits)
  val p2AddrValid = Bool()
}

/** Controller io: I2C pins, the host-facing transaction request, byte
  * seams, protocol status, and one-cycle event pulses.
  *
  * The host owns the byte-storage FIFOs (host buffering is a host concern):
  * io.txData / io.rxData are the byte seams. Interrupt mask / W1C / the IRQ
  * line live in the host adapter ([[I2cAutoMaster]]), fed by the evt pulses.
  */
case class I2cAutoControllerIo(c: I2cAutoController.Config) extends Bundle {
  val i2c = master(I2c())

  // Transaction-level request. The host drives valid as a one-cycle pulse;
  // ready low on that cycle => dropped request + host raises io.reject.
  val req = slave(Stream(I2cAutoControllerReq(if (c.support10bit) 10 else 7)))
  val reject = in Bool()  // one-cycle pulse: request dropped (busy)

  // Byte seams (the host's FIFOs sit on the other side):
  // txData — the controller PULLS the next TX byte when it needs it
  //          (ready = demand; at most one byte per cycle).
  // rxData — the controller PUSHES each received byte (registered
  //          producer); the host must absorb at least as fast as the bus
  //          delivers. The host adapter's RX FIFO does; while it is full
  //          the controller stalls the bus via legal clock-stretching.
  val txData = slave(Stream(Bits(8 bits)))
  val rxData = master(Stream(Bits(8 bits)))

  val status = new Bundle {
    val busy    = out Bool()
    val errCode = out UInt(3 bits)
    val aborted = out Bool()
  }

  // One-cycle event pulses for the host's interrupt controller.
  val evtComplete = out Bool()  // transfer finished OK (raised at the TBUF exit)
  val evtError    = out Bool()  // an error was recorded (NACK / timeout / arb-loss / abort / busy-reject)

  val cfg = new Bundle {
    val sampDiv = in UInt(c.sampDivWidth bits)
    val timeout = in UInt(I2cAutoController.TIMEOUT_W bits)
    val tLow    = in UInt(c.timerWidth bits)
    val tHigh   = in UInt(c.timerWidth bits)
    val tBuf    = in UInt(c.timerWidth bits)
    val abort   = in Bool()  // one-cycle pulse from the CTRL register write
  }
}

/**
  * I2C bus engine (see file header for the host-facing contract).
  *
  * Internals: bus conditioning (2FF sync + unanimous-window glitch filter),
  * timeout watchdog, external-bus tracker, and the transfer FSM
  * (START / address / data / RESTART / STOP / recovery). Byte storage is
  * the host's job (io.txData / io.rxData seams).
  */
class I2cAutoController(c: I2cAutoController.Config) extends Component {
  require(c.sampWinSize >= 2,
    s"I2cAutoController.Config.sampWinSize must be >= 2 (got ${c.sampWinSize}): " +
    "the glitch-filter history window needs at least 2 samples")

  import I2cAutoController.{Config, Err, BUS_RECOVERY_PULSES, TIMEOUT_W, DEFAULT_SEG, BITS_PER_BYTE}

  val io = I2cAutoControllerIo(c)

  // === Input conditioning (#5, #6) ===
  // Two-FF synchronization of the open-drain bus wires: they originate in an
  // external clock domain (another chip's driver). Feeding raw metastable
  // signals into history/edge logic is the classic multi-chip hazard.
  // Precedent: I2cIoFilter in Misc.scala.
  //
  val busFilter = new I2cWireConditioner(io.i2c.scl.read, io.i2c.sda.read, io.cfg.sampDiv, c.sampWinSize, c.sampDivWidth)
  import busFilter.{sclFilt, sdaFilt}

  // (The timeout watchdog lives further down, grouped with its SCL-edge
  //  reload trigger so all timeout logic sits in one place.)


  // === Byte seams (io.txData / io.rxData) ===
  // Byte STORAGE lives in the host (see I2cAutoMaster's FIFO stage); this
  // controller only produces demand and consumes supply.
  //
  // TX: txDemand is a combinational "next TX byte needed now", set by the
  // FSM at each data-byte boundary. io.txData.ready = txDemand, so a fire
  // captures the host's byte into txByte in the SAME cycle the pre-seam
  // design read the FIFO head — timing is bit-identical (no wait-state was
  // added). If the host has no byte (valid low) the fire does not happen
  // and txByte keeps its old value: the exact silent-stale-byte behaviour
  // the pre-seam head-capture had on an empty FIFO, caught in simulation by
  // the assert below. CONTRACT: the host MUST present >= byteCount TX bytes
  // (plus p2ByteCount for phase 2) before/during the transfer.
  val txByte    = Reg(Bits(8 bits)) init(0)  // next byte to shift out (host-fed)
  val txDemand  = Bool()
  txDemand := False  // default; the FSM sets True on each byte-boundary need
  io.txData.ready := txDemand
  when(io.txData.fire) { txByte := io.txData.payload }
  assert(!(txDemand && !io.txData.valid), "tx underflow: byte demanded but host has none")

  // RX: the FSM fills rxByte bit-by-bit; rxByteReady pulses (combinationally)
  // when a received byte completes. The registered producer below mirrors
  // the pre-seam registered push-intent timing: valid goes high one cycle
  // after completion, payload is a snapshot of rxByte taken at that edge
  // (rxByte itself resets to 0 the same cycle, so the snapshot register is
  // mandatory — mirroring the old rxPushData). A byte completing while the
  // previous one is still pending and not absorbed this cycle would be
  // silently dropped: the assert below makes that loud in simulation.
  val rxByte      = Reg(Bits(8 bits)) init(0)  // received-byte accumulator (FSM fills)
  val rxByteReady = Bool()
  rxByteReady := False  // default; the FSM sets True when a byte completes
  val rxDataValid   = RegInit(False)
  val rxDataPayload = Reg(Bits(8 bits)) init(0)
  when(io.rxData.fire) { rxDataValid := False }
  when(rxByteReady) {
    rxDataValid   := True
    rxDataPayload := rxByte
  }
  io.rxData.valid   := rxDataValid
  io.rxData.payload := rxDataPayload
  assert(!(rxByteReady && rxDataValid && !io.rxData.fire),
         "rx overflow: byte completed while the previous one is still pending")

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

  // === Timeout watchdog ===
  // Counts down each cycle; timeoutFired pulses on the 1->0 crossing.
  // Reloaded on any SCL edge or while the bus is idle (below), and the FSM
  // pokes timeoutCtr directly on state entries / the stretch keep-alive.
  val watchdog = new Area {
    val timeoutFired = RegInit(False)
    val timeoutCtr   = Reg(UInt(TIMEOUT_W bits)) init(0)
    when(timeoutCtr =/= U(0)) {
      timeoutCtr := timeoutCtr - U(1)
      timeoutFired := timeoutCtr === U(1)
    } otherwise {
      timeoutFired := False
    }

    // Reload on SCL edge or when not busy (keeps the watchdog alive during
    // normal activity and idle).
    val sclPrev = RegInit(True)
    val sclEdge = sclFilt =/= sclPrev
    sclPrev := sclFilt
    when(sclEdge || !status.busy) {
      timeoutCtr := io.cfg.timeout
    }
  }
  import watchdog.{timeoutFired, timeoutCtr}

  // === Protocol events ===
  // One-cycle combinational pulses; the host adapter latches them into its
  // sticky interrupt flags and owns mask / W1C / the IRQ line.
  io.evtComplete := False
  io.evtError    := False

  // Forward internal protocol status to the io (TX/RX availability and
  // fullness now derive from the host's FIFOs — see I2cAutoMaster).
  io.status.busy    := status.busy
  io.status.errCode := status.errCode
  io.status.aborted := status.aborted

  // === External-bus occupancy tracker (§3.1.10 UM10204) ===
  // A master must not initiate a START while the bus is busy. Track external
  // occupancy from the filtered wires while we are NOT driving (our own edges
  // would otherwise feed back): a START sets busy, a STOP clears it. While
  // status.busy (our own transfer owns the wires) the tracker is frozen.
  val extBusy = new Area {
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
  }
  val busBusyExt = extBusy.busBusyExt  // white-box: FSM + testbench observe external-bus occupancy
  import extBusy.{busSeenOnce}

  // === Transfer FSM ===

  val fsm = new StateMachine {

    // Timer with armed flag to avoid spurious timerDone on entry cycle
    val timer      = Reg(UInt(c.timerWidth bits)) init(0)
    val timerArmed = RegInit(False)
    val timerDone  = timerArmed && timer === U(0)

    // 9-pulse recovery counter
    val recovCtr   = Reg(UInt(4 bits)) init(0)

    // --- #10: START/STOP setup-hold segments derived from the bit timing ---
    // The I2C spec's START/STOP setup/hold minima (UM10204 Table 25, e.g.
    // TSU:STA >= 4.7 us and TSU:STO >= 4.0 us in Standard Mode) map onto the
    // FSM segments as follows:
    //   TSU:STA = START1      = tSegQ          (SCL High before SDA falls)
    //   THD:STA = START3      = tLow           (SDA Low  after  SCL falls)
    //   TSU:STO = STOP1       = tSegQ          (SCL High before SDA rises)
    //   THD:STO = STOP0+STOP1 = tSegFull+tSegQ (SDA Low  after  SCL falls)
    // tSegQ = max(tLow/4, 1) keeps the FSM snappy when tLow is tiny
    // (simulation), but it GOVERNS TSU:STA/TSU:STO: a fresh START right after
    // reset/recovery and every Repeated START only enjoy >= tSegQ of SCL-High
    // setup (a START preceded by a STOP additionally gains the tBuf bus-free
    // window). Firmware must therefore size tLow so that tSegQ in time
    // (tSegQ ticks x sampDiv x clk period) meets the target speed mode's
    // TSU:STA/TSU:STO minima - see README "Timing compliance". No fixed
    // tick floor is applied in hardware: the spec minima are time-based and
    // depend on clk/sampDiv, which the RTL intentionally leaves to firmware.
    // Sum wraps at 2^timerWidth, which merely caps the segment at a plausible
    // maximum.
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
    val latP2ByteCount     = Reg(UInt(8 bits)) init(0)
    val latP2Addr      = Reg(Bits(addrW bits)) init(0)
    val latP2AddrValid = RegInit(False)

    // Derived
    val dataStart    = U(addrBytes, 9 bits)
    val isAddrByte   = byteNum < dataStart
    val isLastAddrByte = byteNum === U(addrBytes - 1, 8 bits)
    val isAckBit     = bitPos === U(BITS_PER_BYTE)
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
    // Host request acceptance: ready is high only when the FSM is idle AND
    // no external transfer occupies the bus. Driven combinationally below
    // the FSM; the IDLE state latches on req.fire (see class doc for the
    // fire-and-drop / reject convention).
    io.req.ready := !status.busy && !busBusyExt

    val IDLE: State = new State with EntryPoint {
      whenIsActive {
        when(io.req.fire) {
          busSeenOnce := True  // arm external-bus tracker from now on
          latAddr     := io.req.payload.addr
          latRw       := io.req.payload.rw
          latHasP2    := io.req.payload.phase2
          latNackLast := io.req.payload.nackLast
          latP2ByteCount     := io.req.payload.p2ByteCount
          latP2Addr      := io.req.payload.p2Addr
          latP2AddrValid := io.req.payload.p2AddrValid
          phase   := U(0)
          byteNum := U(0)
          bitPos  := U(0)
          curLen  := io.req.payload.byteCount
          curRw   := io.req.payload.rw
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
                  io.evtError      := True
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
                      curLen  := latP2ByteCount
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
                      txDemand := True  // pull the first data byte from the host
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
                rxByteReady := True  // byte complete: publish via io.rxData
              }
              when(!curRw && sdaFilt) {  // Data NACK: use direct signal
                status.errCode := U(Err.DATA_NACK, 3 bits)
                io.evtError      := True
                // busy stays high through the STOP tail; released in TBUF.
                goto(STOP0)
              } otherwise {
                when(isLastByte) {
                  when(latHasP2 && phase === U(0)) {
                    phase   := U(1)
                    byteNum := U(0)
                    bitPos  := U(0)
                    curLen  := latP2ByteCount
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
                    txDemand := True  // pull the next data byte from the host
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
          when(transferOk) { io.evtComplete := True }
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
      // Precedence when several handlers fire in the SAME cycle: these are
      // stacked `when`s in one scope, so by last-effective-law the later one
      // wins: abort > timeout > arbitration-loss (both errCode and goto take
      // the last write). That ordering is intentional: a local abort request
      // must outrank external bus events.
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
        io.evtError      := True
        timeoutCtr     := io.cfg.timeout  // disarm watchdog for the passive-exit tail
        // busy stays high; released in ARB_LOSS (bus idle). Uses sdaFilt (the
        // glitch-filtered wire, consistent with all other SDA sampling here)
        // rather than the raw sdaSync so a single-tick glitch cannot false-fire.
        goto(ARB_LOSS)  // passive exit: no STOP
      }
      when(timeoutFired && status.busy) {
        status.errCode := U(Err.TIMEOUT, 3 bits)
        io.evtError      := True
        recovCtr       := U(BUS_RECOVERY_PULSES)  // initiate 9-pulse recovery
        // busy stays high through the 9-pulse recovery + STOP tail; released
        // in TBUF. The recovery SCL edges keep the watchdog alive (sclEdge reload).
        goto(RECOV_LO)
      }
      when(io.cfg.abort) {
        status.aborted := True
        io.evtError      := True
        goto(STOP0)  // abort: no recovery needed (local request); busy released in TBUF
      }
    }

    // RX clock-stretching (Q6a): while clocking out a received data bit and
    // the host has not absorbed the pending received byte (io.rxData not
    // ready — in the CSR adapter this means the host RX FIFO is full), hold
    // SCL Low to stall the bus (legal I2C clock stretching) until the host
    // drains. `inBitHigh` is True only in BIT_HIGH, so this never touches
    // the START/STOP/RESTART tails or the address phase. Holding SCL Low
    // reuses the existing `when(!sclFilt) { timer := tHigh }` reload in
    // BIT_HIGH, which freezes the advance (it requires sclFilt) while the
    // wire is held Low.
    val rxStretch = Bool()
    rxStretch := inBitHigh && curRw && !isAddrByte && !io.rxData.ready

    this.build()
  }

  // Busy-reject (errCode=5): the host raised io.reject because a request
  // arrived while the bus was occupied — by our own unfinished transfer
  // (FSM not in IDLE, incl. the recovery tail) or by external
  // START-without-STOP. Recorded here as an error + interrupt instead of
  // being silently dropped (previous behaviour: a trigger arriving during
  // the recovery tail vanished with no errCode and no interrupt).
  when(io.reject) {
    status.errCode := U(Err.BUSY_REJECT, 3 bits)
    io.evtError      := True
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
}

/**
  * Sliding-window glitch filter for I2C bus wires.
  *
  * Samples the raw (externally-driven) SCL/SDA through a 2-FF synchronizer,
  * then advances a `sampWinSize`-bit history register every `sampDiv` clocks.
  * The filtered output updates only when the ENTIRE window is unanimous
  * (`andR || nandR`), guaranteeing that a transient narrower than the
  * window cannot propagate.
  *
  * NOTE: This is NOT timing-equivalent to [[I2cIoFilter]] (used by the legacy
  * I2CSlave/I2cCtrl family). The window indexing and assigned value differ by
  * 2 ticks. Do not substitute one for the other.
  */
class I2cWireConditioner(
  sclRaw: Bool,
  sdaRaw: Bool,
  sampDiv: UInt,
  sampWinSize: Int,
  sampDivWidth: Int
) extends Area {
  val sclSync = BufferCC(sclRaw, 2)
  val sdaSync = BufferCC(sdaRaw, 2)

  val timer = new Area {
    val counter = Reg(UInt(sampDivWidth bits)) init(0)
    val tick    = counter === U(0)
    counter := Mux(tick, sampDiv, counter - U(1))
  }

  val sclFilt = RegInit(True)
  val sdaFilt = RegInit(True)
  val sclHist = RegInit(~U(0, sampWinSize bits).asBits)
  val sdaHist = RegInit(~U(0, sampWinSize bits).asBits)

  when(timer.tick) {
    sclHist := Cat(sclSync, sclHist(sampWinSize - 2 downto 0))
    sdaHist := Cat(sdaSync, sdaHist(sampWinSize - 2 downto 0))
    when(sclHist.andR || sclHist.nandR) { sclFilt := sclSync }
    when(sdaHist.andR || sdaHist.nandR) { sdaFilt := sdaSync }
  }
}
