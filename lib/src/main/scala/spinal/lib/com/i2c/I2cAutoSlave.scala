package spinal.lib.com.i2c

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, BusSlaveFactoryAddressWrapper}

/**
  * Fully-automatic I2C slave controller.
  *
  * Self-contained: does NOT use I2cSlave internally. Implements its own
  * START/STOP detection, bit counting, and SDA driving.
  *
  * The controller core is register-bus agnostic: it exposes the signals it
  * needs ([[I2cAutoSlaveIo]]) and leaves register-map instantiation to
  * [[I2cAutoSlave.driveFrom]], which binds a caller-supplied
  * [[BusSlaveFactory]] to parent-side registers wired into the io. Any bus
  * family whose slave factory extends BusSlaveFactory (Bmb, Apb3, TileLink, ...)
  * can serve the same register map.
  *
  * Register map (byte addresses, 32-bit data width; addrW = 7 or 10, ramAddrW = log2Up(ramDepth)):
  *   0x00  ADDR        RW: {matchEn[addrW], addr[addrW-1:0]}
  *   0x04  PTR         RW: data pointer (write loads the HW counter; read returns the live value)
  *   0x08  RAM_IND     W:  indirect RAM write: addr[ramAddrW-1:0] @ bits[ramAddrW-1:0],
  *                         data[7:0] @ bits[ramAddrW+7:ramAddrW]
  *   0x0C  STATUS      R:  {errNack[1], inFrame[0]}
  *   0x10  INT_EN      RW: {rxComplete[0]}
  *   0x14  INT_FLAG    R:  interrupt flags
  *   0x18  INT_CLR     W:  W1C
  *   0x1C  CFG_SAMPDIV W:  sampling window divider
  *   0x20  CFG_TIMEOUT W:  reserved (declared, unused in RTL)
  *   0x24  STRETCH_REQ RW: SCL stretch request (auto-cleared on STOP)
  */
object I2cAutoSlave {


  case class Config(
    ramDepth   : Int    = 64,
    validAddrs : Seq[Int] = (0 until 64).toSeq,
    support10bit: Boolean = false,
    sampWinSize: Int    = 3,
    sampDivWidth: Int   = 10
  )

  def apply(c: Config = Config()): I2cAutoSlave = new I2cAutoSlave(c)

  /**
   * Instantiate the register map on a caller-supplied bus factory and wire it
   * to the controller's io. Must be elaborated in a scope that can see both
   * the factory's bus and the io (typically the parent component).
   *
   * Event registers (PTR load, RAM_IND write) are one-cycle pulses derived
   * from bus write handshakes; value registers are plain registers. Pulse
   * timing matches the legacy internal BmbSlaveFactory implementation.
   */
  def driveFrom(io: I2cAutoSlaveIo, busCtrl: BusSlaveFactory, baseAddress: BigInt): Unit = new Area {
    import io._
    val factory  = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)
    val addrW    = cfg.slaveAddr.getWidth
    val ramAddrW = cfg.ptrVal.getWidth

    // ADDR (0x00): {matchEn[addrW], addr[addrW-1:0]}
    val addrReg    = Reg(Bits(addrW bits)) init(0)
    val matchEnReg = RegInit(True)
    factory.write(addrReg, 0x00, 0)
    factory.write(matchEnReg, 0x00, addrW)
    cfg.slaveAddr   := addrReg
    cfg.addrMatchEn := matchEnReg

    // PTR (0x04): value + one-cycle load pulse; live readback from the core
    val ptrVal  = Reg(UInt(ramAddrW bits)) init(0)
    val ptrLoad = RegInit(False)
    factory.write(ptrVal, 0x04)
    ptrLoad := False
    factory.onWrite(0x04) { ptrLoad := True }
    cfg.ptrVal  := ptrVal
    cfg.ptrLoad := ptrLoad
    factory.read(status.ptr, 0x04)

    // RAM_IND (0x08): value + one-cycle write pulse
    val ramWrAddr  = Reg(UInt(ramAddrW bits)) init(0)
    val ramWrData  = Reg(Bits(8 bits)) init(0)
    val ramWrValid = RegInit(False)
    factory.write(ramWrAddr, 0x08, 0)
    factory.write(ramWrData, 0x08, ramAddrW)
    ramWrValid := False
    factory.onWrite(0x08) { ramWrValid := True }
    cfg.ramWrAddr  := ramWrAddr
    cfg.ramWrData  := ramWrData
    cfg.ramWrValid := ramWrValid

    // STATUS (0x0C): {errNack[1], inFrame[0]}
    factory.read(0x0C, 0 -> status.inFrame, 1 -> status.errNack)

    // Interrupts
    val intEn  = RegInit(B(0, 2 bits))
    val intClr = RegInit(B(0, 2 bits))
    factory.write(intEn, 0x10)
    factory.read(intr.fl, 0x14)
    factory.write(intClr, 0x18)
    intr.en  := intEn
    intr.clr := intClr

    // Config
    val sampDiv = Reg(UInt(cfg.sampDiv.getWidth bits)) init(0)
    val timeout = Reg(UInt(20 bits)) init(0)
    factory.write(sampDiv, 0x1C)
    factory.write(timeout, 0x20)
    cfg.sampDiv := sampDiv
    cfg.timeout := timeout

    // STRETCH_REQ (0x24): level, auto-cleared on STOP (pulse from the core)
    val stretchReq = RegInit(False)
    factory.readAndWrite(stretchReq, 0x24)
    stretchReq.clearWhen(status.stretchClr)
    cfg.stretchReq := stretchReq
  }
}

/** Controller io: I2C pins, IRQ, SCL stretch, and the firmware-facing control/status signals bound by driveFrom. */
case class I2cAutoSlaveIo(c: I2cAutoSlave.Config) extends Bundle {
  val i2c = slave(I2c())
  val irq = out Bool()
  val sclStretch = out Bool()  // Active when slave is stretching SCL

  val cfg = new Bundle {
    val slaveAddr   = in Bits((if (c.support10bit) 10 else 7) bits)
    val addrMatchEn = in Bool()
    val ptrVal      = in UInt(log2Up(c.ramDepth) bits)  // PTR load value
    val ptrLoad     = in Bool()                          // one-cycle load pulse
    val ramWrAddr   = in UInt(log2Up(c.ramDepth) bits)
    val ramWrData   = in Bits(8 bits)
    val ramWrValid  = in Bool()                          // one-cycle RAM write pulse
    val sampDiv     = in UInt(c.sampDivWidth bits)
    val timeout     = in UInt(20 bits)                   // reserved (unused in RTL)
    val stretchReq  = in Bool()                          // level: set by firmware, auto-cleared on STOP
  }

  val intr = new Bundle {
    val en  = in Bits(2 bits)
    val fl  = out Bits(2 bits)
    val clr = in Bits(2 bits)  // W1C
  }

  val status = new Bundle {
    val inFrame    = out Bool()
    val errNack    = out Bool()
    val ptr        = out UInt(log2Up(c.ramDepth) bits)  // live pointer readback
    val stretchClr = out Bool()                          // one-cycle pulse on STOP (releases stretch)
  }

  /** Bind the register map to a bus factory (see [[I2cAutoSlave.driveFrom]]). */
  def driveFrom(busCtrl: BusSlaveFactory, baseAddress: BigInt): Unit =
    I2cAutoSlave.driveFrom(this, busCtrl, baseAddress)
}

class I2cAutoSlave(c: I2cAutoSlave.Config) extends Component {

  import I2cAutoSlave.Config

  val io = I2cAutoSlaveIo(c)

  val ramAddrW = log2Up(c.ramDepth)

  // === Input filtering ===
  val filterTick = new Area {
    val counter = Reg(UInt(c.sampDivWidth bits)) init(0)
    val tick    = counter === U(0)
    counter := Mux(tick, io.cfg.sampDiv, counter - U(1))
  }

  val sclFilt = RegInit(True)
  val sdaFilt = RegInit(True)
  val sclHist = RegInit(B(1, c.sampWinSize bits))
  val sdaHist = RegInit(B(1, c.sampWinSize bits))

  when(filterTick.tick) {
    sclHist := Cat(io.i2c.scl.write, sclHist(c.sampWinSize - 2 downto 0))
    sdaHist := Cat(io.i2c.sda.write, sdaHist(c.sampWinSize - 2 downto 0))
    when(sclHist.andR || sclHist.orR) { sclFilt := io.i2c.scl.write }
    when(sdaHist.andR || sdaHist.orR) { sdaFilt := io.i2c.sda.write }
  }

  // === Edge detection ===
  // Raw edges for bit timing (zero delay)
  val sclRawPrev = RegInit(True)
  val sclRise = io.i2c.scl.write && !sclRawPrev
  val sclFall = !io.i2c.scl.write && sclRawPrev
  sclRawPrev := io.i2c.scl.write

  // START/STOP detection on RAW signals (fast enough for 2-cycle RESTART)
  val sdaRaw = io.i2c.sda.write
  val sdaRawPrev = RegInit(True)
  val sdaFallWhileSclHigh = !sdaRaw && sdaRawPrev && io.i2c.scl.write  // START
  val sdaRiseWhileSclHigh = sdaRaw && !sdaRawPrev && io.i2c.scl.write  // STOP
  sdaRawPrev := sdaRaw

  // === Slave address & config ===
  val addrW: Int = if (c.support10bit) 10 else 7
  val addrBytes: Int = if (c.support10bit) 2 else 1

  // 10-bit address: buffer upper 3 bits from first byte
  val tenBitUpper = Reg(Bits(3 bits)) init(0)
  val addrByteIdx = Reg(UInt(1 bits)) init(0)  // 0=first addr byte, 1=second
  tenBitUpper.allowUnsetRegToAvoidLatch
  addrByteIdx.allowUnsetRegToAvoidLatch

  // === Data pointer (hardware-owned; firmware loads via ptrLoad) ===
  val dataPtr = Reg(UInt(ramAddrW bits)) init(0)

  // === Internal RAM ===
  val ram = Mem(Bits(8 bits), c.ramDepth)

  // Indirect RAM write (one-cycle pulse from the RAM_IND register)
  when(io.cfg.ramWrValid) {
    ram.write(io.cfg.ramWrAddr, io.cfg.ramWrData)
  }

  // === Frame state ===
  val inFrame   = RegInit(False)
  val isRead    = RegInit(False)
  val bitPos    = RegInit(U(0, 4 bits))
  val rxAccum   = Reg(Bits(8 bits)) init(0)
  val addrValid = RegInit(False)
  val hasStarted = RegInit(False)  // True after address+ACK complete
  val inAck     = RegInit(False)  // True when next SCL rise is the ACK bit
  // Set when the master NACKs a read byte: the slave must release SDA and
  // wait for STOP. Without this, the slave re-arms the next data bit after
  // the NACK; if that bit is Low it holds SDA Low through the master's STOP
  // sequence, masking the STOP edge (open-drain) so the slave never exits
  // the frame and desynchronizes on the next transfer.
  val masterNack = RegInit(False)
  // Set when a read data byte completes (sclFall, bitPos==8, isRead) and we
  // have released SDA; the master's ACK/NACK for that byte is then sampled on
  // the FOLLOWING sclRise (inAck), when the open-drain line is purely
  // master-driven and stable. Sampling at the byte-complete fall itself would
  // read the line while we are still driving our own last data bit, masking a
  // master NACK (release) whenever that bit is Low.
  val readAckPending = RegInit(False)

  // === Response: what to drive on SDA ===
  // True = release (high-Z in open-drain), False = drive low
  val sdaOut = RegInit(True)

  // Independent latch: set when address matches, cleared on STOP.
  // Lives in its own when-block to avoid SpinalHDL elaboration conflicts
  // with the main SCL edge handler.
  val wasAddressed = RegInit(False)

  // === Interrupts ===
  val intr = new Area {
    val fl = RegInit(B(0, 2 bits))
    for (i <- 0 until 2) { fl(i) clearWhen(io.intr.clr(i)) }
  }
  io.irq := (io.intr.en & intr.fl).orR

  val statusErrNack = RegInit(False).allowUnsetRegToAvoidLatch

  // Helper: is the current pointer address valid?
  def ptrValid(p: UInt): Bool = {
    if (c.validAddrs.size == c.ramDepth) True
    else {
      val terms = c.validAddrs.map(idx => (p === U(idx, ramAddrW bits)))
      if (terms.isEmpty) False else terms.reduce(_ || _)
    }
  }

  // === Main state machine (event-driven on filtered edges) ===

  // START detection
  when(sdaFallWhileSclHigh) {
    inFrame    := True
    isRead     := False
    bitPos     := U(0)
    rxAccum    := B(0)
    addrValid  := False
    hasStarted := False
    inAck      := False
    masterNack := False
    readAckPending := False
    statusErrNack := False
    sdaOut     := True  // release during address phase
    if (c.support10bit) { addrByteIdx := U(0) }
  }

  // STOP detection
  when(sdaRiseWhileSclHigh) {
    when(inFrame) {
      intr.fl(0) := True  // transfer complete
    }
    inFrame    := False
    hasStarted := False
    bitPos     := U(0)
    inAck      := False
    masterNack := False
    readAckPending := False
    sdaOut     := True
    wasAddressed := False
  }

  // Independent wasAddressed latch: set on address match (own when-block)
  when(sclFall && inFrame && !hasStarted && bitPos === U(8)) {
    if (c.support10bit) {
      when(addrByteIdx === U(1)) {
        val fullAddr = Cat(tenBitUpper, rxAccum(7 downto 1))
        wasAddressed := (fullAddr === io.cfg.slaveAddr)
      }
    } else {
      wasAddressed := (rxAccum(7 downto 1) === io.cfg.slaveAddr)
    }
  }

  // SCL rising edge: sample SDA, advance bit position
  when(sclRise && inFrame) {
    when(inAck) {
      // ACK bit. In read mode this is the master's data-ACK/NACK: we released
      // SDA on the preceding fall, so the wire is purely master-driven and
      // stable here. Sample it to decide whether to send the next byte.
      when(readAckPending) {
        // The byte was transferred either way (the master received it), so the
        // pointer advances on ACK and NACK alike — symmetric with the write
        // phase, which also increments unconditionally. Incrementing only on
        // ACK made a following write phase (read->write composite) start one
        // slot early after a NACKed last read byte.
        dataPtr := dataPtr + U(1)
        when(io.i2c.sda.write) {
          // High = master NACKed: release and stop driving data. The master
          // will issue STOP or RESTART; we must not hold SDA Low (would mask
          // the STOP/START edge on the open-drain bus).
          masterNack := True
          sdaOut     := True
        }
        readAckPending := False
      }
      inAck  := False
      bitPos := U(0)
    } otherwise {
      when(!hasStarted) {
        // Receiving address byte
        rxAccum(7 - bitPos(2 downto 0)) := io.i2c.sda.write
        bitPos := bitPos + U(1)
      } otherwise {
        // Data phase
        when(!isRead) {
          // Write: sample data bit
          rxAccum(7 - bitPos(2 downto 0)) := io.i2c.sda.write
        }
        bitPos := bitPos + U(1)
      }
    }
  }

  // SCL falling edge: set up response for next bit
  when(sclFall && inFrame) {
    when(!hasStarted) {
      when(bitPos === U(8)) {
        // All 8 address bits received (SCL fell after 8th data bit)
        if (c.support10bit) {
          when(addrByteIdx === U(0)) {
            // First 10-bit address byte: buffer upper bits, ACK, wait for second
            tenBitUpper := rxAccum(2 downto 0)  // A[9:7]
            addrByteIdx := U(1)
            inAck       := True
            bitPos      := U(0)
            rxAccum     := B(0)
            sdaOut      := !io.cfg.addrMatchEn  // ACK if enabled, NACK if disabled
          } otherwise {
            // Second 10-bit address byte: full match
            val fullAddr = Cat(tenBitUpper, rxAccum(7 downto 1))  // A[9:0]
            val recvRw   = rxAccum(0)
            val matchNow = (fullAddr === io.cfg.slaveAddr)
            isRead    := recvRw
            addrValid := matchNow
            hasStarted := True
            inAck      := True
            bitPos     := U(0)
            sdaOut     := !(matchNow && io.cfg.addrMatchEn)
            when(!(matchNow && io.cfg.addrMatchEn)) { statusErrNack := True; intr.fl(1) := True }
          }
        } else {
          // 7-bit: single address byte
          val recvAddr = rxAccum(7 downto 1)
          val recvRw   = rxAccum(0)
          val matchNow = (recvAddr === io.cfg.slaveAddr)
          isRead    := recvRw
          addrValid := matchNow
          hasStarted := True
          inAck      := True
          bitPos     := U(0)
          sdaOut     := !(matchNow && io.cfg.addrMatchEn)
          when(!(matchNow && io.cfg.addrMatchEn)) { statusErrNack := True; intr.fl(1) := True }
        }
      } otherwise {
        // Between address bits: release SDA (master is driving data)
        sdaOut := True
      }
    } otherwise {
      when(bitPos === U(8)) {
        // Data byte complete (8 bits received)
        when(!isRead) {
          // Write: store byte, advance pointer, prepare ACK.
          // Multi-slave isolation: only the addressed slave drives the ACK;
          // non-addressed slaves release SDA (cannot mask the addressed
          // slave's NACK on the shared bus).
          when(wasAddressed) {
            when(ptrValid(dataPtr)) {
              ram.write(dataPtr, rxAccum)
            }
            dataPtr := dataPtr + U(1)
            sdaOut  := !ptrValid(dataPtr)
          } otherwise {
            sdaOut := True
          }
        } otherwise {
          // Read: release SDA NOW so the master's ACK/NACK is not masked by
          // our own last data bit (open-drain AND). The actual sample is taken
          // on the following sclRise (see readAckPending), when the line is
          // purely master-driven and stable.
          sdaOut         := True
          readAckPending := True
        }
        inAck   := True  // next SCL rise is data ACK
        bitPos  := U(0)
        rxAccum := B(0)
      } otherwise {
        // Setup SDA for next bit (during SCL low), gated by wasAddressed
        // and masterNack (after a NACK the slave must release the bus).
        when(isRead && !inAck && wasAddressed && !masterNack) {
          sdaOut := ram.readAsync(dataPtr)(7 - bitPos(2 downto 0))
        } otherwise {
          sdaOut := True
        }
      }
    }
  }

  // Firmware PTR load: registered after the HW increment above so it wins in
  // the same cycle (matches the legacy register-write precedence).
  when(io.cfg.ptrLoad) {
    dataPtr := io.cfg.ptrVal
  }

  // Drive physical SDA output (what this slave contributes to the wire)
  // Multi-slave isolation: data-phase SDA is gated by wasAddressed.
  // Non-addressed slaves release SDA (High) during data phase.
  io.i2c.sda.read := sdaOut
  // SCL stretching: firmware requests via the STRETCH_REQ register; the
  // actual hold (stretchActive) is deferred to the SCL-Low phase. If the
  // request is asserted while SCL is High, engaging immediately would
  // truncate the High window: the slave has already sampled the rising edge
  // (bitPos advanced) but the master's timer-based bit completion
  // (timerDone && sclFilt) would be aborted, desynchronizing the two bit
  // counters. Deferring to the next Low phase keeps every SCL edge seen by
  // both sides identical, so mid-byte stretching resumes cleanly.
  val stretchActive = RegInit(False)
  when(!io.cfg.stretchReq) {
    stretchActive := False
  } otherwise {
    // Engage only while the bus SCL is Low (hold it there until released)
    when(!io.i2c.scl.write) { stretchActive := True }
  }
  stretchActive.simPublic()
  io.i2c.scl.read := !stretchActive
  io.sclStretch := stretchActive

  // Auto-clear pulse for the parent STRETCH_REQ register on STOP
  io.status.stretchClr := sdaRiseWhileSclHigh

  // Forward status to the io
  io.status.inFrame := inFrame
  io.status.errNack := statusErrNack
  io.status.ptr     := dataPtr
  io.intr.fl        := intr.fl

  // Expose internals for simulation
  dataPtr.simPublic()
  ram.simPublic()
  tenBitUpper.simPublic()
  addrByteIdx.simPublic()
  hasStarted.simPublic()
  inFrame.simPublic()
  sdaOut.simPublic()
  bitPos.simPublic()
  inAck.simPublic()
  rxAccum.simPublic()
  wasAddressed.simPublic()
  isRead.simPublic()
}
