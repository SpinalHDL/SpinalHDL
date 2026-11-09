package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, BusSlaveFactoryAddressWrapper}

/**
  * Fully-automatic I2C slave controller.
  *
  * Self-contained: does NOT use I2cSlave internally. Implements its own
  * START/STOP detection, bit counting, and SDA driving.
  *
  * Config.validAddrs is the single source of truth for the address space: physical RAM depth
  * equals the number of distinct entries and every supported address owns exactly one storage
  * slot (compact mapping, no holes). Pointer values seen by firmware are always bus addresses.
  *
  * The controller core is register-bus agnostic: it exposes the signals it
  * needs ([[I2cAutoSlaveIo]]) and leaves register-map instantiation to
  * [[I2cAutoSlave.driveFrom]], which binds a caller-supplied
  * [[BusSlaveFactory]] to parent-side registers wired into the io. Any bus
  * family whose slave factory extends BusSlaveFactory (Bmb, Apb3, TileLink, ...)
  * can serve the same register map.
  *
  * Register map (byte addresses, 32-bit data width; addrW = 7 or 10, ptrW = Config.fwAddrW):
  *   0x00  ADDR        RW: {matchEn[addrW], addr[addrW-1:0]}
  *   0x04  PTR         RW: data pointer (write loads the HW counter; read returns the live value).
  *                       Value space = the supported address set (Config.validAddrs). A load of an
  *                       address outside the set is REJECTED (pointer unchanged) and sets
  *                       STATUS.errNack + INT_FLAG[1]. Hardware-owned; walks the set in a ring.
  *   0x08  RAM_IND     W:  indirect RAM write: addr @ bits[ptrW-1:0], data @ bits[ptrW+7:ptrW];
  *                         an addr outside the supported set is ignored.
  *   0x0C  STATUS      R:  {errNack[1], inFrame[0]}
  *   0x10  INT_EN      RW: {errNack[1], rxComplete[0]}
  *   0x14  INT_FLAG    R:  {errNack[1], rxComplete[0]}  (W1C via INT_CLR)
  *                          rxComplete: set on STOP of a frame this slave was addressed in.
  *                          errNack:    set when a PTR load names an address outside the supported set.
  *   0x18  INT_CLR     W:  W1C
  *   0x1C  CFG_SAMPDIV W:  sampling window divider
  *   0x20  CFG_TIMEOUT W:  reserved (declared, unused in RTL). SCL stretch is UNBOUNDED by
  *                          design: firmware MUST clear STRETCH_REQ to release the bus; a
  *                          forgotten request holds SCL Low indefinitely.
  *   0x24  STRETCH_REQ RW: SCL stretch request (level; auto-cleared on STOP). While set and
  *                          SCL is Low the slave holds SCL Low (see CFG_TIMEOUT note).
  */
object I2cAutoSlave {

  /** I2C transmits 8 data bits per byte (plus the ACK bit). */
  val BITS_PER_BYTE = 8


  case class Config(
    validAddrs : Seq[Int] = (0 until 8).toSeq,   // supported address set; the single source of
                                                // truth: RAM depth = distinct-entry count
    support10bit: Boolean = false,
    sampWinSize: Int    = 3,
    sampDivWidth: Int   = 10
  ){
    /** Number of storage slots = cardinality of the supported set. */
    def ramDepth: Int = validAddrs.distinct.size
    /** Width to express any supported (or firmware-proposed) address. */
    def fwAddrW: Int = scala.math.max(1, log2Up(validAddrs.distinct.max + 1))
  }

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
    factory.read(addrReg, 0x00, 0)      // documented RW: allow readback
    factory.read(matchEnReg, 0x00, addrW)
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
    factory.read(intEn, 0x10)  // INT_EN is documented RW: allow readback
    factory.read(intr.flags, 0x14)
    // INT_CLR is a W1C pulse: default to 0 every cycle; the register-map
    // write materializes after this default, so the clear lasts exactly one
    // cycle instead of sticking.
    intClr := B(0, 2 bits)
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
    val ptrVal      = in UInt(c.fwAddrW bits)  // PTR load value (bus-space address)
    val ptrLoad     = in Bool()                          // one-cycle load pulse
    val ramWrAddr   = in UInt(c.fwAddrW bits)            // RAM_IND address (bus-space)
    val ramWrData   = in Bits(8 bits)
    val ramWrValid  = in Bool()                          // one-cycle RAM write pulse
    val sampDiv     = in UInt(c.sampDivWidth bits)
    val timeout     = in UInt(20 bits)                   // reserved (unused in RTL)
    val stretchReq  = in Bool()                          // level: set by firmware, auto-cleared on STOP
  }

  val intr = new Bundle {
    val en  = in Bits(2 bits)
    val flags  = out Bits(2 bits)
    val clr = in Bits(2 bits)  // W1C
  }

  val status = new Bundle {
    val inFrame    = out Bool()
    val errNack    = out Bool()
    val ptr        = out UInt(c.fwAddrW bits)  // live pointer readback (bus-space address)
    val stretchClr = out Bool()                          // one-cycle pulse on STOP (releases stretch)
  }

  /** Bind the register map to a bus factory (see [[I2cAutoSlave.driveFrom]]). */
  def driveFrom(busCtrl: BusSlaveFactory, baseAddress: BigInt): Unit =
    I2cAutoSlave.driveFrom(this, busCtrl, baseAddress)
}

class I2cAutoSlave(c: I2cAutoSlave.Config) extends Component {

  import I2cAutoSlave.Config

  val io = I2cAutoSlaveIo(c)

  val ramAddrW = log2Up(c.ramDepth) // compact slot index width (distinct entries packed)

  require(c.sampWinSize >= 2,
    s"I2cAutoSlave.Config.sampWinSize must be >= 2 (got ${c.sampWinSize}): the glitch-filter " +
    "history window needs at least 2 samples")
  require(c.validAddrs.forall(_ >= 0),
    "I2cAutoSlave.Config.validAddrs must contain only non-negative addresses")
  require(c.ramDepth >= 2,
    s"I2cAutoSlave.Config.validAddrs must contain at least 2 distinct addresses (got ${c.ramDepth})")

  // Address set <-> compact storage slots: slot k backs bus address addrTable(k). Firmware
  // only ever exchanges bus addresses (PTR / RAM_IND); the slot numbering stays internal.
  val addrTable: Seq[Int] = c.validAddrs.distinct.sorted

  private def addrHit(a: UInt): Bool =
    addrTable.map(x => a === U(x, c.fwAddrW bits)).reduce(_ || _)

  private def addrToSlot(a: UInt): UInt =
    addrTable.zipWithIndex.foldRight(U(0, ramAddrW bits)) {
      case ((busAddr, slot), acc) => Mux(a === U(busAddr, c.fwAddrW bits), U(slot, ramAddrW bits), acc)
    }

  private def slotToAddr(s: UInt): UInt =
    addrTable.zipWithIndex.foldRight(U(addrTable.last, c.fwAddrW bits)) {
      case ((busAddr, slot), acc) => Mux(s === U(slot, ramAddrW bits), U(busAddr, c.fwAddrW bits), acc)
    }

  /** Ring successor over slots; explicit wrap so non-power-of-two set sizes stay correct. */
  private def nextSlot(p: UInt): UInt =
    Mux(p === U(c.ramDepth - 1, ramAddrW bits), U(0, ramAddrW bits), p + U(1))

  // === Input conditioning (#5, #6, #16) ===
  val filterTick = new Area {
    val counter = Reg(UInt(c.sampDivWidth bits)) init(0)
    val tick    = counter === U(0)
    counter := Mux(tick, io.cfg.sampDiv, counter - U(1))
  }

  // Two-FF synchronization of the observed bus wires (external clock domain);
  // the glitch filter updates only on a UNANIMOUS window (the old andR||orR
  // reduced to orR — any non-zero pattern passed through). ALL protocol logic
  // below consumes the filtered signals, never the raw pins.
  val sclSync = BufferCC(io.i2c.scl.write, 2)
  val sdaSync = BufferCC(io.i2c.sda.write, 2)

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

  // === Edge detection (filtered, #16) ===
  val sclFiltPrev = RegInit(True)
  val sclRise = sclFilt && !sclFiltPrev
  val sclFall = !sclFilt && sclFiltPrev
  sclFiltPrev := sclFilt

  // START/STOP detection on filtered signals
  val sdaFiltPrev = RegInit(True)
  val sdaFallWhileSclHigh = !sdaFilt && sdaFiltPrev && sclFilt  // START
  val sdaRiseWhileSclHigh = sdaFilt && !sdaFiltPrev && sclFilt  // STOP
  sdaFiltPrev := sdaFilt

  // === Slave address & config ===

  // 10-bit address, standard format (UM10204 §4.2.2): byte0 = 11110 A9 A8 R/W,
  // byte1 = A7..A0. A[9:8] and R/W are buffered from the first byte; the
  // prefix guard rejects frames lacking the 11110 marker (e.g. 7-bit traffic
  // on a shared bus).
  val tenBitUpper = Reg(Bits(2 bits)) init(0)
  val tenBitRw    = RegInit(False)
  val tenBitPrefixOk = RegInit(False)
  val addrByteIdx = Reg(UInt(1 bits)) init(0)  // 0=first addr byte, 1=second
  // tenBitUpper / addrByteIdx are only written inside the `if (c.support10bit)`
  // generation branch (and only on 11110-prefix frames); in 7-bit builds they
  // are never assigned at all. The tag tells SpinalHDL the missing assignments
  // are deliberate (hold value), not an accidental latch hole.
  tenBitUpper.allowUnsetRegToAvoidLatch
  addrByteIdx.allowUnsetRegToAvoidLatch

  // === Data pointer (hardware-owned; firmware loads via ptrLoad) ===
  val dataPtr = Reg(UInt(ramAddrW bits)) init(0)

  // === Internal RAM ===
  // readAsync emits a benign "can only be write first into Verilog" warning.
  // A same-cycle, same-address collision between the indirect write (ramWrValid)
  // and the data read (dataPtr) resolves to readAsync returning the PRE-WRITE
  // value (the write commits on the next cycle) — at worst one stale bit is
  // shifted out, which is acceptable for a firmware-managed data buffer.
  val ram = Mem(Bits(8 bits), c.ramDepth)

  // Indirect RAM write (one-cycle pulse from the RAM_IND register). Takes a bus
  // address; one outside the supported set is silently ignored (firmware mistake,
  // not a bus event).
  when(io.cfg.ramWrValid && addrHit(io.cfg.ramWrAddr)) {
    ram.write(addrToSlot(io.cfg.ramWrAddr), io.cfg.ramWrData)
  }

  // === Frame state ===
  val inFrame   = RegInit(False)
  val isRead    = RegInit(False)
  val bitPos    = RegInit(U(0, 4 bits))
  val rxAccum   = Reg(Bits(8 bits)) init(0)
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
    val flags = RegInit(B(0, 2 bits))
    for (i <- 0 until 2)  { flags(i) clearWhen(io.intr.clr(i)) }
  }
  io.irq := (io.intr.en & intr.flags).orR

  // Sticky error flag: set when a PTR load names an address outside the supported
  // set, cleared on the next START
  // or STOP. Assigned inside conditional branches only, hence the tag
  // (deliberate hold-across-other-cycles semantics, not a latch hole).
  val statusErrNack = RegInit(False).allowUnsetRegToAvoidLatch

  // === Main state machine (event-driven on filtered edges) ===

  // START detection
  when(sdaFallWhileSclHigh) {
    inFrame    := True
    isRead     := False
    bitPos     := U(0)
    rxAccum    := B(0)
    hasStarted := False
    inAck      := False
    masterNack := False
    readAckPending := False
    statusErrNack := False
    sdaOut     := True  // release during address phase
    if (c.support10bit) { addrByteIdx := U(0); tenBitPrefixOk := False; tenBitRw := False }
  }

  // STOP detection
  when(sdaRiseWhileSclHigh) {
    when(inFrame) {
      // Only raise rx-complete if we were actually addressed and participated;
      // a NACKed / foreign frame must not set the flag (avoids spurious
      // interrupts on a shared bus).
      when(wasAddressed) { intr.flags(0) := True }
    }
    inFrame    := False
    hasStarted := False
    bitPos     := U(0)
    inAck      := False
    masterNack := False
    readAckPending := False
    sdaOut     := True
    wasAddressed := False
    statusErrNack := False  // a completed frame must not retain a stale NACK flag
    // NOTE: dataPtr is intentionally NOT reset on STOP. It is a firmware-owned
    // ring-buffer cursor (advanced by the data phase and repositioned via the PTR
    // register); resetting it here would break multi-frame sequential reads.
  }

  // Independent wasAddressed latch: set on address match (own when-block).
  // Requires addrMatchEn so a DISABLED instance whose address coincides with
  // the selected one neither latches wasAddressed nor participates in the
  // data phase (it NACKed the address; driving data would fight the
  // addressed slave on the open-drain wire).
  //
  // 10-bit mode: the latch fires on the SECOND address byte. When that byte is
  // reached but the first byte lacked the 11110 prefix (tenBitPrefixOk=False),
  // the frame is really 7-bit / foreign, so the "2nd address byte" is data and
  // wasAddressed is explicitly dropped. This closes the isolation hole where a
  // stale wasAddressed (cleared only on STOP otherwise) from a prior 10-bit
  // frame leaks into a different slave's data phase after a Repeated-START.
  // Clearing it on START instead (the naive fix) breaks multi-master
  // arbitration (sec 3.1.8 AR-11), so the drop is scoped to this edge only.
  when(sclFall && inFrame && !hasStarted && bitPos === U(I2cAutoSlave.BITS_PER_BYTE)) {
    if (c.support10bit) {
      when(addrByteIdx === U(1)) {
        when(tenBitPrefixOk) {
          val fullAddr = Cat(tenBitUpper, rxAccum(7 downto 0))  // A[9:0]
          wasAddressed := (fullAddr === io.cfg.slaveAddr) && io.cfg.addrMatchEn
        } otherwise {
          wasAddressed := False
        }
      }
    } else {
      wasAddressed := (rxAccum(7 downto 1) === io.cfg.slaveAddr) && io.cfg.addrMatchEn
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
        dataPtr := nextSlot(dataPtr)
        when(sdaFilt) {
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
        rxAccum(7 - bitPos(2 downto 0)) := sdaFilt
        bitPos := bitPos + U(1)
      } otherwise {
        // Data phase
        when(!isRead) {
          // Write: sample data bit
          rxAccum(7 - bitPos(2 downto 0)) := sdaFilt
        }
        bitPos := bitPos + U(1)
      }
    }
  }

  // SCL falling edge: set up response for next bit
  when(sclFall && inFrame) {
    when(!hasStarted) {
      when(bitPos === U(I2cAutoSlave.BITS_PER_BYTE)) {
        // All 8 address bits received (SCL fell after 8th data bit)
        if (c.support10bit) {
          when(addrByteIdx === U(0)) {
            // First 10-bit address byte: standard format 11110 A9 A8 R/W.
            // §3.1.11 A1: ACK the header only if it addresses US in WRITE
            // direction — 11110 prefix, A[9:8] equal to our upper bits, R/W=0.
            // A read-direction header (R/W=1) is answered on the SECOND byte
            // after Sr (A3); ACKing it here would let every 10-bit slave on
            // the bus grab the read. The NACK is deliberate but silent (no
            // errNack/interrupt) to avoid interrupt storms on unrelated
            // 7-bit frames or foreign 10-bit frames.
            val prefixOk   = rxAccum(7 downto 3) === B"11110"
            val upperMatch = rxAccum(2 downto 1) === io.cfg.slaveAddr(9 downto 8)
            val hdrIsWrite = !rxAccum(0)
            val ackHdr     = prefixOk && upperMatch && hdrIsWrite && io.cfg.addrMatchEn
            tenBitUpper    := rxAccum(2 downto 1)  // A[9:8]
            tenBitRw       := rxAccum(0)           // R/W lives in byte 0
            tenBitPrefixOk := prefixOk
            addrByteIdx := U(1)
            inAck       := True
            bitPos      := U(0)
            rxAccum     := B(0)
            sdaOut      := !ackHdr
          } otherwise {
            // Second 10-bit address byte: A[7:0]; R/W was consumed in byte 0.
            // Require tenBitPrefixOk so a 7-bit frame whose byte0 happened to
            // carry the 11110 marker (or arbitrary data) cannot spuriously match
            // our lower 10 bits and grab a foreign byte. Mirrors the guard on the
            // wasAddressed latch.
            val fullAddr = Cat(tenBitUpper, rxAccum(7 downto 0))  // A[9:0]
            val matchNow = tenBitPrefixOk && (fullAddr === io.cfg.slaveAddr)
            isRead    := tenBitRw
            hasStarted := True
            inAck      := True
            bitPos     := U(0)
            sdaOut     := !(matchNow && io.cfg.addrMatchEn)
            // Silent NACK on a non-addressed frame (consistent with the first
            // 10-bit header path): avoids interrupt storms / sticky errNack on
            // multi-slave buses. Genuine errors (rejected PTR loads) still report.
          }
        } else {
          // 7-bit: single address byte
          val recvAddr = rxAccum(7 downto 1)
          val recvRw   = rxAccum(0)
          val matchNow = (recvAddr === io.cfg.slaveAddr)
          isRead    := recvRw
          hasStarted := True
          inAck      := True
          bitPos     := U(0)
          sdaOut     := !(matchNow && io.cfg.addrMatchEn)
          // Silent NACK on a non-addressed frame (avoids interrupt storms on
          // multi-slave buses). Genuine errors (rejected PTR loads) still report.
        }
      } otherwise {
        // Between address bits: release SDA (master is driving data)
        sdaOut := True
      }
    } otherwise {
      when(bitPos === U(I2cAutoSlave.BITS_PER_BYTE)) {
        // Data byte complete (8 bits received)
        when(!isRead) {
          // Write: store byte, advance pointer, prepare ACK.
          // Multi-slave isolation: only the addressed slave drives the ACK;
          // non-addressed slaves release SDA (cannot mask the addressed
          // slave's NACK on the shared bus). Every slot of the mapped set is
          // writable, so an addressed slave always ACKs data bytes; a data NACK
          // on the wire can only come from elsewhere on the bus.
          when(wasAddressed) {
            ram.write(dataPtr, rxAccum)
            dataPtr := nextSlot(dataPtr)
            sdaOut  := False
          } otherwise {
            sdaOut := True
          }
        } otherwise {
          // Read: the ADDRESSED slave releases SDA so the master's ACK/NACK
          // is not masked by our own last data bit (open-drain AND); the
          // sample is taken on the following sclRise (see readAckPending).
          // Non-addressed slaves must NOT arm readAckPending: arming it makes
          // the next sclRise advance their dataPtr — silent pointer drift
          // while another slave is being read.
          sdaOut := True
          when(wasAddressed) { readAckPending := True }
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
    when(addrHit(io.cfg.ptrVal)) {
      dataPtr := addrToSlot(io.cfg.ptrVal)
    } otherwise {
      // Rejected load: the address is outside the supported set. Reported like every
      // other error (STATUS.errNack + INT_FLAG[1]); the pointer stays where it is.
      statusErrNack := True
      intr.flags(1) := True
    }
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
    when(!sclFilt) { stretchActive := True }
  }
  // (simPublic taps for this component live in the TEST-SOURCE top classes —
  //  pubSlave in I2cAutoSim/I2cAutoFuzz — keeping this file sim-free.)
  // intr.flags: tests poll the flag directly (it lags the master's busy release)
  io.i2c.scl.read := !stretchActive
  io.sclStretch := stretchActive

  // Auto-clear pulse for the parent STRETCH_REQ register on STOP
  io.status.stretchClr := sdaRiseWhileSclHigh

  // Forward status to the io
  io.status.inFrame := inFrame
  io.status.errNack := statusErrNack
  io.status.ptr     := slotToAddr(dataPtr)
  io.intr.flags        := intr.flags

  // (Internal observation taps are simPublic()'d from the TEST-SOURCE top
  //  classes — pubSlave in I2cAutoSim/I2cAutoFuzz — so this file stays
  //  free of spinal.core.sim.)
}
