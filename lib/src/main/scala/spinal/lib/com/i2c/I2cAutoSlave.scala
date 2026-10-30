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
  * Register map:
  *   0x00  ADDR       RW: {matchEn[7], addr[6:0]}
  *   0x04  PTR        RW: data pointer
  *   0x08  RAM_BASE   R/W: indirect RAM access (addr[5:0] in bits[5:0], data[7:0] in bits[15:8])
  *   0x0C  STATUS     R: {errNack[1], busy[0]}
  *   0x10  INT_EN     RW: {rxComplete[0]}
  *   0x14  INT_FLAG   R:  interrupt flags
  *   0x18  INT_CLR    W:  W1C
  *   0x1C  CFG_SAMPDIV W: sampling divider
  *   0x20  CFG_TIMEOUT W: timeout (0=disabled)
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
}

class I2cAutoSlave(c: I2cAutoSlave.Config) extends Component {

  import I2cAutoSlave.Config

  val io = new Bundle {
    val i2c = slave(I2c())
    val irq = out Bool()
    val sclStretch = out Bool()  // Active when slave is stretching SCL
  }

  val ramAddrW = log2Up(c.ramDepth)

  // === Input filtering ===
  val cfg = new Area {
    val sampDiv = Reg(UInt(c.sampDivWidth bits)) init(0)
    val timeout = Reg(UInt(20 bits)) init(0)
    sampDiv.allowUnsetRegToAvoidLatch
    timeout.allowUnsetRegToAvoidLatch
  }

  val filterTick = new Area {
    val counter = Reg(UInt(c.sampDivWidth bits)) init(0)
    val tick    = counter === U(0)
    counter := Mux(tick, cfg.sampDiv, counter - U(1))
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
  val slaveAddr = Reg(Bits(addrW bits)) init(0)
  val addrMatchEn = RegInit(True)
  slaveAddr.allowUnsetRegToAvoidLatch
  addrMatchEn.allowUnsetRegToAvoidLatch

  // 10-bit address: buffer upper 3 bits from first byte
  val tenBitUpper = Reg(Bits(3 bits)) init(0)
  val addrByteIdx = Reg(UInt(1 bits)) init(0)  // 0=first addr byte, 1=second
  tenBitUpper.allowUnsetRegToAvoidLatch
  addrByteIdx.allowUnsetRegToAvoidLatch

  // === Data pointer ===
  val dataPtr = Reg(UInt(ramAddrW bits)) init(0)

  // === Internal RAM ===
  val ram = Mem(Bits(8 bits), c.ramDepth)

  // === Frame state ===
  val inFrame   = RegInit(False)
  val isRead    = RegInit(False)
  val bitPos    = RegInit(U(0, 4 bits))
  val rxAccum   = Reg(Bits(8 bits)) init(0)
  val addrValid = RegInit(False)
  val hasStarted = RegInit(False)  // True after address+ACK complete
  val inAck     = RegInit(False)  // True when next SCL rise is the ACK bit

  // === Response: what to drive on SDA ===
  // True = release (high-Z in open-drain), False = drive low
  val sdaOut = RegInit(True)

  // Independent latch: set when address matches, cleared on STOP.
  // Lives in its own when-block to avoid SpinalHDL elaboration conflicts
  // with the main SCL edge handler.
  val wasAddressed = RegInit(False)

  // === Interrupts ===
  val intr = new Area {
    val en  = RegInit(B(0, 2 bits))
    val fl  = RegInit(B(0, 2 bits))
    val clr = RegInit(B(0, 2 bits))
    en.allowUnsetRegToAvoidLatch
    clr.allowUnsetRegToAvoidLatch
    val irq = (en & fl).orR
    for (i <- 0 until 2) { fl(i) clearWhen(clr(i)) }
  }

  val statusErrNack = RegInit(False)

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
    sdaOut     := True
    wasAddressed := False
  }

  // Independent wasAddressed latch: set on address match (own when-block)
  when(sclFall && inFrame && !hasStarted && bitPos === U(8)) {
    if (c.support10bit) {
      when(addrByteIdx === U(1)) {
        val fullAddr = Cat(tenBitUpper, rxAccum(7 downto 1))
        wasAddressed := (fullAddr === slaveAddr)
      }
    } else {
      wasAddressed := (rxAccum(7 downto 1) === slaveAddr)
    }
  }

  // SCL rising edge: sample SDA, advance bit position
  when(sclRise && inFrame) {
    when(inAck) {
      // ACK bit: skip sampling, just clear the flag
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
            sdaOut      := !addrMatchEn  // ACK if enabled, NACK if disabled
          } otherwise {
            // Second 10-bit address byte: full match
            val fullAddr = Cat(tenBitUpper, rxAccum(7 downto 1))  // A[9:0]
            val recvRw   = rxAccum(0)
            val matchNow = (fullAddr === slaveAddr)
            isRead    := recvRw
            addrValid := matchNow
            hasStarted := True
            inAck      := True
            bitPos     := U(0)
            sdaOut     := !(matchNow && addrMatchEn)
          }
        } else {
          // 7-bit: single address byte
          val recvAddr = rxAccum(7 downto 1)
          val recvRw   = rxAccum(0)
          val matchNow = (recvAddr === slaveAddr)
          isRead    := recvRw
          addrValid := matchNow
          hasStarted := True
          inAck      := True
          bitPos     := U(0)
          sdaOut     := !(matchNow && addrMatchEn)
        }
      } otherwise {
        // Between address bits: release SDA (master is driving data)
        sdaOut := True
      }
    } otherwise {
      when(bitPos === U(8)) {
        // Data byte complete (8 bits received)
        when(!isRead) {
          // Write: store byte, advance pointer, prepare ACK
          when(ptrValid(dataPtr)) {
            ram.write(dataPtr, rxAccum)
          }
          dataPtr := dataPtr + U(1)
          sdaOut  := !ptrValid(dataPtr)
        } otherwise {
          // Read: check master's ACK/NACK
          when(!(io.i2c.sda.write)) {
            dataPtr := dataPtr + U(1)
            sdaOut  := ram.readAsync(dataPtr)(7)
          } otherwise {
            sdaOut := True
          }
        }
        inAck   := True  // next SCL rise is data ACK
        bitPos  := U(0)
        rxAccum := B(0)
      } otherwise {
        // Setup SDA for next bit (during SCL low)
        when(isRead && !inAck) {
          sdaOut := ram.readAsync(dataPtr)(7 - bitPos(2 downto 0))
        } otherwise {
          sdaOut := True
        }
      }
    }
  }

  // Drive physical SDA output (what this slave contributes to the wire)
  // TODO(multi-slave): Non-addressed slave still ACKs/drives SDA in data phase.
  // Root cause: SpinalHDL multi-assignment conflict - any additional sdaOut
  // override (Mux or separate when-block) breaks the main SCL handler elaboration.
  // Workaround: use a separate I2cAutoSlave instance per address in SoC integration,
  // or implement at the bus-arbitration level (gate the entire slave enable).
  io.i2c.sda.read := sdaOut
  // SCL stretching: firmware can request to hold SCL Low
  val stretchReq = RegInit(False)  // Set by firmware to stretch, cleared on STOP or manually
  stretchReq.allowUnsetRegToAvoidLatch
  when(sdaRiseWhileSclHigh) { stretchReq := False }  // auto-clear on STOP
  io.i2c.scl.read := !stretchReq
  io.sclStretch := stretchReq

  // === IRQ output ===
  io.irq := intr.irq

  // === Register map ===
  def driveFrom(busCtrl: BusSlaveFactory, baseAddress: BigInt): Area = new Area {
    val bus = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    // 0x00 ADDR (width depends on support10bit)
    bus.write(0x00,
      0 -> slaveAddr,
      addrW -> addrMatchEn
    )

    // 0x04 PTR
    bus.drive(dataPtr, 0x04)

    // 0x08 RAM indirect access
    val ramAddr = bus.createReadWrite(UInt(ramAddrW bits), 0x08)
    val ramData = bus.createReadWrite(Bits(8 bits), 0x08, ramAddrW)
    bus.onWrite(0x08) {
      ram.write(ramAddr, ramData)
    }
    bus.read(ram.readAsync(ramAddr), 0x08, ramAddrW)

    // 0x0C STATUS
    bus.read(0x0C,
      0 -> inFrame,
      1 -> statusErrNack
    )

    // 0x10 INT_EN
    bus.drive(intr.en, 0x10)

    // 0x14 INT_FLAG
    bus.read(intr.fl.asUInt, 0x14)

    // 0x18 INT_CLR
    val clr0 = bus.createWriteOnly(Bool(), 0x18, 0)
    val clr1 = bus.createWriteOnly(Bool(), 0x18, 1)
    intr.clr := B(clr1, clr0)

    // 0x1C CFG_SAMPDIV
    bus.drive(cfg.sampDiv, 0x1C)

    // 0x20 CFG_TIMEOUT
    bus.drive(cfg.timeout, 0x20)
  }

  // Expose internals for simulation
  cfg.sampDiv.simPublic()
  slaveAddr.simPublic()
  addrMatchEn.simPublic()
  dataPtr.simPublic()
  ram.simPublic()
  stretchReq.simPublic()
  tenBitUpper.simPublic()
  addrByteIdx.simPublic()
  hasStarted.simPublic()
  inFrame.simPublic()
  sdaOut.simPublic()
  bitPos.simPublic()
  inAck.simPublic()
  rxAccum.simPublic()
}
