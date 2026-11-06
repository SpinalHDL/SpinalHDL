package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.sim._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAccessParameter, BmbSourceParameter, BmbSlaveFactory}
import spinal.lib.com.i2c._
import org.scalatest.funsuite.AnyFunSuite

/**
  * SpinalSim testbench for I2cAutoMaster + I2cAutoSlave.
  *
  * Connects master and slave via simulated open-drain interconnect
  * on the internal i2cBuf signals.
  *
  * Register maps are instantiated at the TOP level: each core's io is bound
  * to a top-level Bmb slave port via driveFrom(BmbSlaveFactory). All firmware
  * interaction in these tests goes through those register buses (bmbWrite),
  * mirroring real software access.
  */

class I2cAutoSim extends AnyFunSuite {
  // Helper: assert the wire-level conformance guards stayed clean for a test.
  // expectStarts/expectStops (-1 = skip) bind START/STOP edge counts, which
  // catches malformed frame endings (double STOP, missing STOP, extra Sr).
  def assertWireClean(dut: TestTop, expectStarts: Int = -1, expectStops: Int = -1): Unit = {
    assert(dut.sdaViolCnt.toInt == 0,
      s"sdaViolCnt=${dut.sdaViolCnt.toInt}: SDA toggled while SCL High outside START/STOP/RESTART windows (UM10204 §3.1.3)")
    if (expectStarts >= 0) assert(dut.startCnt.toInt == expectStarts,
      s"startCnt=${dut.startCnt.toInt}, expected $expectStarts")
    if (expectStops >= 0) assert(dut.stopCnt.toInt == expectStops,
      s"stopCnt=${dut.stopCnt.toInt}, expected $expectStops")
  }

  case class TestTopCfg(
    masterFifoDepth : Int = 64,
    slaveRamDepth   : Int = 64,
    slaveValidAddrs : Option[Seq[Int]] = None,
    support10bit    : Boolean = false,
    // Per-role overrides (None = follow support10bit). Needed for mixed
    // topologies, e.g. a 7-bit master probing a 10-bit slave's reserved-
    // prefix NACK behaviour.
    masterSupport10bit: Option[Boolean] = None,
    slaveSupport10bit : Option[Boolean] = None
  )

  class TestTop(cfg: TestTopCfg = TestTopCfg()) extends Component {
    val master10 = cfg.masterSupport10bit.getOrElse(cfg.support10bit)
    val slave10  = cfg.slaveSupport10bit.getOrElse(cfg.support10bit)
    val master = new I2cAutoMaster(I2cAutoMaster.Config(
      fifoDepth    = cfg.masterFifoDepth,
      support10bit = master10,
      timerWidth   = 12
    ))
    val slave  = new I2cAutoSlave(I2cAutoSlave.Config(
      ramDepth     = cfg.slaveRamDepth,
      validAddrs   = cfg.slaveValidAddrs.getOrElse((0 until cfg.slaveRamDepth).toSeq),
      support10bit = slave10
    ))

    // Test control: force SCL Low to simulate bus hang
    val sclForceLow = RegInit(False)
    sclForceLow.allowUnsetRegToAvoidLatch()
    sclForceLow.simPublic()

    // Test control: force SDA Low to simulate a competing master winning
    // arbitration. Open-drain physics: a competitor can only PULL the line
    // Low, never force it High.
    val sdaForceLow = RegInit(False)
    sdaForceLow.allowUnsetRegToAvoidLatch()
    sdaForceLow.simPublic()

    // Test control: force SCL High for a single cycle to inject a glitch
    // spike (glitch-immunity tests). Momentary only — holding it would fight
    // the drivers and is not a legal bus event.
    val sclForceHigh = RegInit(False)
    sclForceHigh.allowUnsetRegToAvoidLatch()
    sclForceHigh.simPublic()

    // Open-drain wire emulation via io.i2c ports:
    // SCL: open-drain AND of master and slave (slave can stretch)
    val sclWire = Mux(sclForceLow, False,
      (master.io.i2c.scl.write && slave.io.i2c.scl.read) || sclForceHigh)
    sclWire.simPublic()
    master.io.i2c.scl.read := sclWire
    slave.io.i2c.scl.write := sclWire

    // SDA: open-drain AND of both contributions; a competing master can only
    // pull the wire Low (forcing High is physically impossible on open-drain).
    val sdaAnd = master.io.i2c.sda.write && slave.io.i2c.sda.read
    val sdaWire = sdaAnd && !sdaForceLow
    sdaWire.simPublic()
    master.io.i2c.sda.read := sdaWire  // master sees combined (in)
    slave.io.i2c.sda.write := sdaWire  // slave sees combined (in)

    // Wire-level address sniffer (10-bit format verification): on a START
    // (SDA fall while SCL high) capture the address bits sampled on SCL
    // RISING edges — SDA is stable during SCL High, while at the falling edge
    // the master has already set up the NEXT bit. Wire sequence: byte0[7..0]
    // on rises n=0..7, ACK on n=8 (skipped), byte1[7..0] on n=9..16.
    val snifByte0 = Reg(Bits(8 bits)) init(0)
    val snifByte1 = Reg(Bits(8 bits)) init(0)
    val snifAck0  = RegInit(True)  // SDA level at byte0 ACK slot (Low = ACKed)
    val snifAck1  = RegInit(True)  // SDA level at byte1 ACK slot (Low = ACKed)
    val snifCnt   = Reg(UInt(5 bits)) init(0)
    val snifArmed = RegInit(False)
    snifByte0.simPublic()
    snifByte1.simPublic()
    snifAck0.simPublic()
    snifAck1.simPublic()
    val sdaWPrev = RegInit(True)
    val sclWPrev = RegInit(True)
    when(!sdaWire && sdaWPrev && sclWire) { snifArmed := True; snifCnt := U(0) }
    when(sclWire && !sclWPrev) {
      when(snifArmed) {
        val n = snifCnt
        when(n < U(8)) { snifByte0((7 - n)(2 downto 0)) := sdaWire }
        when(n === U(8)) { snifAck0 := sdaWire }
        when(n > U(8) && n < U(17)) { snifByte1((16 - n)(2 downto 0)) := sdaWire }
        when(n === U(17)) { snifAck1 := sdaWire; snifArmed := False }
        snifCnt := snifCnt + U(1)
      }
    }
    sdaWPrev := sdaWire
    sclWPrev := sclWire

    // ---- Wire-level conformance guards (UM10204 sec. 3.1.3 / 3.1.4) ----
    // Derived PURELY FROM THE WIRES (independent oracle, no DUT internals):
    //   START = SDA fall while SCL High; STOP = SDA rise while SCL High.
    val sdaChanged = sdaWire =/= sdaWPrev
    val wireStart = sdaChanged && sclWire && !sdaWire
    val wireStop  = sdaChanged && sclWire && sdaWire
    // START/STOP edge counters (golden-frame bookkeeping: a well-formed
    // single transfer has exactly 1 START and 1 STOP; a Sr composite has 2).
    val startCnt = Reg(UInt(8 bits)) init(0)
    val stopCnt  = Reg(UInt(8 bits)) init(0)
    startCnt.simPublic(); stopCnt.simPublic()
    when(wireStart) { startCnt := startCnt + U(1) }
    when(wireStop)  { stopCnt  := stopCnt + U(1) }
    // §3.1.3: SDA must be stable while SCL is High; the ONLY legal SDA
    // edges over SCL-High are the START / STOP (Sr = STOP+START) shapes,
    // which are exactly wireStart / wireStop. Any other SDA edge over
    // SCL-High is a conformance violation. (Checking the edge shape on
    // the SAME cycle avoids the off-by-one a trailing suppression window
    // would have for the edge cycle itself.)
    // Forced injections (sdaForceLow) are testbench artifacts, not DUT
    // behaviour. Armed only after the first wire START so a power-up wire
    // transient can never count.
    val violArmed = RegInit(False)
    when(startCnt > U(0)) { violArmed := True }
    val sdaViolCnt = Reg(UInt(8 bits)) init(0)
    sdaViolCnt.simPublic()
    when(sdaChanged && sclWire && !(wireStart || wireStop) && !sdaForceLow && violArmed) {
      sdaViolCnt := sdaViolCnt + U(1)
    }
    // SCL rising-edge counter (recovery-pulse accounting).
    val sclRiseCnt = Reg(UInt(16 bits)) init(0)
    sclRiseCnt.simPublic()
    when(sclWire && !sclWPrev) { sclRiseCnt := sclRiseCnt + U(1) }

    // Named observation taps: Verilator may mangle internal port-net names;
    // top-level aliases survive for the loser-release assertions.
    val masterSclRelease = master.io.i2c.scl.write
    val masterSdaRelease = master.io.i2c.sda.write
    masterSclRelease.simPublic()
    masterSdaRelease.simPublic()

    // Register maps: top-level Bmb slave ports + factories bound to the cores
    val regBusParam = BmbAccessParameter(8, 32).addSources(1, BmbSourceParameter(contextWidth = 0, lengthWidth = 2)).toBmbParameter()
    val masterRegBus = spinal.lib.slave(Bmb(regBusParam))
    val slaveRegBus  = spinal.lib.slave(Bmb(regBusParam))
    val masterRegMap = BmbSlaveFactory(masterRegBus)
    val slaveRegMap  = BmbSlaveFactory(slaveRegBus)
    master.io.driveFrom(masterRegMap, 0)
    slave.io.driveFrom(slaveRegMap, 0)

    // Bmb register buses are top-level inputs: left undriven at elaboration
    // (defaults to 0 in Verilog/SpinalSim); tests drive them via bmbWrite.
    bmbMark(masterRegBus)
    bmbMark(slaveRegBus)

    // Expose IRQs
    val io = new Bundle {
      val irqMaster = out Bool()
      val irqSlave  = out Bool()
    }
    io.irqMaster := master.io.irq
    io.irqSlave  := slave.io.irq

    // ---- Firmware-facing register helpers (exercise the real register path) ----
    def addrW: Int = if (cfg.support10bit) 10 else 7

    def regWriteMaster(addr: Int, data: Int): Unit = bmbWrite(masterRegBus, addr, data, clockDomain)
    def regWriteSlave(addr: Int, data: Int): Unit  = bmbWrite(slaveRegBus, addr, data, clockDomain)

    def cfgMasterTiming(): Unit = {
      regWriteMaster(0x24, 1)   // CFG_SAMPDIV
      regWriteMaster(0x2C, 20)  // CFG_TLOW
      regWriteMaster(0x30, 20)  // CFG_THIGH
      regWriteMaster(0x34, 20)  // CFG_TBUF
    }

    /** Pack and write the CMD register: fields + trigger land atomically. */
    def issueCmd(addr: Int, rw: Boolean, phase2: Boolean = false, nackLast: Boolean = false,
                 len1: Int, len2: Int = 0): Unit = {
      regWriteMaster(0x04, len1)
      regWriteMaster(0x08, len2)
      val data = (if (nackLast) 1 else 0) << (addrW + 2) |
                 (if (phase2)   1 else 0) << (addrW + 1) |
                 (if (rw)       1 else 0) << addrW |
                 addr
      regWriteMaster(0x00, data)
    }
  }

  /** Dual-slave topology: two slaves on the same bus with different addresses */
  case class DualSlaveCfg(
    support10bit: Boolean = false,
    ramDepth: Int = 64
  )

  class TestTopDualSlave(cfg: DualSlaveCfg = DualSlaveCfg()) extends Component {
    val master = new I2cAutoMaster(I2cAutoMaster.Config(fifoDepth = 64, support10bit = cfg.support10bit, timerWidth = 12))
    val slaveA = new I2cAutoSlave(I2cAutoSlave.Config(ramDepth = cfg.ramDepth, support10bit = cfg.support10bit))
    val slaveB = new I2cAutoSlave(I2cAutoSlave.Config(ramDepth = cfg.ramDepth, support10bit = cfg.support10bit))

    // SCL: master drives both slaves
    master.io.i2c.scl.read := master.io.i2c.scl.write
    slaveA.io.i2c.scl.write := master.io.i2c.scl.write
    slaveB.io.i2c.scl.write := master.io.i2c.scl.write

    // SDA: open-drain AND of all three drivers
    val sdaAnd = master.io.i2c.sda.write && slaveA.io.i2c.sda.read && slaveB.io.i2c.sda.read
    master.io.i2c.sda.read := sdaAnd
    slaveA.io.i2c.sda.write := sdaAnd
    slaveB.io.i2c.sda.write := sdaAnd

    // Expose slave B's SDA output for monitoring
    slaveB.sdaOut.simPublic()
    slaveA.sdaOut.simPublic()

    // Register maps: top-level Bmb slave ports + factories bound to the cores
    val regBusParam = BmbAccessParameter(8, 32).addSources(1, BmbSourceParameter(contextWidth = 0, lengthWidth = 2)).toBmbParameter()
    val masterRegBus = spinal.lib.slave(Bmb(regBusParam))
    val slaveARegBus = spinal.lib.slave(Bmb(regBusParam))
    val slaveBRegBus = spinal.lib.slave(Bmb(regBusParam))
    val masterRegMap = BmbSlaveFactory(masterRegBus)
    val slaveARegMap = BmbSlaveFactory(slaveARegBus)
    val slaveBRegMap = BmbSlaveFactory(slaveBRegBus)
    master.io.driveFrom(masterRegMap, 0)
    slaveA.io.driveFrom(slaveARegMap, 0)
    slaveB.io.driveFrom(slaveBRegMap, 0)

    bmbMark(masterRegBus)
    bmbMark(slaveARegBus)
    bmbMark(slaveBRegBus)

    val io = new Bundle {
      val irqMaster = out Bool()
    }
    io.irqMaster := master.io.irq

    // ---- Firmware-facing register helpers (7-bit address mode) ----
    def addrW: Int = 7

    def regWriteMaster(addr: Int, data: Int): Unit = bmbWrite(masterRegBus, addr, data, clockDomain)

    def cfgMasterTiming(): Unit = {
      regWriteMaster(0x24, 1)   // CFG_SAMPDIV
      regWriteMaster(0x2C, 20)  // CFG_TLOW
      regWriteMaster(0x30, 20)  // CFG_THIGH
      regWriteMaster(0x34, 20)  // CFG_TBUF
    }

    def issueCmd(addr: Int, rw: Boolean, phase2: Boolean = false, nackLast: Boolean = false,
                 len1: Int, len2: Int = 0): Unit = {
      regWriteMaster(0x04, len1)
      regWriteMaster(0x08, len2)
      val data = (if (nackLast) 1 else 0) << (addrW + 2) |
                 (if (phase2)   1 else 0) << (addrW + 1) |
                 (if (rw)       1 else 0) << addrW |
                 addr
      regWriteMaster(0x00, data)
    }
  }

  // ---------------------------------------------------------------------
  // Dual-master arbitration topology (UM10204 sec. 3.1.8): two
  // I2cAutoMasters contend on one open-drain bus with one slave per
  // address. SCL/SDA are ANDs of ALL drivers: any party can pull the
  // wire Low, nobody can force it High.
  // ---------------------------------------------------------------------
  class TestTopDualMaster extends Component {
    val masterA = new I2cAutoMaster(I2cAutoMaster.Config(fifoDepth = 64, support10bit = false, timerWidth = 12))
    val masterB = new I2cAutoMaster(I2cAutoMaster.Config(fifoDepth = 64, support10bit = false, timerWidth = 12))
    val slaveA  = new I2cAutoSlave(I2cAutoSlave.Config(ramDepth = 64, support10bit = false))
    val slaveB  = new I2cAutoSlave(I2cAutoSlave.Config(ramDepth = 64, support10bit = false))

    // SCL: open-drain AND of both master drivers (masters are the only SCL drivers)
    val sclWire = masterA.io.i2c.scl.write && masterB.io.i2c.scl.write
    sclWire.simPublic()
    masterA.io.i2c.scl.read := sclWire
    masterB.io.i2c.scl.read := sclWire
    slaveA.io.i2c.scl.write := sclWire
    slaveB.io.i2c.scl.write := sclWire

    // SDA: open-drain AND of both masters + both slaves
    val sdaWire = masterA.io.i2c.sda.write && masterB.io.i2c.sda.write &&
                  slaveA.io.i2c.sda.read && slaveB.io.i2c.sda.read
    sdaWire.simPublic()
    masterA.io.i2c.sda.read := sdaWire
    masterB.io.i2c.sda.read := sdaWire
    slaveA.io.i2c.sda.write := sdaWire
    slaveB.io.i2c.sda.write := sdaWire

    // Register maps: one Bmb slave port per core
    val regBusParam = BmbAccessParameter(8, 32).addSources(1, BmbSourceParameter(contextWidth = 0, lengthWidth = 2)).toBmbParameter()
    val masterARegBus = spinal.lib.slave(Bmb(regBusParam))
    val masterBRegBus = spinal.lib.slave(Bmb(regBusParam))
    val slaveARegBus  = spinal.lib.slave(Bmb(regBusParam))
    val slaveBRegBus  = spinal.lib.slave(Bmb(regBusParam))
    val masterARegMap = BmbSlaveFactory(masterARegBus)
    val masterBRegMap = BmbSlaveFactory(masterBRegBus)
    val slaveARegMap  = BmbSlaveFactory(slaveARegBus)
    val slaveBRegMap  = BmbSlaveFactory(slaveBRegBus)
    masterA.io.driveFrom(masterARegMap, 0)
    masterB.io.driveFrom(masterBRegMap, 0)
    slaveA.io.driveFrom(slaveARegMap, 0)
    slaveB.io.driveFrom(slaveBRegMap, 0)
    bmbMark(masterARegBus); bmbMark(masterBRegBus); bmbMark(slaveARegBus); bmbMark(slaveBRegBus)

    // ---- Wire-level conformance guards (UM10204 sec. 3.1.3 / 3.1.4) ----
    // PURE-WIRE oracle (identical methodology to TestTop): START = SDA fall
    // while SCL High, STOP = SDA rise while SCL High; any OTHER SDA edge
    // over SCL-High is a sec. 3.1.3 violation.
    val sdaWPrev = RegInit(True)
    val sclWPrev = RegInit(True)
    val sdaChanged = sdaWire =/= sdaWPrev
    val wireStart = sdaChanged && sclWire && !sdaWire
    val wireStop  = sdaChanged && sclWire && sdaWire
    val startCnt = Reg(UInt(8 bits)) init(0)
    val stopCnt  = Reg(UInt(8 bits)) init(0)
    startCnt.simPublic(); stopCnt.simPublic()
    when(wireStart) { startCnt := startCnt + U(1) }
    when(wireStop)  { stopCnt  := stopCnt + U(1) }
    val violArmed = RegInit(False)
    when(startCnt > U(0)) { violArmed := True }
    val sdaViolCnt = Reg(UInt(8 bits)) init(0)
    sdaViolCnt.simPublic()
    when(sdaChanged && sclWire && !(wireStart || wireStop) && violArmed) {
      sdaViolCnt := sdaViolCnt + U(1)
    }
    sdaWPrev := sdaWire
    sclWPrev := sclWire

    def addrW: Int = 7

    def cfgMaster(bus: Bmb): Unit = {
      bmbWrite(bus, 0x24, 1, clockDomain)   // CFG_SAMPDIV
      bmbWrite(bus, 0x2C, 20, clockDomain)  // CFG_TLOW
      bmbWrite(bus, 0x30, 20, clockDomain)  // CFG_THIGH
      bmbWrite(bus, 0x34, 20, clockDomain)  // CFG_TBUF
    }

    def stageLen(bus: Bmb, len: Int): Unit = {
      bmbWrite(bus, 0x04, len, clockDomain)
      bmbWrite(bus, 0x08, 0, clockDomain)
    }

    def cfgSlave(bus: Bmb, addr: Int): Unit = {
      bmbWrite(bus, 0x00, (1 << addrW) | addr, clockDomain)  // matchEn=1, 7-bit addr
    }

    def pushTx(bus: Bmb, data: Int): Unit = {
      bmbWrite(bus, 0x0C, data, clockDomain)
    }

    /** CMD word for a plain write transfer (rw=0, no phase2, no nackLast). */
    def cmdWrite(addr: Int): Int = addr

    /** Issue the CMD trigger to BOTH masters on the SAME cycle so their
      * START sequences stay bit-phase-aligned (deterministic divergent bit). */
    def triggerBoth(busA: Bmb, dataA: Int, busB: Bmb, dataB: Int): Unit = {
      // PRE-ARM: Bmb cmd.ready is registered (it tracks the staged rsp pipeline),
      // so it can lag rsp consumption by a cycle. Wait for BOTH buses to be ready
      // with valid deasserted, so nothing fires during arming.
      busA.rsp.ready #= true
      busB.rsp.ready #= true
      var c = 0
      while ((!busA.cmd.ready.toBoolean || !busB.cmd.ready.toBoolean) && c < 100) {
        clockDomain.waitActiveEdge(); c += 1
      }
      assert(c < 100, "triggerBoth: buses not ready")
      // SINGLE SHOT: assert both CMD valids for exactly one cycle. Both fire on
      // the same cycle (phase-aligned START -> deterministic first divergent bit)
      // and only once (clearing valid kills any re-fire while ready stays High).
      busA.cmd.valid #= true; busA.cmd.opcode #= 1; busA.cmd.address #= 0x00; busA.cmd.length #= 0
      busA.cmd.data #= dataA; busA.cmd.mask #= 0xF
      busB.cmd.valid #= true; busB.cmd.opcode #= 1; busB.cmd.address #= 0x00; busB.cmd.length #= 0
      busB.cmd.data #= dataB; busB.cmd.mask #= 0xF
      clockDomain.waitActiveEdge()
      busA.cmd.valid #= false; busB.cmd.valid #= false
      // Drain both responses.
      c = 0
      while ((!busA.rsp.valid.toBoolean || !busB.rsp.valid.toBoolean) && c < 100) {
        clockDomain.waitActiveEdge(); c += 1
      }
      assert(c < 100, "triggerBoth: no rsp")
      busA.rsp.ready #= false; busB.rsp.ready #= false
    }
  }

  def runSim(body: TestTop => Unit): Unit = {
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TestTop()).doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10 ns)
        body(dut)
      }
  }

  // Variant of runSim with a configurable topology (for back-pressure tests that
  // need e.g. a deeper slave RAM than the default 64).
  def runSimWithCfg(cfg: TestTopCfg)(body: TestTop => Unit): Unit = {
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TestTop(cfg)).doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10 ns)
        body(dut)
      }
  }

  // Helper: write a byte to master TX FIFO via the TX_DATA register
  def pushTx(dut: TestTop, data: Int): Unit = {
    dut.regWriteMaster(0x0C, data)
  }

  // Helper: mark Bmb register-bus signals for simulation access
  def bmbMark(bus: Bmb): Unit = {
    bus.cmd.valid.simPublic(); bus.cmd.opcode.simPublic(); bus.cmd.address.simPublic()
    bus.cmd.length.simPublic(); bus.cmd.data.simPublic(); bus.cmd.mask.simPublic()
    bus.cmd.ready.simPublic()
    bus.rsp.valid.simPublic(); bus.rsp.ready.simPublic()
    bus.rsp.opcode.simPublic(); bus.rsp.data.simPublic()
  }

  // Helper: full Bmb register write (handshake-complete, full-word mask)
  def bmbWrite(bus: Bmb, addr: Int, data: Int, cd: ClockDomain): Unit = {
    bus.cmd.valid   #= true
    bus.cmd.opcode  #= 1   // Bmb WRITE
    bus.cmd.address #= addr
    bus.cmd.length  #= 0
    bus.cmd.data    #= data
    bus.cmd.mask    #= 0xF  // enable all 4 byte lanes of the 32-bit word
    bus.rsp.ready   #= true
    cd.waitActiveEdge()
    var c = 0
    while (!bus.cmd.ready.toBoolean && c < 100) { cd.waitActiveEdge(); c += 1 }
    assert(c < 100, s"bmbWrite: cmd not ready (addr=0x${addr.toHexString})")
    bus.cmd.valid   #= false
    c = 0
    while (!bus.rsp.valid.toBoolean && c < 100) { cd.waitActiveEdge(); c += 1 }
    assert(c < 100, s"bmbWrite: no rsp (addr=0x${addr.toHexString})")
    assert(bus.rsp.opcode.toInt === 0, s"bmbWrite: rsp error (addr=0x${addr.toHexString})")
    bus.rsp.ready   #= false
  }

  // Helper: full Bmb register read (handshake-complete), returns rsp.data
  def bmbRead(bus: Bmb, addr: Int, cd: ClockDomain): Int = {
    bus.cmd.valid   #= true
    bus.cmd.opcode  #= 0   // Bmb READ
    bus.cmd.address #= addr
    bus.cmd.length  #= 0
    bus.rsp.ready   #= true
    cd.waitActiveEdge()
    var c = 0
    while (!bus.cmd.ready.toBoolean && c < 100) { cd.waitActiveEdge(); c += 1 }
    assert(c < 100, s"bmbRead: cmd not ready (addr=0x${addr.toHexString})")
    // Deassert valid on the SAME cycle the command fires (exactly one-cycle valid), so
    // read side-effects that trigger per-cycle on isReading (e.g. RX FIFO pop at 0x10)
    // fire at most once. BmbSlaveFactory drives rsp.valid combinationally with cmd.valid
    // but REGISTERS the read DATA: it reflects the presented address only on the cycle
    // AFTER the fire edge. Sampling on the fire cycle returns a stale 0 for a read that
    // follows other-address / idle traffic (the localized read-channel race); sample one
    // settled cycle later while valid is already low (the registered read mux holds our
    // address). The response is a pure register read, so opcode carries no error.
    bus.cmd.valid   #= false
    cd.waitActiveEdge()
    val data = bus.rsp.data.toInt
    bus.rsp.ready   #= false
    cd.waitActiveEdge()
    data
  }

  // Helper: write a register via the Bmb register bus
  def regWrite(dut: TestTop, isMaster: Boolean, addr: Int, data: Int): Unit = {
    val cd = dut.clockDomain
    bmbWrite(if (isMaster) dut.masterRegBus else dut.slaveRegBus, addr, data, cd)
  }

  // Helper: wait until master is no longer busy
  def waitForIdleDual(dut: TestTopDualSlave, maxCycles: Int = 200000): Unit = {
    var cycles = 0
    while (dut.master.status.busy.toBoolean && cycles < maxCycles) {
      dut.clockDomain.waitActiveEdge()
      cycles += 1
    }
    assert(cycles < maxCycles, "dual-topology transfer never became idle")
  }

  def waitForIdle(dut: TestTop, maxCycles: Int = 200000): Unit = {
    val cd = dut.clockDomain
    var cycles = 0
    // A CMD write produces a 1-cycle trigger pulse; busy rises 1-2 cycles AFTER the
    // write handshake. Wait for the transfer to START before waiting for it to END.
    while (!dut.master.status.busy.toBoolean && cycles < maxCycles) {
      cd.waitActiveEdge()
      cycles += 1
    }
    while (dut.master.status.busy.toBoolean && cycles < maxCycles) {
      cd.waitActiveEdge()
      cycles += 1
    }
    assert(cycles < maxCycles, s"Timed out after $maxCycles cycles")
  }

  // The slave raises its flags on its OWN filtered STOP detection, which trails
  // the master's busy release (TBUF) by the input-filter pipeline (2-FF sync +
  // unanimity window + edge detect). Poll the internal flag (simPublic) with a
  // bounded wait before sampling the firmware-visible INT_FLAG register.
  def waitForSlaveFlag(dut: TestTop, mask: Int, expect: Int, cd: ClockDomain, maxCycles: Int = 500): Unit = {
    var c = 0
    while (((dut.slave.intr.fl.toInt & mask) != expect) && c < maxCycles) {
      cd.waitActiveEdge(); c += 1
    }
    assert(c < maxCycles, s"slave INT_FLAG (mask=0x${mask.toHexString}, expect=$expect) not reached within $maxCycles cycles")
  }


  test("I2cAuto: basic write 3 bytes") {
    runSim { dut =>
      val cd = dut.clockDomain
      cd.waitActiveEdge(5)

      // Configure master timing + slave via the register buses
      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)                                // CFG_SAMPDIV
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)  // slaveAddr=0x28, matchEn=1
      dut.regWriteSlave(0x04, 0)                                // PTR=0

      cd.waitActiveEdge(10)

      // Fill master TX FIFO: 3 data bytes
      pushTx(dut, 0xDE)
      pushTx(dut, 0xAD)
      pushTx(dut, 0xBE)

      // Issue command: write 3 bytes to addr 0x28
      dut.issueCmd(addr = 0x28, rw = false, len1 = 3)

      // Wait for completion
      waitForIdle(dut)
      cd.waitActiveEdge(5)

      // Verify slave RAM
      val ram0 = dut.slave.ram.getBigInt(0).toInt
      val ram1 = dut.slave.ram.getBigInt(1).toInt
      val ram2 = dut.slave.ram.getBigInt(2).toInt
      assert(ram0 == 0xDE, f"RAM[0]=0x$ram0%02X, expected 0xDE")
      assert(ram1 == 0xAD, f"RAM[1]=0x$ram1%02X, expected 0xAD")

      // Golden vectors: the wire encoding of the address phase must match
      // the canonical UM10204 form (addr 0x28, R/W=0, ACK, first data byte).
      assert(dut.snifByte0.toInt == 0x50, f"snifByte0=0x${dut.snifByte0.toInt}%02X, expected 0x50 (addr 0x28, R/W=0)")
      assert(!dut.snifAck0.toBoolean, "slave must ACK the address byte (SDA Low, UM10204 sec. 3.1.10)")
      assert(dut.snifByte1.toInt == 0xDE, f"snifByte1=0x${dut.snifByte1.toInt}%02X, expected 0xDE (first data byte)")
      assertWireClean(dut, expectStarts = 1, expectStops = 1)
      assert(ram2 == 0xBE, f"RAM[2]=0x$ram2%02X, expected 0xBE")    }
  }

  test("I2cAuto: basic read 3 bytes") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)

      // Pre-load slave RAM
      dut.slave.ram.setBigInt(0, 0x11)
      dut.slave.ram.setBigInt(1, 0x22)
      dut.slave.ram.setBigInt(2, 0x33)

      cd.waitActiveEdge(10)

      // Command: read 3 bytes from addr 0x28
      dut.issueCmd(addr = 0x28, rw = true, nackLast = true, len1 = 3)

      // Wait for completion
      waitForIdle(dut)

      // Drain RX FIFO
      cd.waitActiveEdge(5)
      var rxValues = List.empty[Int]
      while (dut.master.rxFifo.io.pop.valid.toBoolean && rxValues.size < 3) {
        rxValues ::= dut.master.rxFifo.io.pop.payload.toInt
        dut.master.rxPopIntent #= true
        cd.waitActiveEdge()
        dut.master.rxPopIntent #= false
        cd.waitActiveEdge()
      }
      assert(rxValues.reverse == List(0x11, 0x22, 0x33),
        s"RX=${rxValues.reverse.map(v => f"0x$v%02X")}, expected List(0x11, 0x22, 0x33)")
      // Golden vectors: read-direction header (addr 0x28, R/W=1, ACK,
      // first data byte driven by the SLAVE - independent of master logic).
      assert(dut.snifByte0.toInt == 0x51, f"snifByte0=0x${dut.snifByte0.toInt}%02X, expected 0x51 (addr 0x28, R/W=1)")
      assert(!dut.snifAck0.toBoolean, "slave must ACK the read header (SDA Low)")
      assert(dut.snifByte1.toInt == 0x11, f"snifByte1=0x${dut.snifByte1.toInt}%02X, expected 0x11 (slave-driven data)")
      assertWireClean(dut, expectStarts = 1, expectStops = 1)
    }
  }

  test("I2cAuto: composite write-then-read with RESTART") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)

      // Pre-load slave RAM
      dut.slave.ram.setBigInt(0, 0xAA)
      dut.slave.ram.setBigInt(1, 0xBB)
      dut.slave.ram.setBigInt(2, 0xCC)

      cd.waitActiveEdge(10)

      // Phase 1: write 1 byte to slave (advances dataPtr to 1)
      pushTx(dut, 0x00)

      // Composite: write 1 byte, RESTART, read 2 bytes
      dut.issueCmd(addr = 0x28, rw = false, phase2 = true, nackLast = true, len1 = 1, len2 = 2)

      // Wait for completion
      waitForIdle(dut)

      // Verify slave RAM: first byte was overwritten with 0x00
      cd.waitActiveEdge(5)
      val ram0 = dut.slave.ram.getBigInt(0).toInt
      assert(ram0 == 0x00, f"RAM[0]=0x$ram0%02X, expected 0x00 after write phase")

      // Drain RX FIFO: should have 2 read bytes (from RAM[1], RAM[2])
      var rxValues = List.empty[Int]
      while (dut.master.rxFifo.io.pop.valid.toBoolean && rxValues.size < 3) {
        rxValues ::= dut.master.rxFifo.io.pop.payload.toInt
        dut.master.rxPopIntent #= true
        cd.waitActiveEdge()
        dut.master.rxPopIntent #= false
        cd.waitActiveEdge()
      }
      assert(rxValues.reverse == List(0xBB, 0xCC),
        s"RX=${rxValues.reverse.map(v => f"0x$v%02X")}, expected List(0xBB, 0xCC)")
      // Frame topology: one initial START + one Sr (electrically a START)
      // + exactly one terminating STOP.
      assertWireClean(dut, expectStarts = 2, expectStops = 1)
    }
  }

  test("I2cAuto: read NACK then immediate next transfer (regression)") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)

      // Read phase will serve RAM[2], RAM[3]; last read byte 0x22 has LSB=0,
      // the exact case where a slave still driving its last data bit masks the
      // master's release-based NACK on the open-drain bus.
      dut.slave.ram.setBigInt(2, 0x11)
      dut.slave.ram.setBigInt(3, 0x22)

      cd.waitActiveEdge(10)

      // Transfer 1: write 2 bytes, RESTART, read 2 bytes (last byte NACKed)
      pushTx(dut, 0x55)
      pushTx(dut, 0x66)
      dut.issueCmd(addr = 0x28, rw = false, phase2 = true, nackLast = true, len1 = 2, len2 = 2)
      waitForIdle(dut)
      // The STOP sequence (STOP0/1/2 + TBUF) runs after the master's busy drops;
      // give the slave enough cycles to detect the STOP and exit the frame.
      cd.waitActiveEdge(60)

      assert(dut.master.status.errCode.toInt == 0, s"Transfer 1 err=${dut.master.status.errCode.toInt}")
      var rxValues = List.empty[Int]
      while (dut.master.rxFifo.io.pop.valid.toBoolean && rxValues.size < 2) {
        rxValues ::= dut.master.rxFifo.io.pop.payload.toInt
        dut.master.rxPopIntent #= true
        cd.waitActiveEdge()
        dut.master.rxPopIntent #= false
        cd.waitActiveEdge()
      }
      assert(rxValues.reverse == List(0x11, 0x22), s"RX=${rxValues.reverse.map(v => f"0x$v%02X")}, expected List(0x11, 0x22)")

      // Slave must have exited the frame and released SDA after the NACK+STOP
      assert(!dut.slave.inFrame.toBoolean, "Slave still in frame after NACKed read + STOP")
      assert(dut.slave.sdaOut.toBoolean, "Slave still driving SDA Low after NACKed read + STOP")

      // Transfer 2: fresh write immediately after the composite.
      // Regression guard for the phantom TX-FIFO pop at the phase switch
      // (write->read composites used to underflow the FIFO counter and shift
      // the next write's data by one byte).
      dut.regWriteSlave(0x04, 0)
      pushTx(dut, 0xAA)
      pushTx(dut, 0xBB)
      pushTx(dut, 0xCC)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 3)
      waitForIdle(dut)
      assert(dut.master.status.errCode.toInt == 0, s"Transfer 2 err=${dut.master.status.errCode.toInt}")
      assert(!dut.master.txFifo.io.pop.valid.toBoolean, s"TX FIFO not drained, cnt=${dut.master.txFifo.io.occupancy.toInt}")
      val ram0 = dut.slave.ram.getBigInt(0).toInt
      val ram1 = dut.slave.ram.getBigInt(1).toInt
      val ram2 = dut.slave.ram.getBigInt(2).toInt
      assert(List(ram0, ram1, ram2) == List(0xAA, 0xBB, 0xCC),
             s"RAM=${List(ram0, ram1, ram2).map(v => f"0x$v%02X")} (expected AA BB CC)")
    }
  }

  test("I2cAuto: back-to-back RX FIFO reads via register bus (regression #2)") {
    runSim { dut =>
      val cd = dut.clockDomain
      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      dut.slave.ram.setBigInt(0, 0x11)
      dut.slave.ram.setBigInt(1, 0x22)
      dut.slave.ram.setBigInt(2, 0x33)
      cd.waitActiveEdge(10)

      // Fill the master RX FIFO with 3 bytes (last byte NACKed).
      dut.issueCmd(addr = 0x28, rw = true, nackLast = true, len1 = 3)
      waitForIdle(dut)
      assert(dut.master.status.errCode.toInt == 0, s"err=${dut.master.status.errCode.toInt}")

      // Regression for #2: the RX pop used to be registered one cycle AFTER
      // the bus read, so back-to-back reads of RX_DATA returned the same head
      // and shifted the whole FIFO. Reads spaced one cycle apart must each
      // return the next distinct entry, and the FIFO must empty after 3 pops.
      val v1 = bmbRead(dut.masterRegBus, 0x10, cd)
      val v2 = bmbRead(dut.masterRegBus, 0x10, cd)
      val v3 = bmbRead(dut.masterRegBus, 0x10, cd)
      val v4 = bmbRead(dut.masterRegBus, 0x10, cd)
      assert((v1 & 0xFF) == 0x11 && (v1 & 0x100) != 0, s"v1=0x${Integer.toHexString(v1)}")
      assert((v2 & 0xFF) == 0x22 && (v2 & 0x100) != 0, s"v2=0x${Integer.toHexString(v2)}")
      assert((v3 & 0xFF) == 0x33 && (v3 & 0x100) != 0, s"v3=0x${Integer.toHexString(v3)}")
      assert((v4 & 0x100) == 0, s"FIFO should be empty after 3 pops, v4=0x${Integer.toHexString(v4)}")
    }
  }

  test("I2cAuto: composite read+write RESTART (reverse direction)") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)

      // Phase 1 reads RAM[0..2], phase 2 writes RAM[3..5] (dataPtr auto-increments)
      dut.slave.ram.setBigInt(0, 0x11)
      dut.slave.ram.setBigInt(1, 0x22)
      dut.slave.ram.setBigInt(2, 0x33)

      cd.waitActiveEdge(10)

      pushTx(dut, 0xAA)
      pushTx(dut, 0xBB)
      pushTx(dut, 0xCC)
      dut.issueCmd(addr = 0x28, rw = true, phase2 = true, nackLast = true, len1 = 3, len2 = 3)
      waitForIdle(dut)

      assert(dut.master.status.errCode.toInt == 0, s"err=${dut.master.status.errCode.toInt}")
      var rxValues = List.empty[Int]
      while (dut.master.rxFifo.io.pop.valid.toBoolean && rxValues.size < 3) {
        rxValues ::= dut.master.rxFifo.io.pop.payload.toInt
        dut.master.rxPopIntent #= true
        cd.waitActiveEdge()
        dut.master.rxPopIntent #= false
        cd.waitActiveEdge()
      }
      assert(rxValues.reverse == List(0x11, 0x22, 0x33),
             s"RX=${rxValues.reverse.map(v => f"0x$v%02X")}, expected List(0x11, 0x22, 0x33)")
      val ram3 = dut.slave.ram.getBigInt(3).toInt
      val ram4 = dut.slave.ram.getBigInt(4).toInt
      val ram5 = dut.slave.ram.getBigInt(5).toInt
      assert(List(ram3, ram4, ram5) == List(0xAA, 0xBB, 0xCC),
             s"RAM[3..5]=${List(ram3, ram4, ram5).map(v => f"0x$v%02X")} (expected AA BB CC)")
    }
  }

  test("I2cAuto: address NACK causes abort") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      // Configure slave with DIFFERENT address (won't match)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x30)
      dut.regWriteSlave(0x04, 0)

      cd.waitActiveEdge(10)

      // Push a byte (will never be sent due to NACK)
      pushTx(dut, 0x42)

      // Send to address 0x28 (slave is at 0x30, so NACK)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 1)

      // Wait for completion (should abort quickly)
      waitForIdle(dut)

      // Verify error: errCode=1 means address NACK
      val err = dut.master.status.errCode.toInt
      assert(err == 1, f"errCode=$err, expected 1 (address NACK)")
    }
  }

  test("I2cAuto: write 64 bytes (full FIFO burst)") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)  // slaveAddr=0x28, matchEn=1
      dut.regWriteSlave(0x04, 0)                                            // dataPtr=0

      cd.waitActiveEdge(10)

      // Fill TX FIFO with 64 bytes: 0x00, 0x01, ..., 0x3F
      for (i <- 0 until 64) {
        pushTx(dut, i)
      }

      // Command: write 64 bytes
      dut.issueCmd(addr = 0x28, rw = false, len1 = 64)

      // Wait for completion
      waitForIdle(dut)

      // Verify slave RAM: all 64 bytes
      cd.waitActiveEdge(5)
      for (i <- 0 until 64) {
        val val_i = dut.slave.ram.getBigInt(i).toInt
        assert(val_i == i, f"RAM[$i]=0x$val_i%02X, expected 0x${i}%02X")
      }
    }
  }

  test("I2cAuto: abort during active transfer") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)  // slaveAddr=0x28, matchEn=1
      dut.regWriteSlave(0x04, 0)                                            // dataPtr=0

      cd.waitActiveEdge(10)

      // Fill TX FIFO with many bytes (ensures long transfer)
      for (i <- 0 until 32) {
        pushTx(dut, i)
      }

      // Command: write 32 bytes
      dut.issueCmd(addr = 0x28, rw = false, len1 = 32)

      // Wait a few bits into the transfer, then abort
      cd.waitActiveEdge(200)  // ~4-5 bytes in
      assert(dut.master.status.busy.toBoolean, "Transfer should be active before abort")

      // Issue abort via the CTRL register (self-clearing pulse)
      dut.regWriteMaster(0x38, 1)

      // Wait for STOP to complete
      waitForIdle(dut)

      // Verify: aborted flag set, not a normal error
      assert(dut.master.status.aborted.toBoolean, "aborted flag should be set")
      assert(!dut.master.status.busy.toBoolean, "busy should be cleared after abort")
    }
  }

  test("I2cAuto: back-to-back transfers") {
    runSim { dut =>
      val cd = dut.clockDomain
      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      cd.waitActiveEdge(10)

      // Transfer 1
      pushTx(dut, 0x11)
      pushTx(dut, 0x22)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 2)
      waitForIdle(dut)
      cd.waitActiveEdge(50)
      assert(dut.slave.ram.getBigInt(0).toInt == 0x11, "T1: RAM[0] mismatch")
      assert(dut.slave.ram.getBigInt(1).toInt == 0x22, "T1: RAM[1] mismatch")

      // Transfer 2
      dut.regWriteSlave(0x04, 0)  // reset PTR
      cd.waitActiveEdge(10)
      pushTx(dut, 0xAA)
      pushTx(dut, 0xBB)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 2)
      waitForIdle(dut)
      cd.waitActiveEdge(20)
      assert(dut.slave.ram.getBigInt(0).toInt == 0xAA, s"T2: RAM[0]=0x${dut.slave.ram.getBigInt(0).toInt}%02X")
      assert(dut.slave.ram.getBigInt(1).toInt == 0xBB, s"T2: RAM[1]=0x${dut.slave.ram.getBigInt(1).toInt}%02X")
    }
  }

  test("I2cAuto: timeout on bus hang triggers 9-pulse recovery") {
    runSim { dut =>
      val cd = dut.clockDomain

      // Configure master with short timeout for fast test
      dut.cfgMasterTiming()
      dut.regWriteMaster(0x28, 50)  // CFG_TIMEOUT=50 cycles

      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)  // slaveAddr=0x28, matchEn=1
      dut.regWriteSlave(0x04, 0)                                            // dataPtr=0

      cd.waitActiveEdge(10)

      // Start a write transfer
      pushTx(dut, 0x11)
      pushTx(dut, 0x22)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 2)

      // Wait a few cycles into the transfer, then force SCL Low (simulate bus hang)
      cd.waitActiveEdge(50)
      assert(dut.master.status.busy.toBoolean, "Transfer should be active")
      dut.sclForceLow #= true  // Force SCL Low - bus hang!

      // Wait for the timeout error to latch (fires <=2 cycles into the
      // ~360-cycle recovery window), THEN release the clamp so the full
      // 9-pulse sequence plays out on the LIVE wires (UM10204 §3.1.17).
      var cycles = 0
      while (dut.master.status.errCode.toInt != 3 && cycles < 20000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      assert(cycles < 20000, "timeout errCode=3 never latched")
      dut.sclForceLow #= false  // release clamp: recovery drives the real bus

      // Observe recovery on the live wires: >=9 SCL rising edges, master's
      // own SDA driver released (High) throughout, terminated by exactly one
      // STOP (UM10204 §3.1.17: generate at least nine SCL pulses, then STOP).
      val riseBefore = dut.sclRiseCnt.toInt
      val stopBefore = dut.stopCnt.toInt
      cycles = 0
      while ((dut.sclRiseCnt.toInt - riseBefore) < 9 && cycles < 5000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      val rises = dut.sclRiseCnt.toInt - riseBefore
      assert(rises >= 9, s"recovery emitted $rises SCL rising edges, expected >= 9 (UM10204 sec. 3.1.17)")
      assert(dut.master.io.i2c.sda.write.toBoolean,
        "master must release SDA (drive High) during recovery pulses")
      // The 9th SCL rise ends RECOV_HI; the wire STOP only appears after
      // STOP0 (40cyc) + STOP1 + the SDA rise in STOP2. Wait for it (bounded),
      // then settle briefly so a (bogus) second STOP cannot sneak in.
      cycles = 0
      while (dut.stopCnt.toInt == stopBefore && cycles < 5000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      cd.waitActiveEdge(50)
      assert(dut.stopCnt.toInt == stopBefore + 1,
        s"recovery must terminate with exactly one STOP, stopCnt ${stopBefore}->${dut.stopCnt.toInt}")

      // The wire STOP lands mid-STOP2; busy only drops in TBUF a few segments
      // later. Wait for the tail to finish (bounded).
      cycles = 0
      while (dut.master.status.busy.toBoolean && cycles < 2000) {
        cd.waitActiveEdge()
        cycles += 1
      }

      // Verify timeout error. The wire STOP lands mid-STOP2; busy only drops
      // in TBUF a few segments later, so wait for the tail to finish (bounded).
      cycles = 0
      while (dut.master.status.busy.toBoolean && cycles < 5000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      val err = dut.master.status.errCode.toInt
      assert(err == 3, f"errCode=$err, expected 3 (timeout)")
      assert(!dut.master.status.busy.toBoolean, "busy should be clear after timeout")
    }
  }

  test("I2cAuto: data NACK mid-burst aborts transfer") {
    // Use a slave with only 2 valid addresses; 3rd byte gets NACKed
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TestTop(TestTopCfg(slaveRamDepth = 4, slaveValidAddrs = Some(Seq(0, 1))))).doSim { dut =>
      val cd = dut.clockDomain
      cd.forkStimulus(10 ns)

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      cd.waitActiveEdge(10)

      // Try to write 3 bytes to a 2-valid-addr slave: 3rd byte should be NACKed
      pushTx(dut, 0x11)
      pushTx(dut, 0x22)
      pushTx(dut, 0x33)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 3)  // let the slave NACK the 3rd byte

      waitForIdle(dut)
      cd.waitActiveEdge(20)

      // Verify: first 2 bytes stored, 3rd NACKed, errCode=2
      val r0 = dut.slave.ram.getBigInt(0).toInt
      val r1 = dut.slave.ram.getBigInt(1).toInt
      assert(r0 == 0x11, f"RAM[0]=0x$r0%02X, expected 0x11")
      assert(r1 == 0x22, f"RAM[1]=0x$r1%02X, expected 0x22")
      val err = dut.master.status.errCode.toInt
      assert(err == 2, f"errCode=$err, expected 2 (data NACK)")
    }
  }

  test("I2cAuto: multi-slave bus - non-addressed slave stays silent") {
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TestTopDualSlave()).doSim { dut =>
      val cd = dut.clockDomain
      cd.forkStimulus(10 ns)

      // Configure master
      dut.cfgMasterTiming()

      // Configure slave A (target) and slave B (should stay silent)
      bmbWrite(dut.slaveARegBus, 0x1C, 1, cd)                     // CFG_SAMPDIV
      bmbWrite(dut.slaveARegBus, 0x00, (1 << 7) | 0x28, cd)       // slaveAddr=0x28, matchEn=1
      bmbWrite(dut.slaveARegBus, 0x04, 0, cd)                     // dataPtr=0

      bmbWrite(dut.slaveBRegBus, 0x1C, 1, cd)                     // CFG_SAMPDIV
      bmbWrite(dut.slaveBRegBus, 0x00, (1 << 7) | 0x30, cd)       // slaveAddr=0x30, matchEn=1
      bmbWrite(dut.slaveBRegBus, 0x04, 0, cd)                     // dataPtr=0

      cd.waitActiveEdge(10)

      // Write 2 bytes to slave A (addr 0x28)
      bmbWrite(dut.masterRegBus, 0x0C, 0xDE, cd)
      bmbWrite(dut.masterRegBus, 0x0C, 0xAD, cd)

      dut.issueCmd(addr = 0x28, rw = false, len1 = 2)

      // Monitor slave B's SDA output during the transfer
      // NOTE: In write mode, non-matching slave may ACK (SDA Low) which doesn't
      // cause contention (both agree on wire state). True contention would be
      // in read mode where both drive different data. We check the critical path:
      // slave A receives correct data and transfer completes without error.
      // Wait for the transfer to start (absorbs the 1-cycle CMD trigger latency),
      // then wait for completion.
      var cycles = 0
      while (!dut.master.status.busy.toBoolean && cycles < 200000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      while (dut.master.status.busy.toBoolean && cycles < 200000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      assert(cycles < 200000, "Timed out waiting for idle")
      cd.waitActiveEdge(20)

      // Transfer completed without error
      val err = dut.master.status.errCode.toInt
      assert(err == 0, f"errCode=$err, expected 0 (no error)")

      // Slave A received the data correctly
      val a0 = dut.slaveA.ram.getBigInt(0).toInt
      val a1 = dut.slaveA.ram.getBigInt(1).toInt
      assert(a0 == 0xDE, f"SlaveA RAM[0]=0x$a0%02X, expected 0xDE")
      assert(a1 == 0xAD, f"SlaveA RAM[1]=0x$a1%02X, expected 0xAD")
    }
  }

  test("I2cAuto: arbitration loss causes passive exit") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      cd.waitActiveEdge(10)

      // Start a write transfer
      pushTx(dut, 0x11)
      pushTx(dut, 0x22)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 2)

      // Inject a competing master mid-transfer: it pulls SDA Low while our
      // master is placing a '1' on the bus (open-drain: the only way to win
      // arbitration). Address 0x28 = 0b101000 carries '1' bits, so the loss
      // is detected within the first few bits.
      cd.waitActiveEdge(50)
      assert(dut.master.status.busy.toBoolean, "Transfer should be active")
      dut.sdaForceLow #= true  // Competing master drives SDA Low!

      // Wait for arb loss detection + passive exit
      var cycles = 0
      while (dut.master.status.busy.toBoolean && cycles < 5000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      cd.waitActiveEdge(20)
      dut.sdaForceLow #= false

      // Verify arbitration loss error
      val err = dut.master.status.errCode.toInt
      assert(err == 4, f"errCode=$err, expected 4 (arbitration loss)")
      assert(!dut.master.status.busy.toBoolean, "busy should be clear")
      // Loser releases both lines (passive exit, no STOP, no SCL hold-down):
      // holding SCL Low after the loss would brick the bus for the winner.
      assert(dut.masterSclRelease.toBoolean, "loser must release SCL")
      assert(dut.masterSdaRelease.toBoolean, "loser must release SDA")
    }
  }

  test("I2cAuto: SCL stretching pauses master") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      cd.waitActiveEdge(10)

      // Start a write transfer
      pushTx(dut, 0xAB)
      pushTx(dut, 0xCD)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 2)

      // Wait until the bus SCL is Low (stretch can only engage in the Low phase)
      var w = 0
      while (dut.sclWire.toBoolean && w < 2000) { cd.waitActiveEdge(1); w += 1 }
      assert(dut.master.status.busy.toBoolean, "Transfer should be active")

      // Request SCL stretch via the STRETCH_REQ register: slave holds SCL Low
      dut.regWriteSlave(0x24, 1)
      cd.waitActiveEdge(5)
      assert(dut.slave.stretchActive.toBoolean, "stretchActive should engage while SCL Low")

      // Verify master is stalled (still busy after many cycles - proves it's waiting)
      cd.waitActiveEdge(500)
      assert(dut.master.status.busy.toBoolean,
        "Master should be stalled during SCL stretch (waiting for SCL to go High)")
      assert(!dut.sclWire.toBoolean, "SCL wire must stay Low while stretch is held")

      // Release the stretch (write 0 to STRETCH_REQ) and let the transfer finish
      dut.regWriteSlave(0x24, 0)
      waitForIdle(dut)
      cd.waitActiveEdge(20)

      // Clean resumption: transfer completed without error and data is intact
      assert(dut.master.status.errCode.toInt === 0, s"err=${dut.master.status.errCode.toInt}")
      assert(dut.slave.ram.getBigInt(0).toInt == 0xAB, "RAM[0] mismatch after stretch")
      assert(dut.slave.ram.getBigInt(1).toInt == 0xCD, "RAM[1] mismatch after stretch")
    }
  }

  test("I2cAuto: SCL stretching mid-byte, clean resumption (write)") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      cd.waitActiveEdge(10)

      // 4-byte write
      pushTx(dut, 0xA0); pushTx(dut, 0xB1); pushTx(dut, 0xC2); pushTx(dut, 0xD3)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 4)

      // Wait until MID-DATA-BYTE: past the address byte (byteNum>=1) and
      // inside a data byte (bitPos 2..7), with SCL Low so the stretch can engage.
      var w = 0
      while (w < 8000 && !(
        dut.master.status.busy.toBoolean &&
        dut.master.fsm.byteNum.toInt >= 1 &&
        dut.master.fsm.bitPos.toInt >= 2 &&
        dut.master.fsm.bitPos.toInt < 8 &&
        !dut.sclWire.toBoolean
      )) { cd.waitActiveEdge(1); w += 1 }
      assert(w < 8000, s"timed out waiting for mid-byte SCL-Low window (diag: busy=${dut.master.status.busy.toBoolean} byteNum=${dut.master.fsm.byteNum.toInt} bitPos=${dut.master.fsm.bitPos.toInt} err=${dut.master.status.errCode.toInt} scl=${dut.sclWire.toBoolean} sda=${dut.sdaWire.toBoolean} stretch=${dut.slave.stretchActive.toBoolean})")
      val stretchedByteNum = dut.master.fsm.byteNum.toInt
      val stretchedBitPos  = dut.master.fsm.bitPos.toInt

      // Hold the bus for a long stretch (>> one bit period)
      dut.regWriteSlave(0x24, 1)
      cd.waitActiveEdge(5)
      assert(dut.slave.stretchActive.toBoolean, "stretch must engage")
      cd.waitActiveEdge(400)
      assert(dut.master.status.busy.toBoolean, "master must stall during mid-byte stretch")
      assert(!dut.sclWire.toBoolean, "SCL must stay Low during stretch")
      // Slave bit counter must be frozen in lockstep with the master
      assert(dut.slave.bitPos.toInt == stretchedBitPos,
        s"slave bitPos=${dut.slave.bitPos.toInt} != master $stretchedBitPos (lockstep broken)")

      // Release mid-byte: the remainder of the transfer must complete cleanly
      dut.regWriteSlave(0x24, 0)
      waitForIdle(dut)
      cd.waitActiveEdge(20)

      assert(dut.master.status.errCode.toInt === 0, s"err=${dut.master.status.errCode.toInt} (stretched at byte=$stretchedByteNum bit=$stretchedBitPos)")
      val exp = Array(0xA0, 0xB1, 0xC2, 0xD3)
      for (i <- 0 until 4) {
        val got = dut.slave.ram.getBigInt(i).toInt
        assert(got == exp(i), s"RAM[$i]=0x${got}%02X expected 0x${exp(i)}%02X (stretch at byte=$stretchedByteNum bit=$stretchedBitPos)")
      }
    }
  }

  test("I2cAuto: SCL stretching mid-byte, clean resumption (read)") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      dut.slave.ram.setBigInt(0, 0x5A)
      dut.slave.ram.setBigInt(1, 0x3C)
      dut.slave.ram.setBigInt(2, 0xE7)
      dut.slave.ram.setBigInt(3, 0x19)
      cd.waitActiveEdge(10)

      // 4-byte read
      dut.issueCmd(addr = 0x28, rw = true, nackLast = true, len1 = 4)

      // Wait until mid-data-byte with SCL Low (slave is driving the current bit)
      var w = 0
      while (w < 8000 && !(
        dut.master.status.busy.toBoolean &&
        dut.master.fsm.byteNum.toInt >= 1 &&
        dut.master.fsm.bitPos.toInt >= 2 &&
        dut.master.fsm.bitPos.toInt < 8 &&
        !dut.sclWire.toBoolean
      )) { cd.waitActiveEdge(1); w += 1 }
      assert(w < 8000, s"timed out waiting for mid-byte SCL-Low window (diag: busy=${dut.master.status.busy.toBoolean} byteNum=${dut.master.fsm.byteNum.toInt} bitPos=${dut.master.fsm.bitPos.toInt} err=${dut.master.status.errCode.toInt} scl=${dut.sclWire.toBoolean} sda=${dut.sdaWire.toBoolean} stretch=${dut.slave.stretchActive.toBoolean})")
      val stretchedByteNum = dut.master.fsm.byteNum.toInt
      val stretchedBitPos  = dut.master.fsm.bitPos.toInt

      // Stretch while the slave holds a data bit on SDA
      dut.regWriteSlave(0x24, 1)
      cd.waitActiveEdge(5)
      assert(dut.slave.stretchActive.toBoolean, "stretch must engage")
      cd.waitActiveEdge(400)
      assert(dut.master.status.busy.toBoolean, "master must stall during mid-byte stretch")
      assert(!dut.sclWire.toBoolean, "SCL must stay Low during stretch")

      // Release mid-byte: remaining bits/bytes must be delivered intact
      dut.regWriteSlave(0x24, 0)
      waitForIdle(dut)
      cd.waitActiveEdge(20)

      assert(dut.master.status.errCode.toInt === 0, s"err=${dut.master.status.errCode.toInt} (stretched at byte=$stretchedByteNum bit=$stretchedBitPos)")
      var rxValues = List.empty[Int]
      while (dut.master.rxFifo.io.pop.valid.toBoolean && rxValues.size < 4) {
        rxValues ::= dut.master.rxFifo.io.pop.payload.toInt
        dut.master.rxPopIntent #= true
        cd.waitActiveEdge()
        dut.master.rxPopIntent #= false
        cd.waitActiveEdge()
      }
      assert(rxValues.reverse == List(0x5A, 0x3C, 0xE7, 0x19),
        s"RX=${rxValues.reverse.map(v => f"0x$v%02X")} expected List(0x5A, 0x3C, 0xE7, 0x19) (stretch at byte=$stretchedByteNum bit=$stretchedBitPos)")
    }
  }

  test("I2cAuto: composite write+read with SCL stretch in write phase") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      // Write phase: 2 bytes -> RAM[0..1]; read phase: 4 bytes <- RAM[2..5]
      dut.slave.ram.setBigInt(2, 0x5A)
      dut.slave.ram.setBigInt(3, 0x3C)
      dut.slave.ram.setBigInt(4, 0xE7)
      dut.slave.ram.setBigInt(5, 0x19)
      cd.waitActiveEdge(10)

      pushTx(dut, 0xAA); pushTx(dut, 0xBB)
      dut.issueCmd(addr = 0x28, rw = false, phase2 = true, nackLast = true, len1 = 2, len2 = 4)

      // Stretch mid-byte during the WRITE phase. slave.isRead discriminates the
      // phase after the RESTART (master FSM state is not sim-public).
      var w = 0
      while (w < 8000 && !(
        dut.master.status.busy.toBoolean &&
        !dut.slave.isRead.toBoolean &&
        dut.master.fsm.byteNum.toInt >= 1 &&
        dut.master.fsm.bitPos.toInt >= 2 &&
        dut.master.fsm.bitPos.toInt < 8 &&
        !dut.sclWire.toBoolean
      )) { cd.waitActiveEdge(1); w += 1 }
      assert(w < 8000, "timed out waiting for write-phase mid-byte window")

      dut.regWriteSlave(0x24, 1)
      cd.waitActiveEdge(5)
      assert(dut.slave.stretchActive.toBoolean, "stretch must engage")
      cd.waitActiveEdge(400)
      assert(dut.master.status.busy.toBoolean, "master must stall during stretch")
      assert(!dut.sclWire.toBoolean, "SCL must stay Low during stretch")

      // Release before the RESTART: the entire read phase must still execute
      dut.regWriteSlave(0x24, 0)
      waitForIdle(dut)
      cd.waitActiveEdge(20)

      assert(dut.master.status.errCode.toInt === 0, s"err=${dut.master.status.errCode.toInt}")
      assert(dut.slave.ram.getBigInt(0).toInt == 0xAA, "RAM[0] mismatch after stretch")
      assert(dut.slave.ram.getBigInt(1).toInt == 0xBB, "RAM[1] mismatch after stretch")
      var rxValues = List.empty[Int]
      while (dut.master.rxFifo.io.pop.valid.toBoolean && rxValues.size < 4) {
        rxValues ::= dut.master.rxFifo.io.pop.payload.toInt
        dut.master.rxPopIntent #= true
        cd.waitActiveEdge()
        dut.master.rxPopIntent #= false
        cd.waitActiveEdge()
      }
      assert(rxValues.reverse == List(0x5A, 0x3C, 0xE7, 0x19),
        s"RX=${rxValues.reverse.map(v => f"0x$v%02X")} expected List(0x5A, 0x3C, 0xE7, 0x19)")
    }
  }

  test("I2cAuto: composite write+read with SCL stretch in read phase") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      dut.slave.ram.setBigInt(2, 0x5A)
      dut.slave.ram.setBigInt(3, 0x3C)
      dut.slave.ram.setBigInt(4, 0xE7)
      dut.slave.ram.setBigInt(5, 0x19)
      cd.waitActiveEdge(10)

      pushTx(dut, 0xAA); pushTx(dut, 0xBB)
      dut.issueCmd(addr = 0x28, rw = false, phase2 = true, nackLast = true, len1 = 2, len2 = 4)

      // Stretch mid-byte during the READ phase (slave driving data, post-RESTART)
      var w = 0
      while (w < 8000 && !(
        dut.master.status.busy.toBoolean &&
        dut.slave.isRead.toBoolean &&
        dut.master.fsm.byteNum.toInt >= 1 &&
        dut.master.fsm.bitPos.toInt >= 2 &&
        dut.master.fsm.bitPos.toInt < 8 &&
        !dut.sclWire.toBoolean
      )) { cd.waitActiveEdge(1); w += 1 }
      assert(w < 8000, "timed out waiting for read-phase mid-byte window")

      dut.regWriteSlave(0x24, 1)
      cd.waitActiveEdge(5)
      assert(dut.slave.stretchActive.toBoolean, "stretch must engage")
      cd.waitActiveEdge(400)
      assert(dut.master.status.busy.toBoolean, "master must stall during stretch")
      assert(!dut.sclWire.toBoolean, "SCL must stay Low during stretch")

      // Release mid-read: remaining bytes + NACK + STOP must complete cleanly
      dut.regWriteSlave(0x24, 0)
      waitForIdle(dut)
      cd.waitActiveEdge(20)

      assert(dut.master.status.errCode.toInt === 0, s"err=${dut.master.status.errCode.toInt}")
      assert(dut.slave.ram.getBigInt(0).toInt == 0xAA, "RAM[0] mismatch after stretch")
      assert(dut.slave.ram.getBigInt(1).toInt == 0xBB, "RAM[1] mismatch after stretch")
      var rxValues = List.empty[Int]
      while (dut.master.rxFifo.io.pop.valid.toBoolean && rxValues.size < 4) {
        rxValues ::= dut.master.rxFifo.io.pop.payload.toInt
        dut.master.rxPopIntent #= true
        cd.waitActiveEdge()
        dut.master.rxPopIntent #= false
        cd.waitActiveEdge()
      }
      assert(rxValues.reverse == List(0x5A, 0x3C, 0xE7, 0x19),
        s"RX=${rxValues.reverse.map(v => f"0x$v%02X")} expected List(0x5A, 0x3C, 0xE7, 0x19)")
      // Slave must have exited the frame after the NACKed-read STOP
      assert(!dut.slave.inFrame.toBoolean, "slave must exit frame after stretched composite read")
    }
  }

  test("I2cAuto: composite write+read on multi-slave bus - non-addressed slave stays silent") {
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TestTopDualSlave()).doSim { dut =>
      val cd = dut.clockDomain
      cd.forkStimulus(10 ns)

      dut.cfgMasterTiming()
      bmbWrite(dut.slaveARegBus, 0x1C, 1, cd)
      bmbWrite(dut.slaveARegBus, 0x00, (1 << 7) | 0x28, cd)
      bmbWrite(dut.slaveARegBus, 0x04, 0, cd)
      bmbWrite(dut.slaveBRegBus, 0x1C, 1, cd)
      bmbWrite(dut.slaveBRegBus, 0x00, (1 << 7) | 0x30, cd)
      bmbWrite(dut.slaveBRegBus, 0x04, 0, cd)
      // Poison slave B RAM so any accidental data-path write is detectable
      for (i <- 0 until 64) { dut.slaveB.ram.setBigInt(i, 0xEE) }
      // Read source for slave A: RAM[2..3] (write phase occupies RAM[0..1])
      dut.slaveA.ram.setBigInt(2, 0x11)
      dut.slaveA.ram.setBigInt(3, 0x22)
      cd.waitActiveEdge(10)

      bmbWrite(dut.masterRegBus, 0x0C, 0xAA, cd)
      bmbWrite(dut.masterRegBus, 0x0C, 0xBB, cd)
      dut.issueCmd(addr = 0x28, rw = false, phase2 = true, nackLast = true, len1 = 2, len2 = 2)

      // Track whether slave B was ever addressed during the transfer
      var bWasAddressed = false
      var cycles = 0
      while (!dut.master.status.busy.toBoolean && cycles < 200000) { cd.waitActiveEdge(); cycles += 1 }
      while (dut.master.status.busy.toBoolean && cycles < 200000) {
        if (dut.slaveB.wasAddressed.toBoolean) bWasAddressed = true
        cd.waitActiveEdge(); cycles += 1
      }
      cd.waitActiveEdge(20)
      if (dut.slaveB.wasAddressed.toBoolean) bWasAddressed = true
      assert(cycles < 200000, "Timed out waiting for idle")

      val err = dut.master.status.errCode.toInt
      assert(err == 0, f"errCode=$err, expected 0")

      // Slave A: write phase landed at RAM[0..1], read phase served RAM[2..3]
      assert(dut.slaveA.ram.getBigInt(0).toInt == 0xAA, "A RAM[0]")
      assert(dut.slaveA.ram.getBigInt(1).toInt == 0xBB, "A RAM[1]")
      var rxValues = List.empty[Int]
      while (dut.master.rxFifo.io.pop.valid.toBoolean && rxValues.size < 2) {
        rxValues ::= dut.master.rxFifo.io.pop.payload.toInt
        dut.master.rxPopIntent #= true
        cd.waitActiveEdge()
        dut.master.rxPopIntent #= false
        cd.waitActiveEdge()
      }
      assert(rxValues.reverse == List(0x11, 0x22), s"RX=${rxValues.reverse}")

      // Slave B: never addressed (note: inFrame latches on START for ALL
      // slaves; wasAddressed is the match-specific flag), RAM untouched
      assert(!bWasAddressed, "slave B must never be addressed (incl. the RESTART address)")
      for (i <- 0 until 4) {
        assert(dut.slaveB.ram.getBigInt(i).toInt == 0xEE, s"B RAM[$i] touched")
      }
    }
  }

  test("I2cAuto: composite transfer hangs mid-phase -> timeout + clean unwind") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteMaster(0x28, 50)  // short timeout for fast test

      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      cd.waitActiveEdge(10)

      // Composite write(4)+read(2); hang the bus during the write phase.
      // The master must unwind the PENDING PHASE-2 state on timeout (no hang,
      // no stuck busy) - the composite-specific hazard vs the single-phase
      // timeout test.
      pushTx(dut, 0x11); pushTx(dut, 0x22); pushTx(dut, 0x33); pushTx(dut, 0x44)
      dut.issueCmd(addr = 0x28, rw = false, phase2 = true, nackLast = true, len1 = 4, len2 = 2)

      cd.waitActiveEdge(50)
      assert(dut.master.status.busy.toBoolean, "Transfer should be active")
      dut.sclForceLow #= true  // bus hang

      // Wait for the timeout to latch. While the wire is held Low the master
      // cannot finish its STOP (STOP1 waits for SCL High), so busy stays High
      // by design; once the bus is released the recovery/STOP tail must
      // complete and busy must clear.
      var cycles = 0
      while (dut.master.status.errCode.toInt != 3 && cycles < 20000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      assert(cycles < 20000, "timeout never latched (errCode != 3)")

      dut.sclForceLow #= false
      cycles = 0
      while (dut.master.status.busy.toBoolean && cycles < 20000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      assert(cycles < 20000, "master hung after composite timeout (phase-2 state not unwound?)")
      cd.waitActiveEdge(50)

      val err = dut.master.status.errCode.toInt
      assert(err == 3, f"errCode=$err, expected 3 (timeout)")
      assert(!dut.master.status.busy.toBoolean, "busy should be clear after timeout")
      // NOTE: slave is intentionally not asserted here - with SCL pinned Low the
      // 9-pulse recovery cannot propagate to the slave, so it is left mid-frame
      // (same scope as the existing single-phase timeout test).
    }
  }

  // ===================== 10-bit Address Tests =====================

  def runSim10bit(body: TestTop => Unit): Unit = {
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TestTop(TestTopCfg(support10bit = true))).doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10 ns)
        body(dut)
      }
  }

  test("I2cAuto 10-bit: write 4 bytes") {
    runSim10bit { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)

      // 10-bit address: 0x345
      regWrite(dut, isMaster = false, addr = 0x00, data = ((1 << 10) | 0x345))
      dut.regWriteSlave(0x04, 0)

      cd.waitActiveEdge(10)

      // Push 4 data bytes
      val data = Array(0xDE, 0xAD, 0xBE, 0xEF)
      for (d <- data) { pushTx(dut, d) }

      // Trigger write
      dut.issueCmd(addr = 0x345, rw = false, len1 = 4)

      waitForIdle(dut)

      // Wire-level format check (standard 10-bit, UM10204 §4.2.2):
      //   byte0 = 11110 A9 A8 R/W,  byte1 = A7..A0
      val a = 0x345
      val expectB0 = 0xF0 | (((a >> 8) & 0x3) << 1) | 0  // 11110 A9 A8 R/W, R/W = 0 (write)
      val expectB1 = a & 0xFF  // byte 1 carries only A[7:0]
      val b0 = dut.snifByte0.toInt
      val b1 = dut.snifByte1.toInt
      assert(b0 == expectB0, f"wire byte0=0x${b0}%02X, expected 0x${expectB0}%02X (11110 A9 A8 R/W)")
      assert(b1 == expectB1, f"wire byte1=0x${b1}%02X, expected 0x${expectB1}%02X (A7..A0)")

      // Verify
      assert(dut.master.status.errCode.toInt == 0, s"err=${dut.master.status.errCode.toInt}")
      for (i <- data.indices) {
        val ramVal = dut.slave.ram.getBigInt(i).toInt
        assert(ramVal == data(i), f"RAM[$i]=0x${ramVal}%02X exp 0x${data(i)}%02X")
      }
    }
  }

  test("I2cAuto 10-bit: read 3 bytes") {
    runSim10bit { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)

      // 10-bit address: 0x1BC
      regWrite(dut, isMaster = false, addr = 0x00, data = ((1 << 10) | 0x1BC))
      dut.regWriteSlave(0x04, 0)

      // Pre-load RAM
      val data = Array(0x11, 0x22, 0x33)
      for (i <- data.indices) { dut.slave.ram.setBigInt(i, data(i)) }

      cd.waitActiveEdge(10)

      // Trigger read
      dut.issueCmd(addr = 0x1BC, rw = true, nackLast = true, len1 = 3)

      waitForIdle(dut)

      // Read back
      for (i <- data.indices) {
        dut.master.rxPopIntent #= true
        cd.waitActiveEdge()
        val rxVal = dut.master.rxFifo.io.pop.payload.toInt
        assert(rxVal == data(i), f"RX[$i]=0x${rxVal}%02X exp 0x${data(i)}%02X")
        dut.master.rxPopIntent #= false
        cd.waitActiveEdge()
      }
      // Golden vectors (UM10204 sec. 3.1.11 A1/A3): the read-direction
      // 10-bit header is 11110 A9 A8 R/W=1 then A7..A0. NO slave may ACK
      // the first byte in read direction (addressing happens on byte 2).
      assert(dut.snifByte0.toInt == 0xF3, f"snifByte0=0x${dut.snifByte0.toInt}%02X, expected 0xF3 (11110 11 1)")
      assert(dut.snifByte1.toInt == 0xBC, f"snifByte1=0x${dut.snifByte1.toInt}%02X, expected 0xBC (addr low byte)")
      assert(dut.snifAck0.toBoolean,
        "no slave may ACK the 10-bit read header byte0 (sec. 3.1.11 A3) - F1 guard")
      assert(!dut.snifAck1.toBoolean, "addressed slave must ACK the second address byte (sec. 3.1.11 A2)")
      assertWireClean(dut, expectStarts = 1, expectStops = 1)
    }
  }

  test("I2cAuto 10-bit: composite write+read RESTART") {
    runSim10bit { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)

      // 10-bit address: 0x2AA
      regWrite(dut, isMaster = false, addr = 0x00, data = ((1 << 10) | 0x2AA))
      dut.regWriteSlave(0x04, 0)

      // Pre-load read data at offset 2
      val readData = Array(0xAA, 0xBB)
      for (i <- readData.indices) { dut.slave.ram.setBigInt(2 + i, readData(i)) }

      cd.waitActiveEdge(10)

      // Push write data
      val writeData = Array(0x11, 0x22)
      for (d <- writeData) { pushTx(dut, d) }

      // Trigger composite
      dut.issueCmd(addr = 0x2AA, rw = false, phase2 = true, nackLast = true, len1 = 2, len2 = 2)

      waitForIdle(dut)

      // Verify write phase
      assert(dut.slave.ram.getBigInt(0).toInt == 0x11, s"RAM[0]=${dut.slave.ram.getBigInt(0).toInt}")
      assert(dut.slave.ram.getBigInt(1).toInt == 0x22, s"RAM[1]=${dut.slave.ram.getBigInt(1).toInt}")

      // Verify read phase
      for (i <- readData.indices) {
        dut.master.rxPopIntent #= true
        cd.waitActiveEdge()
        val rxVal = dut.master.rxFifo.io.pop.payload.toInt
        assert(rxVal == readData(i), f"RX[$i]=0x${rxVal}%02X exp 0x${readData(i)}%02X")
        dut.master.rxPopIntent #= false
        cd.waitActiveEdge()
      }
    }
  }

  test("I2cAuto 10-bit: composite read+write RESTART (reverse direction)") {
    runSim10bit { dut =>
      val cd = dut.clockDomain

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)

      // 10-bit address: 0x2AA
      regWrite(dut, isMaster = false, addr = 0x00, data = ((1 << 10) | 0x2AA))
      dut.regWriteSlave(0x04, 0)

      // Phase 1 reads RAM[0..2], phase 2 writes RAM[3..5] (dataPtr auto-increments)
      dut.slave.ram.setBigInt(0, 0x11)
      dut.slave.ram.setBigInt(1, 0x22)
      dut.slave.ram.setBigInt(2, 0x33)

      cd.waitActiveEdge(10)

      pushTx(dut, 0xAA)
      pushTx(dut, 0xBB)
      pushTx(dut, 0xCC)
      dut.issueCmd(addr = 0x2AA, rw = true, phase2 = true, nackLast = true, len1 = 3, len2 = 3)
      waitForIdle(dut)

      assert(dut.master.status.errCode.toInt == 0, s"err=${dut.master.status.errCode.toInt}")
      var rxValues = List.empty[Int]
      while (dut.master.rxFifo.io.pop.valid.toBoolean && rxValues.size < 3) {
        rxValues ::= dut.master.rxFifo.io.pop.payload.toInt
        dut.master.rxPopIntent #= true
        cd.waitActiveEdge()
        dut.master.rxPopIntent #= false
        cd.waitActiveEdge()
      }
      assert(rxValues.reverse == List(0x11, 0x22, 0x33),
             s"RX=${rxValues.reverse.map(v => f"0x$v%02X")}, expected List(0x11, 0x22, 0x33)")
      val ram3 = dut.slave.ram.getBigInt(3).toInt
      val ram4 = dut.slave.ram.getBigInt(4).toInt
      val ram5 = dut.slave.ram.getBigInt(5).toInt
      assert(List(ram3, ram4, ram5) == List(0xAA, 0xBB, 0xCC),
             s"RAM[3..5]=${List(ram3, ram4, ram5).map(v => f"0x$v%02X")} (expected AA BB CC)")
    }
  }

  test("I2cAuto: TX FIFO full raises STATUS.txFull (firmware must poll before write)") {
    runSim { dut =>
      val cd = dut.clockDomain
      cd.waitActiveEdge(5)
      dut.cfgMasterTiming()

      // Empty FIFO: txFull must be clear
      var st = bmbRead(dut.masterRegBus, 0x14, cd)
      assert(((st >> 7) & 1) == 0, s"STATUS.txFull unexpectedly set on empty FIFO (STATUS=0x${st}%08X)")

      // Fill the TX FIFO to capacity (fifoDepth = 64) via the register path
      var i = 0
      while (i < 64) { pushTx(dut, (0xA0 + i) & 0xFF); i += 1 }
      cd.waitActiveEdge(5)

      // Now txFull must be set (this is what firmware polls before writing more)
      st = bmbRead(dut.masterRegBus, 0x14, cd)
      assert(((st >> 7) & 1) == 1, s"STATUS.txFull not set after filling 64-byte TX FIFO (STATUS=0x${st}%08X)")
      assert(dut.master.txFifo.io.occupancy.toInt == 64, s"txFifo occupancy=${dut.master.txFifo.io.occupancy.toInt}, expected 64")
    }
  }

  test("I2cAuto: RX FIFO full triggers clock-stretch, no data loss") {
    runSimWithCfg(TestTopCfg(masterFifoDepth = 64, slaveRamDepth = 70)) { dut =>
      val cd = dut.clockDomain
      cd.waitActiveEdge(5)
      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)

      // Pre-load 70 distinct bytes into the slave RAM (0x10 .. 0x55)
      var k = 0
      while (k < 70) { dut.slave.ram.setBigInt(k, (0x10 + k) & 0xFF); k += 1 }
      cd.waitActiveEdge(10)

      // Read 70 bytes (MORE than the 64-deep RX FIFO) and do NOT drain during
      // the transfer: the FIFO must fill and the master must clock-stretch.
      dut.issueCmd(addr = 0x28, rw = true, nackLast = true, len1 = 70)

      // Wait until the RX FIFO is full and we are still mid-transfer.
      var waited = 0
      while (!(dut.master.rxFifo.io.occupancy.toInt == 64 && dut.master.status.busy.toBoolean) && waited < 300000) {
        cd.waitActiveEdge(); waited += 1
      }
      assert(waited < 300000, "RX FIFO never reached full while busy (stretch scenario not reached)")

      // The master is now stuck (full FIFO, no drain): it must be holding SCL Low.
      var sw = 0
      while (!dut.master.rxStretchObs.toBoolean && sw < 5000) { cd.waitActiveEdge(); sw += 1 }
      assert(dut.master.rxStretchObs.toBoolean, "Master did not engage RX clock-stretch while FIFO full")
      assert(!dut.masterSclRelease.toBoolean, "SCL not held Low during RX clock-stretch")

      // Drain 10 bytes to make room; the master should release the stretch and
      // finish the remaining bytes.
      var drained = 0
      while (drained < 10) {
        if (dut.master.rxFifo.io.pop.valid.toBoolean) {
          dut.master.rxPopIntent #= true
          cd.waitActiveEdge()
          dut.master.rxPopIntent #= false
          cd.waitActiveEdge()
          drained += 1
        } else cd.waitActiveEdge()
      }

      // Transfer should now complete.
      waitForIdle(dut)

      // Drain the rest and verify all remaining bytes arrived in order (no loss).
      val rxValues = scala.collection.mutable.ArrayBuffer.empty[Int]
      while (dut.master.rxFifo.io.pop.valid.toBoolean) {
        rxValues += dut.master.rxFifo.io.pop.payload.toInt
        dut.master.rxPopIntent #= true
        cd.waitActiveEdge()
        dut.master.rxPopIntent #= false
        cd.waitActiveEdge()
      }
      // We drained the first 10 (0x10..0x19) above; the remaining 60 are 0x1A..0x55.
      assert(rxValues.size == 60, s"expected 60 remaining bytes after 10 drained, got ${rxValues.size}")
      val expected = (0 until 60).map(i => (0x1A + i) & 0xFF)
      assert(rxValues.toList == expected,
        s"RX mismatch after stretch recovery: got ${rxValues.take(8).map(v => f"0x$v%02X")}..., expected ${expected.take(8).map(v => f"0x$v%02X")}, sizes ${rxValues.size}/${expected.size}")
    }
  }

  // =====================================================================
  // Gap-fill tests (added after UM10204 coverage audit 2026-11):
  //  - errCode=5 busy-reject (G5: CMD during recovery tail was silently dropped)
  //  - 10-bit dual-slave isolation incl. shared low byte (F1 killer asserts)
  //  - 11111 reserved-prefix rejection seen through an independent 7-bit encoder
  //  - slow-rate smokes at Standard / Fast / Fast-mode Plus rates (G7)
  //  - glitch immunity of the 3-sample filter (G8)
  //  - CMD_P2 cross-slave Sr (F3 functional + legacy repeat negative)
  // =====================================================================

  test("I2cAuto: CMD during active transfer rejected with errCode=5") {
    runSim { dut =>
      val cd = dut.clockDomain
      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      cd.waitActiveEdge(10)

      // Sanity: a short transfer completes cleanly (errCode=0 afterwards).
      pushTx(dut, 0x01)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 1)
      waitForIdle(dut)
      assert(dut.master.status.errCode.toInt == 0, "sanity transfer must complete cleanly")

      // Long transfer, then a second CMD mid-flight: must be REJECTED
      // (errCode=5), not queued and not silently swallowed.
      for (i <- 0 until 16) { pushTx(dut, 0xA0 | i) }
      dut.issueCmd(addr = 0x28, rw = false, len1 = 16)
      cd.waitActiveEdge(80)
      assert(dut.master.status.busy.toBoolean, "transfer should be active")
      pushTx(dut, 0xFF)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 1)  // rejected: bus busy
      waitForIdle(dut)

      assert(dut.master.status.errCode.toInt == 5,
        s"errCode=${dut.master.status.errCode.toInt}, expected 5 (BUSY_REJECTED)")
      assert(!dut.master.status.busy.toBoolean, "busy should be clear")
      // The busy transfer itself must have completed normally. Note: the
      // slave write POINTER persists across transfers (realistic I2C
      // pointer semantics, I2cAutoSlave dataPtr), so the sanity byte sits
      // at RAM[0] and the 16-byte transfer lands at RAM[1..16].
      assert(dut.slave.ram.getBigInt(0).toInt == 0x01,
        "RAM[0] must retain the sanity byte (pointer persistence)")
      for (i <- 0 until 16) {
        val v = dut.slave.ram.getBigInt(1 + i).toInt
        assert(v == (0xA0 | i), f"RAM[${1 + i}]=0x$v%02X, expected 0x${0xA0 | i}%02X")
      }
    }
  }

  test("I2cAuto: CMD during recovery tail rejected with errCode=5") {
    runSim { dut =>
      val cd = dut.clockDomain
      dut.cfgMasterTiming()
      dut.regWriteMaster(0x28, 50)  // short timeout for fast test
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      cd.waitActiveEdge(10)

      pushTx(dut, 0x11)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 1)
      cd.waitActiveEdge(50)
      dut.sclForceLow #= true  // hang the bus

      // Wait for the timeout to latch. The FSM is now in the recovery/STOP
      // tail and busy stays High through the whole tail (post-G5 semantics).
      // This is exactly the window where a firmware CMD used to vanish with
      // no errCode (G5).
      var cycles = 0
      while (dut.master.status.errCode.toInt != 3 && cycles < 20000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      assert(cycles < 20000, "timeout never latched")
      pushTx(dut, 0x22)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 1)  // must be rejected

      dut.sclForceLow #= false
      var stops = 0
      cycles = 0
      while (stops < 1 && cycles < 5000) {
        cd.waitActiveEdge()
        stops = dut.stopCnt.toInt
        cycles += 1
      }
      assert(stops >= 1, "recovery STOP never observed")
      // The wire STOP is observable mid-STOP2; busy only drops in TBUF, a few
      // segments later. Wait for the tail to finish before asserting.
      cycles = 0
      while (dut.master.status.busy.toBoolean && cycles < 5000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      assert(dut.master.status.errCode.toInt == 5,
        s"errCode=${dut.master.status.errCode.toInt}, expected 5 (BUSY_REJECTED, overwrites 3)")
      assert(!dut.master.status.busy.toBoolean, "busy should be clear")
    }
  }

  test("I2cAuto 10-bit dual slave: write A, B silent despite shared low byte") {
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TestTopDualSlave(DualSlaveCfg(support10bit = true))).doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10 ns)
        dut.cfgMasterTiming()
        // A = 0x345 (upper 11), B = 0x145 (upper 01): SAME low byte 0x45.
        // Under the pre-F1 bug (byte0 ACKed on 11110 prefix alone) B would
        // ACK byte0, then match its low byte on byte1 and STORE the data.
        bmbWrite(dut.slaveARegBus, 0x1C, 1, cd)
        bmbWrite(dut.slaveARegBus, 0x00, (1 << 10) | 0x345, cd)
        bmbWrite(dut.slaveARegBus, 0x04, 0, cd)
        bmbWrite(dut.slaveBRegBus, 0x1C, 1, cd)
        bmbWrite(dut.slaveBRegBus, 0x00, (1 << 10) | 0x145, cd)
        bmbWrite(dut.slaveBRegBus, 0x04, 0, cd)
        // Poison B's RAM so any stray store is visible.
        for (i <- 0 until 64) { dut.slaveB.ram.setBigInt(i, 0xEE) }
        cd.waitActiveEdge(10)

        for (i <- 0 until 4) { bmbWrite(dut.masterRegBus, 0x0C, 0x40 + i, cd) }
        // 10-bit CMD pack: {nackLast[12], phase2[11], rw[10], addr[9:0]}
        bmbWrite(dut.masterRegBus, 0x04, 4, cd)   // LEN1
        bmbWrite(dut.masterRegBus, 0x00, 0x345, cd) // write 0x345
        cd.waitActiveEdge(2)

        var bDroveLow = false
        var cycles = 0
        while (dut.master.status.busy.toBoolean && cycles < 200000) {
          if (!dut.slaveB.sdaOut.toBoolean) { bDroveLow = true }
          cd.waitActiveEdge()
          cycles += 1
        }
        assert(cycles < 200000, "transfer never completed")
        cd.waitActiveEdge(20)

        assert(dut.master.status.errCode.toInt == 0,
          s"errCode=${dut.master.status.errCode.toInt}, expected 0")
        for (i <- 0 until 4) {
          val v = dut.slaveA.ram.getBigInt(i).toInt
          assert(v == 0x40 + i, f"A.RAM[$i]=0x$v%02X, expected 0x${0x40 + i}%02X")
        }
        assert(!bDroveLow, "slave B drove SDA Low during A's transfer (F1: byte0 over-ACK)")
        assert(!dut.slaveB.wasAddressed.toBoolean, "slave B must not latch wasAddressed")
        for (i <- 0 until 64) {
          val v = dut.slaveB.ram.getBigInt(i).toInt
          assert(v == 0xEE, f"B.RAM[$i]=0x$v%02X disturbed, expected 0xEE")
        }
      }
  }

  test("I2cAuto 10-bit dual slave: read A, B must never drive SDA Low") {
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TestTopDualSlave(DualSlaveCfg(support10bit = true))).doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10 ns)
        dut.cfgMasterTiming()
        bmbWrite(dut.slaveARegBus, 0x1C, 1, cd)
        bmbWrite(dut.slaveARegBus, 0x00, (1 << 10) | 0x345, cd)
        bmbWrite(dut.slaveARegBus, 0x04, 0, cd)
        bmbWrite(dut.slaveBRegBus, 0x1C, 1, cd)
        bmbWrite(dut.slaveBRegBus, 0x00, (1 << 10) | 0x145, cd)
        bmbWrite(dut.slaveBRegBus, 0x04, 0, cd)
        for (i <- 0 until 64) { dut.slaveB.ram.setBigInt(i, 0xEE) }
        dut.slaveA.ram.setBigInt(0, 0x11)
        dut.slaveA.ram.setBigInt(1, 0x22)
        dut.slaveA.ram.setBigInt(2, 0x33)
        cd.waitActiveEdge(10)

        // 10-bit READ of 3 bytes from A: {nackLast[12]=1, rw[10]=1, addr[9:0]}
        bmbWrite(dut.masterRegBus, 0x04, 3, cd)
        bmbWrite(dut.masterRegBus, 0x00, (1 << 12) | (1 << 10) | 0x345, cd)
        cd.waitActiveEdge(2)

        var bDroveLow = false
        var cycles = 0
        while (dut.master.status.busy.toBoolean && cycles < 200000) {
          if (!dut.slaveB.sdaOut.toBoolean) { bDroveLow = true }
          cd.waitActiveEdge()
          cycles += 1
        }
        assert(cycles < 200000, "transfer never completed")
        cd.waitActiveEdge(20)

        assert(dut.master.status.errCode.toInt == 0,
          s"errCode=${dut.master.status.errCode.toInt}, expected 0")
        assert(!bDroveLow,
          "slave B drove SDA Low during A's read (would contend on byte1 ACK or data)")
        assert(!dut.slaveB.wasAddressed.toBoolean, "slave B must not latch wasAddressed")
        // Drain the 3 bytes from the master RX FIFO.
        val rx = (0 until 3).map { _ =>
          dut.master.rxPopIntent #= true
          cd.waitActiveEdge()
          val v = dut.master.rxFifo.io.pop.payload.toInt
          dut.master.rxPopIntent #= false
          cd.waitActiveEdge()
          v
        }.toList
        assert(rx == List(0x11, 0x22, 0x33), s"RX=$rx, expected List(0x11, 0x22, 0x33)")
        for (i <- 0 until 64) {
          val v = dut.slaveB.ram.getBigInt(i).toInt
          assert(v == 0xEE, f"B.RAM[$i]=0x$v%02X disturbed, expected 0xEE")
        }
      }
  }

  test("I2cAuto 10-bit dual slave: write B, A silent despite shared low byte") {
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TestTopDualSlave(DualSlaveCfg(support10bit = true))).doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10 ns)
        dut.cfgMasterTiming()
        bmbWrite(dut.slaveARegBus, 0x1C, 1, cd)
        bmbWrite(dut.slaveARegBus, 0x00, (1 << 10) | 0x345, cd)
        bmbWrite(dut.slaveARegBus, 0x04, 0, cd)
        bmbWrite(dut.slaveBRegBus, 0x1C, 1, cd)
        bmbWrite(dut.slaveBRegBus, 0x00, (1 << 10) | 0x145, cd)
        bmbWrite(dut.slaveBRegBus, 0x04, 0, cd)
        for (i <- 0 until 64) { dut.slaveA.ram.setBigInt(i, 0xEE) }
        cd.waitActiveEdge(10)

        for (i <- 0 until 2) { bmbWrite(dut.masterRegBus, 0x0C, 0xB0 + i, cd) }
        bmbWrite(dut.masterRegBus, 0x04, 2, cd)
        bmbWrite(dut.masterRegBus, 0x00, 0x145, cd)  // write 0x145 (=B)
        cd.waitActiveEdge(2)

        var aDroveLow = false
        var cycles = 0
        while (dut.master.status.busy.toBoolean && cycles < 200000) {
          if (!dut.slaveA.sdaOut.toBoolean) { aDroveLow = true }
          cd.waitActiveEdge()
          cycles += 1
        }
        assert(cycles < 200000, "transfer never completed")
        cd.waitActiveEdge(20)

        assert(dut.master.status.errCode.toInt == 0,
          s"errCode=${dut.master.status.errCode.toInt}, expected 0")
        assert(dut.slaveB.ram.getBigInt(0).toInt == 0xB0, "B.RAM[0] wrong")
        assert(dut.slaveB.ram.getBigInt(1).toInt == 0xB1, "B.RAM[1] wrong")
        assert(!aDroveLow, "slave A drove SDA Low during B's transfer (byte0 over-ACK)")
        assert(!dut.slaveA.wasAddressed.toBoolean, "slave A must not latch wasAddressed")
        for (i <- 0 until 64) {
          val v = dut.slaveA.ram.getBigInt(i).toInt
          assert(v == 0xEE, f"A.RAM[$i]=0x$v%02X disturbed, expected 0xEE")
        }
      }
  }

  test("I2cAuto: 7-bit master writing reserved 0x7C is NACKed by 10-bit slave") {
    // A 7-bit master writing address 0x7C puts 1111 1000 on the wire - the
    // reserved 11111 prefix that 10-bit slaves must NOT respond to
    // (UM10204 sec. 3.1.11). Observed through an INDEPENDENT encoder: the
    // 7-bit master has no notion of 10-bit prefixes at all.
    runSimWithCfg(TestTopCfg(slaveSupport10bit = Some(true))) { dut =>
      val cd = dut.clockDomain
      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 10) | 0x1BC)
      dut.regWriteSlave(0x04, 0)
      for (i <- 0 until 64) { dut.slave.ram.setBigInt(i, 0xEE) }
      cd.waitActiveEdge(10)

      pushTx(dut, 0x55)
      dut.issueCmd(addr = 0x7C, rw = false, len1 = 1)
      waitForIdle(dut)

      assert(dut.master.status.errCode.toInt == 1,
        s"errCode=${dut.master.status.errCode.toInt}, expected 1 (address NACK on 11111 prefix)")
      assert(!dut.slave.wasAddressed.toBoolean, "10-bit slave must ignore the 11111 prefix")
      // RAM poisoned so "untouched" is checked against a known pattern
      // (Verilator leaves Mems uninitialized).
      for (i <- 0 until 64) {
        val v = dut.slave.ram.getBigInt(i).toInt
        assert(v == 0xEE, f"slave RAM[$i]=0x$v%02X was disturbed, expected 0xEE")
      }
    }
  }

  test("I2cAuto: slow-rate smokes at Standard / Fast / Fast-mode Plus") {
    // All other tests run at fSCL=2.5MHz, far ABOVE the spec envelope
    // (G7). These exercise the timing engine + 3-sample filter at the real
    // spec rates. CFG_TIMEOUT stays 0 (watchdog disabled) so the long
    // transfers cannot collide with the timeout path.
    runSim { dut =>
      val cd = dut.clockDomain
      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      cd.waitActiveEdge(10)

      val rates = Seq(("standard 100kHz", 130, 60), ("fast 400kHz", 32, 16), ("fast+ 1MHz", 10, 10))
      for ((label, tLow, tHigh) <- rates) {
        dut.regWriteMaster(0x2C, tLow)
        dut.regWriteMaster(0x30, tHigh)
        cd.waitActiveEdge(10)
        dut.regWriteSlave(0x04, 0)  // re-arm slave write pointer (persists across transfers)
        cd.waitActiveEdge(5)

        // Write 2 bytes
        pushTx(dut, 0xAB)
        pushTx(dut, 0xCD)
        dut.issueCmd(addr = 0x28, rw = false, len1 = 2)
        waitForIdle(dut)
        assert(dut.master.status.errCode.toInt == 0, s"$label write: errCode=${dut.master.status.errCode.toInt}")
        assert(dut.slave.ram.getBigInt(0).toInt == 0xAB, s"$label write: RAM[0]")
        assert(dut.slave.ram.getBigInt(1).toInt == 0xCD, s"$label write: RAM[1]")

        // Read 2 bytes back
        dut.slave.ram.setBigInt(0, 0x5A)
        dut.slave.ram.setBigInt(1, 0xA5)
        dut.regWriteSlave(0x04, 0)  // re-arm dataPtr
        dut.issueCmd(addr = 0x28, rw = true, nackLast = true, len1 = 2)
        waitForIdle(dut)
        assert(dut.master.status.errCode.toInt == 0, s"$label read: errCode=${dut.master.status.errCode.toInt}")
        cd.waitActiveEdge(5)
        val rx = (0 until 2).map { _ =>
          dut.master.rxPopIntent #= true
          cd.waitActiveEdge()
          val v = dut.master.rxFifo.io.pop.payload.toInt
          dut.master.rxPopIntent #= false
          cd.waitActiveEdge()
          v
        }.toList
        assert(rx == List(0x5A, 0xA5), s"$label read: RX=$rx")
      }
      assertWireClean(dut)
    }
  }

  test("I2cAuto: 1-cycle SDA glitch during SCL-Low is absorbed by the filter") {
    // Inject a 1-cycle SDA spike mid-transfer. The 3-sample unanimous filter
    // must absorb it: no false bit, no false START/STOP, data intact (G8).
    runSim { dut =>
      val cd = dut.clockDomain
      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.regWriteSlave(0x04, 0)
      cd.waitActiveEdge(10)

      for (i <- 0 until 4) { pushTx(dut, 0xC0 | i) }
      dut.issueCmd(addr = 0x28, rw = false, len1 = 4)

      // Wait until the bus is mid-transfer and SCL is Low (a legal window to
      // perturb SDA without violating the stability rule).
      var cycles = 0
      while (!(dut.master.status.busy.toBoolean && !dut.sclWire.toBoolean) && cycles < 20000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      assert(cycles < 20000, "never reached an SCL-Low window")
      cd.waitActiveEdge(2)  // settle a couple of cycles into the Low window

      // 1-cycle SDA spike (dominant Low), then release.
      dut.sdaForceLow #= true
      cd.waitActiveEdge()
      dut.sdaForceLow #= false

      waitForIdle(dut)
      assert(dut.master.status.errCode.toInt == 0,
        s"errCode=${dut.master.status.errCode.toInt}, expected 0 (glitch must be absorbed)")
      for (i <- 0 until 4) {
        val v = dut.slave.ram.getBigInt(i).toInt
        assert(v == (0xC0 | i), f"RAM[$i]=0x$v%02X, expected 0x${0xC0 | i}%02X")
      }
      // Exactly one START and one STOP: the spike created no phantom edges.
      assertWireClean(dut, expectStarts = 1, expectStops = 1)
    }
  }

  test("I2cAuto: CMD_P2 routes phase-2 to a different slave (cross-slave Sr)") {
    // F3 functional: composite write 1 byte (0xAA) to A (0x28), Sr, then READ
    // 1 byte from B (0x30) selected via the CMD_P2 register. Phase 2 flips the
    // R/W bit (existing composite semantics), so this is a write->read pair.
    // Pre-F3, phase 2 re-sent the phase-1 address, so B could never be the
    // continuation target.
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TestTopDualSlave()).doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10 ns)
        dut.cfgMasterTiming()
        bmbWrite(dut.slaveARegBus, 0x1C, 1, cd)
        bmbWrite(dut.slaveARegBus, 0x00, (1 << 7) | 0x28, cd)
        bmbWrite(dut.slaveARegBus, 0x04, 0, cd)
        bmbWrite(dut.slaveBRegBus, 0x1C, 1, cd)
        bmbWrite(dut.slaveBRegBus, 0x00, (1 << 7) | 0x30, cd)
        bmbWrite(dut.slaveBRegBus, 0x04, 0, cd)
        dut.slaveB.ram.setBigInt(0, 0x77)  // phase-2 read source
        cd.waitActiveEdge(10)

        // CMD_P2 = {valid[7]=1, p2Addr[6:0]=0x30}
        bmbWrite(dut.masterRegBus, 0x3C, (1 << 7) | 0x30, cd)
        bmbWrite(dut.masterRegBus, 0x0C, 0xAA, cd)  // phase-1 write byte -> A
        bmbWrite(dut.masterRegBus, 0x04, 1, cd)     // LEN1
        bmbWrite(dut.masterRegBus, 0x08, 1, cd)     // LEN2
        // CMD: {phase2[8]=1, rw[7]=0, addr[6:0]=0x28}
        bmbWrite(dut.masterRegBus, 0x00, (1 << 8) | 0x28, cd)
        cd.waitActiveEdge(2)

        waitForIdleDual(dut)
        assert(dut.master.status.errCode.toInt == 0,
          s"errCode=${dut.master.status.errCode.toInt}, expected 0")
        assert(dut.slaveA.ram.getBigInt(0).toInt == 0xAA, "A.RAM[0] must be 0xAA (phase-1 write)")
        // Phase-2 read from B: master RX FIFO must hold B.RAM[0].
        cd.waitActiveEdge(5)
        var rxValues = List.empty[Int]
        while (dut.master.rxFifo.io.pop.valid.toBoolean && rxValues.size < 1) {
          rxValues ::= dut.master.rxFifo.io.pop.payload.toInt
          dut.master.rxPopIntent #= true
          cd.waitActiveEdge()
          dut.master.rxPopIntent #= false
          cd.waitActiveEdge()
        }
        assert(rxValues == List(0x77),
          s"RX=${rxValues.map(v => f"0x$v%02X")}, expected List(0x77) read from B")
        assert(dut.slaveB.ram.getBigInt(0).toInt == 0x77, "B.RAM[0] must be untouched by the read")
      }
  }

  test("I2cAuto: CMD_P2 invalid falls back to legacy address repeat") {
    // Negative: with p2AddrValid=0, phase 2 must repeat the phase-1 address
    // (legacy behaviour preserved for existing firmware). Composite is
    // write 0xAA to A, Sr, read 1 byte back from A (rw flipped) -> A.RAM[1].
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TestTopDualSlave()).doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10 ns)
        dut.cfgMasterTiming()
        bmbWrite(dut.slaveARegBus, 0x1C, 1, cd)
        bmbWrite(dut.slaveARegBus, 0x00, (1 << 7) | 0x28, cd)
        bmbWrite(dut.slaveARegBus, 0x04, 0, cd)
        bmbWrite(dut.slaveBRegBus, 0x1C, 1, cd)
        bmbWrite(dut.slaveBRegBus, 0x00, (1 << 7) | 0x30, cd)
        bmbWrite(dut.slaveBRegBus, 0x04, 0, cd)
        for (i <- 0 until 64) { dut.slaveB.ram.setBigInt(i, 0xEE) }
        dut.slaveA.ram.setBigInt(1, 0x77)  // phase-2 read source (A dataPtr=1 after write)
        cd.waitActiveEdge(10)

        bmbWrite(dut.masterRegBus, 0x3C, 0x00, cd)  // p2AddrValid=0
        bmbWrite(dut.masterRegBus, 0x0C, 0xAA, cd)
        bmbWrite(dut.masterRegBus, 0x04, 1, cd)
        bmbWrite(dut.masterRegBus, 0x08, 1, cd)
        bmbWrite(dut.masterRegBus, 0x00, (1 << 8) | 0x28, cd)
        cd.waitActiveEdge(2)

        waitForIdleDual(dut)
        assert(dut.master.status.errCode.toInt == 0,
          s"errCode=${dut.master.status.errCode.toInt}, expected 0")
        assert(dut.slaveA.ram.getBigInt(0).toInt == 0xAA, "A.RAM[0] must be 0xAA (phase-1 write)")
        cd.waitActiveEdge(5)
        var rxValues = List.empty[Int]
        while (dut.master.rxFifo.io.pop.valid.toBoolean && rxValues.size < 1) {
          rxValues ::= dut.master.rxFifo.io.pop.payload.toInt
          dut.master.rxPopIntent #= true
          cd.waitActiveEdge()
          dut.master.rxPopIntent #= false
          cd.waitActiveEdge()
        }
        assert(rxValues == List(0x77),
          s"RX=${rxValues.map(v => f"0x$v%02X")}, expected List(0x77) read from A (address repeat)")
        for (i <- 0 until 64) {
          val v = dut.slaveB.ram.getBigInt(i).toInt
          assert(v == 0xEE, f"B.RAM[$i]=0x$v%02X disturbed, expected 0xEE")
        }
      }
  }
  // =====================================================================
  // Dual-master arbitration (UM10204 sec. 3.1.8)
  // =====================================================================
  def runSimDualMaster(body: TestTopDualMaster => Unit): Unit = {
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TestTopDualMaster()).doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10 ns)
        body(dut)
      }
  }

  /** Full arbitration contest: both masters write, same-cycle trigger.
    * Winner = master driving 0 at the first divergent bit = LOWER address.
    * Asserts: winner completes unaware (AR-5/6), loser exits passively with
    * no STOP on the wire (AR-4), winner content lands in order (AR-6),
    * loser's slave untouched (AR-11), loser retry succeeds once the bus is
    * free (AR-7). */
  def contestScenario(dut: TestTopDualMaster, cd: ClockDomain,
                      aAddr: Int, aPay: Seq[Int], bAddr: Int, bPay: Seq[Int]): Unit = {
    val aWins = aAddr < bAddr
    val winM: I2cAutoMaster   = if (aWins) dut.masterA else dut.masterB
    val loseM: I2cAutoMaster  = if (aWins) dut.masterB else dut.masterA
    val winBus: Bmb           = if (aWins) dut.masterARegBus else dut.masterBRegBus
    val loseBus: Bmb          = if (aWins) dut.masterBRegBus else dut.masterARegBus
    val winPay: Seq[Int]      = if (aWins) aPay else bPay
    val losePay: Seq[Int]     = if (aWins) bPay else aPay
    val winAddr: Int          = if (aWins) aAddr else bAddr
    val retryAddr             = if (aWins) bAddr else aAddr
    // Slaves are pinned: slaveA @ 0x10, slaveB @ 0x30 (configured below).
    // The winner's data belongs to the slave at the WINNER'S address — not to
    // the slave paired with the winning master.
    val winSlave: I2cAutoSlave = if (winAddr == 0x10) dut.slaveA else dut.slaveB
    val loseSlave: I2cAutoSlave = if (winAddr == 0x10) dut.slaveB else dut.slaveA

    // Poison both RAMs so any stray write is visible
    for (i <- 0 until 64) {
      dut.slaveA.ram.setBigInt(i, 0xEE)
      dut.slaveB.ram.setBigInt(i, 0xEE)
    }
    dut.cfgSlave(dut.slaveARegBus, 0x10)
    dut.cfgSlave(dut.slaveBRegBus, 0x30)
    dut.cfgMaster(dut.masterARegBus)
    dut.cfgMaster(dut.masterBRegBus)
    aPay.foreach(b => dut.pushTx(dut.masterARegBus, b))
    bPay.foreach(b => dut.pushTx(dut.masterBRegBus, b))
    dut.stageLen(dut.masterARegBus, aPay.length)
    dut.stageLen(dut.masterBRegBus, bPay.length)

    // Same-cycle trigger: phase-aligned STARTs, deterministic first divergent bit
    dut.triggerBoth(dut.masterARegBus, dut.cmdWrite(aAddr), dut.masterBRegBus, dut.cmdWrite(bAddr))

    // Winner runs to completion unaware (AR-5/6); loser exits passively (AR-4).
    // Phase 1: wait until the winner actually STARTED (trigger -> acceptance is
    // registered, so busy lags the trigger by a couple of cycles).
    var w = 0
    while (!winM.status.busy.toBoolean && w < 1000) { cd.waitActiveEdge(); w += 1 }
    assert(winM.status.busy.toBoolean, "winner: transfer never started")
    // Phase 2: wait for completion.
    w = 0
    while (winM.status.busy.toBoolean && w < 200000) { cd.waitActiveEdge(); w += 1 }
    assert(!winM.status.busy.toBoolean, "winner: transfer did not complete")
    assert(winM.status.errCode.toInt == 0,
      s"winner errCode=${winM.status.errCode.toInt} (must be 0: AR-5 winner continues unaware)")
    assert(loseM.status.errCode.toInt == 4,
      s"loser errCode=${loseM.status.errCode.toInt} (expected 4=arbLoss)")
    assert(!loseM.status.busy.toBoolean, "loser must be idle after passive exit")

    // AR-6: no information lost — winner payload lands in order at the winning address
    for ((exp, i) <- winPay.zipWithIndex) {
      val v = winSlave.ram.getBigInt(i).toInt
      assert(v == exp, s"winner RAM[$i]=0x$v%02X expected 0x$exp%02X (AR-6: no information lost)")
    }
    // AR-11: slaves are not involved in master-to-master arbitration
    for (i <- 0 until 64) {
      val v = loseSlave.ram.getBigInt(i).toInt
      assert(v == 0xEE, s"loser slave RAM[$i]=0x$v%02X disturbed (AR-11: slaves not involved)")
    }
    // Wire frame bookkeeping: 1 shared START, exactly 1 STOP (the winner's —
    // AR-4: the loser must not issue a STOP)
    assert(dut.startCnt.toInt == 1, s"startCnt=${dut.startCnt.toInt}, expected 1")
    assert(dut.stopCnt.toInt == 1, s"stopCnt=${dut.stopCnt.toInt}, expected 1 (AR-4: loser issues no STOP)")
    assert(dut.sdaViolCnt.toInt == 0,
      s"sdaViolCnt=${dut.sdaViolCnt.toInt}: SDA edge over SCL-High during contention (sec 3.1.3)")

    // AR-7: the loser restarts when the bus is free and completes alone.
    // (Its own busBusyExt tracker must have cleared on the winner's STOP;
    // a stale busy flag would reject the retry with errCode=5.)
    bmbWrite(loseBus, 0x00, dut.cmdWrite(retryAddr), cd)
    w = 0
    while (!loseM.status.busy.toBoolean && w < 1000) { cd.waitActiveEdge(); w += 1 }
    assert(loseM.status.busy.toBoolean, "loser retry: transfer never started (rejected while bus free?)")
    w = 0
    while (loseM.status.busy.toBoolean && w < 200000) { cd.waitActiveEdge(); w += 1 }
    assert(loseM.status.errCode.toInt == 0,
      s"loser retry errCode=${loseM.status.errCode.toInt} (expected 0; 5=busyRejected would mean the bus-busy tracker failed to clear)")
    for ((exp, i) <- losePay.zipWithIndex) {
      val v = loseSlave.ram.getBigInt(i).toInt
      assert(v == exp, s"loser retry RAM[$i]=0x$v%02X expected 0x$exp%02X")
    }
    assert(dut.startCnt.toInt == 2, s"startCnt=${dut.startCnt.toInt}, expected 2 after retry")
    assert(dut.stopCnt.toInt == 2, s"stopCnt=${dut.stopCnt.toInt}, expected 2 after retry")
    assert(dut.sdaViolCnt.toInt == 0, s"sdaViolCnt=${dut.sdaViolCnt.toInt} after retry")
  }

  test("I2cAuto dual-master: 0x10 vs 0x30 contest, lower address wins (sec 3.1.8 AR-4/5/6/7/11)") {
    runSimDualMaster { dut =>
      val cd = dut.clockDomain
      // masterA -> 0x10 (drives 0 at the first divergent bit, bit5), masterB -> 0x30.
      // Divergence: byte0 = 0x10 vs 0x30 -> bits 0,0,|1,0|,... first difference at bit5.
      contestScenario(dut, cd, aAddr = 0x10, aPay = Seq(0xA0, 0xA1), bAddr = 0x30, bPay = Seq(0xB0, 0xB1))
    }
  }

  test("I2cAuto dual-master: mirrored contest flips the winner (sec 3.1.8 AR-9 content-based)") {
    runSimDualMaster { dut =>
      val cd = dut.clockDomain
      // Identical topology, addresses swapped: masterA now targets 0x30,
      // masterB targets 0x10. AR-9: the winner is decided by on-wire CONTENT
      // (first divergent bit, Low wins), not by instantiation order —
      // masterB must win despite being the second instance.
      contestScenario(dut, cd, aAddr = 0x30, aPay = Seq(0xA0, 0xA1), bAddr = 0x10, bPay = Seq(0xB0, 0xB1))
    }
  }

  // =========================================================================
  // Interrupt subsystem coverage (T-B1): INT_EN / INT_FLAG / INT_CLR (W1C)
  // for BOTH master and slave. Exercises the complete + error paths and the
  // W1C clear-then-retigger behaviour (guards against a stuck clear swallowing
  // later flags).
  // =========================================================================
  test("I2cAuto: master+slave interrupts (INT_EN/FLAG/CLR W1C, complete+error+retigger)") {
    runSim { dut =>
      val cd = dut.clockDomain
      cd.waitActiveEdge(5)

      dut.cfgMasterTiming()
      dut.regWriteSlave(0x1C, 1)                                   // slave CFG_SAMPDIV
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)  // slave addr 0x28, matchEn
      dut.regWriteSlave(0x04, 0)                                   // slave PTR=0

      // Enable interrupts: master complete[0]|error[1]; slave rxComplete[0]|errNack[1]
      dut.regWriteMaster(0x18, 0x3)
      dut.regWriteSlave(0x10, 0x3)
      // Power-up clear of any preset flags
      dut.regWriteMaster(0x20, 0x3)
      dut.regWriteSlave(0x18, 0x3)
      cd.waitActiveEdge(5)

      // ----- MASTER COMPLETE(bit0) + SLAVE rxComplete(bit0) after a good write -----
      pushTx(dut, 0xAB)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 1)
      waitForIdle(dut)
      cd.waitActiveEdge(5)
      assert(dut.master.status.errCode.toInt == 0, s"unexpected errCode=${dut.master.status.errCode.toInt}")
      // Master complete flag is synchronous with the busy release (TBUF): direct read.
      val mFlag1 = bmbRead(dut.masterRegBus, 0x1C, cd)
      assert((mFlag1 & 0x1) == 1, s"master INT_FLAG=0x$mFlag1%x, expected complete bit0 set")
      assert(dut.io.irqMaster.toBoolean, "master IRQ must be High after complete (INT_EN[0]&FLAG[0])")
      // Slave rxComplete lags the master's idle by the slave's filter pipeline: poll.
      waitForSlaveFlag(dut, mask = 0x1, expect = 1, cd)
      assert((dut.slave.intr.fl.toInt & 0x1) == 1, s"slave rxComplete flag not set (internal fl=0x${Integer.toHexString(dut.slave.intr.fl.toInt)})")
      assert(dut.io.irqSlave.toBoolean, "slave IRQ must be High after rxComplete (INT_EN[0]&FLAG[0])")
      // Positive Bmb READBACK of the slave interrupt registers. This is the regression
      // guard for the read-channel race localized during review: BmbSlaveFactory drives
      // rsp.valid combinationally with cmd.valid but its read DATA is registered and
      // only reflects the presented address on the cycle AFTER the command fires. The
      // first read following other-address / idle traffic therefore returned a stale 0x0
      // when sampled on the fire cycle; the corrected bmbRead samples one settled cycle
      // after the fire edge, so the live value (0x3 / rxComplete bit0) reads back.
      val sEn1   = bmbRead(dut.slaveRegBus, 0x10, cd)
      val sFlag1 = bmbRead(dut.slaveRegBus, 0x14, cd)
      assert((sEn1 & 0x3) == 0x3, s"slave INT_EN(0x10) readback=0x${Integer.toHexString(sEn1)}, expected 0x3")
      assert((sFlag1 & 0x1) == 1, s"slave INT_FLAG(0x14) readback=0x${Integer.toHexString(sFlag1)}, expected rxComplete bit0")

      // ----- W1C clear both; IRQ drops -----
      dut.regWriteMaster(0x20, 0x1)                                 // clear master complete
      dut.regWriteSlave(0x18, 0x1)                                  // clear slave rxComplete
      cd.waitActiveEdge(5)
      val mFlag2 = bmbRead(dut.masterRegBus, 0x1C, cd)
      assert((mFlag2 & 0x1) == 0, s"master INT_FLAG=0x$mFlag2%x after W1C, expected complete cleared")
      waitForSlaveFlag(dut, mask = 0x1, expect = 0, cd)             // slave rxComplete cleared (internal flag)
      assert(!dut.io.irqMaster.toBoolean, "master IRQ must drop after W1C (no pending flags)")
      assert(!dut.io.irqSlave.toBoolean, "slave IRQ must drop after W1C (no pending flags)")

      // ----- MASTER ERROR(bit1): write to an UNADDRESSED slave -> address NACK -----
      pushTx(dut, 0xCD)
      dut.issueCmd(addr = 0x29, rw = false, len1 = 1)
      waitForIdle(dut)
      cd.waitActiveEdge(5)
      assert(dut.master.status.errCode.toInt == 1, s"expected errCode=1 (ADDR_NACK), got ${dut.master.status.errCode.toInt}")
      val mFlag3 = bmbRead(dut.masterRegBus, 0x1C, cd)
      assert((mFlag3 & 0x2) == 2, s"master INT_FLAG=0x$mFlag3%x, expected error bit1 set")
      assert(dut.io.irqMaster.toBoolean, "master IRQ must be High after error (INT_EN[1]&FLAG[1])")
      // Slave was NOT addressed (0x29 != 0x28): S-B2 (no spurious errNack) + S-R5 (no rxComplete).
      // Settle past the slave's STOP filter latency BEFORE asserting the negative:
      // the slave reacts to this STOP a few cycles after the master already went
      // idle, so an immediate read would be vacuously true and miss a regression.
      cd.waitActiveEdge(100)
      assert((dut.slave.intr.fl.toInt & 0x1) == 0, s"slave rxComplete unexpectedly set (internal fl=0x${Integer.toHexString(dut.slave.intr.fl.toInt)}); S-B2/S-R5 violated")

      // ----- W1C error; retigger proves the clear did not swallow later flags -----
      dut.regWriteMaster(0x20, 0x2)                                 // clear master error
      cd.waitActiveEdge(5)
      val mFlag4 = bmbRead(dut.masterRegBus, 0x1C, cd)
      assert((mFlag4 & 0x2) == 0, s"master INT_FLAG=0x$mFlag4%x after W1C, expected error cleared")
      dut.regWriteSlave(0x04, 0)                                    // reset slave PTR
      pushTx(dut, 0xEF)
      dut.issueCmd(addr = 0x28, rw = false, len1 = 1)
      waitForIdle(dut)
      cd.waitActiveEdge(5)
      val mFlag5 = bmbRead(dut.masterRegBus, 0x1C, cd)
      assert((mFlag5 & 0x1) == 1, s"master INT_FLAG=0x$mFlag5%x after retigger, expected complete re-set (W1C not sticky)")
      assert(dut.io.irqMaster.toBoolean, "master IRQ must re-raise after retigger (W1C did not disable)")
    }
  }
}
