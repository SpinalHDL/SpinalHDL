package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAccessParameter, BmbSourceParameter, BmbSlaveFactory}
import spinal.lib.com.i2c._
import spinal.lib.com.i2c.sim.{I2cSimDriver, I2cSimEvent, OpenDrainInterconnect => SimInterconnect}
import org.scalatest.funsuite.AnyFunSuite

/**
  * SpinalSim tests for I2cSimDriver — the soft-host I2C master / bus oracle
  * on the open-drain interconnect (UM10204 conformance map F-T2 / F-T3).
  *
  * The DUT is attached to an [[OpenDrainInterconnect]] per line (master- or
  * slave-side pin roles); the soft host drives and decodes the very same
  * wires. Covered: complete frames driven by the soft host against the
  * I2cAutoSlave DUT (START / address / data / ACK / STOP / Repeated START);
  * Table 9 spike immunity (sub-threshold SCL/SDA pulses add no spurious
  * bit / data / ACK / START / STOP events); and UM10204 §3.1.8 arbitration
  * contention (the soft host as loser: releases both lines, stays a passive
  * receiver until the winner's STOP, issues no STOP); plus the I2cAutoMaster
  * DUT's external-bus tracker rejecting a CMD while the soft host holds the
  * bus.
  */
class I2cSimDriverSim extends AnyFunSuite {

  // The two simulated wires of a topology (created in the sim body, where
  // the sim context is live; DUT pins attach to them there as well).
  case class SimBus(scl: SimInterconnect, sda: SimInterconnect)

  /** DUT = I2cAutoSlave; its i2c input pins get elaboration-time default-High
    * RTL drivers (the sim-side interconnect `#=` overrides them from t=0). */
  class TopSlave extends Component {
    val slave = new I2cAutoSlave(I2cAutoSlave.Config(validAddrs = (0 until 64).toSeq))
    pubSlave(slave)
    slave.io.i2c.scl.write := True
    slave.io.i2c.sda.write := True
    // simPublic taps (repo convention: DUT observation taps live in the
    // test-source top class — see the note in I2cAutoSlave.scala; without
    // these ram/flags are UNACCESSIBLE in the simulation).

    val regBusParam = BmbAccessParameter(8, 32).addSources(1, BmbSourceParameter(contextWidth = 0, lengthWidth = 2)).toBmbParameter()
    val slaveRegBus = spinal.lib.slave(Bmb(regBusParam))
    val slaveRegMap = BmbSlaveFactory(slaveRegBus)
    slave.io.driveFrom(slaveRegMap, 0)
    bmbMark(slaveRegBus)
  }

  /** DUT = I2cAutoMaster + I2cAutoSlave on the same simulated wires. */
  class TopMaster extends Component {
    val master = new I2cAutoMaster(I2cAutoMaster.Config(fifoDepth = 64, timerWidth = 12))
    val slave  = new I2cAutoSlave(I2cAutoSlave.Config(validAddrs = (0 until 64).toSeq))
    pubMaster(master); pubSlave(slave)
    master.io.i2c.scl.read := True
    master.io.i2c.sda.read := True
    slave.io.i2c.scl.write  := True
    slave.io.i2c.sda.write  := True

    val masterRegParam = BmbAccessParameter(8, 32).addSources(1, BmbSourceParameter(contextWidth = 0, lengthWidth = 2)).toBmbParameter()
    val slaveRegParam  = BmbAccessParameter(8, 32).addSources(1, BmbSourceParameter(contextWidth = 0, lengthWidth = 2)).toBmbParameter()
    val masterRegBus = spinal.lib.slave(Bmb(masterRegParam))
    val slaveRegBus  = spinal.lib.slave(Bmb(slaveRegParam))
    val masterRegMap = BmbSlaveFactory(masterRegBus)
    val slaveRegMap  = BmbSlaveFactory(slaveRegBus)
    master.io.driveFrom(masterRegMap, 0)
    slave.io.driveFrom(slaveRegMap, 0)
    bmbMark(masterRegBus); bmbMark(slaveRegBus)
  }

  // ---- simPublic taps (published in test sources, keeping the lib sim-free) ----
  private def pubMaster(m: I2cAutoMaster): Unit = {
    m.status.busy.simPublic(); m.status.errCode.simPublic()
  }

  private def pubSlave(s: I2cAutoSlave): Unit = {
    s.intr.flags.simPublic()
    s.ram.simPublic()
  }

  // ---- Bmb register-bus helpers (same handshake style as I2cAutoFuzz) ----
  def bmbMark(bus: Bmb): Unit = {
    bus.cmd.valid.simPublic(); bus.cmd.opcode.simPublic(); bus.cmd.address.simPublic()
    bus.cmd.length.simPublic(); bus.cmd.data.simPublic(); bus.cmd.mask.simPublic()
    bus.cmd.ready.simPublic()
    bus.rsp.valid.simPublic(); bus.rsp.ready.simPublic()
    bus.rsp.opcode.simPublic(); bus.rsp.data.simPublic()
  }

  def bmbWrite(bus: Bmb, addr: Int, data: Int, cd: ClockDomain): Unit = {
    // Drain any pending rsp first: while rsp.valid is High, cmd_ready is Low
    // (stage back-pressure), so a command issued into a pending rsp would
    // never fire and its side effect would be silently lost.
    bus.rsp.ready #= true
    var c = 0
    while (bus.rsp.valid.toBoolean && c < 100) { cd.waitActiveEdge(); c += 1 }
    assert(c < 100, s"bmbWrite: rsp never drained (addr=0x${addr.toHexString})")
    bus.rsp.ready #= false
    bus.cmd.valid   #= true
    bus.cmd.opcode  #= 1   // Bmb WRITE
    bus.cmd.address #= addr
    bus.cmd.length  #= 0
    bus.cmd.data    #= data
    bus.cmd.mask    #= 0xF
    // Hold cmd.valid until the regmap's rsp register latches it (the load is
    // one DUT edge after the fire edge). rsp.ready stays Low while holding,
    // so cmd_ready drops with the latched rsp and the command fires exactly
    // once (no duplicate onWrite side effects); the rsp valid then stays High
    // until consumed below. This is robust even when the set lands mid-cycle
    // (e.g. right after a sleep() while a soft-host engine fork consumes
    // edges) — a one-cycle-valid pulse can be cleared before the load edge.
    c = 0
    while (!bus.rsp.valid.toBoolean && c < 100) { cd.waitActiveEdge(); c += 1 }
    assert(c < 100, s"bmbWrite: no rsp (addr=0x${addr.toHexString})")
    assert(bus.rsp.opcode.toInt === 0, s"bmbWrite: rsp error (addr=0x${addr.toHexString})")
    bus.cmd.valid #= false
    bus.rsp.ready #= true
    // Consume and wait until the rsp register is actually empty again —
    // the next bmbWrite's drain makes this invariant explicit.
    c = 0
    while (bus.rsp.valid.toBoolean && c < 100) { cd.waitActiveEdge(); c += 1 }
    assert(c < 100, s"bmbWrite: rsp not consumed (addr=0x${addr.toHexString})")
    bus.rsp.ready #= false
  }

  // ---- DUT configuration helpers (real firmware register paths) ----
  def cfgSlave(bus: Bmb, cd: ClockDomain, addr: Int): Unit = {
    bmbWrite(bus, 0x1C, 1, cd)                      // CFG_SAMPDIV
    bmbWrite(bus, 0x00, (1 << 7) | addr, cd)        // ADDR: matchEn + 7-bit address
    bmbWrite(bus, 0x04, 0, cd)                      // PTR = 0
  }

  def cfgMaster(bus: Bmb, cd: ClockDomain): Unit = {
    bmbWrite(bus, 0x28, 1, cd)                      // CFG_SAMPDIV
    bmbWrite(bus, 0x30, 20, cd)                     // CFG_TLOW  (20 cyc x 10 ns = 200 ns)
    bmbWrite(bus, 0x34, 20, cd)                     // CFG_THIGH
    bmbWrite(bus, 0x38, 20, cd)                     // CFG_TBUF
  }

  def pushTx(bus: Bmb, cd: ClockDomain, data: Int): Unit = bmbWrite(bus, 0x10, data, cd)

  def issueCmd(bus: Bmb, cd: ClockDomain, addr: Int, byteCount: Int): Unit = {
    bmbWrite(bus, 0x04, byteCount, cd)              // BYTE_COUNT
    bmbWrite(bus, 0x00, addr, cd)                   // CMD: plain write, rw=0
  }

  def waitForDutIdle(dut: TopMaster, maxCycles: Int = 300000): Unit = {
    val cd = dut.clockDomain
    var c = 0
    // A CMD write produces a 1-cycle trigger; busy rises 1-2 cycles later.
    while (!dut.master.status.busy.toBoolean && c < maxCycles) { cd.waitActiveEdge(); c += 1 }
    while (dut.master.status.busy.toBoolean && c < maxCycles) { cd.waitActiveEdge(); c += 1 }
    assert(c < maxCycles, "DUT master never became idle")
  }

  def waitForSlaveRxDone(dut: TopSlave): Unit = waitForSlaveRxDone(dut.slave, dut.clockDomain)

  def waitForSlaveRxDone(dut: TopMaster): Unit = waitForSlaveRxDone(dut.slave, dut.clockDomain)

  private def waitForSlaveRxDone(slave: I2cAutoSlave, cd: ClockDomain): Unit = {
    var c = 0
    while ((slave.intr.flags.toInt & 1) != 1 && c < 1000) { cd.waitActiveEdge(); c += 1 }
    assert(c < 1000, "slave rxComplete flag not set")
  }

  // Verilator does not guarantee undriven top-level input pins start Low
  // (observed: a Bmb cmd.valid pin born High re-latches the regmap rsp
  // register every edge, so a drain that requires cmd.valid=0 can never
  // finish). Drive every BMB pin Low once at sim start, like the house
  // TestTop does for the raw I2C pins.
  private def initBmb(bus: Bmb): Unit = {
    bus.cmd.valid #= false
    bus.cmd.opcode #= 0
    bus.cmd.address #= 0
    bus.cmd.length #= 0
    bus.cmd.data #= 0
    bus.cmd.mask #= 0
    bus.rsp.ready #= false
  }

  // ---- run helpers: compile + wire the DUT into the simulated interconnects ----
  def runSlave(body: (TopSlave, I2cSimDriver, SimBus) => Unit): Unit = {
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TopSlave()).doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10 ns)
        initBmb(dut.slaveRegBus)
        val bus = SimBus(new SimInterconnect, new SimInterconnect)
        bus.scl.addHardSlave(dut.slave.io.i2c.scl)
        bus.sda.addHardSlave(dut.slave.io.i2c.sda)
        // default-High until the interconnect takes over (input pins need a
        // driver; the sim-time assignments below win over the interconnect's
        // initial value in either case, since both start High)
        dut.slave.io.i2c.scl.write #= true
        dut.slave.io.i2c.sda.write #= true
        body(dut, new I2cSimDriver(bus.scl, bus.sda), bus)
      }
  }

  def runMaster(body: (TopMaster, I2cSimDriver, SimBus) => Unit): Unit = {
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TopMaster()).doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10 ns)
        initBmb(dut.masterRegBus)
        initBmb(dut.slaveRegBus)
        val bus = SimBus(new SimInterconnect, new SimInterconnect)
        bus.scl.addHardMaster(dut.master.io.i2c.scl)
        bus.scl.addHardSlave(dut.slave.io.i2c.scl)
        bus.sda.addHardMaster(dut.master.io.i2c.sda)
        bus.sda.addHardSlave(dut.slave.io.i2c.sda)
        dut.master.io.i2c.scl.read #= true
        dut.master.io.i2c.sda.read #= true
        dut.slave.io.i2c.scl.write  #= true
        dut.slave.io.i2c.sda.write  #= true
        body(dut, new I2cSimDriver(bus.scl, bus.sda), bus)
      }
  }

  // =================================================================
  // F-T2: the soft host as master against the I2cAutoSlave DUT
  // =================================================================

  test("I2cSimDriver: write frame to I2cAutoSlave (S, addr, data, ACK, P)") {
    runSlave { (dut, driver, _) =>
      import I2cSimEvent._
      val cd = dut.clockDomain
      cfgSlave(dut.slaveRegBus, cd, 0x28)
      cd.waitActiveEdge(10)

      driver.feedWriteFrame(0x28, Seq(0xDE, 0xAD, 0xBE))
      driver.waitDrained()
      sleep(100 ns)

      val evts = driver.takeEvents()
      assert(evts == List(Start, Data(0x50), Ack, Data(0xDE), Ack, Data(0xAD), Ack, Data(0xBE), Ack, Stop),
        s"decoded events=$evts")
      assert(!driver.arbLost, "no contention here: arbLost must stay clear")
      assert(dut.slave.ram.getBigInt(0).toInt == 0xDE, f"RAM[0]=0x${dut.slave.ram.getBigInt(0).toInt}%02X, expected 0xDE")
      assert(dut.slave.ram.getBigInt(1).toInt == 0xAD, f"RAM[1]=0x${dut.slave.ram.getBigInt(1).toInt}%02X, expected 0xAD")
      assert(dut.slave.ram.getBigInt(2).toInt == 0xBE, f"RAM[2]=0x${dut.slave.ram.getBigInt(2).toInt}%02X, expected 0xBE")
      waitForSlaveRxDone(dut)
    }
  }

  test("I2cSimDriver: read frame from I2cAutoSlave (slave-driven data, host ACK/NACK)") {
    runSlave { (dut, driver, _) =>
      import I2cSimEvent._
      val cd = dut.clockDomain
      cfgSlave(dut.slaveRegBus, cd, 0x28)
      dut.slave.ram.setBigInt(0, 0x11)
      dut.slave.ram.setBigInt(1, 0x22)
      dut.slave.ram.setBigInt(2, 0x33)
      cd.waitActiveEdge(10)

      driver.feedStart()
      driver.feedAddrRead(0x28)
      driver.feedReadByte(ack = true)
      driver.feedReadByte(ack = true)
      driver.feedReadByte(ack = false)   // NACK the last byte
      driver.feedStop()
      driver.waitDrained()
      sleep(100 ns)

      val evts = driver.takeEvents()
      assert(evts == List(Start, Data(0x51), Ack, Data(0x11), Ack, Data(0x22), Ack, Data(0x33), Nack, Stop),
        s"decoded events=$evts")
      var c = 0
      while (dut.slave.inFrame.toBoolean && c < 1000) { cd.waitActiveEdge(); c += 1 }
      assert(!dut.slave.inFrame.toBoolean, "slave still in frame after NACKed read + STOP")
      waitForSlaveRxDone(dut)
    }
  }

  // =================================================================
  // F-T2: Repeated START / arbitration / spike immunity
  // =================================================================

  test("I2cSimDriver: Repeated-START composite (write phase, Sr, read phase)") {
    runSlave { (dut, driver, _) =>
      import I2cSimEvent._
      val cd = dut.clockDomain
      cfgSlave(dut.slaveRegBus, cd, 0x28)
      dut.slave.ram.setBigInt(0, 0xEE)   // sentinel: must be overwritten by the write phase
      dut.slave.ram.setBigInt(1, 0x44)
      dut.slave.ram.setBigInt(2, 0x55)
      cd.waitActiveEdge(10)

      driver.feedStart()
      driver.feedAddrWrite(0x28)
      driver.feedByte(0x00)
      driver.feedAckSlot()
      driver.feedRestart()
      driver.feedAddrRead(0x28)
      driver.feedReadByte(ack = true)
      driver.feedReadByte(ack = false)
      driver.feedStop()
      driver.waitDrained()
      sleep(100 ns)

      val evts = driver.takeEvents()
      assert(evts == List(Start, Data(0x50), Ack, Data(0x00), Ack,
                          Start, Data(0x51), Ack, Data(0x44), Ack, Data(0x55), Nack, Stop),
        s"decoded events=$evts")
      assert(driver.stopCount == 1, s"stopCount=${driver.stopCount}, expected 1 (no stop at the Repeated START)")
      assert(dut.slave.ram.getBigInt(0).toInt == 0x00, "write phase did not store data")
      waitForSlaveRxDone(dut)
    }
  }

  test("I2cSimDriver: Table 9 spike immunity (20 ns SCL/SDA dips add no events)") {
    runSlave { (dut, driver, bus) =>
      import I2cSimEvent._
      val cd = dut.clockDomain
      cfgSlave(dut.slaveRegBus, cd, 0x28)
      dut.slave.ram.setBigInt(0, 0xA5)
      dut.slave.ram.setBigInt(1, 0x5A)
      dut.slave.ram.setBigInt(2, 0xA5)
      cd.waitActiveEdge(10)

      val spikeScl = bus.scl.newSoftConnection()
      val spikeSda = bus.sda.newSoftConnection()
      // Host SCL period is 500 ns (400 ns bit + 50 ns filter-commit lag); SCL-High
      // phase p covers [t0+450+500p, t0+700+500p] with t0 ~= now (the engine starts
      // the frame within 10 ns). Both dips are sub-threshold (20 ns < 50 ns filter /
      // < Table 9 tSP) and must produce no event.
      delayed(2500 ns) { // p4, mid-address (SCL High phase)
        spikeScl.write(false)
        delayed(20 ns) { spikeScl.write(true) }
      }
      delayed(5000 ns) { // p9, data byte bit 7; slave releases (bit=1) so the dip is wire-visible
        spikeSda.write(false)
        delayed(20 ns) { spikeSda.write(true) }
      }

      driver.feedStart()
      driver.feedAddrRead(0x28)
      driver.feedReadByte(ack = true)
      driver.feedReadByte(ack = true)
      driver.feedReadByte(ack = false)
      driver.feedStop()
      driver.waitDrained()
      sleep(100 ns)

      val evts = driver.takeEvents()
      assert(evts == List(Start, Data(0x51), Ack, Data(0xA5), Ack, Data(0x5A), Ack, Data(0xA5), Nack, Stop),
        s"decoded events=$evts")
      waitForSlaveRxDone(dut)
    }
  }

  test("I2cSimDriver: arbitration contest — soft host loses, DUT master wins, no STOP from loser") {
    runMaster { (dut, driver, _) =>
      import I2cSimEvent._
      val cd = dut.clockDomain
      cfgMaster(dut.masterRegBus, cd)
      cfgSlave(dut.slaveRegBus, cd, 0x10)
      cd.waitActiveEdge(10)

      // Soft host contends at 0x30 (110 0000) while the DUT master (its first
      // CMD: external-bus tracker unarmed, so it joins the bus) writes 0x10
      // (010 0000). They diverge on the first bit: DUT drives Low, host releases.
      driver.feedContestFrame(0x30, Seq(0xEE, 0xFF))
      pushTx(dut.masterRegBus, cd, 0x12)
      pushTx(dut.masterRegBus, cd, 0x34)
      issueCmd(dut.masterRegBus, cd, 0x10, 2)

      var c = 0
      while (!driver.arbLost && c < 30000) { cd.waitActiveEdge(); c += 1 }
      assert(driver.arbLost, "soft host never lost arbitration")
      waitForDutIdle(dut)
      sleep(200 ns)

      // The loser's decoder must have seen the WINNER's frame.
      val evts = driver.takeEvents()
      assert(evts == List(Start, Data(0x20), Ack, Data(0x12), Ack, Data(0x34), Ack, Stop),
        s"decoded events=$evts")
      assert(driver.stopCount == 0, "loser must not issue STOP")
      assert(driver.sdaReleased && driver.sclReleased, "loser must release both lines")
      assert(dut.master.status.errCode.toInt == 0, s"errCode=${dut.master.status.errCode.toInt}, expected 0")
      assert(dut.slave.ram.getBigInt(0).toInt == 0x12, "DUT write byte 0 lost")
      assert(dut.slave.ram.getBigInt(1).toInt == 0x34, "DUT write byte 1 lost")
      waitForSlaveRxDone(dut)
    }
  }

  test("I2cAutoMaster: external-bus tracker rejects CMD while the soft host holds the bus") {
    runMaster { (dut, driver, _) =>
      val cd = dut.clockDomain
      cfgMaster(dut.masterRegBus, cd)
      cfgSlave(dut.slaveRegBus, cd, 0x28)
      cd.waitActiveEdge(10)

      // 1) A dummy transfer arms the external-bus tracker (busSeenOnce).
      pushTx(dut.masterRegBus, cd, 0x11)
      issueCmd(dut.masterRegBus, cd, 0x28, 1)
      waitForDutIdle(dut)
      assert(dut.master.status.errCode.toInt == 0, "dummy transfer failed")

      // 2) The soft host occupies the bus with a live frame (no STOP yet).
      cd.waitActiveEdge(20)
      driver.feedStart()
      driver.feedAddrWrite(0x28)
      driver.feedByte(0x77)
      driver.feedAckSlot()
      sleep(500 ns)   // the DUT's filtered START edge latches busBusyExt

      pushTx(dut.masterRegBus, cd, 0x99)
      issueCmd(dut.masterRegBus, cd, 0x28, 1)
      sleep(200 ns)
      assert(dut.master.status.errCode.toInt == 5, s"expected BUSY_REJECT(5), got ${dut.master.status.errCode.toInt}")
      assert(!dut.master.status.busy.toBoolean, "rejected CMD must not start a transfer")

      // 3) The host releases with STOP; the tracker clears; the next CMD runs.
      driver.feedStop()
      driver.waitDrained()
      sleep(500 ns)
      pushTx(dut.masterRegBus, cd, 0x88)
      issueCmd(dut.masterRegBus, cd, 0x28, 1)
      waitForDutIdle(dut)
      assert(dut.master.status.errCode.toInt == 0, "CMD after host STOP must be accepted")

      // RAM holds: dummy (0x11), soft-host write (0x77), post-STOP DUT write (0x88);
      // the rejected 0x99 is absent.
      assert(dut.slave.ram.getBigInt(0).toInt == 0x11, "RAM[0]")
      assert(dut.slave.ram.getBigInt(1).toInt == 0x77, "RAM[1]")
      assert(dut.slave.ram.getBigInt(2).toInt == 0x88, "RAM[2]")
      waitForSlaveRxDone(dut)
    }
  }
}
