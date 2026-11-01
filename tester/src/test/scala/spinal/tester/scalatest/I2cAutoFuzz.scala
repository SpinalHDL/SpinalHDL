package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.sim._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbSlaveFactory}
import spinal.lib.com.i2c._
import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random

/**
  * Randomized stress test for I2cAutoMaster + I2cAutoSlave.
  *
  * Register maps are instantiated at the TOP level: each core's io is bound
  * to a top-level Bmb slave port via driveFrom(BmbSlaveFactory). All firmware
  * interaction goes through those register buses (bmbWrite), mirroring real
  * software access.
  */
class I2cAutoFuzz extends AnyFunSuite {

  class FuzzTop extends Component {
    val master = new I2cAutoMaster(I2cAutoMaster.Config(fifoDepth = 64, timerWidth = 12))
    val slave  = new I2cAutoSlave(I2cAutoSlave.Config(ramDepth = 64))

    val sclWire = master.io.i2c.scl.write && slave.io.i2c.scl.read
    master.io.i2c.scl.read := sclWire
    slave.io.i2c.scl.write := sclWire

    val sdaAnd = master.io.i2c.sda.write && slave.io.i2c.sda.read
    master.io.i2c.sda.read := sdaAnd
    slave.io.i2c.sda.write := sdaAnd

    // Register maps: top-level Bmb slave ports + factories bound to the cores
    val masterRegBus = spinal.lib.slave(Bmb(I2cAutoMaster.getBmbParameter()))
    val slaveRegBus  = spinal.lib.slave(Bmb(I2cAutoSlave.getBmbParameter()))
    val masterRegMap = BmbSlaveFactory(masterRegBus)
    val slaveRegMap  = BmbSlaveFactory(slaveRegBus)
    master.io.driveFrom(masterRegMap, 0)
    slave.io.driveFrom(slaveRegMap, 0)

    // Bmb register buses are top-level inputs: left undriven at elaboration;
    // tests drive them via bmbWrite.
    bmbMark(masterRegBus)
    bmbMark(slaveRegBus)
  }

  // Push a byte via the TX_DATA register (real firmware path)
  def pushTx(cd: ClockDomain, dut: FuzzTop, data: Int): Unit = {
    bmbWrite(dut.masterRegBus, 0x0C, data, cd)
  }

  // Issue a command via the LEN1/LEN2/CMD registers (atomic trigger)
  def issueCmd(cd: ClockDomain, dut: FuzzTop, addr: Int, rw: Boolean,
               phase2: Boolean = false, nackLast: Boolean = false,
               len1: Int, len2: Int = 0): Unit = {
    bmbWrite(dut.masterRegBus, 0x04, len1, cd)
    bmbWrite(dut.masterRegBus, 0x08, len2, cd)
    val data = (if (nackLast) 1 else 0) << 9 |
               (if (phase2)   1 else 0) << 8 |
               (if (rw)       1 else 0) << 7 |
               addr
    bmbWrite(dut.masterRegBus, 0x00, data, cd)
  }

  def waitForIdle(cd: ClockDomain, dut: FuzzTop, maxCycles: Int = 500000): Boolean = {
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
    // Extended settle: wait for STOP sequence + slave to exit frame
    cd.waitActiveEdge(50)
    cycles < maxCycles
  }

  def resetBetweenIter(cd: ClockDomain, dut: FuzzTop): Unit = {
    // Drain RX FIFO
    while (dut.master.rxFifo.notEmpty.toBoolean) {
      dut.master.rxFifo.popValid #= true
      cd.waitActiveEdge()
      dut.master.rxFifo.popValid #= false
      cd.waitActiveEdge()
    }
    // Wait for TX FIFO to drain (master should have consumed all)
    var waitTx = 0
    while (dut.master.txFifo.cnt.toInt > 0 && waitTx < 2000) {
      cd.waitActiveEdge()
      waitTx += 1
    }
    // Extra settle for slave to fully exit frame
    cd.waitActiveEdge(30)
  }

  def drainRx(cd: ClockDomain, dut: FuzzTop): List[Int] = {
    var result = List.empty[Int]
    while (dut.master.rxFifo.notEmpty.toBoolean) {
      result ::= dut.master.rxFifo.head.toInt
      dut.master.rxFifo.popValid #= true
      cd.waitActiveEdge()
      dut.master.rxFifo.popValid #= false
      cd.waitActiveEdge()
    }
    result.reverse
  }

  // Helper: tie Bmb input signals to defaults (elaboration needs drivers; tests override via #=)
  def bmbTieOff(bus: Bmb): Unit = {
    bus.cmd.valid   := False
    bus.cmd.opcode  := 0
    bus.cmd.address := 0
    bus.cmd.length  := 0
    bus.cmd.data    := 0
    bus.cmd.mask    := 0
    bus.rsp.ready   := False
  }

  // Helper: mark Bmb register-bus signals for simulation access
  def bmbMark(bus: Bmb): Unit = {
    bus.cmd.valid.simPublic(); bus.cmd.opcode.simPublic(); bus.cmd.address.simPublic()
    bus.cmd.length.simPublic(); bus.cmd.data.simPublic(); bus.cmd.mask.simPublic()
    bus.cmd.ready.simPublic()
    bus.rsp.valid.simPublic(); bus.rsp.ready.simPublic()
    bus.rsp.opcode.simPublic(); bus.rsp.data.simPublic()
  }

  // Helper: full Bmb register write (handshake-complete)
  def bmbWrite(bus: Bmb, addr: Int, data: Int, cd: ClockDomain): Unit = {
    bus.cmd.valid   #= true
    bus.cmd.opcode  #= 1   // Bmb WRITE
    bus.cmd.address #= addr
    bus.cmd.length  #= 0
    bus.cmd.data    #= data
    bus.cmd.mask    #= 0xF
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

  // Helper: write a register via the Bmb register bus
  def regWrite(dut: FuzzTop, isMaster: Boolean, addr: Int, data: Int): Unit = {
    val cd = dut.clockDomain
    bmbWrite(if (isMaster) dut.masterRegBus else dut.slaveRegBus, addr, data, cd)
  }

  // Configure master + slave timing via the register buses
  def cfgTiming(cd: ClockDomain, dut: FuzzTop): Unit = {
    regWrite(dut, isMaster = true,  addr = 0x24, data = 1)   // CFG_SAMPDIV
    regWrite(dut, isMaster = true,  addr = 0x2C, data = 20)  // CFG_TLOW
    regWrite(dut, isMaster = true,  addr = 0x30, data = 20)  // CFG_THIGH
    regWrite(dut, isMaster = true,  addr = 0x34, data = 20)  // CFG_TBUF
    regWrite(dut, isMaster = false, addr = 0x1C, data = 1)   // CFG_SAMPDIV
  }

  test("I2cAuto fuzz: 100 random writes") {
    val rng = new Random(42L)
    var failures = 0

    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new FuzzTop()).doSim { dut =>
      val cd = dut.clockDomain
      cd.forkStimulus(10 ns)

      cfgTiming(cd, dut)
      regWrite(dut, isMaster = false, addr = 0x00, data = ((1 << 7) | 0x28))
      cd.waitActiveEdge(10)

      for (iter <- 1 to 100) {
        val len = 1 + rng.nextInt(32)  // 1..32 for speed
        val firstByte = rng.nextInt(256)

        // Reset slave pointer
        regWrite(dut, isMaster = false, addr = 0x04, data = 0)
        cd.waitActiveEdge(5)

        // Push data: first byte is known, rest are random
        pushTx(cd, dut, firstByte)
        for (_ <- 1 until len) {
          pushTx(cd, dut, rng.nextInt(256))
        }

        // Trigger write
        issueCmd(cd, dut, addr = 0x28, rw = false, len1 = len)

        // Wait
        val ok = waitForIdle(cd, dut)
        if (!ok) {
          failures += 1
          if (failures <= 3) println(s"  iter=$iter len=$len TIMEOUT")
        }

        val err = dut.master.status.errCode.toInt
        if (err != 0) {
          failures += 1
          if (failures <= 3) println(s"  iter=$iter len=$len ERR=$err")
        }

        // Spot-check: first byte in slave RAM[0]
        val r0 = dut.slave.ram.getBigInt(0).toInt
        if (r0 != firstByte) {
          failures += 1
          if (failures <= 5) println(s"  iter=$iter len=$len r0=0x${r0.toHexString} exp=0x${firstByte.toHexString}")
        }

        // Cleanup between iterations
        resetBetweenIter(cd, dut)
      }

      assert(failures == 0, s"$failures/100 write iterations failed")
    }
  }

  test("I2cAuto fuzz: 50 random reads") {
    val rng = new Random(777L)
    var failures = 0

    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new FuzzTop()).doSim { dut =>
      val cd = dut.clockDomain
      cd.forkStimulus(10 ns)

      cfgTiming(cd, dut)
      regWrite(dut, isMaster = false, addr = 0x00, data = ((1 << 7) | 0x28))
      cd.waitActiveEdge(10)

      for (iter <- 1 to 50) {
        val len = 1 + rng.nextInt(16)  // 1..16 for speed
        val expectedFirst = rng.nextInt(256)

        // Pre-load slave RAM with identifiable pattern
        for (i <- 0 until 64) {
          dut.slave.ram.setBigInt(i, if (i == 0) expectedFirst else 0xFF)
        }
        regWrite(dut, isMaster = false, addr = 0x04, data = 0)
        cd.waitActiveEdge(5)

        // Trigger read
        issueCmd(cd, dut, addr = 0x28, rw = true, nackLast = true, len1 = len)

        // Per-iteration failure tracking: one bad iteration counts as 1,
        // regardless of how many sub-checks (timeout/err/rx) it trips.
        var bad = false
        val ok = waitForIdle(cd, dut)
        if (!ok) { bad = true; if (failures <= 3) println(s"  iter=$iter len=$len TIMEOUT") }

        val err = dut.master.status.errCode.toInt
        if (err != 0) { bad = true; if (failures <= 3) println(s"  iter=$iter len=$len ERR=$err") }

        val rx = drainRx(cd, dut)
        if (rx.isEmpty || rx.head != expectedFirst) {
          bad = true
          if (failures <= 5) println(s"  iter=$iter len=$len rx=${if (rx.isEmpty) "EMPTY" else f"0x${rx.head}%02X"} exp=0x${expectedFirst}%02X count=${rx.size}")
        }

        if (bad) failures += 1

        // Cleanup between iterations
        resetBetweenIter(cd, dut)
      }

      // Root cause of the former ~1/50 flake was a slave-side STOP-mask bug
      // (slave re-armed next data bit after master NACK, holding SDA Low
      // through the STOP edge). Fixed via masterNack flag -> zero tolerance.
      assert(failures == 0, s"$failures/50 read iterations failed")
    }
  }

  // Previously ignored due to dataPtr inter-iteration state leak from
  // #=-poking an active register. Now the dataPtr reset goes through the
  // register bus (proper PTR load pulse), so the test is re-enabled.
  test("I2cAuto fuzz: 20 random composite (write+read RESTART)") {
    val rng = new Random(31415L)
    var failures = 0

    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new FuzzTop()).doSim { dut =>
      val cd = dut.clockDomain
      cd.forkStimulus(10 ns)

      cfgTiming(cd, dut)
      regWrite(dut, isMaster = false, addr = 0x00, data = ((1 << 7) | 0x28))
      cd.waitActiveEdge(10)

      for (iter <- 1 to 20) {
        val wLen = 1 + rng.nextInt(8)   // write 1-8 bytes
        val rLen = 1 + rng.nextInt(8)   // read 1-8 bytes
        val writeData = (0 until wLen).map(_ => rng.nextInt(256))
        val readExpect = (0 until rLen).map(_ => rng.nextInt(256))

        // Pre-load slave RAM:
        // Positions 0..wLen-1: will be overwritten by write phase
        // Positions wLen..wLen+rLen-1: read source (pre-loaded)
        for (i <- 0 until 64) {
          val v = if (i >= wLen && i < wLen + rLen) readExpect(i - wLen) else 0x00
          dut.slave.ram.setBigInt(i, v)
        }
        regWrite(dut, isMaster = false, addr = 0x04, data = 0)
        cd.waitActiveEdge(5)

        // Push write data
        for (d <- writeData) {
          pushTx(cd, dut, d)
        }

        // Trigger composite
        issueCmd(cd, dut, addr = 0x28, rw = false, phase2 = true, nackLast = true,
                 len1 = wLen, len2 = rLen)

        val ok = waitForIdle(cd, dut)
        if (!ok) {
          failures += 1
          if (failures <= 3) println(s"  iter=$iter w=$wLen r=$rLen TIMEOUT")
        }

        val err = dut.master.status.errCode.toInt
        if (err != 0) {
          failures += 1
          if (failures <= 3) println(s"  iter=$iter w=$wLen r=$rLen ERR=$err")
        }

        // Verify read data
        val rx = drainRx(cd, dut)
        if (rx.size != rLen) {
          failures += 1
          if (failures <= 5) println(s"  iter=$iter w=$wLen r=$rLen rxCount=${rx.size} exp=$rLen")
        } else {
          val mismatches = rx.zip(readExpect).count { case (a, b) => a != b }
          if (mismatches > 0) {
            failures += 1
            if (failures <= 5) println(s"  iter=$iter w=$wLen r=$rLen mismatches=$mismatches rx=${rx.take(3)} exp=${readExpect.take(3)}")
          }
        }

        // Cleanup between iterations
        resetBetweenIter(cd, dut)
      }

      assert(failures == 0, s"$failures/20 composite iterations failed")
    }
  }
}
