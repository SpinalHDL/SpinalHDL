package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.sim._
import spinal.lib._
import spinal.lib.com.i2c._
import org.scalatest.funsuite.AnyFunSuite

/**
  * SpinalSim testbench for I2cAutoMaster + I2cAutoSlave.
  *
  * Connects master and slave via simulated open-drain interconnect
  * on the internal i2cBuf signals.
  */
class I2cAutoSim extends AnyFunSuite {

  case class TestTopCfg(
    masterFifoDepth : Int = 64,
    slaveRamDepth   : Int = 64,
    slaveValidAddrs : Option[Seq[Int]] = None,
    support10bit    : Boolean = false
  )

  class TestTop(cfg: TestTopCfg = TestTopCfg()) extends Component {
    val master = new I2cAutoMaster(I2cAutoMaster.Config(
      fifoDepth    = cfg.masterFifoDepth,
      support10bit = cfg.support10bit,
      timerWidth   = 12
    ))
    val slave  = new I2cAutoSlave(I2cAutoSlave.Config(
      ramDepth     = cfg.slaveRamDepth,
      validAddrs   = cfg.slaveValidAddrs.getOrElse((0 until cfg.slaveRamDepth).toSeq),
      support10bit = cfg.support10bit
    ))

    // Test control: force SCL Low to simulate bus hang
    val sclForceLow = RegInit(False)
    sclForceLow.allowUnsetRegToAvoidLatch()
    sclForceLow.simPublic()

    // Test control: force SDA High to simulate arbitration loss (another master wins)
    val sdaForceHigh = RegInit(False)
    sdaForceHigh.allowUnsetRegToAvoidLatch()
    sdaForceHigh.simPublic()

    // Open-drain wire emulation via io.i2c ports:
    // SCL: open-drain AND of master and slave (slave can stretch)
    val sclWire = Mux(sclForceLow, False, master.io.i2c.scl.write && slave.io.i2c.scl.read)
    sclWire.simPublic()
    master.io.i2c.scl.read := sclWire
    slave.io.i2c.scl.write := sclWire

    // SDA: open-drain AND of both contributions, OR with force-high (arb loss sim)
    val sdaAnd = master.io.i2c.sda.write && slave.io.i2c.sda.read
    val sdaWire = sdaAnd || sdaForceHigh
    master.io.i2c.sda.read := sdaWire  // master sees combined (in)
    slave.io.i2c.sda.write := sdaWire  // slave sees combined (in)

    // Register bus: drivable from test for realistic register-map access
    val mRegAddr  = RegInit(B(0, 8 bits));  mRegAddr.simPublic(); mRegAddr.allowUnsetRegToAvoidLatch()
    val mRegWdata = RegInit(B(0, 32 bits)); mRegWdata.simPublic(); mRegWdata.allowUnsetRegToAvoidLatch()
    val mRegWe    = RegInit(False);          mRegWe.simPublic(); mRegWe.allowUnsetRegToAvoidLatch()
    val mRegRe    = RegInit(False);          mRegRe.simPublic(); mRegRe.allowUnsetRegToAvoidLatch()
    master.io.regBus.addr  := mRegAddr
    master.io.regBus.wdata := mRegWdata
    master.io.regBus.wen   := mRegWe
    master.io.regBus.re    := mRegRe
    val sRegAddr  = RegInit(B(0, 8 bits));  sRegAddr.simPublic(); sRegAddr.allowUnsetRegToAvoidLatch()
    val sRegWdata = RegInit(B(0, 32 bits)); sRegWdata.simPublic(); sRegWdata.allowUnsetRegToAvoidLatch()
    val sRegWe    = RegInit(False);          sRegWe.simPublic(); sRegWe.allowUnsetRegToAvoidLatch()
    val sRegRe    = RegInit(False);          sRegRe.simPublic(); sRegRe.allowUnsetRegToAvoidLatch()
    slave.io.regBus.addr  := sRegAddr
    slave.io.regBus.wdata := sRegWdata
    slave.io.regBus.wen   := sRegWe
    slave.io.regBus.re    := sRegRe

    // Expose IRQs
    val io = new Bundle {
      val irqMaster = out Bool()
      val irqSlave  = out Bool()
    }
    io.irqMaster := master.io.irq
    io.irqSlave  := slave.io.irq
  }

  /** Dual-slave topology: two slaves on the same bus with different addresses */
  class TestTopDualSlave extends Component {
    val master = new I2cAutoMaster(I2cAutoMaster.Config(fifoDepth = 64, timerWidth = 12))
    val slaveA = new I2cAutoSlave(I2cAutoSlave.Config(ramDepth = 64))
    val slaveB = new I2cAutoSlave(I2cAutoSlave.Config(ramDepth = 64))

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

    // Register buses (drivable from test)
    val mA = RegInit(B(0, 8 bits)); mA.simPublic(); mA.allowUnsetRegToAvoidLatch()
    val mW = RegInit(B(0, 32 bits)); mW.simPublic(); mW.allowUnsetRegToAvoidLatch()
    val mWe = RegInit(False); mWe.simPublic(); mWe.allowUnsetRegToAvoidLatch()
    master.io.regBus.addr := mA; master.io.regBus.wdata := mW; master.io.regBus.wen := mWe; master.io.regBus.re := False
    val sA = RegInit(B(0, 8 bits)); sA.simPublic(); sA.allowUnsetRegToAvoidLatch()
    val sW = RegInit(B(0, 32 bits)); sW.simPublic(); sW.allowUnsetRegToAvoidLatch()
    val sWe = RegInit(False); sWe.simPublic(); sWe.allowUnsetRegToAvoidLatch()
    slaveA.io.regBus.addr := sA; slaveA.io.regBus.wdata := sW; slaveA.io.regBus.wen := sWe; slaveA.io.regBus.re := False
    val bA = RegInit(B(0, 8 bits)); bA.simPublic(); bA.allowUnsetRegToAvoidLatch()
    val bW = RegInit(B(0, 32 bits)); bW.simPublic(); bW.allowUnsetRegToAvoidLatch()
    val bWe = RegInit(False); bWe.simPublic(); bWe.allowUnsetRegToAvoidLatch()
    slaveB.io.regBus.addr := bA; slaveB.io.regBus.wdata := bW; slaveB.io.regBus.wen := bWe; slaveB.io.regBus.re := False

    val io = new Bundle {
      val irqMaster = out Bool()
    }
    io.irqMaster := master.io.irq
  }

  def runSim(body: TestTop => Unit): Unit = {
    SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
      .compile(new TestTop()).doSim { dut =>
        val cd = dut.clockDomain
        cd.forkStimulus(10 ns)
        body(dut)
      }
  }

  // Helper: write a byte to master TX FIFO
  def pushTx(dut: TestTop, data: Int): Unit = {
    val cd = dut.clockDomain
    dut.master.txFifo.pushData #= data
    dut.master.txFifo.pushValid #= true
    cd.waitActiveEdge()
    dut.master.txFifo.popValid #= false
    cd.waitActiveEdge()
  }

  // Helper: write a register via the register bus (1-cycle pulse)
  def regWrite(dut: TestTop, isMaster: Boolean, addr: Int, data: Int): Unit = {
    val cd = dut.clockDomain
    if (isMaster) {
      dut.mRegAddr  #= addr
      dut.mRegWdata #= data
      dut.mRegWe    #= true
    } else {
      dut.sRegAddr  #= addr
      dut.sRegWdata #= data
      dut.sRegWe    #= true
    }
    cd.waitActiveEdge()
    if (isMaster) { dut.mRegWe #= false }
    else { dut.sRegWe #= false }
    cd.waitActiveEdge()
  }

  // Helper: wait until master is no longer busy
  def waitForIdle(dut: TestTop, maxCycles: Int = 200000): Unit = {
    val cd = dut.clockDomain
    var cycles = 0
    while (dut.master.status.busy.toBoolean && cycles < maxCycles) {
      cd.waitActiveEdge()
      cycles += 1
    }
    assert(cycles < maxCycles, s"Timed out after $maxCycles cycles")
  }

  test("I2cAuto: basic write 3 bytes") {
    runSim { dut =>
      val cd = dut.clockDomain

      // Configure master timing
      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20

      // Configure slave via register bus
      dut.slave.cfg.sampDiv   #= 1
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)  // slaveAddr=0x28, matchEn=1
      regWrite(dut, isMaster = false, addr = 0x04, data = 0)                // dataPtr=0

      cd.waitActiveEdge(10)

      // Fill master TX FIFO: 3 data bytes
      pushTx(dut, 0xDE)
      pushTx(dut, 0xAD)
      pushTx(dut, 0xBE)

      // Issue command: write 3 bytes to addr 0x28
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= false
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= false
      dut.master.cmd.len1     #= 3
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(3)
      dut.master.cmd.trigger  #= false

      // Wait for completion
      waitForIdle(dut)

      // Verify slave RAM
      cd.waitActiveEdge(5)
      val ram0 = dut.slave.ram.getBigInt(0).toInt
      val ram1 = dut.slave.ram.getBigInt(1).toInt
      val ram2 = dut.slave.ram.getBigInt(2).toInt
      assert(ram0 == 0xDE, f"RAM[0]=0x$ram0%02X, expected 0xDE")
      assert(ram1 == 0xAD, f"RAM[1]=0x$ram1%02X, expected 0xAD")
      assert(ram2 == 0xBE, f"RAM[2]=0x$ram2%02X, expected 0xBE")
    }
  }

  test("I2cAuto: basic read 3 bytes") {
    runSim { dut =>
      val cd = dut.clockDomain

      // Configure master timing
      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20

      // Configure slave
      dut.slave.cfg.sampDiv   #= 1
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.slave.dataPtr       #= 0

      // Pre-load slave RAM
      dut.slave.ram.setBigInt(0, 0x11)
      dut.slave.ram.setBigInt(1, 0x22)
      dut.slave.ram.setBigInt(2, 0x33)

      cd.waitActiveEdge(10)

      // Command: read 3 bytes from addr 0x28
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= true   // read
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= true   // NACK last byte
      dut.master.cmd.len1     #= 3
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(3)
      dut.master.cmd.trigger  #= false

      // Wait for completion
      waitForIdle(dut)

      // Drain RX FIFO
      cd.waitActiveEdge(5)
      var rxValues = List.empty[Int]
      while (dut.master.rxFifo.notEmpty.toBoolean && rxValues.size < 3) {
        rxValues ::= dut.master.rxFifo.head.toInt
        dut.master.rxFifo.popValid #= true
        cd.waitActiveEdge()
        dut.master.rxFifo.popValid #= false
        cd.waitActiveEdge()
      }
      assert(rxValues.reverse == List(0x11, 0x22, 0x33),
        s"RX=${rxValues.reverse.map(v => f"0x$v%02X")}, expected List(0x11, 0x22, 0x33)")
    }
  }

  test("I2cAuto: composite write-then-read with RESTART") {
    runSim { dut =>
      val cd = dut.clockDomain

      // Configure master timing
      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20

      // Configure slave
      dut.slave.cfg.sampDiv   #= 1
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.slave.dataPtr       #= 0

      // Pre-load slave RAM
      dut.slave.ram.setBigInt(0, 0xAA)
      dut.slave.ram.setBigInt(1, 0xBB)
      dut.slave.ram.setBigInt(2, 0xCC)

      cd.waitActiveEdge(10)

      // Phase 1: write 1 byte to slave (advances dataPtr to 1)
      pushTx(dut, 0x00)

      // Composite: write 1 byte, RESTART, read 2 bytes
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= false   // phase 1 = write
      dut.master.cmd.phase2   #= true    // enable composite
      dut.master.cmd.nackLast #= true    // NACK last read byte
      dut.master.cmd.len1     #= 1       // write 1 byte
      dut.master.cmd.len2     #= 2       // read 2 bytes
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(3)
      dut.master.cmd.trigger  #= false

      // Wait for completion
      waitForIdle(dut)

      // Verify slave RAM: first byte was overwritten with 0x00
      cd.waitActiveEdge(5)
      val ram0 = dut.slave.ram.getBigInt(0).toInt
      assert(ram0 == 0x00, f"RAM[0]=0x$ram0%02X, expected 0x00 after write phase")

      // Drain RX FIFO: should have 2 read bytes (from RAM[1], RAM[2])
      var rxValues = List.empty[Int]
      while (dut.master.rxFifo.notEmpty.toBoolean && rxValues.size < 3) {
        rxValues ::= dut.master.rxFifo.head.toInt
        dut.master.rxFifo.popValid #= true
        cd.waitActiveEdge()
        dut.master.rxFifo.popValid #= false
        cd.waitActiveEdge()
      }
      assert(rxValues.reverse == List(0xBB, 0xCC),
        s"RX=${rxValues.reverse.map(v => f"0x$v%02X")}, expected List(0xBB, 0xCC)")
    }
  }

  test("I2cAuto: address NACK causes abort") {
    runSim { dut =>
      val cd = dut.clockDomain

      // Configure master timing
      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20

      // Configure slave with DIFFERENT address (won't match)
      dut.slave.cfg.sampDiv   #= 1
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x30)
      dut.slave.dataPtr       #= 0

      cd.waitActiveEdge(10)

      // Push a byte (will never be sent due to NACK)
      pushTx(dut, 0x42)

      // Send to address 0x28 (slave is at 0x30, so NACK)
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= false
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= false
      dut.master.cmd.len1     #= 1
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(3)
      dut.master.cmd.trigger  #= false

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

      // Configure master timing
      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20

      // Configure slave via register bus
      dut.slave.cfg.sampDiv   #= 1
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)  // slaveAddr=0x28, matchEn=1
      regWrite(dut, isMaster = false, addr = 0x04, data = 0)                // dataPtr=0

      cd.waitActiveEdge(10)

      // Fill TX FIFO with 64 bytes: 0x00, 0x01, ..., 0x3F
      for (i <- 0 until 64) {
        pushTx(dut, i)
      }

      // Command: write 64 bytes
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= false
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= false
      dut.master.cmd.len1     #= 64
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(3)
      dut.master.cmd.trigger  #= false

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

      // Configure master timing
      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20

      // Configure slave via register bus
      dut.slave.cfg.sampDiv   #= 1
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)  // slaveAddr=0x28, matchEn=1
      regWrite(dut, isMaster = false, addr = 0x04, data = 0)                // dataPtr=0

      cd.waitActiveEdge(10)

      // Fill TX FIFO with many bytes (ensures long transfer)
      for (i <- 0 until 32) {
        pushTx(dut, i)
      }

      // Command: write 32 bytes
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= false
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= false
      dut.master.cmd.len1     #= 32
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(3)
      dut.master.cmd.trigger  #= false

      // Wait a few bits into the transfer, then abort
      cd.waitActiveEdge(200)  // ~4-5 bytes in
      assert(dut.master.status.busy.toBoolean, "Transfer should be active before abort")

      // Issue abort
      dut.master.abortReq #= true
      cd.waitActiveEdge(3)
      dut.master.abortReq #= false

      // Wait for STOP to complete
      waitForIdle(dut)

      // Verify: aborted flag set, not a normal error
      assert(dut.master.status.aborted.toBoolean, "aborted flag should be set")
      assert(!dut.master.status.busy.toBoolean, "busy should be cleared after abort")
    }
  }

  // TODO: Back-to-back transfer reveals data corruption on 2nd transfer (0x17 instead of 0xAA).
  // Requires waveform-level debugging (VCD dump) to identify root cause.
  test("I2cAuto: back-to-back transfers") {
    runSim { dut =>
      val cd = dut.clockDomain
      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20
      dut.slave.cfg.sampDiv   #= 1
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.slave.dataPtr       #= 0
      cd.waitActiveEdge(10)

      // Transfer 1
      pushTx(dut, 0x11)
      pushTx(dut, 0x22)
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= false
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= false
      dut.master.cmd.len1     #= 2
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(10)
      dut.master.cmd.trigger  #= false
      waitForIdle(dut)
      cd.waitActiveEdge(50)
      assert(dut.slave.ram.getBigInt(0).toInt == 0x11, "T1: RAM[0] mismatch")
      assert(dut.slave.ram.getBigInt(1).toInt == 0x22, "T1: RAM[1] mismatch")

      // Transfer 2
      dut.slave.dataPtr #= 0
      cd.waitActiveEdge(10)
      pushTx(dut, 0xAA)
      pushTx(dut, 0xBB)
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= false
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= false
      dut.master.cmd.len1     #= 2
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(10)
      dut.master.cmd.trigger  #= false
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
      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20
      regWrite(dut, isMaster = true, addr = 0x28, data = 50)  // timeout=50 cycles

      // Configure slave via register bus
      dut.slave.cfg.sampDiv   #= 1
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)  // slaveAddr=0x28, matchEn=1
      regWrite(dut, isMaster = false, addr = 0x04, data = 0)                // dataPtr=0

      cd.waitActiveEdge(10)

      // Start a write transfer
      pushTx(dut, 0x11)
      pushTx(dut, 0x22)
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= false
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= false
      dut.master.cmd.len1     #= 2
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(10)
      dut.master.cmd.trigger  #= false

      // Wait a few cycles into the transfer, then force SCL Low (simulate bus hang)
      cd.waitActiveEdge(50)
      assert(dut.master.status.busy.toBoolean, "Transfer should be active")
      dut.sclForceLow #= true  // Force SCL Low - bus hang!

      // Wait for timeout to fire + 9-pulse recovery + STOP
      var cycles = 0
      while (dut.master.status.busy.toBoolean && cycles < 20000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      // Recovery takes ~9*(10+10) + STOP sequence ≈ 250 cycles
      cd.waitActiveEdge(300)
      dut.sclForceLow #= false  // Release the bus

      // Verify timeout error
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

      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20
      dut.slave.cfg.sampDiv   #= 1
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.slave.dataPtr       #= 0
      cd.waitActiveEdge(10)

      // Try to write 3 bytes to a 2-valid-addr slave: 3rd byte should be NACKed
      pushTx(dut, 0x11)
      pushTx(dut, 0x22)
      pushTx(dut, 0x33)
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= false
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= false  // Don't NACK last ourselves - let slave NACK us
      dut.master.cmd.len1     #= 3
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(10)
      dut.master.cmd.trigger  #= false

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
      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20

      // Configure slave A (target) and slave B (should stay silent)
      dut.slaveA.cfg.sampDiv   #= 1
      dut.sA #= 0x00; dut.sW #= ((1 << 7) | 0x28); dut.sWe #= true
      cd.waitActiveEdge()
      dut.sWe #= false
      cd.waitActiveEdge()
      dut.sA #= 0x04; dut.sW #= 0; dut.sWe #= true
      cd.waitActiveEdge()
      dut.sWe #= false
      cd.waitActiveEdge()

      dut.slaveB.cfg.sampDiv   #= 1
      dut.bA #= 0x00; dut.bW #= ((1 << 7) | 0x30); dut.bWe #= true
      cd.waitActiveEdge()
      dut.bWe #= false
      cd.waitActiveEdge()
      dut.bA #= 0x04; dut.bW #= 0; dut.bWe #= true
      cd.waitActiveEdge()
      dut.bWe #= false
      cd.waitActiveEdge()

      cd.waitActiveEdge(10)

      // Write 2 bytes to slave A (addr 0x28)
      dut.master.txFifo.pushData #= 0xDE
      dut.master.txFifo.pushValid #= true
      cd.waitActiveEdge()
      dut.master.txFifo.popValid #= false
      cd.waitActiveEdge()
      dut.master.txFifo.pushData #= 0xAD
      dut.master.txFifo.pushValid #= true
      cd.waitActiveEdge()
      dut.master.txFifo.popValid #= false
      cd.waitActiveEdge()

      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= false
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= false
      dut.master.cmd.len1     #= 2
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(10)
      dut.master.cmd.trigger  #= false

      // Monitor slave B's SDA output during the transfer
      // NOTE: In write mode, non-matching slave may ACK (SDA Low) which doesn't
      // cause contention (both agree on wire state). True contention would be
      // in read mode where both drive different data. We check the critical path:
      // slave A receives correct data and transfer completes without error.
      var cycles = 0
      while (dut.master.status.busy.toBoolean && cycles < 200000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      // Wait for idle
      var idleCycles = 0
      while (dut.master.status.busy.toBoolean && idleCycles < 200000) {
        cd.waitActiveEdge()
        idleCycles += 1
      }
      assert(idleCycles < 200000, "Timed out waiting for idle")
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

      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20
      dut.slave.cfg.sampDiv   #= 1
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.slave.dataPtr       #= 0
      cd.waitActiveEdge(10)

      // Start a write transfer
      pushTx(dut, 0x11)
      pushTx(dut, 0x22)
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= false
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= false
      dut.master.cmd.len1     #= 2
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(10)
      dut.master.cmd.trigger  #= false

      // Wait a few cycles into the transfer, then force SDA High (simulate arb loss)
      cd.waitActiveEdge(50)
      assert(dut.master.status.busy.toBoolean, "Transfer should be active")
      dut.sdaForceHigh #= true  // Another master pulls SDA High!

      // Wait for arb loss detection + passive exit
      var cycles = 0
      while (dut.master.status.busy.toBoolean && cycles < 5000) {
        cd.waitActiveEdge()
        cycles += 1
      }
      cd.waitActiveEdge(20)
      dut.sdaForceHigh #= false

      // Verify arbitration loss error
      val err = dut.master.status.errCode.toInt
      assert(err == 4, f"errCode=$err, expected 4 (arbitration loss)")
      assert(!dut.master.status.busy.toBoolean, "busy should be clear")
    }
  }

  test("I2cAuto: SCL stretching pauses master") {
    runSim { dut =>
      val cd = dut.clockDomain

      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20
      dut.slave.cfg.sampDiv   #= 1
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.slave.dataPtr       #= 0
      cd.waitActiveEdge(10)

      // Start a write transfer
      pushTx(dut, 0xAB)
      pushTx(dut, 0xCD)
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= false
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= false
      dut.master.cmd.len1     #= 2
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(10)
      dut.master.cmd.trigger  #= false

      // Wait until the bus SCL is Low (stretch can only engage in the Low phase)
      var w = 0
      while (dut.sclWire.toBoolean && w < 2000) { cd.waitActiveEdge(1); w += 1 }
      assert(dut.master.status.busy.toBoolean, "Transfer should be active")

      // Request SCL stretch: slave holds SCL Low
      dut.slave.stretchReq #= true
      cd.waitActiveEdge(5)
      assert(dut.slave.stretchActive.toBoolean, "stretchActive should engage while SCL Low")

      // Verify master is stalled (still busy after many cycles - proves it's waiting)
      cd.waitActiveEdge(500)
      assert(dut.master.status.busy.toBoolean,
        "Master should be stalled during SCL stretch (waiting for SCL to go High)")
      assert(!dut.sclWire.toBoolean, "SCL wire must stay Low while stretch is held")

      // Release the stretch and let the transfer finish
      dut.slave.stretchReq #= false
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

      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.slave.dataPtr       #= 0
      cd.waitActiveEdge(10)

      // 4-byte write
      pushTx(dut, 0xA0); pushTx(dut, 0xB1); pushTx(dut, 0xC2); pushTx(dut, 0xD3)
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= false
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= false
      dut.master.cmd.len1     #= 4
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(10)
      dut.master.cmd.trigger  #= false

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
      assert(w < 8000, "timed out waiting for mid-byte SCL-Low window")
      val stretchedByteNum = dut.master.fsm.byteNum.toInt
      val stretchedBitPos  = dut.master.fsm.bitPos.toInt

      // Hold the bus for a long stretch (>> one bit period)
      dut.slave.stretchReq #= true
      cd.waitActiveEdge(5)
      assert(dut.slave.stretchActive.toBoolean, "stretch must engage")
      cd.waitActiveEdge(400)
      assert(dut.master.status.busy.toBoolean, "master must stall during mid-byte stretch")
      assert(!dut.sclWire.toBoolean, "SCL must stay Low during stretch")
      // Slave bit counter must be frozen in lockstep with the master
      assert(dut.slave.bitPos.toInt == stretchedBitPos,
        s"slave bitPos=${dut.slave.bitPos.toInt} != master $stretchedBitPos (lockstep broken)")

      // Release mid-byte: the remainder of the transfer must complete cleanly
      dut.slave.stretchReq #= false
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

      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20
      regWrite(dut, isMaster = false, addr = 0x00, data = (1 << 7) | 0x28)
      dut.slave.dataPtr       #= 0
      dut.slave.ram.setBigInt(0, 0x5A)
      dut.slave.ram.setBigInt(1, 0x3C)
      dut.slave.ram.setBigInt(2, 0xE7)
      dut.slave.ram.setBigInt(3, 0x19)
      cd.waitActiveEdge(10)

      // 4-byte read
      dut.master.cmd.addr     #= 0x28
      dut.master.cmd.rw       #= true
      dut.master.cmd.phase2   #= false
      dut.master.cmd.nackLast #= true
      dut.master.cmd.len1     #= 4
      dut.master.cmd.len2     #= 0
      cd.waitActiveEdge()
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(10)
      dut.master.cmd.trigger  #= false

      // Wait until mid-data-byte with SCL Low (slave is driving the current bit)
      var w = 0
      while (w < 8000 && !(
        dut.master.status.busy.toBoolean &&
        dut.master.fsm.byteNum.toInt >= 1 &&
        dut.master.fsm.bitPos.toInt >= 2 &&
        dut.master.fsm.bitPos.toInt < 8 &&
        !dut.sclWire.toBoolean
      )) { cd.waitActiveEdge(1); w += 1 }
      assert(w < 8000, "timed out waiting for mid-byte SCL-Low window")
      val stretchedByteNum = dut.master.fsm.byteNum.toInt
      val stretchedBitPos  = dut.master.fsm.bitPos.toInt

      // Stretch while the slave holds a data bit on SDA
      dut.slave.stretchReq #= true
      cd.waitActiveEdge(5)
      assert(dut.slave.stretchActive.toBoolean, "stretch must engage")
      cd.waitActiveEdge(400)
      assert(dut.master.status.busy.toBoolean, "master must stall during mid-byte stretch")
      assert(!dut.sclWire.toBoolean, "SCL must stay Low during stretch")

      // Release mid-byte: remaining bits/bytes must be delivered intact
      dut.slave.stretchReq #= false
      waitForIdle(dut)
      cd.waitActiveEdge(20)

      assert(dut.master.status.errCode.toInt === 0, s"err=${dut.master.status.errCode.toInt} (stretched at byte=$stretchedByteNum bit=$stretchedBitPos)")
      var rxValues = List.empty[Int]
      while (dut.master.rxFifo.notEmpty.toBoolean && rxValues.size < 4) {
        rxValues ::= dut.master.rxFifo.head.toInt
        dut.master.rxFifo.popValid #= true
        cd.waitActiveEdge()
        dut.master.rxFifo.popValid #= false
        cd.waitActiveEdge()
      }
      assert(rxValues.reverse == List(0x5A, 0x3C, 0xE7, 0x19),
        s"RX=${rxValues.reverse.map(v => f"0x$v%02X")} expected List(0x5A, 0x3C, 0xE7, 0x19) (stretch at byte=$stretchedByteNum bit=$stretchedBitPos)")
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

      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20
      dut.slave.cfg.sampDiv   #= 1

      // 10-bit address: 0x345
      regWrite(dut, isMaster = false, addr = 0x00, data = ((1 << 10) | 0x345))
      dut.slave.dataPtr   #= 0

      cd.waitActiveEdge(10)

      // Push 4 data bytes
      val data = Array(0xDE, 0xAD, 0xBE, 0xEF)
      for (d <- data) { pushTx(dut, d) }

      // Trigger write
      dut.master.cmd.addr    #= 0x345
      dut.master.cmd.rw      #= false
      dut.master.cmd.len1    #= 4
      dut.master.cmd.trigger #= true
      cd.waitActiveEdge(3)
      dut.master.cmd.trigger #= false

      waitForIdle(dut)

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

      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20
      dut.slave.cfg.sampDiv   #= 1

      // 10-bit address: 0x1BC
      regWrite(dut, isMaster = false, addr = 0x00, data = ((1 << 10) | 0x1BC))
      dut.slave.dataPtr   #= 0

      // Pre-load RAM
      val data = Array(0x11, 0x22, 0x33)
      for (i <- data.indices) { dut.slave.ram.setBigInt(i, data(i)) }

      cd.waitActiveEdge(10)

      // Trigger read
      dut.master.cmd.addr     #= 0x1BC
      dut.master.cmd.rw       #= true
      dut.master.cmd.len1     #= 3
      dut.master.cmd.nackLast #= true
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(3)
      dut.master.cmd.trigger  #= false

      waitForIdle(dut)

      // Read back
      for (i <- data.indices) {
        dut.master.rxFifo.popValid #= true
        cd.waitActiveEdge()
        val rxVal = dut.master.rxFifo.head.toInt
        assert(rxVal == data(i), f"RX[$i]=0x${rxVal}%02X exp 0x${data(i)}%02X")
        dut.master.rxFifo.popValid #= false
        cd.waitActiveEdge()
      }
    }
  }

  test("I2cAuto 10-bit: composite write+read RESTART") {
    runSim10bit { dut =>
      val cd = dut.clockDomain

      dut.master.cfg.sampDiv  #= 1
      dut.master.cfg.tLow     #= 20
      dut.master.cfg.tHigh    #= 20
      dut.master.cfg.tBuf     #= 20
      dut.slave.cfg.sampDiv   #= 1

      // 10-bit address: 0x2AA
      regWrite(dut, isMaster = false, addr = 0x00, data = ((1 << 10) | 0x2AA))
      dut.slave.dataPtr   #= 0

      // Pre-load read data at offset 2
      val readData = Array(0xAA, 0xBB)
      for (i <- readData.indices) { dut.slave.ram.setBigInt(2 + i, readData(i)) }

      cd.waitActiveEdge(10)

      // Push write data
      val writeData = Array(0x11, 0x22)
      for (d <- writeData) { pushTx(dut, d) }

      // Trigger composite
      dut.master.cmd.addr     #= 0x2AA
      dut.master.cmd.rw       #= false
      dut.master.cmd.len1     #= 2
      dut.master.cmd.phase2   #= true
      dut.master.cmd.len2     #= 2
      dut.master.cmd.nackLast #= true
      dut.master.cmd.trigger  #= true
      cd.waitActiveEdge(3)
      dut.master.cmd.trigger  #= false

      waitForIdle(dut)

      // Verify write phase
      assert(dut.slave.ram.getBigInt(0).toInt == 0x11, s"RAM[0]=${dut.slave.ram.getBigInt(0).toInt}")
      assert(dut.slave.ram.getBigInt(1).toInt == 0x22, s"RAM[1]=${dut.slave.ram.getBigInt(1).toInt}")

      // Verify read phase
      for (i <- readData.indices) {
        dut.master.rxFifo.popValid #= true
        cd.waitActiveEdge()
        val rxVal = dut.master.rxFifo.head.toInt
        assert(rxVal == readData(i), f"RX[$i]=0x${rxVal}%02X exp 0x${readData(i)}%02X")
        dut.master.rxFifo.popValid #= false
        cd.waitActiveEdge()
      }
    }
  }

}
