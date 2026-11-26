package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.wishbone._

/** Slave used by the abort/retry regression: a sync-mem read with a 1-cycle wait
  * (multiCycleRead(2)), a plain register and a slow register with 7 wait states.
  */
class WishboneAckDeadlockSlave(config: WishboneConfig) extends Component {
  val io = new Bundle {
    val bus   = slave(Wishbone(config))
    val errIn = in Bool()
  }

  val ctrl = WishboneSlaveFactory(io.bus, reg_feedback = true, errorOnUnmapped = true)
  val mem  = Mem(Bits(config.dataWidth bits), 16)
  val reg  = Reg(Bits(config.dataWidth bits)) init (0x1234)
  val slow = B(BigInt(0x55), config.dataWidth bits)

  ctrl.readSyncMemWordAligned(mem, 0x00)
  ctrl.readAndWrite(reg, 0x40)
  ctrl.read(slow, 0x80)
  ctrl.multiCycleRead(SizeMapping(0x80, 4), 8)
  ctrl.setError(io.errIn)
}

/** Regression for the classic ACK/ERR CYC qualification (#1969) and for the ACK
  * register phase lock after an aborted multiCycleRead() (deadlock in #1974).
  */
class SpinalSimWishboneSlaveFactoryAckTester extends AnyFunSuite {

  private def initBus(bus: Wishbone): Unit = {
    bus.CYC #= false
    bus.STB #= false
    bus.WE  #= false
    bus.ADR #= 0
    bus.DAT_MOSI #= 0
    if (bus.config.useSEL) bus.SEL #= 0
  }

  private def readReq(bus: Wishbone, adr: Int): Unit = {
    bus.CYC #= true
    bus.STB #= true
    bus.WE  #= false
    bus.ADR #= adr
    if (bus.config.useSEL) bus.SEL #= (1 << (bus.config.dataWidth / 8)) - 1
  }

  test("classic ACK/ERR must not respond while CYC is low") {
    val conf = WishboneConfig(32, 32, useERR = true)
    SimConfig.allOptimisation.compile(rtl = new WishboneSimpleSlave(conf)).doSim("classic_cyc") { dut =>
      val bus = dut.io.bus
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(1000 * 20 * 100)

      initBus(bus)
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()

      // Shared bus: STB is broadcast, this slave is not selected (CYC low).
      bus.STB #= true
      bus.ADR #= 0
      for (_ <- 0 until 4) {
        dut.clockDomain.waitSampling()
        assert(!bus.ACK.toBoolean && !bus.ERR.toBoolean, "ghost response while CYC=0")
      }
      bus.STB #= false
      dut.clockDomain.waitSampling()

      // Normal read: one wait state, then ACK.
      readReq(bus, 0)
      dut.clockDomain.waitSamplingWhere(bus.ACK.toBoolean || bus.ERR.toBoolean)
      assert(bus.ACK.toBoolean && !bus.ERR.toBoolean, "mapped read must ACK")
      bus.STB #= false
      bus.CYC #= false
      dut.clockDomain.waitSampling()
    }
  }

  test("classic held STB gets one response per beat") {
    val conf = WishboneConfig(32, 32, useERR = true)
    SimConfig.allOptimisation.compile(rtl = new WishboneSimpleSlave(conf)).doSim("classic_back_to_back") { dut =>
      val bus = dut.io.bus
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(1000 * 20 * 100)

      initBus(bus)
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()

      bus.CYC #= true
      bus.STB #= true
      bus.WE  #= false

      // Mapped beat.
      bus.ADR #= 0
      dut.clockDomain.waitSampling()
      assert(!bus.ACK.toBoolean, "first cycle of a beat must be a wait state")
      dut.clockDomain.waitSampling()
      assert(bus.ACK.toBoolean && !bus.ERR.toBoolean, "mapped beat must ACK")

      // Unmapped beat, STB/CYC stay high.
      bus.ADR #= 5
      dut.clockDomain.waitSampling()
      assert(!bus.ACK.toBoolean && !bus.ERR.toBoolean, "unmapped beat must not be ACKed")
      dut.clockDomain.waitSampling()
      assert(bus.ERR.toBoolean && !bus.ACK.toBoolean, "unmapped beat must ERR")

      // Back to a mapped beat: no stale ERR.
      bus.ADR #= 0
      dut.clockDomain.waitSampling()
      assert(!bus.ACK.toBoolean && !bus.ERR.toBoolean, "ERR must not leak into the next beat")
      dut.clockDomain.waitSampling()
      assert(bus.ACK.toBoolean && !bus.ERR.toBoolean, "mapped beat after ERR must ACK")

      bus.STB #= false
      bus.CYC #= false
      dut.clockDomain.waitSampling()
    }
  }

  test("classic aborted sync-mem read does not lock the slave") {
    val conf = WishboneConfig(32, 32, useERR = true)
    SimConfig.allOptimisation.compile(rtl = new WishboneAckDeadlockSlave(conf)).doSim("abort_retry") { dut =>
      val bus = dut.io.bus
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(1000 * 20 * 100)

      initBus(bus)
      dut.io.errIn #= false
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()

      // Request the sync-mem address, then abandon after its first wait cycle
      // (timeout / reset / arbitration / a ghost ACK from another slave).
      readReq(bus, 0)
      dut.clockDomain.waitSampling()
      assert(!bus.ACK.toBoolean)
      bus.CYC #= false
      bus.STB #= false
      dut.clockDomain.waitSampling()

      // Retry and keep STB/CYC asserted.
      readReq(bus, 0)
      var cycles = 0
      while (cycles < 8 && !bus.ACK.toBoolean && !bus.ERR.toBoolean) {
        dut.clockDomain.waitSampling()
        cycles += 1
      }
      assert(bus.ACK.toBoolean && !bus.ERR.toBoolean, "slave did not answer after an aborted read (phase lock)")

      bus.STB #= false
      bus.CYC #= false
      dut.clockDomain.waitSampling()
    }
  }

  test("pipelined ACK/ERR only answer requests with CYC asserted") {
    val conf = WishboneConfig(32, 32, useERR = true).pipelined
    SimConfig.allOptimisation.compile(rtl = new WishboneSimpleSlave(conf)).doSim("pipelined_cyc") { dut =>
      val bus = dut.io.bus
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(1000 * 20 * 100)

      initBus(bus)
      dut.clockDomain.waitSampling()
      dut.clockDomain.waitSampling()

      // STB with CYC low is not a request: no response may be registered.
      bus.STB #= true
      bus.CYC #= false
      bus.ADR #= 0
      dut.clockDomain.waitSampling()
      bus.STB #= false
      for (_ <- 0 until 2) {
        dut.clockDomain.waitSampling()
        assert(!bus.ACK.toBoolean && !bus.ERR.toBoolean, "ghost response for STB seen with CYC low")
      }

      // Real pipelined request: ACK one cycle later.
      bus.CYC #= true
      bus.STB #= true
      bus.ADR #= 0
      dut.clockDomain.waitSampling()
      bus.STB #= false
      dut.clockDomain.waitSampling()
      assert(bus.ACK.toBoolean && !bus.ERR.toBoolean, "pipelined request must ACK one cycle later")

      bus.CYC #= false
      dut.clockDomain.waitSampling()

      // Unmapped pipelined request: ERR one cycle later.
      bus.CYC #= true
      bus.STB #= true
      bus.ADR #= 5
      dut.clockDomain.waitSampling()
      bus.STB #= false
      dut.clockDomain.waitSampling()
      assert(bus.ERR.toBoolean && !bus.ACK.toBoolean, "pipelined unmapped request must ERR one cycle later")

      bus.CYC #= false
      dut.clockDomain.waitSampling()
    }
  }
}
