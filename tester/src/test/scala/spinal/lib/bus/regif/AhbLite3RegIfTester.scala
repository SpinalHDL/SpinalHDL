package spinal.lib.bus.regif

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.regif.AccessType._
import spinal.tester._

/** Partial accesses over AHB-Lite: HSIZE + HADDR select the byte lanes. */
class AhbLite3RegIfTester extends SpinalAnyFunSuite {

  case class AhbLite3RegIfDut() extends Component {
    val io = new Bundle {
      val ahb = slave(AhbLite3(AhbLite3Config(addressWidth = 16, dataWidth = 32)))
    }

    val busif = BusInterface(io.ahb, (0x000, 0x100 Byte))
    busif.setReservedAddressErrorState(true)

    val reg0 = busif.newReg("Reg0")
    reg0.field(Bits(32 bits), RW, 0x12345678L, "Reg0")
    val reg1 = busif.newReg("Reg1")
    reg1.field(Bits(32 bits), RW, 0x0L, "Reg1")
  }

  case class AhbLite3RegIfFirewallDut() extends Component {
    val io = new Bundle {
      val ahb = slave(AhbLite3(AhbLite3Config(addressWidth = 16, dataWidth = 32)))
      val wProtect = in Bool ()
      val rProtect = in Bool ()
    }
    val busif = BusInterface(io.ahb, (0x000, 0x100 Byte), "", true)
    val grp = busif.newGrp(64, "Secure group", Secure.CS(io.wProtect, io.rProtect))
    val reg0 = grp.newReg("Reg0")
    reg0.field(Bits(32 bits), RW, 0x0L, "Reg0")
  }

  test("securityFirewallElaborates") {
    SpinalConfig().generateVerilog(AhbLite3RegIfFirewallDut())
  }

  /** Minimal AHB-Lite master. */
  class AhbLite3Master(bus: AhbLite3, cd: ClockDomain) {
    bus.HSEL #= false
    bus.HADDR #= 0
    bus.HTRANS #= 0
    bus.HWRITE #= false
    bus.HSIZE #= 2
    bus.HBURST #= 0
    bus.HPROT #= 0
    bus.HMASTLOCK #= false
    bus.HWDATA #= 0
    bus.HREADY #= true

    /** Next cycle, sampled mid-cycle so the edge updates are visible. */
    def tick(): Unit = {
      cd.waitSampling()
      sleep(4)
    }

    def write(addr: Int, size: Int, data: BigInt): Unit = {
      bus.HSEL #= true
      bus.HADDR #= addr
      bus.HSIZE #= size
      bus.HWRITE #= true
      bus.HTRANS #= 2
      tick()
      bus.HWDATA #= data
      bus.HTRANS #= 0
      tick()
      bus.HSEL #= false
      tick()
    }

    def read(addr: Int, size: Int): BigInt = {
      bus.HSEL #= true
      bus.HADDR #= addr
      bus.HSIZE #= size
      bus.HWRITE #= false
      bus.HTRANS #= 2
      tick()
      val ret = bus.HRDATA.toBigInt
      bus.HTRANS #= 0
      bus.HSEL #= false
      tick()
      ret
    }
  }

  def sim(body: (AhbLite3RegIfDut, AhbLite3Master) => Unit) = {
    SimConfig.compile(AhbLite3RegIfDut()).doSim("test") { dut =>
      val master = new AhbLite3Master(dut.io.ahb, dut.clockDomain)
      dut.clockDomain.forkStimulus(10, resetCycles = 2)
      dut.clockDomain.waitSampling(6)
      body(dut, master)
    }
  }

  test("fullWordAccess") {
    sim { (dut, m) =>
      assert(m.read(0x00, 2) == 0x12345678L)
      m.write(0x00, 2, 0xaabbccddL)
      assert(m.read(0x00, 2) == 0xaabbccddL)
      m.write(0x04, 2, 0x01020304L)
      assert(m.read(0x04, 2) == 0x01020304L)
      assert(m.read(0x00, 2) == 0xaabbccddL)
    }
  }

  test("byteWriteOnlyUpdatesSelectedByte") {
    sim { (dut, m) =>
      m.write(0x00, 2, 0x11223344L)

      m.write(0x02, 0, 0x00aa0000L) // byte 2 only
      assert(m.read(0x00, 2) == 0x11aa3344L)

      m.write(0x01, 0, 0x0000bb00L) // byte 1 only
      assert(m.read(0x00, 2) == 0x11aabb44L)

      m.write(0x03, 0, 0xcc000000L) // byte 3 only
      assert(m.read(0x00, 2) == 0xccaabb44L)
    }
  }

  test("halfWordWriteOnlyUpdatesSelectedBytes") {
    sim { (dut, m) =>
      m.write(0x00, 2, 0x11223344L)

      m.write(0x00, 1, 0x0000ccddL)
      assert(m.read(0x00, 2) == 0x1122ccddL)

      m.write(0x02, 1, 0xeeff0000L)
      assert(m.read(0x00, 2) == 0xeeffccddL)
    }
  }

  test("byteAccessDoesNotErrorAndReadsContainingWord") {
    sim { (dut, m) =>
      m.write(0x00, 2, 0x11223344L)

      // non aligned byte read returns the containing word
      assert(m.read(0x01, 0) == 0x11223344L)
      assert(m.read(0x03, 0) == 0x11223344L)

      // and is not an error
      dut.io.ahb.HSEL #= true
      dut.io.ahb.HADDR #= 0x01
      dut.io.ahb.HSIZE #= 0
      dut.io.ahb.HWRITE #= false
      dut.io.ahb.HTRANS #= 2
      m.tick()
      assert(!dut.io.ahb.HRESP.toBoolean, "byte read on a valid register must not raise HRESP")
      dut.io.ahb.HTRANS #= 0
      dut.io.ahb.HSEL #= false
      m.tick()
    }
  }

  test("pipelinedPartialWriteKeepsTheRightByteLanes") {
    sim { (dut, m) =>
      // byte lanes come from the write's own address phase, not from the one held during its data phase
      val bus = dut.io.ahb
      bus.HSEL #= true
      bus.HADDR #= 0x00
      bus.HSIZE #= 2
      bus.HWRITE #= true
      bus.HTRANS #= 2 // A: full word write
      m.tick()

      bus.HWDATA #= 0xaabbccddL
      bus.HADDR #= 0x01
      bus.HSIZE #= 0 // B: byte write on lane 1
      m.tick()

      bus.HWDATA #= 0x00001100L
      bus.HTRANS #= 0
      bus.HSEL #= false
      m.tick()

      // A full word, then B replaces lane 1 only
      assert(m.read(0x00, 2) == 0xaabb11ddL)
    }
  }

  test("reservedAddressErrorResponse") {
    sim { (dut, m) =>
      // two cycle error response: HRESP high with HREADYOUT low, then high
      dut.io.ahb.HSEL #= true
      dut.io.ahb.HADDR #= 0x80
      dut.io.ahb.HSIZE #= 2
      dut.io.ahb.HWRITE #= false
      dut.io.ahb.HTRANS #= 2
      m.tick()

      val errorFirst = dut.io.ahb.HRESP.toBoolean
      val readyFirst = dut.io.ahb.HREADYOUT.toBoolean

      dut.io.ahb.HTRANS #= 0
      dut.io.ahb.HSEL #= false
      m.tick()

      val errorSecond = dut.io.ahb.HRESP.toBoolean
      val readySecond = dut.io.ahb.HREADYOUT.toBoolean

      assert(errorFirst, "the first cycle of the error response must set HRESP")
      assert(!readyFirst, "the first cycle of the error response must clear HREADYOUT")
      assert(errorSecond, "the second cycle of the error response must set HRESP")
      assert(readySecond, "the second cycle of the error response must set HREADYOUT")
    }
  }
}
