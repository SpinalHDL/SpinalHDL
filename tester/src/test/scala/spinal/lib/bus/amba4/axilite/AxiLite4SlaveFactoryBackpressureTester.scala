package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.tester.SpinalAnyFunSuite

class AxiLite4SlaveFactoryBackpressureTester extends SpinalAnyFunSuite {
  class BusyRegister extends Component {
    val io = new Bundle {
      val axi = slave(AxiLite4(AxiLite4Config(addressWidth = 8, dataWidth = 32)))
      val complete = in Bool()
      val halt = in Bool()
      val error = in Bool()
      val reads0 = out UInt(8 bits)
      val reads4 = out UInt(8 bits)
    }

    val busy = RegInit(True)
    when(io.complete) { busy := False }
    val factory = new AxiLite4SlaveFactory(io.axi)
    factory.read(busy, address = 0)
    factory.read(B(0x9abcdef0L, 32 bits), address = 4)
    val reads0 = Reg(UInt(8 bits)) init(0)
    val reads4 = Reg(UInt(8 bits)) init(0)
    factory.onRead(0) { reads0 := reads0 + 1 }
    factory.onRead(4) { reads4 := reads4 + 1 }
    when(io.halt) { factory.readHalt() }
    when(io.error) { factory.readError() }
    io.reads0 := reads0
    io.reads4 := reads4
  }

  lazy val compiled = SimConfig.compile(new BusyRegister)

  def initialize(dut: BusyRegister): Unit = {
    val bus = dut.io.axi
    dut.io.complete #= false
    dut.io.halt #= false
    dut.io.error #= false
    bus.aw.valid #= false
    bus.aw.addr #= 0
    bus.aw.prot #= 0
    bus.w.valid #= false
    bus.w.data #= 0
    bus.w.strb #= 0
    bus.b.ready #= true
    bus.ar.valid #= false
    bus.ar.addr #= 0
    bus.ar.prot #= 0
    bus.r.ready #= false
    dut.clockDomain.forkStimulus(10)
    SimTimeout(10000)
    dut.clockDomain.waitSampling(5)
  }

  def requestRead(dut: BusyRegister, address: Int): Unit = {
    dut.io.axi.ar.addr #= address
    dut.io.axi.ar.valid #= true
    dut.clockDomain.waitSamplingWhere(dut.io.axi.ar.ready.toBoolean)
    dut.io.axi.ar.valid #= false
  }

  test("read data remains stable while the mapped register changes") {
    compiled.doSim(seed = 42) { dut =>
      initialize(dut)
      val bus = dut.io.axi
      requestRead(dut, 0)
      dut.clockDomain.waitSamplingWhere(bus.r.valid.toBoolean)
      val offered = bus.r.data.toBigInt
      assert(offered == 1, "The initial response must contain BUSY=1")

      dut.io.complete #= true
      dut.clockDomain.waitSampling(3)

      assert(!bus.r.ready.toBoolean)
      assert(bus.r.valid.toBoolean, "RVALID dropped while stalled")
      val stalled = bus.r.data.toBigInt
      println(s"RREADY=0: RDATA was 0x${offered.toString(16)}, now 0x${stalled.toString(16)}")
      assert(stalled == offered, "RDATA changed while stalled")
      bus.r.ready #= true
      dut.clockDomain.waitSampling()
    }
  }

  test("an offered response survives changes to read error and halt") {
    compiled.doSim(seed = 42) { dut =>
      initialize(dut)
      val bus = dut.io.axi
      dut.io.error #= true
      requestRead(dut, 0)
      dut.clockDomain.waitSamplingWhere(bus.r.valid.toBoolean)
      assert(bus.r.resp.toInt == 2)

      dut.io.error #= false
      dut.io.halt #= true
      dut.clockDomain.waitSampling(3)
      assert(bus.r.valid.toBoolean, "readHalt withdrew an offered response")
      assert(bus.r.resp.toInt == 2, "RRESP changed while stalled")
      assert(bus.r.data.toBigInt == 1)

      bus.r.ready #= true
      dut.clockDomain.waitSampling()
      bus.r.ready #= false
      requestRead(dut, 4)
      dut.clockDomain.waitSampling(3)
      assert(!bus.r.valid.toBoolean, "A halted request produced a response")
      dut.io.halt #= false
      dut.clockDomain.waitSamplingWhere(bus.r.valid.toBoolean)
      assert(bus.r.resp.toInt == 0)
      assert(bus.r.data.toBigInt == BigInt("9abcdef0", 16))
      bus.r.ready #= true
      dut.clockDomain.waitSampling()
    }
  }

  test("queued reads execute each address callback once") {
    compiled.doSim(seed = 42) { dut =>
      initialize(dut)
      val bus = dut.io.axi
      requestRead(dut, 0)
      dut.clockDomain.waitSamplingWhere(bus.r.valid.toBoolean)
      requestRead(dut, 4)
      dut.clockDomain.waitSampling(3)
      assert(bus.r.data.toBigInt == 1)
      assert(dut.io.reads0.toInt == 1)
      assert(dut.io.reads4.toInt == 0)

      bus.r.ready #= true
      dut.clockDomain.waitSampling()
      bus.r.ready #= false
      sleep(1)
      assert(bus.r.valid.toBoolean)
      assert(bus.r.data.toBigInt == BigInt("9abcdef0", 16))
      assert(dut.io.reads0.toInt == 1)
      assert(dut.io.reads4.toInt == 1)

      dut.clockDomain.waitSampling(3)
      assert(dut.io.reads4.toInt == 1)
      bus.r.ready #= true
      dut.clockDomain.waitSampling(3)
      assert(!bus.r.valid.toBoolean)
      assert(dut.io.reads0.toInt == 1)
      assert(dut.io.reads4.toInt == 1)
    }
  }
}
