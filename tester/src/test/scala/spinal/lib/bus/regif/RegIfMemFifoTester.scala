package spinal.lib.bus.regif

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.localbus.{MemBus, MemBusConfig, MemBusDriver}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif.AccessType.ROV
import scala.util.Random

class RegifFifoMem extends Component{
  val io = new Bundle{
    val bus = slave(MemBus(MemBusConfig(32, 32)))
    val ram = master(MemBus(MemBusConfig(aw = 8, dw = 32)))
    val fifowr = master(Flow(Bits(32 bit)))
    val fiford = slave(Stream(Bits(32 bit)))
  }

  val bif = MemBusInterface(io.bus, SizeMapping(0, 4 KiB), "Test" )
  bif.newReg("Version")(SymbolName("Version")).field(Bits(32 bit), ROV, 0x12345678, "device version")

  val FIFO = bif.newWrFifoAt(0x0004, "fifo doc..")
  FIFO.field(8, doc = "field 0")("param0")
  FIFO.field(8, doc = "field 1")("param1")
  FIFO.fieldAt(16, 4, doc = "field 2")("param2")
  FIFO.bus >> io.fifowr

  val FIFO2  = bif.newRdFifoAt(0x0008, doc = "read Fifo Test")
  FIFO2.field(32, doc ="read")("fiford")

  io.fiford >> FIFO2.bus

  val RAM1 = bif.newRAMAt(0x0100, 0x0800, "RAM1")

  RAM1.field(8, doc = "field 0")("param0")
  RAM1.field(8, doc = "field 1")("param1")
  RAM1.field(16, doc = "field 2")("param2")
  RAM1.bus >> io.ram
}

class RegIfMemFIfoTB extends RegifFifoMem{
  lazy val busdv = MemBusDriver(io.bus, this.clockDomain)
  def init() = {
    this.clockDomain.forkStimulus(10)
    busdv.simClear()
    io.fiford.valid #= false
    io.fiford.payload #= 0
    SpinalProgress("simulation start")
    sleep(20)
    this.clockDomain.waitSampling(10)
  }

  def write(addr: Long, data: BigInt) = {
    busdv.write(addr, data)
  }
  def read(addr: Long): BigInt = {
    busdv.read(addr)
  }

  def testRam() = {
    val rand = new Random(seed = 0)
    MemBusDriver(io.ram, this.clockDomain).hangMem()
    (0 until 0x800/4).foreach{ i =>
      val wdata = rand.nextInt().abs
      val addr = 0x100 + i * 4
      busdv.write(addr, wdata)
      val rdata = busdv.read(addr)
      assert(wdata == rdata,  s"${i}: ${wdata.hexString()} != ${rdata.hexString}")
//      SpinalInfo(s"${i}: ${wdata.hexString()} == ${rdata.hexString}")
    }
  }

  def testRdFifo() = {
    val datas = (0 to 100).map(i => 0xabcd00+i).toList
    val cache = scala.collection.mutable.Queue[Int](datas: _*)
    fork{
      while(true){
        sleep(0)//wait 0 to trigger ready capture
        if(io.fiford.ready.toBoolean){
          io.fiford.valid   #= true
          io.fiford.payload #= cache.dequeue()
        }
        clockDomain.waitSampling()
      }
    }
    (0 until 100).foreach { i =>
      val rdata = busdv.read(0x0008)
      assert(datas(i) == rdata, s"rdFifo: 0x${datas(i).hexString} != 0x${rdata.hexString}")
    }
  }
  def testWrFifo() = {
    val datas = (0 to 100).map(i => 0x123400 + i).toList
    val cache = scala.collection.mutable.Queue[Int](datas: _*)
    fork {
      while (true) {
        clockDomain.waitSampling()
        if (io.fifowr.valid.toBoolean) {
          val a = cache.dequeue()
          val b = io.fifowr.payload.toLong
          assert(a == b, s"WrFifo: 0x${a.hexString} != 0x${b.hexString}")
        }
      }
    }
    (0 until 100).foreach{i =>
      busdv.write(0x0004, datas(i))
    }
  }
}

object RegIfMemFifoSim{
  val spinalConfig = SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING,
      resetKind = ASYNC,
      resetActiveLevel = LOW
    ),
    defaultClockDomainFrequency = FixedFrequency(200 MHz),
    targetDirectory = "./out/rtl/",
    headerWithDate = true,
    inlineConditionalExpression = true,
    nameWhenByFile = false,
    removePruned = true,
    anonymSignalPrefix = "t",
    mergeAsyncProcess = true)

  val simcfg = SpinalSimConfig().withConfig(spinalConfig)

  def sim() = {
    simcfg
//      .withFstWave
      .compile(new RegIfMemFIfoTB)
      .doSim("regifram") { dut =>
        dut.init()
        dut.testRam()
        dut.testWrFifo()
        dut.testRdFifo()
        sleep(100)
        simSuccess()
      }
  }
}