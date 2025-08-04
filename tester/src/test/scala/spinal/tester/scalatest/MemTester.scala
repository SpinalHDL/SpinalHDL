package spinal.tester.scalatest

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._



object MemTester extends App{
  class MemTester extends Component {
    val mem = Mem(Bits(32 bits), 16)
    mem.initBigInt(List(
      0xbf6c8ffel,
      0x6f431025l,
      0xde894aeel,
      0xb007da48l,
      0xe39eca02l,
      0x1732378dl,
      0xd47822e3l,
      0xac711616l,
      0x0d45b17el,
      0xd863729dl,
      0x2210305el,
      0x73cf58e5l,
      0x80d3c954l,
      0xf56fb952l,
      0x9580e19cl,
      0xdd3b620dl
    ))

    val readSync = slave(mem.readSyncPort)
    val readAsync = new Bundle {
      val address = in UInt(4 bits)
      val data = out(mem.readAsync(address))
    }
    val write = new Bundle {
      val valid = in Bool()
      val address = in UInt(4 bits)
      val data = in Bits(32 bits)
      val mask = in Bits(4 bits)
      mem.write(address,data,valid,mask)
    }
    val readWrite = new Bundle {
      val valid = in Bool()
      val write = in Bool()
      val address = in UInt(4 bits)
      val writeData = in Bits(32 bits)
      val mask = in Bits(4 bits)
      val readData = out(mem.readWriteSync(address,writeData,valid,write,mask))

    }
  }

  SpinalVerilog(new MemTester)
  SpinalVhdl(new MemTester)
}

//class MemTesterCocotbBoot extends SpinalTesterCocotbBase {
//  override def getName: String = "MemTester"
//  override def pythonTestLocation: String = "tester/src/test/python/spinal/MemTester"
//  override def createToplevel: Component = new MemTester.MemTester
//  override def backendConfig(config: SpinalConfig): SpinalConfig = config.dumpWave()
//  override def noVhdl = true
//}

class MemTester extends SpinalAnyFunSuite {
  import spinal.core.sim._
  import spinal.lib.sim._
  val ramLength = 8
  test("ram init test") {
    for (i <- 0 to 10)
      SimConfig.withWave
        .compile(
          new Component {

            val mem = Mem(UInt(8 bits), initialContent = Seq.tabulate(ramLength)(U(_, 8 bit)))
            val addr = in port UInt(log2Up(ramLength) bits)
            val cond = in port Bool()
            val value = in port UInt(8 bits)
            mem.readWriteSync(addr, value, True, cond)
            val readAddrStream = slave port Stream(UInt(log2Up(ramLength) bits))
            val readData = master port (mem.streamReadSync(readAddrStream))
          }
        )
        .doSim { (dut) =>
          val clk = dut.clockDomain
          clk.forkStimulus(100)
          dut.cond #= false
          dut.readData.ready #= false
          dut.readAddrStream.valid #= false
          clk.waitSampling()
          var cyclesLeft = 1000

          clk.onSamplings {
            assert(cyclesLeft > 0)
            cyclesLeft -= 1
          }
          StreamDriver.fromIterable(dut.readAddrStream, clk, (0 until ramLength).map(BigInt(_)))
          val rxIt = (0 until ramLength).toSeq.iterator
          StreamReadyRandomizer(dut.readData, clk)
          StreamMonitor(dut.readData, clk) { (res) =>
            val ref = rxIt.next()
            assert(res.toInt == ref)
          }
          clk.waitSamplingWhere(!rxIt.hasNext)
        }
  }
}
