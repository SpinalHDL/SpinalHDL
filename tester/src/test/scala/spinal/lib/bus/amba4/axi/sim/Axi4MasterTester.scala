package spinal.lib.bus.amba4.axi.sim

import spinal.tester.SpinalAnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import scala.util.Random

class Axi4MasterTester extends SpinalAnyFunSuite {
  // regression test for #1692
  val seed = sys.env.get("SEED").map(_.toInt).getOrElse(Random.nextInt(Int.MaxValue))
  val axiConfig = Axi4Config(30, 64, 2)
  class TestBench extends Component {
    val io = new Bundle {
      val axiM = master port new Axi4(axiConfig)
      val axiS = slave port new Axi4(axiConfig)
    }
    io.axiS >> io.axiM
  }

  val byteCount = 4096
  val wordCount = byteCount / axiConfig.bytePerWord
  Seq(0, 1, 31, 255).foreach { len =>
    test(f"full write + readback len=$len") {
      Random.setSeed(seed)
      SimConfig.withFstWave
        .compile(new TestBench())
        .doSim("test", 42) { dut =>
          val clk = dut.clockDomain
          val mem = AxiMemorySim(dut.io.axiM, clk, AxiMemorySimConfig())
          mem.start()
          clk.forkStimulus(10)

          val master = Axi4Master(dut.io.axiS, clk, "named")
          val data = new Array[Byte](byteCount)
          Random.nextBytes(data)
          master.write(0, data.toList, len = len)
          val readBack = master.read(0, data.length, len = len)
          for (((expected, actual), i) <- data.toList zip readBack zipWithIndex) {
            assert(mem.memory.read(i) == expected, f"data in memory $i")
            assert(expected == actual, f"readback data byte $i")
          }
        }
    }
    (1 to axiConfig.bytePerWord).foreach { offset =>
      test(f"unaligned start len=$len,offset=$offset") {
        Random.setSeed(seed)
        SimConfig.withFstWave
          .compile(new TestBench())
          .doSim("test", 42) { dut =>
            val clk = dut.clockDomain
            val mem = AxiMemorySim(dut.io.axiM, clk, AxiMemorySimConfig())
            mem.start()
            clk.forkStimulus(10)
            val master = Axi4Master(dut.io.axiS, clk, "named")
            mem.memory.writeArray(0, Seq.fill(byteCount)(0xff.toByte).toArray)
            val data = new Array[Byte]((2 * axiConfig.bytePerWord) - offset)
            Random.nextBytes(data)
            master.write(offset, data.toList, len = len)
            val readBack = master.read(offset, data.length, len = len)
            for (((expected, actual), i) <- data.toList zip readBack zipWithIndex) {
              assert(mem.memory.read(offset + i) == expected, f"data in memory ${i + offset}")
              assert(expected == actual, f"readback data byte ${i + offset}")
            }
            assert(mem.memory.read(offset + data.length) == 0xff.toByte, "wrote after the burst")
          }
      }
      test(f"unaligned_end,len=$len,offset=$offset") {
        Random.setSeed(seed)
        SimConfig.withFstWave
          .compile(new TestBench())
          .doSim("test", 42) { dut =>
            val clk = dut.clockDomain
            val mem = AxiMemorySim(dut.io.axiM, clk, AxiMemorySimConfig())
            mem.start()
            clk.forkStimulus(10)
            val master = Axi4Master(dut.io.axiS, clk, "named")
            mem.memory.writeBigInt(0, 0, byteCount)
            val data = new Array[Byte]((2 * axiConfig.bytePerWord) - offset)
            Random.nextBytes(data)
            master.write(0, data.toList, len = len)
            val readBack = master.read(0, data.length, len = len)
            for (((expected, actual), i) <- data.toList zip readBack zipWithIndex) {
              assert(mem.memory.read(i) == expected, f"data in memory ${i}")
              assert(expected == actual, f"readback data byte ${i}")
            }
          }
      }
    }
  }
}
