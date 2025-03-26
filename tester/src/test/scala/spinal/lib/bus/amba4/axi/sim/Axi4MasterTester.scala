package spinal.lib.bus.amba4.axi.sim

import spinal.tester.SpinalAnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import scala.util.Random

class Axi4MasterTester extends SpinalAnyFunSuite {
  val seed = sys.env.get("SEED").map(_.toInt).getOrElse(Random.nextInt(Int.MaxValue))

  val axiConfig = Axi4Config(30, 64, 2)
  val byteCount = 4096
  val wordCount = byteCount / axiConfig.bytePerWord
  Seq(0, 1, 31, 255).foreach { len =>
    test(f"full write + readback len=$len") {
      // regression test for #1692
      Random.setSeed(seed)
      SimConfig.withFstWave
        .compile(new Component {
          val io = new Bundle {
            val axiM = master port new Axi4(axiConfig)
            val axiS = slave port new Axi4(axiConfig)
          }
          io.axiS >> io.axiM
        })
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
            assert(expected == actual, f"Byte $i")
          }
        }
    }
  }
}
