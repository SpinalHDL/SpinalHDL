import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.com.eth._
import spinal.lib.sim.{FlowMonitor, StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable
import scala.util.Random

class SpinalSimLibTester extends FunSuite{

  for(bitCount <- 0 until 12) {
    test("CountOnes"+bitCount) {
      SimConfig.noOptimisation.compile(new Component {
        val input = in Bits(bitCount bits)
        val output = out (CountOne(input))
      }).doSim(seed = 42){dut =>
        for(_ <- 0 until 100+(1 << bitCount)*4){
          dut.input.randomize()
          sleep(1)
          assert(dut.output.toInt === dut.input.toBigInt.bitCount)
        }
      }
    }
  }

  for(bitCount <- 0 until 12) {
    test("CountOneOnEach"+bitCount) {
      SimConfig.noOptimisation.compile(new Component {
        val input = in Bits(bitCount bits)
        val output = out Vec(CountOneOnEach(input))
      }).doSim(seed = 42){dut =>
        for(_ <- 0 until 100+(1 << bitCount)*4){
          dut.input.randomize()
          sleep(1)
          val input = dut.input.toBigInt
          for(i <- 0 until bitCount; mask = ((1 << i+1)-1)) assert(dut.output(i).toInt === (input & mask).bitCount)
        }
      }
    }
  }
}
