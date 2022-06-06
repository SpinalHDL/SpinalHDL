import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.com.eth._
import spinal.lib.sim.{FlowMonitor, StreamDriver, StreamMonitor, StreamReadyRandomizer}

import scala.collection.mutable
import scala.util.Random

class SpinalSimLibTester extends AnyFunSuite {
  for (n <- 0 until 12) {
    test("Vec (op) Vec on " + n + " elements") {
      SimConfig.noOptimisation
        .compile(new Component {
          val a, b = in Vec (SInt(8 bits), n)
          val xor = out(a ^ b)
          val or = out(a | b)
          val and = out(a & b)
          val not = out(~a)
        })
        .doSim(seed = 42) { dut =>
          for (_ <- 0 until 100) {
            dut.a.randomize()
            dut.b.randomize()
            sleep(1)
            for (i <- 0 until n) {
              assert(dut.xor(i).toInt == (dut.a(i).toInt ^ dut.b(i).toInt))
              assert(dut.or(i).toInt == (dut.a(i).toInt | dut.b(i).toInt))
              assert(dut.and(i).toInt == (dut.a(i).toInt & dut.b(i).toInt))
              assert(dut.not(i).toInt == ~dut.a(i).toInt)
            }
          }

          def isEq(a: UInt, b: UInt): Boolean = a.toInt == b.toInt
        }
    }
  }

  for (bitCount <- 0 until 12) {
    test("CountOnes" + bitCount) {
      LutInputs(Random.nextInt(5) + 2).on {
        SimConfig.noOptimisation
          .compile(new Component {
            val input = in Bits (bitCount bits)
            val output = out(CountOne(input))
          })
          .doSim(seed = 42) { dut =>
            for (_ <- 0 until 100 + (1 << bitCount) * 4) {
              dut.input.randomize()
              sleep(1)
              assert(dut.output.toInt === dut.input.toBigInt.bitCount)
            }
          }
      }
    }
  }

  for (bitCount <- 0 until 12) {
    test("CountOneOnEach" + bitCount) {
      SimConfig.noOptimisation
        .compile(new Component {
          val input = in Bits (bitCount bits)
          val output = out Vec (CountOneOnEach(input))
        })
        .doSim(seed = 42) { dut =>
          for (_ <- 0 until 100 + (1 << bitCount) * 4) {
            dut.input.randomize()
            sleep(1)
            val input = dut.input.toBigInt
            for (i <- 0 until bitCount; mask = ((1 << i + 1) - 1))
              assert(dut.output(i).toInt === (input & mask).bitCount)
          }
        }
    }
  }
}
