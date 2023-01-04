package spinal.core

import spinal.core.sim._

import org.scalatest.funsuite.AnyFunSuite

class VecBitwisePimperTester extends AnyFunSuite {
  for (n <- 0 until 12) {
    test("Vec (op) Vec on " + n + " elements") {
      SimConfig.noOptimisation
        .compile(new Component {
          val a, b = in Vec (SInt(8 bits), n)
          val xor  = out(a ^ b)
          val or   = out(a | b)
          val and  = out(a & b)
          val not  = out(~a)
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
        }
    }
  }
}
