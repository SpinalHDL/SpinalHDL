package spinal.core

import org.scalatest.funsuite.AnyFunSuite

import spinal.core.sim._

import scala.util.Random

class UIntTester extends AnyFunSuite {
  test("wrap_comparison") {
    val width = 8
    SimConfig
      .compile(new Component {
        val x, y   = in  port UInt(width bits)
        val lt, gt = out port Bool()
        val le, ge = out port Bool()

        lt := x.wrap < y
        gt := x.wrap > y
        le := x.wrap <= y
        ge := x.wrap >= y
      })
      .doSim(seed = 42) { dut =>
        val quarter = 1 << (width - 2)
        for (i <- 0 until 2000) {
          val x = dut.x.randomize()
          // ensure two datas' distance are not beyond a quarter.
          val d = Random.nextInt(2 * quarter) - quarter
          val y = (x + d).abs % (4 * quarter)
          dut.y #= y

          sleep(1)

          val rev = (x - y).abs > 3 * quarter
          assert(dut.lt.toBoolean == (if (rev) x > y else x < y))
          assert(dut.gt.toBoolean == (if (rev) x < y else x > y))
          assert(dut.le.toBoolean == (if (rev) x >= y else x <= y))
          assert(dut.ge.toBoolean == (if (rev) x <= y else x >= y))

          sleep(1)
        }
      }
  }
}
