package spinal.core

import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite

class TuplePimperBaseTest extends SpinalAnyFunSuite {
  test("tuple_assignment_matching") {
    SimConfig.compile(new Component {
      val right = in port Bits(4 bit)
      val lefth = out port Bits(2 bit)
      val leftl = out port Bits(2 bit)
      (lefth, leftl) := right
    }).doSim { dut =>
      for (_ <- 0 to 1000) {
        val expected = dut.right.randomize().toInt
        sleep(1)
        assert(dut.leftl.toInt == (expected & 0x3))
        assert(dut.lefth.toInt == (expected >> 2))
      }
    }
  }

  test("tuple_assignment_multiple") {
    SimConfig.compile(new Component {
      val righth = in port Bool()
      val rightl = in port Bits(3 bit)
      val lefth = out port Bits(2 bit)
      val leftl = out port Bits(2 bit)
      (lefth, leftl) := (righth, rightl)
    }).doSim { dut =>
      for (_ <- 0 to 1000) {
        val expectedh = dut.righth.randomize().toInt
        val expectedl = dut.rightl.randomize().toInt
        val expected = (expectedh << 3) + expectedl
        sleep(1)
        assert(dut.leftl.toInt == (expected & 0x3))
        assert(dut.lefth.toInt == (expected >> 2))
      }
    }
  }

  test("tuple_assignment_resized") {
    SimConfig.compile(new Component {
      val right = in port Bits(3 bit)
      val leftl = out port Bits(2 bit)
      val lefth = out port Bits(2 bit)
      (lefth, leftl) := right.resized
    }).doSim { dut =>
      for (_ <- 0 to 1000) {
        val expected = dut.right.randomize().toInt
        sleep(1)
        assert(dut.leftl.toInt == (expected & 0x3))
        assert(dut.lefth.toInt == (expected >> 2))
      }
    }
  }

  test("tuple_assignment_width_mismatch") {
    import CheckTester._
    generationShouldFail(new Component {
      val right = in port Bits(3 bit)
      val left0 = out port Bits(2 bit)
      val left1 = out port Bits(2 bit)
      (left1, left0) := right
    })
  }
}
