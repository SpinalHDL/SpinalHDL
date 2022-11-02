package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

class LatchTester extends AnyFunSuite {
  test("bad latch") {
    SimConfig.compile(new Component {
      val myCond   = Bool
      val inVal    = Bits(8 bits)
      val latchVal = Bits(8 bits)

      when(myCond) {
        latchVal := inVal
      }
    })
  }

  test("allowed latch") {
    SimConfig.compile(new Component {
      val myCond   = Bool
      val inVal    = Bits(8 bits)
      val latchVal = Latch(Bits(8 bits))

      when(myCond) {
        latchVal := inVal
      }
    })
  }

  test("latchWhen") {
    SimConfig.compile(new Component {
      val myCond   = Bool
      val inVal    = Bits(8 bits)
      val latchVal = LatchWhen(inVal, myCond)
    })
  }
}
