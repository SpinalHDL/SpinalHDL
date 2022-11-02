package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.util.Random

class LatchTester extends AnyFunSuite {
  test("bad latch") {
    var didRaise = false

    try {
      SimConfig.compile(new Component {
        val myCond = Bool
        val inVal = Bits(8 bits)
        val latchVal = Bits(8 bits)

        when(myCond) {
          latchVal := inVal
        }
      })
    } catch {
      case e: SpinalExit if e.getMessage().contains("LATCH DETECTED") =>
        didRaise = true
    }

    assert(didRaise)
  }

  test("allowed latch") {
    SimConfig.compile(new Component {
      definition.addComment("verilator lint_off LATCH")

      val io = new Bundle {
        val myCond   = in Bool()
        val inVal    = in Bits(8 bits)
        val outVal   = out Bits(8 bits)
      }

      val latchVal = Latch(Bits(8 bits))

      when(io.myCond) {
        latchVal := io.inVal
      }

      io.outVal := latchVal
    }).doSim(dut => {
      dut.io.inVal  #= Random.nextInt(255)
      dut.io.myCond #= false
      sleep(10)

      // Trigger the latch
      dut.io.myCond #= true
      sleep(1)
      dut.io.myCond #= false

      // Check that the latch took the value
      assert(dut.io.inVal.toInt == dut.io.outVal.toInt)
      dut.io.inVal  #= Random.nextInt(255)

      // Wait a bit
      sleep(10)

      // Check that the latch did not take the value
      assert(dut.io.inVal.toInt != dut.io.outVal.toInt)
    })
  }

  test("latchWhen") {
    SimConfig.compile(new Component {
      definition.addComment("verilator lint_off LATCH")

      val io = new Bundle {
        val myCond = in Bool()
        val inVal = in Bits (8 bits)
        val outVal = out Bits (8 bits)
      }
      io.outVal := LatchWhen(io.inVal, io.myCond)
    }).doSim(dut => {
      dut.io.myCond #= false
      dut.io.inVal #= Random.nextInt(255)
      dut.io.myCond #= false
      sleep(10)

      // Trigger the latch
      dut.io.myCond #= true
      sleep(1)
      dut.io.myCond #= false

      // Check that the latch took the value
      assert(dut.io.inVal.toInt == dut.io.outVal.toInt)
      dut.io.inVal #= Random.nextInt(255)

      // Wait a bit
      sleep(10)

      // Check that the latch did not take the value
      assert(dut.io.inVal.toInt != dut.io.outVal.toInt)
    })
  }
}
