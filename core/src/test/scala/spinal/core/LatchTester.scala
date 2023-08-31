package spinal.core

import sim._
import spinal.tester.{SpinalAnyFunSuite, SpinalSimTester}

class LatchTester extends SpinalAnyFunSuite {
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

  abstract class Dut extends Component {
    val io = new Bundle {
      val cond   = in  Bool()
      val input  = in  Bits (8 bits)
      val output = out Bits (8 bits)
    }
  }

  class LatchDut extends Dut {
    val latch = Latch(Bits(8 bits))
    when(io.cond) {
      latch := io.input
    }

    io.output := latch
  }

  class LatchWhenDut extends Dut {
    io.output := LatchWhen(io.input, io.cond)
  }

  // Implements the DUT agnostic sim testbench logic
  def simDriver(dut: Dut): Unit = {
    dut.io.input.randomize()
    dut.io.cond #= false
    sleep(1)

    // Trigger the latch
    dut.io.cond #= true
    sleep(1)
    dut.io.cond #= false

    // Check that the latch took the value
    assert(dut.io.input.toInt == dut.io.output.toInt)

    // Make a new input, ensuring it isn't the same as before
    val oldInput = dut.io.input.toInt
    var newInput = dut.io.input.randomizedInt()
    while(newInput == oldInput) newInput = dut.io.input.randomizedInt()
    dut.io.input #= newInput

    // Wait a bit
    sleep(1)

    // Check that the latch did not take the value
    assert(dut.io.output.toInt == oldInput)
  }

  SpinalSimTester { env =>
    import env._
    test(prefix + "Latch") {
      SimConfig.compile(new LatchDut).doSim(simDriver _)
    }

    test(prefix + "LatchWhen") {
      SimConfig.compile(new LatchWhenDut).doSim(simDriver _)
    }
  }
}
