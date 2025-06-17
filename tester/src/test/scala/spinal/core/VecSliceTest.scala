package spinal.core

import spinal.tester._
import spinal.core.sim._

class VecSlice extends Component {
  val io = new Bundle{
    val input = Vec(in UInt(4 bits), 4)
    val output = Vec(out UInt(4 bits), 2)
  }
  io.output <> io.input(1 to 2)
}

class VecSliceDownTo extends Component {
  val io = new Bundle{
    val input = Vec(in UInt(4 bits), 4)
    val output = Vec(out UInt(4 bits), 2)
  }
  val temp = io.input(3 downto 2)
  io.output := temp
}

class VecSliceUntil extends Component {
  val io = new Bundle{
    val input = Vec(in UInt(4 bits), 4)
    val output = Vec(out UInt(4 bits), 2)
  }
  val temp = io.input(1 until 3)
  io.output := temp
}

class VecSliceTest extends SpinalAnyFunSuite{
  test("to range") {
    SimConfig.compile( new VecSlice()).doSim { dut =>
      dut.io.input.randomize()
      sleep(1)
      assert(dut.io.output(0).toInt == dut.io.input(1).toInt)
      assert(dut.io.output(1).toInt == dut.io.input(2).toInt)
    }
  }  
  test("until range") {
    SimConfig.compile( new VecSliceUntil()).doSim { dut =>
      dut.io.input.randomize()
      sleep(1)
      assert(dut.io.output(0).toInt == dut.io.input(1).toInt)
      assert(dut.io.output(1).toInt == dut.io.input(2).toInt)
    }
  }  
  test("downto range") {
    SimConfig.compile( new VecSliceDownTo()).doSim { dut =>
      dut.io.input.randomize()
      sleep(1)
      assert(dut.io.output(0).toInt == dut.io.input(3).toInt)
      assert(dut.io.output(1).toInt == dut.io.input(2).toInt)
    }
  }  
}