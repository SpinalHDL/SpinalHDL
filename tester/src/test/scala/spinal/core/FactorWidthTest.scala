package spinal.core

import spinal.tester._
import spinal.core.sim._

class FactorWidthTest(factor: Int) extends Component {
    val io = new Bundle {
      val din = in UInt (4 bits)
      val dout = out UInt (6 bits)
    }
    io.dout := io.din * factor
}

class FactorWidthTester extends SpinalAnyFunSuite {
  test("compile") {
    SimConfig.allOptimisation
        .compile(new FactorWidthTest(4))
  }
  test("compile_factor_3") {
    SimConfig.allOptimisation
        .compile(new FactorWidthTest(3))
  }
  test("compile_vhdl") {
    SimConfig.allOptimisation.withGhdl
        .compile(new FactorWidthTest(4))
  }
  test("compile_factor_3_vhdl") {
    SimConfig.allOptimisation.withGhdl
        .compile(new FactorWidthTest(3))
  }
  test("compile_factor_5") {
    shouldFail(SimConfig.allOptimisation
        .compile(new FactorWidthTest(5)))
  }
  test("compile_factor_2") {
    shouldFail(SimConfig.allOptimisation
        .compile(new FactorWidthTest(2)))
  }
  test("compile_factor_neg") {
    shouldFail(SimConfig.allOptimisation
        .compile(new FactorWidthTest(-2)))
  }
}


class SFactorWidthTest(factor: Int) extends Component {
  val io = new Bundle {
    val din = in SInt (4 bits)
    val dout = out SInt (6 bits)
  }
  io.dout := io.din * factor
}

class SFactorWidthTester extends SpinalAnyFunSuite {
  test("compile") {
    SimConfig.allOptimisation
        .compile(new SFactorWidthTest(4))
  }
  test("compile_factor_3") {
    SimConfig.allOptimisation
        .compile(new SFactorWidthTest(3))
  }
  test("compile_vhdl") {
    SimConfig.allOptimisation.withGhdl
        .compile(new SFactorWidthTest(4))
  }
  test("compile_factor_3_vhdl") {
    SimConfig.allOptimisation.withGhdl
        .compile(new SFactorWidthTest(3))
  }
  test("compile_factor_5") {
    shouldFail(SimConfig.allOptimisation
        .compile(new SFactorWidthTest(5)))
  }
  test("compile_factor_2") {
    shouldFail(SimConfig.allOptimisation
        .compile(new SFactorWidthTest(2)))
  }
  test("compile_factor_neg") {
    shouldFail(SimConfig.allOptimisation
        .compile(new SFactorWidthTest(-2)))
  }
  test("compile_factor_neg_3") {
    SimConfig.allOptimisation
        .compile(new SFactorWidthTest(-3))
  }
  test("compile_factor_neg_4") {
    SimConfig.allOptimisation
        .compile(new SFactorWidthTest(-4))
  }
  test("compile_factor_neg_5") {
    shouldFail(SimConfig.allOptimisation
        .compile(new SFactorWidthTest(-5)))
  }
}