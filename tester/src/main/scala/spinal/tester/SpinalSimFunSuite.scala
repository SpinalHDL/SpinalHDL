package spinal.tester

import org.scalatest.funsuite.AnyFunSuite

class SpinalSimFunSuite extends AnyFunSuite {
  var tester: SpinalSimTester = null
  def SimConfig = tester.SimConfig
  var durationFactor = 0.0
  var ghdlEnabled = true
  var iverilogEnabled = true

  def onlyVerilator(): Unit = {
    iverilogEnabled = false
    ghdlEnabled = false
  }

  def test(testName: String)(testFun: => Unit): Unit = {
    super.test("verilator_" + testName) {
      tester = SpinalSimTester.Verilator
      durationFactor = SpinalSimTester.Verilator.durationFactor
      testFun
    }
    if (ghdlEnabled) super.test("ghdl_" + testName) {
      tester = SpinalSimTester.Ghdl
      durationFactor = SpinalSimTester.Ghdl.durationFactor
      testFun
    }
    if (iverilogEnabled) super.test("iverilog_" + testName) {
      tester = SpinalSimTester.IVerilog
      durationFactor = SpinalSimTester.IVerilog.durationFactor
      testFun
    }
  }
}
