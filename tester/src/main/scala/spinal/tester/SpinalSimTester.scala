package spinal.tester

import spinal.core._
import spinal.core.sim.{SpinalSimConfig, _}
import spinal.tester.SpinalAnyFunSuite

abstract class SpinalSimTester{
  def SimConfig : SpinalSimConfig
  def durationFactor : Double
  def designFactor : Double
  def prefix : String
  def language : SpinalMode
}

object SpinalSimTesterGhdl extends SpinalSimTester{
  override def SimConfig: SpinalSimConfig = spinal.core.sim.SimConfig.withGhdl
  override def durationFactor: Double = 0.005
  override def designFactor: Double = 0.05
  override def prefix: String = "ghdl_"
  override def language: SpinalMode = VHDL
}

object SpinalSimTesterIVerilog extends SpinalSimTester{
  override def SimConfig: SpinalSimConfig = spinal.core.sim.SimConfig.withIVerilog
  override def durationFactor: Double = 0.005
  override def designFactor: Double = 0.05
  override def prefix: String = "iverilog_"
  override def language: SpinalMode = Verilog
}

object SpinalSimTesterVerilator extends SpinalSimTester{
  override def SimConfig: SpinalSimConfig = spinal.core.sim.SimConfig.withVerilator
  override def durationFactor: Double = 0.5
  override def designFactor: Double = 0.5
  override def prefix: String = "verilator_"
  override def language: SpinalMode = Verilog
}

object SpinalSimTester{

  def apply(body :  => SpinalSimTester => Unit): Unit = {
    body(SpinalSimTesterGhdl)
    body(SpinalSimTesterIVerilog)
    body(SpinalSimTesterVerilator)
  }
}

class SpinalSimFunSuite extends SpinalAnyFunSuite{
  var tester : SpinalSimTester = null
  def SimConfig = tester.SimConfig
  var durationFactor = 0.0
  var ghdlEnabled = true
  var iverilogEnabled = true

  def onlyVerilator(): Unit ={
    iverilogEnabled = false
    ghdlEnabled = false
  }

  def withAll(): Unit ={
    iverilogEnabled = true
    ghdlEnabled = true
  }
  def test(testName: String)(testFun: => Unit): Unit = {
    super.test("verilator_" + testName) {
      tester = SpinalSimTesterVerilator
      durationFactor = SpinalSimTesterVerilator.durationFactor
      testFun
    }
    if(ghdlEnabled) super.test("ghdl_" + testName) {
      tester = SpinalSimTesterGhdl
      durationFactor = SpinalSimTesterGhdl.durationFactor
      testFun
    }
    if(iverilogEnabled) super.test("iverilog_" + testName) {
      tester = SpinalSimTesterIVerilog
      durationFactor = SpinalSimTesterIVerilog.durationFactor
      testFun
    }
  }
}
