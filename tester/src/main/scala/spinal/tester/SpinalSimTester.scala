package spinal.tester

import spinal.core.sim.SpinalSimConfig
import spinal.core.{SpinalMode, Verilog, VHDL}

abstract class SpinalSimTester {
  def SimConfig: SpinalSimConfig
  def durationFactor: Double
  def designFactor: Double
  def prefix: String
  def language: SpinalMode
}

object SpinalSimTester {
  def apply(body: => SpinalSimTester => Unit): Unit = {
    body(Ghdl)
    body(IVerilog)
    body(Verilator)
  }

  object Ghdl extends SpinalSimTester {
    override def SimConfig: SpinalSimConfig = spinal.core.sim.SimConfig.withGhdl
    override def durationFactor: Double = 0.005
    override def designFactor: Double = 0.05
    override def prefix: String = "ghdl_"
    override def language: SpinalMode = VHDL
  }

  object IVerilog extends SpinalSimTester {
    override def SimConfig: SpinalSimConfig = spinal.core.sim.SimConfig.withIVerilog
    override def durationFactor: Double = 0.005
    override def designFactor: Double = 0.05
    override def prefix: String = "iverilog_"
    override def language: SpinalMode = Verilog
  }

  object Verilator extends SpinalSimTester {
    override def SimConfig: SpinalSimConfig = spinal.core.sim.SimConfig.withVerilator
    override def durationFactor: Double = 0.5
    override def designFactor: Double = 0.5
    override def prefix: String = "verilator_"
    override def language: SpinalMode = Verilog
  }
}
