package spinal.sim

import java.math.BigInteger
import spinal.sim.vpi._
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import sys.process._
import java.lang.RuntimeException
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object TestIVerilog1 extends App{
  val config = new IVerilogBackendConfig()
  config.rtlSourcesPaths += "rtl/TestMemIVerilog.v"
  config.toplevelName = "TestMemIVerilog"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val (iverilogbackend, _) = new IVerilogBackend(config).instanciate
  val clk = iverilogbackend.get_signal_handle("TestMemIVerilog.clk")
  val reset = iverilogbackend.get_signal_handle("TestMemIVerilog.reset")
  val addr = iverilogbackend.get_signal_handle("TestMemIVerilog.io_addr")
  val wvalue = iverilogbackend.get_signal_handle("TestMemIVerilog.io_wvalue")
  val wenable = iverilogbackend.get_signal_handle("TestMemIVerilog.io_wenable")
  val mem = iverilogbackend.get_signal_handle("TestMemIVerilog.mem")

  iverilogbackend.write32(clk, 0)
  iverilogbackend.write32(reset, 0)
  iverilogbackend.write32(addr, 0)
  iverilogbackend.write32(wvalue, 0)
  iverilogbackend.write32(wenable, 1)
  iverilogbackend.eval
  for(i <- 0 to 1023){
    iverilogbackend.write32(addr, 1023-i)
    iverilogbackend.write32(wvalue, i)
    iverilogbackend.write32(clk, 1)
    iverilogbackend.sleep(1)
    iverilogbackend.write32(clk, 0)
    iverilogbackend.sleep(1)
  }

  for(i <- 0 to 1023){
    println(i.toString + " -> " + iverilogbackend.read32_mem(mem, i)) // read from index 'i'
  }

  iverilogbackend.close
  println("Finished TestIVerilog1")
}

object TestIVerilog2 extends App{
  val config = new IVerilogBackendConfig()
  config.rtlSourcesPaths += "rtl/TestMemIVerilog.v"
  config.toplevelName = "TestMemIVerilog"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val (iverilogbackend, _) = new IVerilogBackend(config).instanciate
  val clk = iverilogbackend.get_signal_handle("TestMemIVerilog.clk")
  val reset = iverilogbackend.get_signal_handle("TestMemIVerilog.reset")
  val addr = iverilogbackend.get_signal_handle("TestMemIVerilog.io_addr")
  val wvalue = iverilogbackend.get_signal_handle("TestMemIVerilog.io_wvalue")
  val rvalue = iverilogbackend.get_signal_handle("TestMemIVerilog.io_rvalue")
  val wenable = iverilogbackend.get_signal_handle("TestMemIVerilog.io_wenable")
  val mem = iverilogbackend.get_signal_handle("TestMemIVerilog.mem")

  iverilogbackend.write32(clk, 0)
  iverilogbackend.write32(reset, 0)
  iverilogbackend.write32(addr, 0)
  iverilogbackend.write32(wvalue, 0)
  iverilogbackend.write32(wenable, 0)
  iverilogbackend.eval

  for(i <- 0 to 1023){
    iverilogbackend.write32_mem(mem, 1023-i, i) // write '1023-i' at index 'i' 
  }

  iverilogbackend.eval

  for(i <- 0 to 1023){
    iverilogbackend.write32(addr, i)
    iverilogbackend.write32(clk, 1)
    iverilogbackend.sleep(1)
    iverilogbackend.write32(clk, 0)
    iverilogbackend.sleep(1)
    println(i.toString + " -> " + iverilogbackend.read32(rvalue).toString)
  }

  iverilogbackend.close
  println("Finished TestIVerilog2")
}
