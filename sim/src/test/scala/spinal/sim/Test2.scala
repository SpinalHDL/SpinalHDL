package spinal.sim

import java.math.BigInteger


import scala.collection.mutable.ArrayBuffer
import sys.process._

object Bench {
  def apply(testbench: => Unit): Unit = {
    var retry = 0
    while (retry < 2) {
      val startAt = System.nanoTime
      testbench
      val endAt = System.nanoTime
      System.out.println((endAt - startAt) * 1e-6 + " ms")
      retry += 1;
    }
  }
}
//
//object Test2 {
//  def main(args: Array[String]): Unit = {
//    val config = new BackendConfig()
//    config.rtlSourcesPaths += "sim/TopLevel.v"
//    config.toplevelName = "TopLevel"
//    config.workspacePath = "yolo"
//
//    val vConfig = new VerilatorBackendConfig
////    vConfig.signals ++= List("io_a", "io_b", "io_result").map(name => new VerilatorSignal(List(name),8))
//    val backend = new VerilatorBackend(config,vConfig)
//    val wrapper = backend.instanciate()
//
////    Bench {
////      val handle = wrapper.wrapperNewHandle()
////      var counter = 0
////
////      var idx = 1000000
////      while (idx != 0) {
////        idx -= 1
////        wrapper.wrapperSetCData(handle, 0, 3)
////        wrapper.wrapperSetCData(handle, 1, 6)
////        wrapper.wrapperSetCData(handle, 0, 3)
////        wrapper.wrapperSetCData(handle, 1, 6)
////        wrapper.wrapperSetCData(handle, 0, 3)
////        wrapper.wrapperSetCData(handle, 1, 6)
////        wrapper.wrapperSetCData(handle, 0, 3)
////        wrapper.wrapperSetCData(handle, 1, 6)
////        wrapper.wrapperSetCData(handle, 0, 3)
////        wrapper.wrapperSetCData(handle, 1, 6)
////        wrapper.wrapperEval(handle)
////        counter += wrapper.wrapperGetCData(handle, 2)
////      }
////      println(counter)
////    }
//  }
//}

object PlayGhdl extends App{
  val config = new GhdlBackendConfig()
  config.rtlSourcesPaths += "/home/bellaz/Projects/HDL/GHDLInteropREPL/adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"

  val ghdlbackend = new GhdlBackend(config).instanciate
  val nibble1 = ghdlbackend.get_signal_handle("adder.nibble1")
  val nibble2 = ghdlbackend.get_signal_handle("adder.nibble2")
  val sum = ghdlbackend.get_signal_handle("adder.sum")
  ghdlbackend.write_u32(nibble1, 3)
  ghdlbackend.write_u32(nibble2, 5)
  ghdlbackend.eval
  val res = ghdlbackend.read_u32(sum)
  ghdlbackend.close
  println("3 + 5 = " + res.toString)
  println("Finished PlayGhdl")
}

object PlayIVerilog extends App{
  val config = new IVerilogBackendConfig()
  config.rtlSourcesPaths += "/home/bellaz/Projects/HDL/GHDLInteropREPL/adder.v"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"

  val iverilogbackend = new IVerilogBackend(config).instanciate
  val nibble1 = iverilogbackend.get_signal_handle("adder.nibble1")
  val nibble2 = iverilogbackend.get_signal_handle("adder.nibble2")
  val sum = iverilogbackend.get_signal_handle("adder.sum")
  iverilogbackend.write_u32(nibble1, 3)
  iverilogbackend.write_u32(nibble2, 5)
  iverilogbackend.eval
  val res = iverilogbackend.read_u32(sum)
  iverilogbackend.close
  println("3 + 5 = " + res.toString)
  println("Finished PlayIVerilog")
}
