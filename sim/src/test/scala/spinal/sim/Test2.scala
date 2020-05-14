package spinal.sim

import java.math.BigInteger
import spinal.sim.vpi._
import scala.collection.JavaConverters._

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
  config.rtlSourcesPaths += "adder.vhd"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val (ghdlbackend, _) = new GhdlBackend(config).instanciate
  println(ghdlbackend.print_signals())
  val nibble1 = ghdlbackend.get_signal_handle("adder.nibble1")
  val nibble2 = ghdlbackend.get_signal_handle("adder.nibble2")
  val sum = ghdlbackend.get_signal_handle("adder.sum")
  ghdlbackend.write32(nibble1, 0)
  ghdlbackend.eval
  ghdlbackend.write32(nibble1, 3)
  ghdlbackend.write32(nibble2, 5)
  println("? = " + ghdlbackend.read32(nibble1).toString)
  ghdlbackend.eval
  println("3 = " + ghdlbackend.read32(nibble1).toString)
  println("3 + 5 = " + ghdlbackend.read32(sum).toString)
  ghdlbackend.write64(nibble1, 4)
  ghdlbackend.write64(nibble2, 1)
  ghdlbackend.sleep(3)
  println("4 + 1 = " + ghdlbackend.read64(sum).toString)
  ghdlbackend.write(nibble1, new VectorInt8(BigInt(2).toByteArray))
  ghdlbackend.write(nibble2, new VectorInt8(BigInt(3).toByteArray))
  ghdlbackend.eval
  println("2 + 3 = " + BigInt(ghdlbackend.read(sum)
                                         .asScala
                                         .toArray
                                         .map{x => x.toByte}).toString)
  ghdlbackend.close
  println("Finished PlayGhdl")
}


