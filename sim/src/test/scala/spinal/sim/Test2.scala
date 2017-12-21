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
