package spinal.sim

import spinal.sim._

import scala.collection.JavaConverters._


object TestXSim extends App{
  val config = new XSimBackendConfig()
  config.rtlSourcesPaths += "./yolo/counter.v"
  config.toplevelName = "counter"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.wdb"
  config.waveFormat = WaveFormat.WDB

  val xsimBackend = new XSimBackend(config)
  val iFace = xsimBackend.getInterface()
  val clk = iFace.get_signal_handle("clk")
  val reset = iFace.get_signal_handle("reset")
  val enable = iFace.get_signal_handle("enable")
  val count = iFace.get_signal_handle("count")

  iFace.write32(clk, 0)
  iFace.sleep(10)

  iFace.write32(reset, 1)
  iFace.write32(enable, 0)
  iFace.write32(clk, 1)
  iFace.sleep(10)

  iFace.write32(reset, 0)
  iFace.write32(enable, 1)
  iFace.write32(clk, 0)
  iFace.sleep(10)

  for (i <- 0 until 15) {
    iFace.write32(clk, 1)
    iFace.sleep(10)
    val value = iFace.read32(count)
    println(value)

    iFace.write32(clk, 0)
    iFace.sleep(10)
  }

  println("Finished TestVCS1")
}
