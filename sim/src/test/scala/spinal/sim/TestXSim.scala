package spinal.sim

import spinal.sim._

import scala.collection.JavaConverters._


object TestXSim extends App{
  val config = new XSimBackendConfig()
  config.rtlSourcesPaths += "./yolo/adder.v"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val vcsBackend = new XSimBackend(config)
  println("Finished TestVCS1")
}
