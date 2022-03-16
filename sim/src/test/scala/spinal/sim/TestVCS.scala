package spinal.sim

import spinal.sim.vpi._

import scala.collection.JavaConverters._

object TestVCS1 extends App{
  val config = new VCSBackendConfig()
  config.rtlSourcesPaths += "adder.v"
  config.toplevelName = "adder"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD
  config.vcsCC = Some("gcc-4.4")
  config.vcsLd = Some("g++-4.4")

  val (vcsBackend, _) = new VCSBackend(config).instanciate()
  println(vcsBackend.print_signals())
  val nibble1 = vcsBackend.get_signal_handle("adder.nibble1")
  val nibble2 = vcsBackend.get_signal_handle("adder.nibble2")
  val sum = vcsBackend.get_signal_handle("adder.sum")
  vcsBackend.write32(nibble1, 0)
  vcsBackend.eval
  vcsBackend.write32(nibble1, 3)
  vcsBackend.write32(nibble2, 5)
  println("? = " + vcsBackend.read32(nibble1).toString)
  vcsBackend.eval
  println("3 = " + vcsBackend.read32(nibble1).toString)
  println("3 + 5 = " + vcsBackend.read32(sum).toString)
  vcsBackend.write64(nibble1, 4)
  vcsBackend.write64(nibble2, 1)
  vcsBackend.sleep(3)
  println("4 + 1 = " + vcsBackend.read64(sum).toString)
  vcsBackend.write(nibble1, new VectorInt8(BigInt(2).toByteArray))
  vcsBackend.write(nibble2, new VectorInt8(BigInt(3).toByteArray))
  vcsBackend.eval
  println("2 + 3 = " + BigInt(vcsBackend.read(sum)
    .asScala
    .toArray
    .map{x => x.toByte}).toString)
  vcsBackend.close
  println("Finished TestVCS1")
}
