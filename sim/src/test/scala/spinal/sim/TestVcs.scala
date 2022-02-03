package spinal.sim

object TestVcs1 extends App{
  val config = new VcsBackendConfig()
  config.rtlSourcesPaths += "rtl/TestMemIVerilog.v"
  config.toplevelName = "TestMemIVerilog"
  config.pluginsPath = "simulation_plugins"
  config.workspacePath = "yolo"
  config.workspaceName = "yolo"
  config.wavePath = "test.vcd"
  config.waveFormat = WaveFormat.VCD

  val (vcsbackend, _) = new VcsBackend(config).instanciate
  val clk = vcsbackend.get_signal_handle("TestMemIVerilog.clk")
  val reset = vcsbackend.get_signal_handle("TestMemIVerilog.reset")
  val addr = vcsbackend.get_signal_handle("TestMemIVerilog.io_addr")
  val wvalue = vcsbackend.get_signal_handle("TestMemIVerilog.io_wvalue")
  val wenable = vcsbackend.get_signal_handle("TestMemIVerilog.io_wenable")
  val mem = vcsbackend.get_signal_handle("TestMemIVerilog.mem")

  vcsbackend.write32(clk, 0)
  vcsbackend.write32(reset, 0)
  vcsbackend.write32(addr, 0)
  vcsbackend.write32(wvalue, 0)
  vcsbackend.write32(wenable, 1)
  vcsbackend.eval
  for(i <- 0 to 1023){
    vcsbackend.write32(addr, 1023-i)
    vcsbackend.write32(wvalue, i)
    vcsbackend.write32(clk, 1)
    vcsbackend.sleep(1)
    vcsbackend.write32(clk, 0)
    vcsbackend.sleep(1)
  }

  for(i <- 0 to 1023){
    println(i.toString + " -> " + vcsbackend.read32_mem(mem, i)) // read from index 'i'
  }

  vcsbackend.close
  println("Finished TestVCS1")
}