package spinal.lib.blackbox.ihp.sg13g2

import spinal.core._
import spinal.core.internals.PhaseMemBlackBoxingGeneric
import spinal.lib._

object PhaseIhpSramBlackBoxDemo extends App{
  val sc = SpinalConfig()
  sc.device = Device.ASIC
  sc.dontCareGenAsZero = true
  sc.addTransformationPhase(new eda.asic.PhaseAsicSanity())
  sc.memBlackBoxers += new PhaseMemBlackBoxingGeneric(blackboxAll)
  sc.memBlackBoxers += new PhaseIhpSramBlackBox()

  sc.generateVerilog(new StreamFifo(Bits(16 bits), 1024))

  // Single-port memory example
  sc.generateVerilog(new Component {
    val io = new Bundle {
      val addr  = in  UInt(10 bits)
      val wdata = in  Bits(32 bits)
      val wen   = in  Bool()
      val rdata = out Bits(32 bits)
    }
    val mem = Mem(Bits(32 bits), 1024)
    io.rdata := mem.readWriteSync(io.addr, io.wdata, True, io.wen)
  }.setDefinitionName("SinglePortMemDemo"))
}
