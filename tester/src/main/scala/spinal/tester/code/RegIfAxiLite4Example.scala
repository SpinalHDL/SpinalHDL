package spinal.tester.code

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif.AccessType._
import spinal.lib.bus.regif._
import spinal.core.sim._
import spinal.sim._
import spinal.lib.bus.amba4.axilite.sim.AxiLite4Driver

class Axi4liteRegIfExample extends Component {
  import spinal.lib.bus.amba4.axilite._
  val io = new Bundle {
    val irq = out Bool()
    val axi = slave(AxiLite4(AxiLite4Config(16, 32)))
  }

  val busif = BusInterface(io.axi, (0x000, 100 Byte))
  
  val reg_version = busif.newReg("Version info")
  val f_device    = reg_version.field(8 bits, AccessType.RO, 0xD4l, "Device type")
  val f_maj_ver   = reg_version.field(8 bits, AccessType.RO, 0x01l, "Major version")
  val f_min_ver   = reg_version.field(8 bits, AccessType.RO, 0x00l, "Minor version")
  
  val reg_irq      = busif.newReg("IRQ status")
  val f_irq_status = reg_irq.field(1 bits, AccessType.RW, 0x0l, "IRQ status bits")
  
  val reg_src_addr = busif.newReg("Source buffer")
  val f_src_addr   = reg_src_addr.field(32 bits, AccessType.RW, 0x0l, "Address")
  
  val reg_dst_addr = busif.newReg("Destination buffer")
  val f_dst_addr   = reg_dst_addr.field(32 bits, AccessType.RW, 0x0l, "Address")
  
  val reg_dimension = busif.newReg("Dimension")
  val f_dim_width   = reg_dimension.field(12 bits, AccessType.RW, 0x0l, "Width in pixels")
  val f_dim_rsv_0   = reg_dimension.field( 4 bits, AccessType.NA, 0x0l, "Reserved")
  val f_dim_height  = reg_dimension.field(12 bits, AccessType.RW, 0x0l, "Height in pixels")
  val triggerW      = reg_dimension.eventW
  val triggerR      = reg_dimension.eventR
  
  f_device  := 0xd4
  f_maj_ver := 0x01
  f_min_ver := 0x00

  io.irq := InterruptFactory(busif,"IRQ", False)
}

object simRegIfAxiLite4Example {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new Axi4liteRegIfExample){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      val axiLite = AxiLite4Driver(dut.io.axi, dut.clockDomain)
      
      axiLite.reset()

      dut.clockDomain.waitSampling(4)

      axiLite.read(0x00l)
      axiLite.read(0x04l)

      axiLite.write(0x08, 0xc0001000l)
      axiLite.write(0x0c, 0xc0004000l)
      axiLite.write(0x10, 0x00200010l)

      axiLite.read(0x08l)
      axiLite.read(0x0cl)
      axiLite.read(0x10l)
    }
  }
}
