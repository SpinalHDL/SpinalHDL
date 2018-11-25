package spinal.lib.com.spi.ddr

import spinal.core._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.com.spi.ddr.SpiDdrMasterCtrl.{Cmd, Config, Rsp}
import spinal.lib.io.InOutWrapper
import spinal.lib.{Flow, Stream, master, slave}



object Apb3SpiDdrMasterCtrl{
  def getApb3Config = Apb3Config(
    addressWidth = 8,
    dataWidth = 32,
    selWidth = 1,
    useSlaveError = false
  )

  def main(args: Array[String]): Unit = {
    SpinalConfig(mergeAsyncProcess = false).generateVerilog {
      val c = Apb3SpiDdrMasterCtrl(
        SpiDdrMasterCtrl.MemoryMappingParameters(
          SpiDdrMasterCtrl.Parameters(8, 12, SpiDdrParameter(4, 1, 1)).addFullDuplex(0),
          cmdFifoDepth = 32,
          rspFifoDepth = 32,
          xip = SpiDdrMasterCtrl.XipBusParameters(addressWidth = 24, dataWidth = 32)
        )
      )
      c.rework{
        val sclkShift = in(Bool).setName("clkEarly")
        c.io.spi.sclk.setAsDirectionLess.unsetName().allowDirectionLessIo
        c.io.spi.data.setAsDirectionLess.unsetName().allowDirectionLessIo
        ClockDomain(sclkShift)(master(c.io.spi.sclk.addTag(crossClockDomain).toTriState()).setName("io_spi_sclk"))
        for((d, idx) <- c.io.spi.data.zipWithIndex)
          master(d.toTriState()).setName(s"io_spi_data$idx")
      }
      InOutWrapper(c)
    }
  }
}


case class Apb3SpiDdrMasterCtrl(p : SpiDdrMasterCtrl.MemoryMappingParameters) extends Component{
  val io = new Bundle {
    val apb = slave(Apb3(Apb3SpiDdrMasterCtrl.getApb3Config))
    val xip = ifGen(p.xip != null) (slave(SpiDdrMasterCtrl.XipBus(p.xip)))
    val spi = master(SpiDdrMaster(p.ctrl.spi))
    val interrupt = out Bool()
  }

  val ctrl = SpiDdrMasterCtrl(p.ctrl)
  val mapping = ctrl.io.driveFrom(Apb3SlaveFactory(io.apb, 0))(p)
  if(p.xip != null) io.xip <> mapping.xip.xipBus
  io.spi <> ctrl.io.spi
  io.interrupt <> mapping.interruptCtrl.interrupt
}
