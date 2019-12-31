package spinal.lib.com.spi.ddr

import spinal.core._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{Cmd, Config, Rsp}
import spinal.lib.io.InOutWrapper
import spinal.lib.{Flow, Stream, master, slave}



object Apb3SpiXdrMasterCtrl{
  def getApb3Config = Apb3Config(
    addressWidth = 8,
    dataWidth = 32,
    selWidth = 1,
    useSlaveError = false
  )

  def main(args: Array[String]): Unit = {
    SpinalConfig(mergeAsyncProcess = false).generateVerilog {
      val c = Apb3SpiXdrMasterCtrl(
        SpiXdrMasterCtrl.MemoryMappingParameters(
          SpiXdrMasterCtrl.Parameters(8, 12, SpiXdrParameter(4, 1, 1)).addFullDuplex(0),
          cmdFifoDepth = 32,
          rspFifoDepth = 32,
          xip = SpiXdrMasterCtrl.XipBusParameters(addressWidth = 24, lengthWidth = 2)
        )
      )
//      c.rework{
//        val sclkShift = in(Bool).setName("clkEarly")
//        c.io.spi.sclk.setAsDirectionLess.unsetName().allowDirectionLessIo
//        c.io.spi.data.setAsDirectionLess.unsetName().allowDirectionLessIo
//        ClockDomain(sclkShift)(master(c.io.spi.sclk.addTag(crossClockDomain).toTriState()).setName("io_spi_sclk"))
//        for((d, idx) <- c.io.spi.data.zipWithIndex)
//          master(d.toTriState()).setName(s"io_spi_data$idx")
//      }
      InOutWrapper(c)
    }
  }
}


case class Apb3SpiXdrMasterCtrl(p : SpiXdrMasterCtrl.MemoryMappingParameters) extends Component{
  val io = new Bundle {
    val apb = slave(Apb3(Apb3SpiXdrMasterCtrl.getApb3Config))
    val xip = ifGen(p.xip != null) (slave(SpiXdrMasterCtrl.XipBus(p.xip)))
    val spi = master(SpiXdrMaster(p.ctrl.spi))
    val interrupt = out Bool()
  }

  val ctrl = SpiXdrMasterCtrl(p.ctrl)
  val mapping = SpiXdrMasterCtrl.driveFrom(ctrl, Apb3SlaveFactory(io.apb, 0))(p)
  if(p.xip != null) io.xip <> mapping.xip.xipBus
  io.spi <> ctrl.io.spi
  io.interrupt <> mapping.interruptCtrl.interrupt
}
