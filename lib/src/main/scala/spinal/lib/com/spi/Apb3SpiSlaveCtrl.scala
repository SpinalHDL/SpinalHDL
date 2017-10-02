package spinal.lib.com.spi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3SlaveFactory, Apb3, Apb3Config}


object Apb3SpiSlaveCtrl{
  def getApb3Config = Apb3Config(
    addressWidth = 8,
    dataWidth = 32,
    selWidth = 1,
    useSlaveError = false
  )
}


case class Apb3SpiSlaveCtrl(generics : SpiSlaveCtrlMemoryMappedConfig) extends Component{
  val io = new Bundle{
    val apb =  slave(Apb3(Apb3SpiSlaveCtrl.getApb3Config))
    val spi = master(SpiSlave())
    val interrupt = out Bool
  }

  val spiCtrl = new SpiSlaveCtrl(generics.ctrlGenerics)
  io.spi <> spiCtrl.io.spi

  val busCtrl = Apb3SlaveFactory(io.apb)
  val bridge = spiCtrl.io.driveFrom(busCtrl, 0)(generics)
  io.interrupt := bridge.interruptCtrl.interrupt
}
