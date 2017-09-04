package spinal.lib.com.spi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3SlaveFactory, Apb3, Apb3Config}


object Apb3SpiMasterCtrl{
  def getApb3Config = Apb3Config(
    addressWidth = 8,
    dataWidth = 32,
    selWidth = 1,
    useSlaveError = false
  )
}


case class Apb3SpiMasterCtrl(generics : SpiMasterCtrlMemoryMappedConfig) extends Component{
  val io = new Bundle{
    val apb =  slave(Apb3(Apb3SpiMasterCtrl.getApb3Config))
    val spi = master(SpiMaster(ssWidth = generics.ctrlGenerics.ssWidth))
    val interrupt = out Bool
  }

  val spiCtrl = new SpiMasterCtrl(generics.ctrlGenerics)
  io.spi <> spiCtrl.io.spi

  val busCtrl = Apb3SlaveFactory(io.apb)
  val bridge = spiCtrl.io.driveFrom(busCtrl)(generics)
  io.interrupt := bridge.interruptCtrl.interrupt
}
