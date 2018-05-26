package spinal.lib.com.spi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone.{WishboneSlaveFactory, Wishbone, WishboneConfig}


object WishboneSpiMasterCtrl{
  def getWishboneConfig = WishboneConfig(
    addressWidth = 8,
    dataWidth = 32
  )
}


case class WishboneSpiMasterCtrl(config : SpiMasterCtrlMemoryMappedConfig) extends Component{
  val io = new Bundle{
    val wishbone = slave(Wishbone(WishboneSpiMasterCtrl.getWishboneConfig))
    val spi = master(SpiMaster(ssWidth = config.spiCtrlGenerics.ssWidth))
    val interrupt = out Bool
  }

  val spiCtrl = new SpiMasterCtrl(config.spiCtrlGenerics)
  io.spi <> spiCtrl.io.spi

  val busCtrl = WishboneSlaveFactory(io.wishbone)
  val bridge = spiCtrl.io.driveFrom(busCtrl, config)
  io.interrupt := bridge.interruptCtrl.interrupt
}
