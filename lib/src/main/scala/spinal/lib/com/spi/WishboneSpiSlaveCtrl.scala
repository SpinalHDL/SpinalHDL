package spinal.lib.com.spi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone.{WishboneSlaveFactory, Wishbone, WishboneConfig}


object WishboneSpiSlaveCtrl{
  def getWishboneConfig = WishboneConfig(
    addressWidth = 8,
    dataWidth = 32
  )
}


case class WishboneSpiSlaveCtrl(generics : SpiSlaveCtrlMemoryMappedConfig) extends Component{
  val io = new Bundle{
    val wishbone = slave(Wishbone(WishboneSpiSlaveCtrl.getWishboneConfig))
    val spi = master(SpiSlave())
    val interrupt = out Bool
  }

  val spiCtrl = new SpiSlaveCtrl(generics.ctrlGenerics)
  io.spi <> spiCtrl.io.spi

  val busCtrl = WishboneSlaveFactory(io.wishbone)
  val bridge = spiCtrl.io.driveFrom(busCtrl, 0)(generics)
  io.interrupt := bridge.interruptCtrl.interrupt
}
