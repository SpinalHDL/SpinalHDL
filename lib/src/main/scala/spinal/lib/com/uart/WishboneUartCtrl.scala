package spinal.lib.com.uart

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone.{Wishbone,WishboneConfig, WishboneSlaveFactory}

object WishboneUartCtrl{
  def getWishboneConfig = WishboneConfig(addressWidth = 4,dataWidth = 32)
}

class WishboneUartCtrl(config : UartCtrlMemoryMappedConfig) extends Component{
  val io = new Bundle{
    val bus =  slave(Wishbone(WishboneUartCtrl.getWishboneConfig))
    val uart = master(Uart())
    val interrupt = out Bool
  }

  val uartCtrl = new UartCtrl(config.uartCtrlConfig)
  io.uart <> uartCtrl.io.uart

  val busCtrl = WishboneSlaveFactory(io.bus)
  val bridge = uartCtrl.driveFrom32(busCtrl,config)
  io.interrupt := bridge.interruptCtrl.interrupt
}
