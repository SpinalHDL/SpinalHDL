package spinal.lib.com.uart

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.bmb.{Bmb, BmbAccessCapabilities, BmbAccessParameter, BmbParameter, BmbSlaveFactory}

object BmbUartCtrl{
  def getBmbCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = addressWidth,
    dataWidth = 32
  )
  def addressWidth = 6
}

case class BmbUartCtrl(config : UartCtrlMemoryMappedConfig, bmbParameter: BmbParameter) extends Component{
  val io = new Bundle{
    val bus =  slave(Bmb(bmbParameter))
    val uart = master(Uart())
    val interrupt = out Bool()
  }

  val uartCtrl = new UartCtrl(config.uartCtrlConfig)
  io.uart <> uartCtrl.io.uart

  val busCtrl = BmbSlaveFactory(io.bus)
  val bridge = uartCtrl.driveFrom32(busCtrl,config)
  io.interrupt := bridge.interruptCtrl.interrupt
}


