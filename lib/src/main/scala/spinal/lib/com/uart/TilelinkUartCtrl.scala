package spinal.lib.com.uart

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.bus.tilelink._
import spinal.lib.misc.InterruptNode

object TilelinkUartCtrl{
  def getTilelinkSupport(proposed: bus.tilelink.M2sSupport) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = addressWidth,
    dataWidth = 32,
    allowBurst = true,
    proposed
  )
  def addressWidth = 6
}

case class TilelinkUartCtrl(config : UartCtrlMemoryMappedConfig, tilelinkParameter: BusParameter) extends Component{
  val io = new Bundle{
    val bus =  slave(Bus(tilelinkParameter))
    val uart = master(Uart(ctsGen = config.uartCtrlConfig.ctsGen, rtsGen = config.uartCtrlConfig.rtsGen))
    val interrupt = out Bool()
  }

  val uartCtrl = new UartCtrl(config.uartCtrlConfig)
  io.uart <> uartCtrl.io.uart

  val busCtrl = new SlaveFactory(io.bus, false)
  val bridge = uartCtrl.driveFrom32(busCtrl,config)
  io.interrupt := bridge.interruptCtrl.interrupt
}


case class TilelinkUartFiber() extends Area{
  val node = bus.tilelink.fabric.Node.slave()
  val interrupt = InterruptNode.master()

  var config = UartCtrlMemoryMappedConfig(
    uartCtrlConfig = UartCtrlGenerics(),
    initConfig = UartCtrlInitConfig(
      baudrate = 115200,
      dataLength = 7, // 8 bits
      parity = UartParityType.NONE,
      stop = UartStopType.ONE
    )
  )

  val logic = Fiber build new Area{
    node.m2s.supported.load(TilelinkUartCtrl.getTilelinkSupport(node.m2s.proposed))
    node.s2m.none()

    val core = TilelinkUartCtrl(config, node.bus.p)
    core.io.bus <> node.bus
    interrupt.flag := core.io.interrupt

    val uart = core.io.uart.toIo()
  }
}