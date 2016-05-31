package spinal.lib.bus.avalon

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._
import spinal.lib.tool.QSysify

object AvalonUart{
  def getAvalonMMConfig = AvalonMMSlaveFactory.getAvalonConfig(addressWidth = 4,dataWidth = 32)

  def main(args: Array[String]) {
    val report = SpinalVhdl(new AvalonUart(UartCtrlGenerics())).printPruned()
    val toplevel = report.toplevel
    toplevel.io.bus addTag(ClockDomainTag(toplevel.clockDomain))
    QSysify(toplevel)
  }
}

class AvalonUart( uartCtrlConfig : UartCtrlGenerics) extends Component{
  val io = new Bundle{
    val bus =  slave(AvalonMMBus(AvalonUart.getAvalonMMConfig))
    val uart = master(Uart())
  }

  val busCtrl = new AvalonMMSlaveFactory(io.bus)

  val uartCtrl = new UartCtrl()
  busCtrl.driveAndRead(uartCtrl.io.config.clockDivider,address = 0)
  busCtrl.driveAndRead(uartCtrl.io.config.frame,address = 4)
  busCtrl.createFlow(Bits(uartCtrlConfig.dataWidthMax bits),8).toStream >-> uartCtrl.io.write
  busCtrl.read(uartCtrl.io.write.valid,8)
  busCtrl.readStreamNonBlocking(uartCtrl.io.read.toStream.queue(64),12)
  io.uart <> uartCtrl.io.uart
}


