package spinal.lib.bus.avalon

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._
import spinal.lib.tool.QSysify

object AvalonMMUartCtrl{
  def getAvalonMMConfig = AvalonMMSlaveFactory.getAvalonConfig(addressWidth = 4,dataWidth = 32)

  def main(args: Array[String]) {
    val report = SpinalVhdl(new AvalonMMUartCtrl(UartCtrlGenerics(),64)).printPruned()
    val toplevel = report.toplevel
    toplevel.io.bus addTag(ClockDomainTag(toplevel.clockDomain))
    QSysify(toplevel)
  }
}

class AvalonMMUartCtrl(uartCtrlConfig : UartCtrlGenerics, rxFifoDepth : Int) extends Component{
  val io = new Bundle{
    val bus =  slave(AvalonMM(AvalonMMUartCtrl.getAvalonMMConfig))
    val uart = master(Uart())
  }

  val uartCtrl = new UartCtrl()
  io.uart <> uartCtrl.io.uart

  val busCtrl = AvalonMMSlaveFactory(io.bus)
  uartCtrl.driveFrom(busCtrl,rxFifoDepth)
}


