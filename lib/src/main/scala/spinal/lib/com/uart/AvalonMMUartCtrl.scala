package spinal.lib.com.uart

import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon.{AvalonMM, AvalonMMSlaveFactory}
import spinal.lib.eda.altera.QSysify

object AvalonMMUartCtrl{
  def getAvalonMMConfig = AvalonMMSlaveFactory.getAvalonConfig(addressWidth = 4,dataWidth = 32)

  def main(args: Array[String]) {
    val report = SpinalVerilog(new AvalonMMUartCtrl(UartCtrlMemoryMappedConfig(UartCtrlGenerics()))).printPruned()
    val toplevel = report.toplevel
    toplevel.io.bus addTag(ClockDomainTag(toplevel.clockDomain))
    QSysify(toplevel)
  }
}

class AvalonMMUartCtrl(config : UartCtrlMemoryMappedConfig) extends Component{
  val io = new Bundle{
    val bus =  slave(AvalonMM(AvalonMMUartCtrl.getAvalonMMConfig))
    val uart = master(Uart())
    val interrupt = out Bool
  }

  val uartCtrl = new UartCtrl(config.uartCtrlConfig)
  io.uart <> uartCtrl.io.uart

  val busCtrl = AvalonMMSlaveFactory(io.bus)
  val bridge = uartCtrl.driveFrom32(busCtrl,config)
  io.interrupt := bridge.interruptCtrl.interrupt
}


