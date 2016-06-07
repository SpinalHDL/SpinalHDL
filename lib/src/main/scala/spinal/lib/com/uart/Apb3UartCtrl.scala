package spinal.lib.com.uart

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.tool.QSysify

object Apb3UartCtrl{
  def getApb3Config = Apb3Config(
    addressWidth = 4,
    dataWidth = 32,
    selWidth = 1,
    useSlaveError = false
  )

  def main(args: Array[String]) {
    val report = SpinalVhdl(new Apb3UartCtrl(UartCtrlGenerics(),64)).printPruned()
    val toplevel = report.toplevel
    toplevel.io.bus addTag(ClockDomainTag(toplevel.clockDomain))
    QSysify(toplevel)
  }
}

class Apb3UartCtrl(uartCtrlConfig : UartCtrlGenerics, rxFifoDepth : Int) extends Component{
  val io = new Bundle{
    val bus =  slave(Apb3(Apb3UartCtrl.getApb3Config))
    val uart = master(Uart())
  }

  val uartCtrl = new UartCtrl()
  io.uart <> uartCtrl.io.uart

  val busCtrl = Apb3SlaveFactory(io.bus,0)
  uartCtrl.driveFrom(busCtrl,rxFifoDepth)
}


