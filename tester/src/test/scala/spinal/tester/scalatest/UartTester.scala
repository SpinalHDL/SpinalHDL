package spinal.tester.scalatest

import spinal.core._
import spinal.lib.peripheral.uart._
import spinal.lib.peripheral.uart.controller._

class UartTester extends Component {
  val io = new Bundle {
    val uart = new UartControllerIo(UartParameter.default())
  }
  val uartCtrl = new UartController(UartParameter.default())
  io.uart <> uartCtrl.io
}



//class UartTesterGhdlBoot extends SpinalTesterGhdlBase {
//  override def getName: String = "UartTesterGhdl"
//  override def createToplevel: Component = new UartTester().setDefinitionName(getName)
//}

class UartTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "UartTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/UartTester"
  override def createToplevel: Component = new UartTester
}
