package spinal.tester.scalatest

import spinal.core._
import spinal.lib.com.uart._

class UartTester extends Component {
  val io = new Bundle {
    val uart = new UartCtrlIo(UartCtrlGenerics())
  }
  val uartCtrl = new UartCtrl(UartCtrlGenerics())
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