package spinal.lib.experimental.com.serial

import spinal.core._
import spinal.lib._
import spinal.tester.SpinalTesterGhdlBase

object SerdesSerialTester {

  class BundleA extends Bundle {
    val a = UInt(8 bit)
    val b = Bool()
  }

}

import SerdesSerialTester._

class SerdesSerialTester extends Component {
  val io = new Bundle {
    val rx = slave Flow (Bits(8 bit))
    val tx = master Stream (Bits(8 bit))
  }

  val physicalRx = new SerialCheckerPhysicalfromSerial(8)
  physicalRx.io.input << io.rx

  val serialCheckerRx = new SerialCheckerRx(128)
  serialCheckerRx.io.input << physicalRx.io.output

  val serialLinkRx = new SerialLinkRx
  serialLinkRx.io.input << serialCheckerRx.io.output

  val serialLinkTx = new SerialLinkTx(128,32,1000)
  serialLinkTx.io.rxToTx := serialLinkRx.io.rxToTx
  serialLinkTx.io.input << serialLinkRx.io.output

  val serialCheckerTx = new SerialCheckerTx(8)
  serialCheckerTx.io.input << serialLinkTx.io.output

  val physicalTx = new SerialCheckerPhysicalToSerial(8)
  physicalTx.io.input << serialCheckerTx.io.output

  io.tx << physicalTx.io.output
}


class SerdesSerialTesterBoot extends SpinalTesterGhdlBase {
  override def getName: String = "SerdesSerialTester"
  override def createToplevel: Component = new SerdesSerialTester
}