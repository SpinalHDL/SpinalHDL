package spinal.lib.com.usb.phy

import spinal.core._
import spinal.lib._


object UsbHubLsFs{
  object RxKind extends SpinalEnum{
    val NONE, RESUME, PACKET = newElement()
  }

  case class CtrlPort() extends Bundle with IMasterSlave {
    val disable = Event
    val removable = Bool()
    val power = Bool()
    val reset = Event
    val suspend = Event
    val resume = Event

//    val connected = Bool()
    val connect, disconnect = Bool()
    val overcurrent = Bool()
    val remoteResume = Bool()
    val lowSpeed = Bool()

    // HCD messages
//    val ClearPortEnable = Bool()
//    val ClearPortEnable = Bool()
//    val ClearPortEnable = Bool()
//    val ClearPortEnable = Bool()
//    val ClearPortEnable = Bool()


    override def asMaster(): Unit = {
      out(removable, power)
      master(reset, suspend, resume, disable)
      in(connect, disconnect, overcurrent, lowSpeed, remoteResume)
    }
  }


  case class CtrlRx() extends Bundle with IMasterSlave {
    val valid = Bool()
    val active = Bool()
    val data = Bits(8 bits)
    val stuffingError = Bool()

    override def asMaster(): Unit = out(this)
  }

  case class Ctrl(portCount : Int) extends Bundle with IMasterSlave{
    val lowSpeed = Bool()
    val tx = Stream(Fragment(Bits(8 bits)))
    val rx = CtrlRx()

    val usbReset = Bool()
    val usbResume = Bool()
    val overcurrent = Bool()

    val ports = Vec(CtrlPort(), portCount)

    override def asMaster(): Unit = {
      in(overcurrent)
      out(lowSpeed, usbReset, usbResume)
      master(tx)
      slave(rx)
      ports.foreach(master(_))
    }
  }
}


