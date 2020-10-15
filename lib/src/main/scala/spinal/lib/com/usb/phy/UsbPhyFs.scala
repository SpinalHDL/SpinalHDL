package spinal.lib.com.usb.phy

import spinal.core._
import spinal.lib._


object UsbLsFs{
  object TxKind extends SpinalEnum{
    val NONE, RESET, SUSPEND, RESUME, PACKET = newElement()
  }

  case class Tx() extends Bundle with IMasterSlave {
    val kind = TxKind()
    val data = Bits(8 bits)
    val last = Bool()
    val ready = Bool()

    override def asMaster(): Unit = {
      out(kind,data,last)
      in(ready)
    }
  }


  object RxKind extends SpinalEnum{
    val NONE, RESUME, PACKET = newElement()
  }

  case class Rx() extends Bundle with IMasterSlave {
    val kind = RxKind()
    val skip = Bool()
    val data = Bits(8 bits)

    override def asMaster(): Unit = {
      out(kind,data,skip)
    }
  }


  case class Ctrl() extends Bundle with IMasterSlave{
    val fullSpeed = Bool()
    val tx = Tx()
    val rx = Rx()

    override def asMaster(): Unit = {
      out(fullSpeed)
      master(tx)
      slave(rx)
    }

//    val tx = Stream(Fragment(Bits(8 bits)))
//    val rx = new Bundle with IMasterSlave {
//      val active = Bool()
//      val valid = Bool()
//      val error = Bool()
//      val data = Bits(8 bits)
//
//      override def asMaster(): Unit = out(this)
//    }
//
//    val highSpeed = Bool()
//    val reset = Event
//    val suspend = Event
//    val resume = Event

  }
}


