package spinal.lib.com.usb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.com.usb.ohci.UsbPid
import spinal.lib.com.usb.phy._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.collection.Seq


trait UsbDeviceAgentListener{
  def reset() : Unit
  def hcToUsb(addr : Int, endp : Int, tockenPid : Int, dataPid : Int, data : Seq[Int]) : Unit
  def usbToHc(addr : Int, endp : Int) : Boolean
}

class UsbDeviceAgent(io : UsbLsFsPhyAbstractIoAgent) extends UsbLsFsPhyAbstractIoListener{
  var lowSpeed = false
  var connected = false
  var frameNumber = -1

  var listener : UsbDeviceAgentListener = null
  def onListener(body : UsbDeviceAgentListener => Unit) = if(listener != null) body(listener)


  object WAIT_RESET
  object ENABLED
  object TX_DATA
  object TX_ACK

  var allowSporadicReset = false
  var state : Any = WAIT_RESET
  override def reset() = {
    assert(allowSporadicReset || state == WAIT_RESET)
    state = ENABLED
    onListener(_.reset())
  }

  override def keepAlive() = {
    state match {
      case ENABLED =>
    }
  }

  var addr, endp = 0
  var tockenPid = 0
  override def txPacket(pid : Int, data: Seq[Int]) = {
    state match {
      case ENABLED => {
        addr = data(0) & 0x7F
        endp = data(0) >> 7 | ((data(1) & 0x7) << 1)
        tockenPid = pid
        pid match {
          case UsbPid.SOF => { //SOF
            val fn = data(0) | ((data(1) & 0x3) << 8)
            assert(frameNumber == -1 || ((frameNumber+1) & 0x3FF) == fn)
            frameNumber = fn
          }
          case UsbPid.SETUP | UsbPid.OUT => {
            state = TX_DATA
          }
          case UsbPid.IN => {
            var withAck = false
            onListener{l => withAck |= l.usbToHc(addr, endp)}
            if (withAck) state = TX_ACK
          }
        }
      }
      case TX_DATA => {
        //        println(s"TX $addr $endp ${data.map(e => f"${e}%02x").mkString(",")}")
        state = ENABLED
        onListener(_.hcToUsb(addr, endp, tockenPid, pid, data.dropRight(2)))
      }
      case TX_ACK => {
        assert(pid == UsbPid.ACK && data.size == 0)
        state = ENABLED
      }
    }
  }

  def connect(lowSpeed : Boolean): Unit ={
    this.lowSpeed = lowSpeed
    io.connect(lowSpeed)
    connected = true
  }

  io.listener = this
}

