package spinal.debugger.gui

import java.io.{InputStream, OutputStream}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC on 19.04.2015.
 */
trait IByteStreamHal {
  def open: Unit
  def close: Unit

  def getTxStream: OutputStream
  def setObserver(obs: IByteStreamHalObserver)
}

trait IByteStreamHalObserver {
  def comHalEvent(in: InputStream)
}

trait IBytePacketHal {
  def open: Unit
  def close: Unit

  def tx(packet: Seq[Byte])
  def setObserver(obs: IBytePacketHalObserver)
}

trait IBytePacketHalObserver {
  def packetHalEvent(in: Seq[Byte])
}


class BytePacketHal(hal: IByteStreamHal) extends IBytePacketHal with IByteStreamHalObserver {
  val cMagic = 0x74
  val cLast = 0x53
  val cResetSet = 0x54
  val cResetClear = 0x55
  implicit def b(x: Int) = x.toByte


  hal.setObserver(this)
  var isOpen = false

  override def open: Unit = {
    hal.open
    reset
  }
  override def close: Unit = {
    isOpen = false
    hal.close
    inMagic = false
    bytesBuffer = null
  }

  var obs: IBytePacketHalObserver = null
  override def setObserver(obs: IBytePacketHalObserver): Unit = {
    this.obs = obs
  }
  override def tx(packet: Seq[Byte]): Unit = {
    if(!isOpen) return
    val out = hal.getTxStream
    for (byte <- packet) {
      if (byte == cMagic) out.write(cMagic)
      out.write(byte)
    }
    flush
  }

  def flush: Unit = hal.getTxStream.write(Array[Byte](cMagic, cLast))


  def reset: Unit = {
    isOpen = false
    flush
    hal.getTxStream.write(Array[Byte](cMagic, cResetSet))
    hal.getTxStream.write(Array[Byte](cMagic, cResetSet))
    Thread.sleep(100)
    isOpen = true
    bytesBuffer = null
    inMagic = false
    hal.getTxStream.write(Array[Byte](cMagic, cResetClear))
  }

  var bytesBuffer : ArrayBuffer[Byte] = null
  var inMagic = false
  override def comHalEvent(in: InputStream): Unit = {
    if(!isOpen) {
      in.skip(in.available())
      return
    }
    while (in.available() != 0) {
      if(bytesBuffer == null) bytesBuffer = ArrayBuffer()
      val byte = in.read().toByte
      if (inMagic) {
        if (byte == cMagic) {
          bytesBuffer += byte
        } else {
          obs.packetHalEvent(bytesBuffer)
          bytesBuffer = null
        }
        inMagic = false
      } else {
        if (byte == cMagic) {
          inMagic = true
        } else {
          bytesBuffer += byte
        }
      }
    }
  }
}

