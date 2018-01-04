package spinal.debugger.gui

import java.io.{InputStream, OutputStream}

import purejavacomm.{CommPortIdentifier, SerialPort, SerialPortEvent, SerialPortEventListener}

/**
 * Created by PIC on 19.04.2015.
 */
class SerialPortByteStreamHal(portName: String, baudrate: Int, stopType: Int, parityType: Int) extends IByteStreamHal {
  var p: SerialPort = null
  var out: OutputStream = null
  var in: InputStream = null
  var obs: IByteStreamHalObserver = null

  override def open: Unit = {
    p = CommPortIdentifier.getPortIdentifier(portName).open("SerialPortComHal", 0).asInstanceOf[SerialPort];
    p.setSerialPortParams(baudrate, SerialPort.DATABITS_8, stopType, parityType);
    out = p.getOutputStream
    in = p.getInputStream

    p.notifyOnDataAvailable(true)
    p.addEventListener(new SerialPortEventListener {
      override def serialEvent(serialPortEvent: SerialPortEvent): Unit = {
        obs.comHalEvent(in)
      }
    })
  }

  override def close: Unit = {
    p.close()
    p = null
    out = null
    in = null
  }


  override def setObserver(obs: IByteStreamHalObserver): Unit = {
    this.obs = obs
    if (in != null && in.available() != 0) {
      obs.comHalEvent(in)
    }
  }

  override def getTxStream : OutputStream = out
}


object SerialPortTest{
  def main(args: Array[String]): Unit = {
    val portsId = CommPortIdentifier.getPortIdentifiers
    while(portsId.hasMoreElements){
      println(portsId.nextElement().getName)
    }
  }
}