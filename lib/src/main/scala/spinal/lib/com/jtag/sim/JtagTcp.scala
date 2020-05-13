package spinal.lib.com.jtag.sim

import java.io.{InputStream, OutputStream}
import java.net.ServerSocket

import spinal.core.sim._
import spinal.lib.com.jtag.Jtag


object JtagTcp {
  def apply(jtag: Jtag, jtagClkPeriod: Long) = fork {
    var inputStream: InputStream = null
    var outputStream: OutputStream = null

    val server = new Thread  {
      val socket = new ServerSocket(7894)
      override def run() : Unit = {
        println("WAITING FOR TCP JTAG CONNECTION")
        while (true) {
          val connection = try { socket.accept() } catch { case e : Exception => return }
          connection.setTcpNoDelay(true)
          outputStream = connection.getOutputStream()
          inputStream = connection.getInputStream()
          println("TCP JTAG CONNECTION")
        }
      }
    }
    onSimEnd (server.socket.close())
    server.start()

    while (true) {
      sleep(jtagClkPeriod * 200)
      while (inputStream != null && inputStream.available() != 0) {
        val buffer = inputStream.read()
        jtag.tms #= (buffer & 1) != 0;
        jtag.tdi #= (buffer & 2) != 0;
        jtag.tck #= (buffer & 8) != 0;
        if ((buffer & 4) != 0) {
          outputStream.write(if (jtag.tdo.toBoolean) 1 else 0)
        }
        sleep(jtagClkPeriod / 2)
      }
    }
  }
}
