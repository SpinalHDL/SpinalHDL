package spinal.lib.com.jtag.sim

import spinal.core.{BooleanPimped, IntPimped}
import spinal.core.sim._
import spinal.lib.com.jtag.Jtag

import java.io.{InputStream, OutputStream}
import java.net.ServerSocket

/**
 * Based on :
 * - https://github.com/enjoy-digital/litex/pull/1887
 * - https://github.com/enjoy-digital/litex/blob/c2fd1e9a491d8249cc09007f593c55468f5198ac/litex/build/sim/core/modules/jtagremote/jtagremote.c
 *
 * Can be used in regular openocd via .:
 * adapter speed 10000
 * adapter driver remote_bitbang
 * remote_bitbang_host localhost
 * remote_bitbang_port 44853
 *
 */

object JtagRemote {
  val defaultPort = 44853
  def apply(jtag: Jtag, jtagClkPeriod: Long, port : Int = defaultPort) = fork {
    var inputStream: InputStream = null
    var outputStream: OutputStream = null

    class SocketThread extends Thread  {
      val socket = new ServerSocket(defaultPort)
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
    val server = new SocketThread
    onSimEnd (server.socket.close())
    server.start()

    while (true) {
      sleep(jtagClkPeriod * 200)
      while (inputStream != null && inputStream.available() != 0) {
        var c = inputStream.read()

        if ((c >= '0') && (c <= '7')) {
          c -= '0'
          jtag.tck #= (c & 4) != 0
          jtag.tms #= (c & 2) != 0
          jtag.tdi #= (c & 1) != 0
          sleep(jtagClkPeriod / 2)
        }
        if (c == 'R') {
          outputStream.write(jtag.tdo.toBoolean.mux('1','0'))
        }
      }
    }
  }
}
