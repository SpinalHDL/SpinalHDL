package spinal.lib.com.jtag.sim

import spinal.core.TimeNumber
import spinal.core.sim._
import spinal.lib.com.jtag.Jtag

import java.nio.{ByteBuffer, ByteOrder}
import spinal.sim.SimThread

import java.io.{DataInputStream, DataOutputStream}
import java.net.ServerSocket

/*
 * This is a JTAG VPI interface for openocd
 * It is used to connect openocd to a JTAG interface over TCP
 * the reference interface implementation is in https://github.com/openocd-org/openocd/blob/master/src/jtag/drivers/jtag_vpi.c
 * Example openocd config:

source [find interface/jtag_vpi.cfg]
debug_level 1
set _CHIPNAME SOC
jtag newtap $_CHIPNAME cpu -irlen 5 -expected-id 0x10002fff
# init the CPU
target create $_CHIPNAME.cpu riscv -chain-position $_CHIPNAME.cpu
init
 */

object JtagVpi {
  var connection: java.net.Socket = null

  def apply(jtag: Jtag, port: Int = 5555, jtagClkPeriod: TimeNumber): SimThread = fork {
    val driver = JtagDriver(jtag, jtagClkPeriod)
    var inputStream: DataInputStream = null
    var outputStream: DataOutputStream = null
    // create a thread that waits for a connection,
    // and then notifies the main thread that a connection has been established and gives it the connection
    // in a thread safe manner
    class SocketThread extends Thread {

      override def run(): Unit = {
        println("VPI Server started, waiting for connection...")
        while (true) {
          val socket = new ServerSocket(port)
          val connection =
            try {
              socket.accept()
            } catch {
              case _: Exception => return
            }
          this.synchronized {
            this.synchronized(JtagVpi.connection = connection)
            println("VPI Client connected")
          }
          socket.close()
          // wait for the client to disconnect (in a thread safe manner
          while (this.synchronized(JtagVpi.connection) != null) {
            Thread.sleep(100)
          }
        }
      }
    }

    val server = new SocketThread
    onSimEnd()
    server.start()

    // reconnect loop
    while (true) {
      val buffer = new Array[Byte](MaxSizeOfVpiCmd)
      // wait for a connection from the server thread
      while (this.synchronized(connection) == null) {
        sleep(timeToLong(jtagClkPeriod))
      }
      // create the input and output streams
      inputStream = new DataInputStream(this.synchronized(connection.getInputStream))
      outputStream = new DataOutputStream(this.synchronized(connection.getOutputStream))

      // main receive and processing loop
      while (this.synchronized(connection) != null) {
        try {
          inputStream.readFully(buffer)

          val vpiCmd = deserializeVpiCmd(buffer)

          vpiCmd match {
            case VpiCmd(Cmds.RESET, _, _, _, _) => driver.doResetTap()
            case VpiCmd(Cmds.TMS_SEQ, bufferOut, _, _, nbBits) =>
              val tmsSeq = bufferOut.flatMap(x => (0 until 8).map(i => (x & (1 << i)) != 0)).take(nbBits)
              driver.doTmsSeq(tmsSeq)
            case VpiCmd(Cmds.SCAN_CHAIN, bufferOut, bufferIn, length, nbBits) =>
              // bufferOut is a byte array, we need to convert it to a boolean array
              // bufferIn is a byte array, we need to convert it to a boolean array
              // each byte of bufferOut is a sequence of 8 bits (which is why nbBits exists)
              // convert bufferOut to a boolean array
              val tdiSeq = bufferOut.flatMap(x => (0 until 8).map(i => (x & (1 << i)) != 0)).take(nbBits)
              val tdoSeq = driver.doScanChain(tdiSeq, flipTms = false)
              // convert tdoSeq to a byte array grouped by 8 bits
              val tdoSeqBytes = tdoSeq.grouped(8).map(_.foldLeft(0)((acc, b) => (acc << 1) | (if (b) 1 else 0))).toArray
              // copy the result to bufferIn
              for (i <- 0 until length) {
                bufferIn(i) = tdoSeqBytes(i).toByte
              }
              // create packet to send to the client, exactly the same as the received packet but with the tdoSeq
              val vpiCmd = VpiCmd(Cmds.SCAN_CHAIN, bufferOut, bufferIn, length, nbBits)
              serialize(buffer, vpiCmd)
              outputStream.write(buffer)
            case VpiCmd(Cmds.SCAN_CHAIN_FLIP_TMS, bufferOut, bufferIn, length, nbBits) =>
              // same as SCAN_CHAIN but with the last TMS set to 1
              val tdiSeq = bufferOut.flatMap(x => (0 until 8).map(i => (x & (1 << i)) != 0)).take(nbBits)
              val tdoSeq = driver.doScanChain(tdiSeq, flipTms = true)
              val tdoSeqBytes =
                tdoSeq.grouped(8).map(_.reverse.foldLeft(0)((acc, b) => (acc << 1) | (if (b) 1 else 0))).toArray
              for (i <- 0 until length) {
                bufferIn(i) = tdoSeqBytes(i).toByte
              }
              val vpiCmd = VpiCmd(Cmds.SCAN_CHAIN_FLIP_TMS, bufferOut, bufferIn, length, nbBits)
              serialize(buffer, vpiCmd)
              outputStream.write(buffer)
            case VpiCmd(Cmds.STOP_SIMU, _, _, _, _) =>
              println("Stop simulation")
              simSuccess()
            case _ => println(s"ERROR: received unknown VPI command: $vpiCmd")
          }
        } catch {
          case _: Exception =>
            inputStream = null
            outputStream = null
            this.synchronized {
              this.synchronized(JtagVpi.connection = null)
              println("VPI Client disconnected")
            }
        }
      }
    }
  }

  private def deserializeVpiCmd(buffer: Array[Byte]): VpiCmd = {
    val byteBuffer = ByteBuffer.wrap(buffer)
    byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
    val cmd = byteBuffer.getInt match {
      case 0 => Cmds.RESET
      case 1 => Cmds.TMS_SEQ
      case 2 => Cmds.SCAN_CHAIN
      case 3 => Cmds.SCAN_CHAIN_FLIP_TMS
      case 4 => Cmds.STOP_SIMU
    }
    val bufferOut = new Array[Byte](XFERT_MAX_SIZE)
    byteBuffer.get(bufferOut)
    val bufferIn = new Array[Byte](XFERT_MAX_SIZE)
    byteBuffer.get(bufferIn)
    val length = byteBuffer.getInt
    val nbBits = byteBuffer.getInt
    VpiCmd(cmd, bufferOut, bufferIn, length, nbBits)
  }

  private def XFERT_MAX_SIZE = 512

  private def serialize(buffer: Array[Byte], vpiCmd: VpiCmd) = {
    val byteBuffer = ByteBuffer.wrap(buffer)
    byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
    vpiCmd.cmd match {
      case Cmds.RESET               => byteBuffer.putInt(0)
      case Cmds.TMS_SEQ             => byteBuffer.putInt(1)
      case Cmds.SCAN_CHAIN          => byteBuffer.putInt(2)
      case Cmds.SCAN_CHAIN_FLIP_TMS => byteBuffer.putInt(3)
      case Cmds.STOP_SIMU           => byteBuffer.putInt(4)
    }
    byteBuffer.put(vpiCmd.bufferOut)
    byteBuffer.put(vpiCmd.bufferIn)
    byteBuffer.putInt(vpiCmd.length)
    byteBuffer.putInt(vpiCmd.nbBits)
  }

  private def MaxSizeOfVpiCmd = 4 + 2 * XFERT_MAX_SIZE + 4 + 4

  private case class VpiCmd(cmd: Cmds.Cmd, bufferOut: Array[Byte], bufferIn: Array[Byte], length: Int, nbBits: Int)

  private object Cmds extends Enumeration {
    type Cmd = Value
    val RESET, TMS_SEQ, SCAN_CHAIN, SCAN_CHAIN_FLIP_TMS, STOP_SIMU = Value
  }
}
