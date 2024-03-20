package spinal.lib.bus.amba4.axilite.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.amba4.axi.sim.Axi4Resps
import Axi4Resps._

import scala.collection.mutable

/**
 * Simulation master for the AxiLite4 bus protocol [[spinal.lib.bus.amba4.axilite.AxiLite4]].
 *
 * @constructor create a new simulation master with the bus instance and clock domain.
 * @param axil bus master to drive
 * @param clockDomain clock domain to sample data on
 * @example
 * {{{
 *   SimConfig.compile(new Component {
 *     val io = new Bundle {
 *       val axil = slave(AxiLite4(AxiLite4Config(addressWidth = 32, dataWidth = 32)))
 *     }
 *   }).doSim("sample") { dut =>
 *     val master = AxiLite4Master(dut.io.axil, dut.clockDomain)
 *     val data = master.read(0x1000, 4)
 *   }
 * }}}
 */
case class AxiLite4Master(axil: AxiLite4, clockDomain: ClockDomain) {
  private val busConfig = axil.config

  private val arQueue = mutable.Queue[AxiLite4Ax => Unit]()
  private val awQueue = mutable.Queue[AxiLite4Ax => Unit]()

  private val rQueue = mutable.Queue[AxiLite4R => Unit]()
  private val wQueue = mutable.Queue[AxiLite4W => Unit]()
  private val bQueue = mutable.Queue[AxiLite4B => Unit]()

  /** check if all read channels are idle */
  def readIdle = arQueue.isEmpty && rQueue.isEmpty
  /** check if all write channels are idle */
  def writeIdle = awQueue.isEmpty && wQueue.isEmpty && bQueue.isEmpty

  def idle = readIdle && writeIdle

  private def log(chan: String, msg: String): Unit = {
    println(s"AxiLite4Master [$chan]\t: $msg")
  }

  /**
   * Read synchronously multiple bytes from the specified address.
   *
   * @param address address to read from, does not need to be aligned (data will be truncated automatically)
   * @param totalBytes total number of bytes in the result
   * @return read data
   * @note The bus master will automatically issue multiple transactions if more than one is needed, due to address alignment
   * or length.
   */
  def read(address: BigInt, totalBytes: BigInt): List[Byte] = {
    var result: List[Byte] = null
    val mtx = SimMutex().lock()
    readCB(address, totalBytes) { data =>
      result = data
      mtx.unlock()
    }
    mtx.await()
    result
  }

  private def roundAddr(address: BigInt) = address - (address & (busConfig.bytePerWord - 1))

  /** Read asynchronously multiple bytes.  Same semantics as [[read]], but result is delivered in the callback */
  def readCB(address: BigInt, totalBytes: BigInt)(callback: List[Byte] => Unit): Unit = {
    val roundedAddr = roundAddr(address)
    val dropFront = (address - roundedAddr).toInt
    val builder = new mutable.ArrayBuilder.ofByte
    // FIXME: 4K limitation?
    val numTransactions = ((totalBytes + dropFront).toDouble / busConfig.bytePerWord).ceil.toInt

    if (numTransactions > 1) {
      log("..", f"read $address%#x is $numTransactions transactions")
    }

    def run(addr: BigInt, totalBytes: BigInt, numTransactions: Int) = {
      readSingle(addr)(handleTransaction(addr, totalBytes, numTransactions))
    }

    def handleTransaction(addr: BigInt, tot: BigInt, numTransactions: Int)(data: List[Byte]): Unit = {
      builder ++= data

      if (numTransactions == 1) {
        // we are the last one
        callback(builder.result().toList.slice(dropFront, dropFront + totalBytes.toInt))
      } else {
        run(addr + data.length, tot - data.length, numTransactions - 1)
      }
    }

    run(address, totalBytes.toInt, numTransactions)
  }

  /** Read asynchronously one beat of bus data width.  Address must be aligned */
  def readSingle(address: BigInt)(callback: List[Byte] => Unit): Unit = {
    assert(address % busConfig.bytePerWord == 0, f"address $address%#x not aligned for bus width ${busConfig.bytePerWord}")

    arQueue += { ar =>
      ar.addr #= address
      log("AR", f"addr $address%#x")

      rQueue += { r =>
        val data = r.data.toBytes.toList
        log("R", f"got data ${data.bytesToHex}")
        callback(data)
      }
    }
  }

  private val arDriver = StreamDriver(axil.ar, clockDomain) { ar =>
    if (arQueue.isEmpty) false else {
      arQueue.dequeue()(ar)
      true
    }
  }

  StreamReadyRandomizer(axil.r, clockDomain)
  StreamMonitor(axil.r, clockDomain) { r =>
    if (rQueue.nonEmpty) {
      rQueue.dequeue()(r)
    }
  }

  private def padData(address: BigInt, data: List[Byte]) = {
    val roundedAddr = roundAddr(address)
    val padFront = (address - roundedAddr).toInt
    val totalLen = roundUp(padFront + data.length, busConfig.bytePerWord).toInt
    val paddedData = (List.fill(padFront)(0.toByte) ++ data).padTo(totalLen, 0.toByte)
    val padBack = totalLen - padFront - data.length

    (roundedAddr, padFront, padBack, paddedData)
  }

  /**
   * Write synchronously multiple bytes to the specified address.
   * @param addr address to write to; does not need to be aligned (data will be padded automatically)
   * @param data list of bytes to write to the address.  When longer than the bus width, multiple transactions will
   *             be issued
   */
  def write(addr: BigInt, data: List[Byte]): Unit = {
    val mtx = SimMutex().lock()
    writeCB(addr, data) {
      mtx.unlock()
    }
    mtx.await()
  }
  /** Same as `write`, but asynchronously */
  def writeCB(addr: BigInt, data: List[Byte])(callback: => Unit): Unit = {
    val (_, padFront, _, paddedData) = padData(addr, data)

    val numTransactions = paddedData.length / busConfig.bytePerWord
    if (numTransactions > 1) {
      log("..", f"write $addr%#x in $numTransactions transactions")
    }

    def run(addr: BigInt, data: List[Byte], transactionId: Int): Unit = {
      writeSingle(addr, data.take(busConfig.bytePerWord))(handleTransaction(addr, transactionId, data.drop(busConfig.bytePerWord)))
    }

    def handleTransaction(addr: BigInt, transactionId: Int, remaining: List[Byte])(): Unit = {
      if (transactionId == numTransactions - 1) {
        // we are the last one
        assert(remaining.isEmpty, s"left over ${remaining.length} bytes unsent!")
        callback
      } else {
        run(addr + busConfig.bytePerWord, remaining, transactionId + 1)
      }
    }

    run(addr, paddedData, 0)
  }
  /** Write a single transaction.  Address must be aligned */
  def writeSingle(addr: BigInt, data: List[Byte])(callback: => Unit): Unit = {
    assert(data.length == busConfig.bytePerWord, s"writeSingle only take single transactions of ${busConfig.bytePerWord} bytes (got ${data.length} bytes)")

    awQueue += { aw =>
      aw.addr #= addr
      log("AW", f"addr $addr%#x")

      wQueue += { w =>
        w.data #= data.toArray
        log("W", f"data ${data.bytesToHex}")
      }

      bQueue += { b =>
        log("B", s"transaction finished resp ${b.resp.toInt}")
        callback
      }
    }
  }

  private val awDriver = StreamDriver(axil.aw, clockDomain) { aw =>
    if (awQueue.isEmpty) false else {
      awQueue.dequeue()(aw)
      true
    }
  }

  private val wDriver = StreamDriver(axil.w, clockDomain) { w =>
    if (wQueue.isEmpty) false else {
      wQueue.dequeue()(w)
      true
    }
  }

  StreamReadyRandomizer(axil.b, clockDomain)
  StreamMonitor(axil.b, clockDomain) { b =>
    if (bQueue.nonEmpty) {
      bQueue.dequeue()(b)
    }
  }

  /** Reset bus master and drop all pending transactions */
  def reset(): Unit = {
    arQueue.clear
    rQueue.clear
    awQueue.clear
    wQueue.clear
    bQueue.clear

    arDriver.reset
    awDriver.reset
    wDriver.reset
  }
}
