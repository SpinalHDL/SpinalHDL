package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.core.sim._
import spinal.lib.memory.sdram.dfi.interface.{Dfi, DfiControlInterface, DfiReadInterface, DfiWriteInterface}
import spinal.lib.sim.SparseMemory

import scala.collection.mutable
class DfiMemoryAgent(ctrl: DfiControlInterface, wr: DfiWriteInterface, rd: DfiReadInterface, clockDomain: ClockDomain) {
  val memory = SparseMemory()
  val busConfig = ctrl.config
  val csCount = busConfig.chipSelectNumber
  val phaseCount = busConfig.frequencyRatio
  val cmdPhase = busConfig.cmdPhase
  val oneTaskDataNumber = busConfig.transferPerBurst / busConfig.dataRate
  val oneTaskByteNumber = busConfig.bytePerBurst
  assert(
    ctrl.config == wr.config & rd.config == wr.config,
    "The config of DfiControlInterface is different from DfiWriteInterface."
  )
  val bankWidth = busConfig.sdram.bankWidth
  val rowWidth = busConfig.sdram.rowWidth
  val columnWidth = busConfig.sdram.columnWidth
  val idQueue = mutable.Queue[Int]()
  val rowAddrQueue = mutable.Queue[Long]()
  val columnAddrQueue = mutable.Queue[Long]()
  val bankQueue = mutable.Queue[Long]()
  val wrEnQueue = mutable.Queue[Boolean]()
  val wrAddrQueue = mutable.Queue[Long]()
  val wrByteQueue = mutable.Queue[Byte]()
  val rdAddrQueue = mutable.Queue[Long]()
  val rdEnQueue = mutable.Queue[(Boolean, Int)]()
  val rdDataQueue = mutable.Queue[BigInt]()
  val rProcess = Array.fill(phaseCount)(mutable.Queue[(BigInt) => Unit]())
  val ckeProxy = ctrl.cke.simProxy()
  val csNProxy = ctrl.csN.simProxy()
  val rasNProxy = ctrl.rasN.simProxy()
  val casNProxy = ctrl.casN.simProxy()
  val weNProxy = ctrl.weN.simProxy()
  val wrEnProxy = wr.wr.map(_.wrdataEn.simProxy())
  val wrDataProxy = wr.wr.map(_.wrdata.simProxy())
  val rdEnProxy = rd.rden.map(_.simProxy())
  var rowAddr: Long = 0
  var columnAddr: Long = 0
  var bank: Long = 0
  var byteAddr: Long = 0
  var writeVaild: Boolean = false
  var oneTakeDataCounter: Int = 0
  var rdDataByte: Int = 0
  var rdData: BigInt = 0
  var rdByteAddress: Long = 0
  var rdVaildPhase: Int = 0

  def this(bus: Dfi, clockDomain: ClockDomain) {
    this(bus.control, bus.write, bus.read, clockDomain);
  }

  def getByteAsInt(address: Long) = getByte(address).toInt & 0xff

  def getByte(address: Long) = memory.read(address)

  def setByte(address: Long, value: Byte) = memory.write(address, value)

  def writeNotification(address: Long, value: Byte) = {} // memory.write(address, value)

  def selectBit(bigInt: BigInt, partIndex: Int) = {
    assert(isPow2(bigInt.bitLength))
    val bigIntStr = bigInt.toString(2)
    val parts = bigIntStr.grouped(2).toList.reverse
    assert(partIndex >= 0 && partIndex < parts.size)
    val SelectedBit = BigInt(parts(partIndex), 2)
    Array[Boolean](SelectedBit.testBit(0), SelectedBit.testBit(1))
  }

  clockDomain.onSamplings {
    val cke = selectBit(ckeProxy.toBigInt.asInstanceOf[BigInt], cmdPhase)
    val csN = selectBit(csNProxy.toBigInt.asInstanceOf[BigInt], cmdPhase)
    val ras = rasNProxy.toBigInt.asInstanceOf[BigInt].testBit(cmdPhase)
    val cas = casNProxy.toBigInt.asInstanceOf[BigInt].testBit(cmdPhase)
    val weN = weNProxy.toBigInt.asInstanceOf[BigInt].testBit(cmdPhase)
    val wrEn = wrEnProxy.map(_.toBoolean)
    val wrData = wrDataProxy.map(_.toLong)
    val rdEn = rdEnProxy.map(_.toBoolean)

    for (cs <- cke.zip(csN).map(t => t._1 && !t._2).zipWithIndex) {
      // cmd and address
      val active = cs._1 & !ras & cas & weN
      val write = cs._1 & ras & !cas & !weN
      val read = cs._1 & ras & !cas & weN
      if (active) {
        rowAddrQueue.enqueue(ctrl.address.toLong)
        bankQueue.enqueue(ctrl.bank.toLong)
        idQueue.enqueue(cs._2)
      }
      if (rowAddrQueue.nonEmpty) {
        rowAddr = rowAddrQueue.dequeue()
      }
      if (bankQueue.nonEmpty) {
        bank = bankQueue.dequeue()
      }
      // write cmd
      if (write) {
        columnAddr = ctrl.address.toLong & (1 << columnWidth) - 1
        byteAddr =
          ((cs._2 << busConfig.sdram.wordAddressWidth) + (bank << (columnWidth + rowWidth)) + (rowAddr << columnWidth) + columnAddr) << log2Up(
            busConfig.sdram.bytePerWord
          )
        for (beat <- 0 until (busConfig.beatCount)) {
          for (i <- (0 until (busConfig.bytePerBeat)).reverse) {
            wrAddrQueue.enqueue(byteAddr + beat * busConfig.bytePerBeat + i)
          }
        }
      }

      // read cmd
      if (read) {
        columnAddr = ctrl.address.toLong & (1 << columnWidth) - 1
        byteAddr =
          ((cs._2 << busConfig.sdram.wordAddressWidth) + (bank << (columnWidth + rowWidth)) + (rowAddr << columnWidth) + columnAddr) << log2Up(
            busConfig.sdram.bytePerWord
          )
        for (beat <- (0 until (busConfig.beatCount))) {
          for (phase <- 0.until(phaseCount).reverse) {
            for (j <- 0 until (busConfig.phyIoWidth / 8)) {
              rdByteAddress = byteAddr + beat * busConfig.bytePerBeat + phase * busConfig.phyIoWidth / 8 + j
              rdDataByte = getByteAsInt(rdByteAddress)
              rdData |= BigInt(rdDataByte) << (j * 8)
            }
            rdDataQueue.enqueue(rdData)
            rdData = 0
          }
        }
      }
    }
    // write opcode
    for (i <- 0 until phaseCount) {
      wrEnQueue.enqueue(wrEn(i))

      if (wrEnQueue.length == busConfig.timeConfig.tPhyWrData + 1) {
        writeVaild = wrEnQueue.dequeue()
      }
      if (writeVaild) {
        for (j <- 0.until(busConfig.phyIoWidth / 8).reverse) {
          wrByteQueue.enqueue((wrData(i) >> j * 8).toByte)
        }
        if (oneTakeDataCounter == oneTaskDataNumber) {
          for (i <- 0 until (busConfig.bytePerBurst)) {
            setByte(wrAddrQueue.dequeue(), wrByteQueue.dequeue())
          }
          oneTakeDataCounter = 0
        } else {
          oneTakeDataCounter = oneTakeDataCounter + 1
        }
      }
    }

    // read opcode
    rd.rd.foreach(_.rddataValid #= false)
    for (phase <- 0 until (phaseCount)) {
      if (rProcess(phase).nonEmpty & rdDataQueue.nonEmpty) {
        rdEnQueue.dequeue()
        rProcess(phase).dequeue().apply(rdDataQueue.dequeue())
      }
    }
    for ((en, phase) <- rdEn.zipWithIndex) {
      if (en) {
        rdEnQueue.enqueue((true, phase))
        for ((process, i) <- rProcess.zipWithIndex) {
          if (i == rdVaildPhase) {
            process.enqueue { (bigInt: BigInt) =>
              rd.rd(i).rddataValid #= true
              rd.rd(i).rddata #= bigInt
            }
          }
        }
        rdVaildPhase = (rdVaildPhase + 1) % phaseCount
      }
    }
  }
}
