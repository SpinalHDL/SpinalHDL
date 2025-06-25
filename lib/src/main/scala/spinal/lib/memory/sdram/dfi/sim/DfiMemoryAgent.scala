package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.core.sim._
import spinal.lib.sim.SparseMemory

import scala.collection.mutable
class DfiMemoryAgent(ctrl: DfiControlInterface, wr: DfiWriteInterface, rd: DfiReadInterface, clockDomain: ClockDomain) {
  val memory = SparseMemory()

  assert(
    ctrl.config == wr.config & rd.config == wr.config,
    "The config of DfiMemoryAgent is different."
  )

  val busConfig = ctrl.config
  val csCount = busConfig.chipSelectNumber
  val phaseCount = busConfig.frequencyRatio
  val cmdPhase = busConfig.timeConfig.cmdPhase
  val oneTaskDataNumber = busConfig.transferPerBurst / busConfig.dataRate
  val oneTaskByteNumber = busConfig.bytePerBurst
  val bankWidth = busConfig.sdram.bankWidth
  val rowWidth = busConfig.sdram.rowWidth
  val columnWidth = busConfig.sdram.columnWidth

  val ckeProxy = ctrl.cke.simProxy()
  val csNProxy = ctrl.csN.simProxy()
  val rasNProxy = ctrl.rasN.simProxy()
  val casNProxy = ctrl.casN.simProxy()
  val weNProxy = ctrl.weN.simProxy()

  val rowAddrQueue = mutable.Queue[Long]()
  val bankQueue = mutable.Queue[Long]()
  val wrEnQueue = mutable.Queue[Boolean]()
  val wrAddrQueue = mutable.Queue[Long]()
  val wrByteQueue = mutable.Queue[Byte]()
  val wrLatQuenes = mutable.Queue[Int]()
  val rdEnQueue = mutable.Queue[(Boolean, Int)]()
  val rdDataQueue = mutable.Queue[BigInt]()
  val rProcess = Array.fill(phaseCount)(mutable.Queue[(BigInt) => Unit]())
  val rdLatQuenes = mutable.Queue[Int]()

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

  def selectBit(bigInt: BigInt, partIndex: Int, bitNumber: Int) = {
    assert(isPow2(bigInt.bitLength))
    assert(isPow2(bitNumber))
    val bigIntStr = bigInt.toString(2)
    val parts = bigIntStr.grouped(bitNumber).toList.reverse
    assert(partIndex >= 0 && partIndex < parts.size)
    val SelectedBit = BigInt(parts(partIndex), 2)
    Array[Boolean](SelectedBit.testBit(0), SelectedBit.testBit(1))
  }

  def writeDataRxd(wrEn: collection.immutable.IndexedSeq[Boolean], wrData: collection.immutable.IndexedSeq[Long]) = {
    for (phase <- 0 until phaseCount) {
      wrEnQueue.enqueue(wrEn(phase))
      if (wrEnQueue.length == busConfig.timeConfig.tPhyWrData + 1) {
        writeVaild = wrEnQueue.dequeue()
      }
      if (writeVaild) {
        for (j <- 0.until(busConfig.phyIoWidth / 8).reverse) {
          wrByteQueue.enqueue((wrData(phase) >> j * 8).toByte)
        }
        if (oneTakeDataCounter == oneTaskDataNumber) {
          for (i <- 0 until (busConfig.bytePerBurst)) {
            val wrAddr = wrAddrQueue.dequeue()
            val wrByte = wrByteQueue.dequeue()
            writeNotification(wrAddr, wrByte)
            setByte(wrAddr, wrByte)
          }
          oneTakeDataCounter = 0
        } else {
          oneTakeDataCounter = oneTakeDataCounter + 1
        }
      }
      if(wrLatQuenes.nonEmpty) {
        for(i <- 0 until(wrLatQuenes.length)){
          val wrLat = wrLatQuenes.dequeue()
          if(wrLat == 0){
            assert(wrEn(phase), "The parameter tPhyWrLat is not satisfied ")
          }else{
            wrLatQuenes.enqueue(wrLat - 1)
          }
        }
      }
    }
  }

  def writeNotification(address: Long, value: Byte) = {} // memory.write(address, value)

  def setByte(address: Long, value: Byte) = memory.write(address, value)

  def readDataTxd(rdEn: collection.immutable.IndexedSeq[Boolean]) = {
    rd.rd.foreach(_.rddataValid #= false)
    for ((en, phase) <- rdEn.zipWithIndex) {
      if (rProcess(phase).nonEmpty & rdDataQueue.nonEmpty) {
        rdEnQueue.dequeue()
        rProcess(phase).dequeue().apply(rdDataQueue.dequeue())
      }
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
      if(rdLatQuenes.nonEmpty) {
        for(i <- 0 until(rdLatQuenes.length)){
          val rdLat = rdLatQuenes.dequeue()
          if(rdLat == 0){
            assert(en, "The parameter tRddataEn is not satisfied ")
          }else{
            rdLatQuenes.enqueue(rdLat - 1)
          }
        }
      }
    }
  }

  clockDomain.onSamplings {
    val cke = selectBit(ckeProxy.toBigInt.asInstanceOf[BigInt], cmdPhase, csCount)
    val csN = selectBit(csNProxy.toBigInt.asInstanceOf[BigInt], cmdPhase, csCount)
    val ras = rasNProxy.toBigInt.asInstanceOf[BigInt].testBit(cmdPhase)
    val cas = casNProxy.toBigInt.asInstanceOf[BigInt].testBit(cmdPhase)
    val weN = weNProxy.toBigInt.asInstanceOf[BigInt].testBit(cmdPhase)
    val wrEn = wr.wr.map(_.wrdataEn.toBoolean).toIndexedSeq
    val wrData = wr.wr.map(_.wrdata.toLong).toIndexedSeq
    val rdEn = rd.rden.map(_.toBoolean).toIndexedSeq

    for ((enPerChip, idPerChip) <- cke.zip(csN).map(t => t._1 && !t._2).zipWithIndex) {
      // cmd and address
      val active = enPerChip & !ras & cas & weN
      val write = enPerChip & ras & !cas & !weN
      val read = enPerChip & ras & !cas & weN
      if (active) {
        rowAddrQueue.enqueue(ctrl.address.toLong)
        bankQueue.enqueue(ctrl.bank.toLong)
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
          ((idPerChip << busConfig.sdram.wordAddressWidth) + (bank << (columnWidth + rowWidth)) + (rowAddr << columnWidth) + columnAddr) << log2Up(
            busConfig.sdram.bytePerWord
          )
        for (beat <- 0 until (busConfig.beatCount)) {
          for (i <- (0 until (busConfig.bytePerBeat)).reverse) {
            wrAddrQueue.enqueue(byteAddr + beat * busConfig.bytePerBeat + i)
          }
        }
        wrLatQuenes.enqueue(busConfig.timeConfig.tPhyWrLat)
      }

      // read cmd
      if (read) {
        columnAddr = ctrl.address.toLong & (1 << columnWidth) - 1
        byteAddr =
          ((idPerChip << busConfig.sdram.wordAddressWidth) + (bank << (columnWidth + rowWidth)) + (rowAddr << columnWidth) + columnAddr) << log2Up(
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
        rdLatQuenes.enqueue(busConfig.timeConfig.tRddataEn)
      }
    }
    writeDataRxd(wrEn, wrData)
    readDataTxd(rdEn)
  }
}
