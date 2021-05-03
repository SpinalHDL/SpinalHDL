package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb._
import spinal.lib.bus.bmb.sim._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.com.usb.ohci._
import spinal.lib.com.usb.phy._
import spinal.lib.com.usb.sim.{UsbDeviceAgent, UsbDeviceAgentListener, UsbLsFsPhyAbstractIoAgent, UsbLsFsPhyAbstractIoListener}
import spinal.lib.sim._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class UsbOhciTbTop(val p : UsbOhciParameter) extends Component {
  val ohci = UsbOhci(p, BmbParameter(
    addressWidth = 12,
    dataWidth = 32,
    sourceWidth = 0,
    contextWidth = 0,
    lengthWidth = 2
  ))

  val phy = UsbLsFsPhy(p.portCount, p.fsRatio, sim=true)

  val irq = ohci.io.interrupt.toIo
  val ctrl = propagateIo(ohci.io.ctrl)
  val dma = propagateIo(ohci.io.dma)
  ohci.io.phy <> phy.io.ctrl
  val usb = propagateIo(phy.io.usb)
}

class TesterUtils(dut : UsbOhciTbTop) {
  val p = dut.p
  val hcControl = 0x04
  val hcCommand = 0x08
  val hcHCCA = 0x18
  val hcPeriodicStart = 0x40
  val hcFmInterval = 0x34
  val hcControlHeadED = 0x20
  val hcBulkHeadED = 0x28
  val hcFmNumber = 0x3C
  def hcRhPortStatus(portId : Int) = 0x54+portId*4

  val USB_OPERATIONAL = 2 << 6
  val BLE = 1 << 5
  val BLF = 1 << 2
  val CLF = 1 << 1
  val CLE = 1 << 4
  val IE = 1 << 3
  val PLE = 1 << 2

  val TOCKEN_SETUP = 13
  val TOCKEN_IN = 9
  val TOCKEN_OUT = 1
  val DATA0 = 3
  val DATA1 = 11
  val HANDSHAKE_ACK = 2
  val HANDSHAKE_NACK = 10
  val HANDSHAKE_STALL = 14


  val malloc = MemoryRegionAllocator(0, 1 << 30)

  implicit class BooleanPimper(self: Boolean) {
    def toInt = if (self) 1 else 0
  }

  def HCCA(malloc : MemoryRegionAllocator): HCCA ={
    val addr = malloc.allocateAligned(0x100)
    HCCA(addr.base.toInt)
  }

  case class HCCA(address: Int) {
    var interrupts = Array.fill(32)(0)
    var frameNumber = 0
    var doneHead = 0

    def save(m: SparseMemory): Unit = {
      for (i <- 0 until 32) m.write(address + i * 4, interrupts(i))
      m.write(address + 0x80, frameNumber)
      m.write(address + 0x84, doneHead)
    }
  }

  def ED(malloc : MemoryRegionAllocator): ED ={
    val addr = malloc.allocateAligned(0x10)
    ED(addr.base.toInt)
  }


  case class ED(address: Int) {
    var FA, EN, D, MPS, tailP, headP, nextED = 0
    var C, H, F, K, S = false

    def save(m: SparseMemory): Unit = {
      m.write(address + 0x00, (FA << 0) | (EN << 7) | (D << 11) | (S.toInt << 13) | (K.toInt << 14) | (F.toInt << 15) | (MPS << 16))
      m.write(address + 0x04, tailP)
      m.write(address + 0x08, headP | (C.toInt << 1) | (H.toInt << 0))
      m.write(address + 0x0C, nextED)
    }

    def load(m: SparseMemory): this.type = {
      val w0 = m.readInt(address + 0x00)
      val w2 = m.readInt(address + 0x08)
      FA = (w0 >> 0) & 0x7F
      EN = (w0 >> 7) & 0xF
      D = (w0 >> 11) & 0x3
      S = ((w0 >> 13) & 0x1) != 0
      K = ((w0 >> 14) & 0x1) != 0
      F = ((w0 >> 15) & 0x1) != 0
      MPS = (w0 >> 16) & 0x7FF
      tailP = m.readInt(address + 0x04)
      headP = w2 & ~0xF
      C = ((w2 >> 1) & 0x1) != 0
      H = ((w2 >> 0) & 0x1) != 0
      nextED = m.readInt(address + 0x0C)
      this
    }
  }

  def TD(malloc : MemoryRegionAllocator): TD ={
    val addr = malloc.allocateAligned(0x10)
    TD(addr.base.toInt)
  }

  //4.3 Transfer Descriptor
  case class TD(address: Int) {
    var CC, EC, T, DI, DP, currentBuffer, nextTD, bufferEnd = 0
    var R = false

    def save(m: SparseMemory): Unit = {
      m.write(address + 0x00, (CC << 28) | (EC << 26) | (T << 24) | (DI << 21) | (DP << 19) | (R.toInt << 18))
      m.write(address + 0x04, currentBuffer)
      m.write(address + 0x08, nextTD)
      m.write(address + 0x0C, bufferEnd)
    }
    def load(m: SparseMemory): this.type = {
      val flags = m.readInt(address + 0x00)
      R = ((flags >> 18) & 0x1) != 0
      DP = (flags >> 19) & 0x3
      DI = (flags >> 21) & 0x7
      T = (flags >> 24) & 0x3
      EC = (flags >> 26) & 0x3
      CC = (flags >> 28) & 0xF
      currentBuffer = m.readInt(address + 0x04)
      nextTD = m.readInt(address + 0x08)
      bufferEnd = m.readInt(address + 0x0C)
      this
    }
  }

  def TDI(malloc : MemoryRegionAllocator): TDI ={
    val addr = malloc.allocateAligned(0x20)
    TDI(addr.base.toInt)
  }
  case class TDI(address: Int) {
    var CC, FC, DI, SF, currentBuffer, nextTD, bufferEnd = 0
    val offsets = Array.fill(8)(0)

    def save(m: SparseMemory): Unit = {
      m.write(address + 0x00, (CC << 28)  | (FC << 24) | (DI << 21) | (SF << 0))
      m.write(address + 0x04, currentBuffer)
      m.write(address + 0x08, nextTD)
      m.write(address + 0x0C, bufferEnd)
      for(i <- 0 until 8 by 2) m.write(address + 0x10 + i * 2, offsets(i) | (offsets(i+1) << 16))
    }

    def load(m: SparseMemory): this.type = {
      val flags = m.readInt(address + 0x00)
      SF = (flags >> 0) & 0xFFFF
      DI = (flags >> 21) & 0x7
      FC = (flags >> 24) & 0x7
      CC = (flags >> 28) & 0xF
      currentBuffer = m.readInt(address + 0x04)
      nextTD = m.readInt(address + 0x08)
      bufferEnd = m.readInt(address + 0x0C)
      for(i <- 0 until 8) offsets(i) = m.readInt(address + 0x10 + i * 2) & 0xFFFF
      this
    }
  }


  def getCurrentConnectStatus(portId : Int) = (ctrl.read(hcRhPortStatus(portId)) & 1) != 0
  def waitConnected(portId : Int) = while(!getCurrentConnectStatus(portId)){dut.clockDomain.waitSampling(Random.nextInt(10))}
  def setPortReset(portId : Int) = ctrl.write(1 << 4, hcRhPortStatus(portId))
  def waitPortReset(portId : Int) = while((ctrl.read(hcRhPortStatus(portId)) & (1 << 4)) != 0) {dut.clockDomain.waitSampling(Random.nextInt(10))}
  def setBulkListFilled() = ctrl.write(BLF, hcCommand)
  def setControlListFilled() = ctrl.write(CLF, hcCommand)

  val portAgents = dut.usb.map(new UsbLsFsPhyAbstractIoAgent(_, dut.clockDomain, p.fsRatio))
  val devices = for(i <- 0 until p.portCount) yield new UsbDeviceAgent(portAgents(i))
  val scoreboards = for(i <- 0 until p.portCount) yield new UsbDeviceScoreboard(devices(i))

  dut.clockDomain.forkStimulus(20800)
  val memory = new BmbMemoryAgent(){
    override def writeNotification(address: Long, value: Byte) = {
      assert(malloc.isAllocated(address))
    }
  }
  memory.addPort(dut.dma, 0, dut.clockDomain, true)
  def ram = memory.memory

  val ctrl = BmbDriver(dut.ctrl, dut.clockDomain)
  dut.clockDomain.waitSampling(10)

  val slowDownFactor = 1.35 //Used to ensure the testbench can run a few isochrone transactions
  val interval = 12000*slowDownFactor toInt
  val intervalWithOverhead = (((interval - 210) * 6) / 7)
  ctrl.write((interval-1) | intervalWithOverhead << 16, hcFmInterval)
  ctrl.write(interval*9/10, hcPeriodicStart)
}




case class TockenKey(addr: Int, endp: Int)
case class DataPacket(pid: Int, data: Seq[Int])

class UsbDeviceScoreboard(io : UsbDeviceAgent) extends UsbDeviceAgentListener{
  override def reset() = {

  }

  case class SetupEntry(tockenPid : Int, packet : DataPacket, onCompletion : () => Unit)
  val setupOutRef = mutable.LinkedHashMap[TockenKey, mutable.Queue[SetupEntry]]()
  override def hcToUsb(addr: Int, endp: Int, tockenPid: Int, dataPid: Int, data: Seq[Int]) : Unit = {
    val key = TockenKey(addr, endp)
    val packet = DataPacket(dataPid, data)
    val ed = setupOutRef.get(key).get
    val entry = ed.dequeue()
    assert(tockenPid == entry.tockenPid)
    assert(packet == entry.packet)
    entry.onCompletion()
  }

  case class InEntry(onCompletion : () => Boolean)
  val inRef = mutable.LinkedHashMap[TockenKey, mutable.Queue[InEntry]]()
  override def usbToHc(addr: Int, endp: Int) : Boolean = {
    val key = TockenKey(addr, endp)
    val ed = inRef.get(key).get
    val entry = ed.dequeue()
    entry.onCompletion()
  }

  def pushSetup(key : TockenKey, data : DataPacket)(onCompletion : => Unit) = setupOutRef.getOrElseUpdate(key, mutable.Queue[SetupEntry]()).enqueue(SetupEntry(UsbPid.SETUP, data, () => onCompletion))
  def pushOut(key : TockenKey, data : DataPacket)(onCompletion : => Unit) = setupOutRef.getOrElseUpdate(key, mutable.Queue[SetupEntry]()).enqueue(SetupEntry(UsbPid.OUT, data, () => onCompletion))
  def pushIn(key : TockenKey)(onCompletion : => Boolean) = inRef.getOrElseUpdate(key, mutable.Queue[InEntry]()).enqueue(InEntry(() => onCompletion))

  io.listener = this

  def assertEmpty = {
    assert(setupOutRef.valuesIterator.forall(_.isEmpty))
    assert(inRef.valuesIterator.forall(_.isEmpty))
  }
}

