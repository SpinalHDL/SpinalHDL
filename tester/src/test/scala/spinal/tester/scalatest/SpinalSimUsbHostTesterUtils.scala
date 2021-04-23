package spinal.tester.scalatest

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb._
import spinal.lib.bus.bmb.sim._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.com.usb.ohci._
import spinal.lib.com.usb.phy._
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
      FA = (w0 >> 0) & 0x3F
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


  def getCurrentConnectStatus(portId : Int) = (ctrl.read(hcRhPortStatus(portId)) & 1) != 0
  def waitConnected(portId : Int) = while(!getCurrentConnectStatus(portId)){dut.clockDomain.waitSampling(Random.nextInt(10))}
  def setPortReset(portId : Int) = ctrl.write(1 << 4, hcRhPortStatus(portId))
  def waitPortReset(portId : Int) = while((ctrl.read(hcRhPortStatus(portId)) & (1 << 4)) != 0) {dut.clockDomain.waitSampling(Random.nextInt(10))}
  def setBulkListFilled() = ctrl.write(BLF, hcCommand)

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

  val interval = 12000
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


trait UsbDeviceAgentListener{
  def reset() : Unit
  def hcToUsb(addr : Int, endp : Int, tockenPid : Int, dataPid : Int, data : Seq[Int]) : Unit
  def usbToHc(addr : Int, endp : Int) : Boolean
}

class UsbDeviceAgent(io : UsbLsFsPhyAbstractIoAgent) extends UsbLsFsPhyAbstractIoListener{
  var lowSpeed = false
  var frameNumber = -1

  var listener : UsbDeviceAgentListener = null
  def onListener(body : UsbDeviceAgentListener => Unit) = if(listener != null) body(listener)


  object WAIT_RESET
  object ENABLED
  object TX_DATA
  object TX_ACK

  var state : Any = WAIT_RESET
  override def reset() = {
    assert(state == WAIT_RESET)
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
        endp = data(0) >> 7 | ((data(1) & 0x3) << 1)
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
        println(s"TX $addr $endp ${data.map(e => f"${e}%02x").mkString(",")}")
        state = ENABLED
        onListener(_.hcToUsb(addr, endp,tockenPid, pid, data.dropRight(2)))
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
  }

  io.listener = this
}
trait UsbLsFsPhyAbstractIoListener{
  def reset() : Unit
  def keepAlive() : Unit
  def txPacket(pid : Int, data : Seq[Int]) : Unit
}

class UsbLsFsPhyAbstractIoAgent(usb : UsbLsFsPhyAbstractIo, cd : ClockDomain, cdRatio : Int){
  var lowSpeed = false
  var connected = false
  val rx = new {
    var dp, dm, enable = false
  }

  var listener : UsbLsFsPhyAbstractIoListener = null
  def onListener(body : UsbLsFsPhyAbstractIoListener => Unit) = if(listener != null) body(listener)

  usb.rx.dp #= false
  usb.rx.dm #= false
  usb.overcurrent #= false

  class State
  object DISCONNECTED extends State
  object ENABLED extends State
  object TX_SE0 extends State
  object TX_K extends State
  object BIT extends State
  object EOP_1 extends State
  object EOP_2 extends State
  var state : State = DISCONNECTED
  var phase = 0

  var txEnableLast = false
  var txSe0Last = false
  var txDataLast = false
  var txStable, txStableLast = 0

  var cdBitRatio = 0
  var byteBuffer = 0
  var counter = 0
  val packetBits = ArrayBuffer[Boolean]()

  def log(that : String) =  {} //println(usb + " " + that)

  def decodePacketToggle(packet : Seq[Boolean]): Seq[Boolean] ={
    val ret = ArrayBuffer[Boolean]()
    var last = true
    for(e <- packet){
      ret += e == last
      last = e
    }
    ret
  }

  def decodeStuffing(packet : Seq[Boolean]): Seq[Boolean] ={
    val ret = ArrayBuffer[Boolean]()
    var counter = 0
    for(e <- packet){
      if(counter != 6){
        ret += e
      } else {
        assert(!e)
      }
      if(e){
        counter += 1
      } else {
        counter = 0
      }
    }
    ret
  }

  def decodeBytes(packet : Seq[Boolean]) : Seq[Int] = {
    assert(packet.size % 8 == 0)
    val ret = ArrayBuffer[Int]()
    for(byteId <- 0 until packet.size/8){
      var buf = 0
      for(bitId <- 0 until 8){
        if(packet(byteId*8+bitId)){
          buf |= 1 << bitId
        }
      }
      ret += buf
    }
    ret
  }

  def calcCrc(that : Seq[Int], poly : Int, width : Int, check : Boolean): Int ={
    def getBit(id : Int) = (that(id/8) >> ((id % 8))) & 1
    val mask = ((1l << width)-1).toInt
    var crc = -1
    for(bitId <- 0 until that.size*8){
      val bit = getBit(bitId) ^ ((crc >> (width-1)) & 1)
      crc = (crc << 1) ^ ((if(bit == 1) poly else 0))
    }
    if(!check) {
      val crcReversed = (0 until width).map(i => ((crc >> i) & 1) << (width - i - 1)).reduce(_ | _)
      (~crcReversed) & mask
    } else {
      crc & mask
    }
  }

  def emitBytes(pid : Int, data : Seq[Int], crc16 : Boolean, turnaround : Boolean) : Unit = {
    val head = (pid | (~pid << 4)) & 0xFF
    emitBytes(head +: data, crc16, turnaround)
  }

  def emitBytes(data : Seq[Int], crc16 : Boolean, turnaround : Boolean, stuffingError : Boolean = false, crcError : Boolean = false, eopError : Boolean = false) : Unit = {
    var buf = data
    if(crc16){
      val crc = calcCrc(data.tail, 0x8005, 16, false)
      buf = buf ++ List(crc & 0xFF, crc >> 8)
    }

    buf = 0x80 +: buf
    var booleans = bytesToBoolean(buf)
    if(crcError){
//      do {
      val bit = Random.nextInt(booleans.size - 16) + 16
      booleans(bit) ^= true
//      } while(calcCrc(buf.tail,0x8005, 16, true) == 0x800D)
    }
    val stuffed = encodeStuffing(booleans).toArray
    if(stuffingError) {
      val patchAt = if(stuffed.size <= 16) 8 else 8+Random.nextInt(stuffed.size-16)
      for(i <- 0 until 8){
        stuffed(patchAt + i) = true
      }
    }
    val toggled = encodeToggle(stuffed).toList

    def rec(data : List[Boolean]): Unit ={
      if(data.isEmpty){
        emitEop(Random.nextBoolean(), eopError)
      } else {
        rx.enable = true
        rx.dm = !lowSpeed ^ data.head
        rx.dp =  lowSpeed ^ data.head
        delayed(randomBitTime()){
          rec(data.tail)
        }
      }
    }
    delayed(if(turnaround) bitTime()*2 else 0){//TODO better delay for better testing
      rec(toggled)
    }
  }

  def bitTime(): Long ={
    if(lowSpeed) 666666 else 83333
  }

  def randomBitTime(): Long ={
    bitTime()
  }

  def emitEop(extraBit : Boolean, error : Boolean): Unit ={
    if(extraBit){
      rx.enable = true
      rx.dm = Random.nextBoolean()
      rx.dp = !rx.dm
      delayed(randomBitTime()){
        emitEop(false, error)
      }
    } else if(error) {
      rx.enable = false
      rx.dm = false
      rx.dp = false
    } else {
      rx.enable = true
      rx.dm = false
      rx.dp = false
      delayed(randomBitTime() + randomBitTime()){
        rx.enable = true
        rx.dm =  lowSpeed
        rx.dp = !lowSpeed
        delayed(randomBitTime()){
          rx.enable = false
        }
      }
    }
  }

  def bytesToBoolean(data : Seq[Int]) = {
    val ret = ArrayBuffer[Boolean]()
    for(e <- data) {
      for (bitId <- 0 until 8) {
        ret += ((e >> bitId) & 1) != 0
      }
    }
    ret
  }

  def encodeStuffing(data : Seq[Boolean]) : Seq[Boolean] = {
    val ret = ArrayBuffer[Boolean]()
    var counter = 0
    for(e <- data){
      if(counter == 6){
        ret += false
        counter = 0
      }
      ret += e
      if(e){
        counter = counter + 1
      } else {
        counter = 0
      }
    }
    ret
  }

  def encodeToggle(data : Seq[Boolean]) : Seq[Boolean] = {
    val ret = ArrayBuffer[Boolean]()
    var value = true
    for(e <- data){
      value ^= !e
      ret += value
    }
    ret
  }

  cd.onSamplings{
    val txEnable = usb.tx.enable.toBoolean
    val txSe0 = usb.tx.se0.toBoolean
    val txData = usb.tx.data.toBoolean
    if(txEnable != txEnableLast || txEnable && (txSe0 != txSe0Last || !txSe0 && txData != txDataLast)){
      txStableLast = txStable
      txStable = 0
    }
    phase = phase + 1
    txStable = txStable + 1
    val bitEvent = phase == cdBitRatio
    if(bitEvent) phase = 0

    def txJ = !txSe0 && ( txData ^ lowSpeed)
    def txK = !txSe0 && (!txData ^ lowSpeed)
    assert(!(rx.enable && txEnable))
    if(rx.enable){
      usb.rx.dp #= rx.dp
      usb.rx.dm #= rx.dm
    }else if(txEnable){
      usb.rx.dp #= !txSe0 &&  txData
      usb.rx.dm #= !txSe0 && !txData
    } else {
      usb.rx.dp #= connected && !lowSpeed
      usb.rx.dm #= connected &&  lowSpeed
    }


    state match {
      case DISCONNECTED =>
      case ENABLED => {
        if(txEnable){
          phase = 1
          if(txSe0){
            state = TX_SE0
          } else if(txK){
            state = TX_K
          }
        }
      }
      case TX_SE0 => {
        if (!txEnable) { //Reset
          log("Reset")
          onListener(_.reset())
          state = ENABLED
        } else if (txJ) {
          log("keep alive")
          onListener(_.keepAlive())
          state = EOP_2
        } else {
          assert(txSe0)
        }
      }
      case TX_K => {
        assert(txEnable)
        if(txSe0){
          ??? //RESUME
        } else if(txJ){
          assert(txStableLast == cdBitRatio)
          log("PACKET")
          state = BIT
          byteBuffer = 0
          counter = 1
          packetBits.clear()
          packetBits += false
        } else {
          assert(txK)
        }
      }
      case BIT => {
        assert(txEnable)
        if(bitEvent){
          assert(txStable >= cdBitRatio)
          if(txSe0){
            state = EOP_1
            log("EOP")
          } else {
            packetBits += txJ
          }
        }
      }
      case EOP_1 => {
        assert(txEnable && txSe0)
        if(bitEvent){
          val detoggled = decodePacketToggle(packetBits)
          val destuffed = decodeStuffing(detoggled)
          val bytes = decodeBytes(destuffed)
          log(bytes.map(e => f"${e}%02x").mkString(","))
          assert(bytes(0) == 0x80)
          (bytes(1) & 0x3) match{
            case 1 => {
              val crc = calcCrc(bytes.drop(2), 5, 5, true)
              assert(crc == 0x0C)
            }
            case 3 => {
              val crc = calcCrc(bytes.drop(2), 0x8005, 16, true)
              assert(crc == 0x800D)
            }
            case _ =>
          }

          val pidLow = bytes(1) & 0xF
          val pidHigh = (~bytes(1) >> 4) & 0xF
          assert(pidLow == pidHigh)

          onListener(_.txPacket(pidLow, bytes.drop(2)))
          state = EOP_2
        }
      }
      case EOP_2 => {
        assert(txEnable && !txSe0 && txJ)
        if(bitEvent){
          state = ENABLED
        }
      }
    }
    txEnableLast = txEnable
    txSe0Last = txSe0
    txDataLast = txData
  }

  def connect(lowSpeed : Boolean): Unit ={
    this.lowSpeed = lowSpeed
    this.connected = true
    state = ENABLED
    cdBitRatio = (if(lowSpeed) 8 else 1) * cdRatio
  }
}