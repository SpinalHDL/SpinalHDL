package spinal.lib.com.usb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.com.usb.ohci.UsbPid
import spinal.lib.com.usb.phy._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random



trait UsbDeviceAgentListener{
  def reset() : Unit
  def hcToUsb(addr : Int, endp : Int, tockenPid : Int, dataPid : Int, data : Seq[Int]) : Unit
  def usbToHc(addr : Int, endp : Int) : Boolean
}

class UsbDeviceAgent(io : UsbLsFsPhyAbstractIoAgent) extends UsbLsFsPhyAbstractIoListener{
  var lowSpeed = false
  var connected = false
  var frameNumber = -1

  var listener : UsbDeviceAgentListener = null
  def onListener(body : UsbDeviceAgentListener => Unit) = if(listener != null) body(listener)


  object WAIT_RESET
  object ENABLED
  object TX_DATA
  object TX_ACK

  var allowSporadicReset = false
  var state : Any = WAIT_RESET
  override def reset() = {
    assert(allowSporadicReset || state == WAIT_RESET)
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
        endp = data(0) >> 7 | ((data(1) & 0x7) << 1)
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
        //        println(s"TX $addr $endp ${data.map(e => f"${e}%02x").mkString(",")}")
        state = ENABLED
        onListener(_.hcToUsb(addr, endp, tockenPid, pid, data.dropRight(2)))
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
    connected = true
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

  def emitBytes(pid : Int, data : Seq[Int], crc16 : Boolean, turnaround : Boolean, ls : Boolean) : Unit = {
    val head = (pid | (~pid << 4)) & 0xFF
    emitBytes(head +: data, crc16, turnaround, ls)
  }

  def emitBytes(data : Seq[Int], crc16 : Boolean, turnaround : Boolean, ls : Boolean, stuffingError : Boolean = false, crcError : Boolean = false, eopError : Boolean = false, errorAt : Int = -1) : Unit = {
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
        emitEop(Random.nextBoolean(), eopError, ls)
      } else {
        rx.enable = true
        rx.dm = !lowSpeed ^ data.head
        rx.dp =  lowSpeed ^ data.head
        delayed(randomBitTime(ls)){
          rec(data.tail)
        }
      }
    }
    delayed(if(turnaround) bitTime(ls)*2 else 0){//TODO better delay for better testing
      rec(toggled)
    }
  }

  def bitTime(ls : Boolean): Long ={
    if(ls) 666666 else 83333
  }

  def randomBitTime(ls : Boolean): Long ={
    bitTime(ls)
  }

  def emitEop(extraBit : Boolean, error : Boolean, ls : Boolean): Unit ={
    if(extraBit){
      rx.enable = true
      rx.dm = Random.nextBoolean()
      rx.dp = !rx.dm
      delayed(randomBitTime(ls)){
        emitEop(false, error, ls)
      }
    } else if(error) {
      rx.enable = false
      rx.dm = false
      rx.dp = false
    } else {
      rx.enable = true
      rx.dm = false
      rx.dp = false
      delayed(randomBitTime(ls) + randomBitTime(ls)){
        rx.enable = true
        rx.dm =  lowSpeed
        rx.dp = !lowSpeed
        delayed(randomBitTime(ls)){
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

  var gotPreamble = false
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
            //Handle lowspeed frame PRE on high speed links
            if(packetBits.size == 17 && decodeBytes(decodePacketToggle(packetBits.take(16))) == List(0x80, 0x3C)){
              packetBits.clear()
              state = ENABLED
              gotPreamble = true
              cdBitRatio *= 8
            }
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
          if(gotPreamble) {
            gotPreamble = false
            cdBitRatio /= 8
          }
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