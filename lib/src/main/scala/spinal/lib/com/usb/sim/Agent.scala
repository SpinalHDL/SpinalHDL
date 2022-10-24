package spinal.lib.com.usb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.com.usb.ohci.UsbPid
import spinal.lib.com.usb.phy._
import spinal.sim.SimThread

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.collection.{Seq, mutable}



trait UsbLsFsPhyAbstractIoListener{
  def reset() : Unit
  def keepAlive() : Unit
  def txPacket(pid : Int, data : Seq[Int]) : Unit
}

class UsbLsFsPhyAbstractIoAgent(usb : UsbLsFsPhyAbstractIo, cd : ClockDomain, cdRatio : Int){
  var lowSpeed = false
  var connected = false
  val rx = new Area{
    var dp, dm, enable = false
  }

  var listener : UsbLsFsPhyAbstractIoListener = null
  def onListener(body : UsbLsFsPhyAbstractIoListener => Unit) = if(listener != null) body(listener)

  usb.rx.dp #= false
  usb.rx.dm #= false

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
    for((e, i) <- packet.zipWithIndex){
      if(counter != 6){
        ret += e
      } else {
        assert(!e)
      }
      if(e){
        counter += 1
        if(counter == 6){
          assert(!packet(i+1))
        }
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

  def calcCrc(that : Seq[Int], poly : Int, width : Int, check : Boolean, bitCount : Int = -1): Int ={
    def getBit(id : Int) = (that(id/8) >> ((id % 8))) & 1
    val mask = ((1l << width)-1).toInt
    var crc = -1
    val bitCountPatched = if(bitCount < 0) that.size*8 else bitCount
    for(bitId <- 0 until bitCountPatched){
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

  def emitBytes(data : Seq[Int], crc16 : Boolean, turnaround : Boolean, ls : Boolean, stuffingError : Boolean = false, crcError : Boolean = false, eopError : Boolean = false, errorAt : Int = -1, crc5 : Boolean = false) : Unit = {
    var buf = data
    if(crc16){
      val crc = calcCrc(data.tail, 0x8005, 16, false)
      buf = buf ++ List(crc & 0xFF, crc >> 8)
    }

    if(crc5){
      val crc = calcCrc(data.tail, 5, 5, false, bitCount = 11)
      buf = buf.dropRight(1) :+ (crc*8 | buf.last%8)
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

  val doneQueue = mutable.Queue[SimThread]()
  def waitDone(): Unit ={
    val t = simThread
    doneQueue += t
    t.suspend()
  }
  def doneNotify(): Unit ={
    val l = doneQueue.toList
    doneQueue.clear()
    l.foreach(_.resume())
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
          doneNotify()
        }
      }
    }
  }

  def emitReset(): Unit ={
    rx.enable = true
    rx.dm = false
    rx.dp = false
    delayed(100e-6*1e12 toLong){
      rx.enable = false
      doneNotify()
    }
  }

  def emitSuspend(): Unit ={
    delayed(3e-3*0.005*1e12 toLong){
      doneNotify()
    }
  }

  def emitResume(): Unit ={
    rx.enable = true
    rx.dm = true
    rx.dp = false
    delayed(20e-3*0.005*1e12 toLong){
      rx.dm = false
      rx.dp = false
      delayed(83e-9*2*1e12 toLong){
        rx.enable = false
        doneNotify()
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


  val rxBlocked = mutable.Queue[SimThread]()
  var rxPid = 0
  var rxBytes : Seq[Int] = null
  def rxBlocking(): (Int, Seq[Int]) ={
    val t = simThread
    rxBlocked += t
    t.suspend()
    (rxPid, rxBytes)
  }

  def assertRxNak(): Unit ={
    val (pid, payload) = rxBlocking()
    assert(pid == UsbPid.NAK && payload.isEmpty)
  }
  def assertRxStall(): Unit ={
    val (pid, payload) = rxBlocking()
    assert(pid == UsbPid.STALL && payload.isEmpty)
  }
  def assertRxAck(): Unit ={
    val (pid, payload) = rxBlocking()
    assert(pid == UsbPid.ACK && payload.isEmpty)
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
          if(packetBits.length > 16 && !packetBits.takeRight(7).exists(_ != packetBits.last)){
            println("asd")
          }
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
          rxPid = pidLow
          rxBytes = bytes.drop(2).dropRight(2)
          rxBlocked.foreach(_.resume())
          rxBlocked.clear()

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