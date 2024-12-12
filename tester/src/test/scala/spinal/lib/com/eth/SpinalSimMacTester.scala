package spinal.lib.com.eth

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{FlowMonitor, StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinal.tester.SpinalAnyFunSuite

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable
import scala.util.Random

class Eth{
  val destination, source = new Array[Byte](6)
  var ethType : Int = 0

  def to(bb : ByteBuffer): Unit = {
    bb.put(destination)
    bb.put(source)
    bb.putShort(ethType toShort)
  }
  def randomize(): Unit = {
    simRandom.nextBytes(destination)
    simRandom.nextBytes(source)
    ethType = simRandom.nextInt(1 << 16)
  }
}
class Ip4{
  var IHL, DSCP, ECN, totalLength = 0
  var identification, flags, fragmentOffset = 0
  var ttl, protocol, headerChecksum = 0
  var sourceAddress, destinationAddress = new Array[Byte](4)
  var options = new Array[Int](0)

  def headerSize = IHL*4

  def to(bb : ByteBuffer): Unit = {
    bb.put(0x40 | IHL toByte)
    bb.put((DSCP << 2) | (ECN) toByte)
    bb.putShort(totalLength toShort)
    bb.putShort(identification toShort)
    bb.putShort((flags << 13) | fragmentOffset toShort)
    bb.put(ttl.toByte); bb.put(protocol.toByte); bb.putShort(headerChecksum toShort)
    bb.put(sourceAddress)
    bb.put(destinationAddress)
    options.foreach(o => bb.putInt(o))
  }

  def toArray() = {
    val array = new Array[Byte](IHL*4)
    val buffer = ByteBuffer.wrap(array);
    to(buffer)
    array
  }

  def randomize(): Unit = {
    val ip4Options = simRandom.nextInt(16-5)
    IHL = 5+ip4Options
    DSCP = simRandom.nextInt(1 << 6)
    ECN = simRandom.nextInt(1 << 2)
    totalLength = simRandom.nextInt(1 << 16)
    identification = simRandom.nextInt(1 << 16)
    flags = simRandom.nextInt(1 << 3)
    fragmentOffset = simRandom.nextInt(1 << 13)
    ttl = simRandom.nextInt(1 << 8)
    protocol = simRandom.nextInt(1 << 8)
    headerChecksum = simRandom.nextInt(1 << 16)
    simRandom.nextBytes(sourceAddress)
    simRandom.nextBytes(destinationAddress)
    options = Array.fill(ip4Options)(simRandom.nextInt)
  }

  def updateChecksum(): Unit = {
    headerChecksum = 0
    val cs = new Checksummer
    cs.push(toArray())
    headerChecksum = cs.result()
  }
}
class Tcp{
  var sourcePort, destinationPort = 0
  var sequenceNumber, acknowledgementNumber = 0
  var dataOffset = 0
  var FIN, SYN, RST, PSH, ACK, URG, ECE, CWR = false
  var window = 0
  var checksum = 0
  var urgentPointer = 0
  var options = new Array[Int](0)

  def headerSize = dataOffset*4

  def to(bb : ByteBuffer): Unit = {
    bb.putShort(sourcePort toShort)
    bb.putShort(destinationPort toShort)
    bb.putInt(sequenceNumber)
    bb.putInt(acknowledgementNumber)
    val flags = (FIN.toInt << 0) | (SYN .toInt << 1) | (RST .toInt << 2) | (PSH .toInt << 3) | (ACK .toInt << 4) | (URG .toInt << 5) | (ECE .toInt << 6) | (CWR.toInt << 7)
    bb.putShort((dataOffset << 12) | flags toShort)
    bb.putShort(window toShort)
    bb.putShort(checksum toShort)
    bb.putShort(urgentPointer toShort)
    options.foreach(o => bb.putInt(o))
  }

  def toArray() = {
    val array = new Array[Byte](dataOffset*4)
    val buffer = ByteBuffer.wrap(array);
    to(buffer)
    array
  }

  def randomize() = {
    val tcpOptions = simRandom.nextInt(16-5)
    sourcePort = simRandom.nextInt & 0xFFFF
    destinationPort = simRandom.nextInt & 0xFFFF
    sequenceNumber = simRandom.nextInt
    acknowledgementNumber = simRandom.nextInt
    dataOffset = 5+tcpOptions
    FIN = simRandom.nextBoolean()
    SYN = simRandom.nextBoolean()
    RST = simRandom.nextBoolean()
    PSH = simRandom.nextBoolean()
    ACK = simRandom.nextBoolean()
    URG = simRandom.nextBoolean()
    ECE = simRandom.nextBoolean()
    CWR = simRandom.nextBoolean()
    window = simRandom.nextInt(1 << 16)
    checksum = simRandom.nextInt(1 << 16)
    urgentPointer = simRandom.nextInt(1 << 16)
    options = Array.fill(tcpOptions)(simRandom.nextInt)
  }

  def updateChecksum(ip4 : Ip4, data : Array[Byte]): Unit = {
    checksum = 0
    val cs = new Checksummer
    cs.push(ip4.sourceAddress)
    cs.push(ip4.destinationAddress)
    cs.pushShort(ip4.protocol)
    cs.pushShort(ip4.totalLength - ip4.IHL*4)
    cs.push(this.toArray())
    cs.push(data)
    checksum = cs.result()
  }
}

class Udp{
  var sourcePort, destinationPort = 0
  var length, checksum = 0

  def headerSize = 8

  def to(bb : ByteBuffer): Unit = {
    bb.putShort(sourcePort toShort)
    bb.putShort(destinationPort toShort)
    bb.putShort(length toShort)
    bb.putShort(checksum toShort)
  }

  def toArray() = {
    val array = new Array[Byte](8)
    val buffer = ByteBuffer.wrap(array);
    to(buffer)
    array
  }

  def randomize() = {
    val tcpOptions = simRandom.nextInt(16-5)
    sourcePort = simRandom.nextInt(1 << 16)
    destinationPort = simRandom.nextInt(1 << 16)
    length = simRandom.nextInt(1 << 16)
    checksum = simRandom.nextInt(1 << 16)
  }

  def updateChecksum(ip4 : Ip4, data : Array[Byte]): Unit = {
    checksum = 0
    val cs = new Checksummer
    cs.push(ip4.sourceAddress)
    cs.push(ip4.destinationAddress)
    cs.pushShort(ip4.protocol)
    cs.pushShort(ip4.totalLength - ip4.IHL*4)
    cs.push(this.toArray())
    cs.push(data)
    checksum = cs.result()
  }
}


class Icmp{
  var typ, code, checksum = 0
  var rest = 0

  def headerSize = 8

  def to(bb : ByteBuffer): Unit = {
    bb.put(typ toByte)
    bb.put(code toByte)
    bb.putShort(checksum toShort)
    bb.putInt(rest)
  }

  def toArray() = {
    val array = new Array[Byte](8)
    val buffer = ByteBuffer.wrap(array);
    to(buffer)
    array
  }

  def randomize() = {
    val tcpOptions = simRandom.nextInt(16-5)
    typ = simRandom.nextInt(1 << 8)
    code = simRandom.nextInt(1 << 8)
    checksum = simRandom.nextInt(1 << 16)
    rest = simRandom.nextInt()
  }

  def updateChecksum(data : Array[Byte]): Unit = {
    checksum = 0
    val cs = new Checksummer
    cs.push(this.toArray())
    cs.push(data)
    checksum = cs.result()
  }
}

class Checksummer{
  var accumulator = 0
  def pushShort(that : Int): Unit = {
    accumulator += that
    accumulator += accumulator >> 16
    accumulator &= 0xFFFF
  }
  def push(that : Array[Byte]): Unit = {
    val range = 0 until (that.length & -2) by 2
    for(i <- 0 until (that.length & -2) by 2){
      pushShort(((that(i).toInt & 0xFF) << 8) | (that(i+1).toInt & 0xFF))
    }
    if((that.length & 1) != 0) pushShort((that.last.toInt & 0xFF) << 8)
  }
  def result() = (~accumulator) & 0xFFFF
  def clear() = accumulator = 0
}

object SpinalSimMacTester{
  def calcCrc32(that : Seq[Int]): Int = calcCrc32Byte(that.map(_.toByte))

  def calcCrc32Byte(that : Seq[Byte]): Int ={
    def getBit(id : Int) = (that(id/8) >> ((id % 8))) & 1
    var crc = -1
    for(bitId <- 0 until that.size*8){
      val bit = getBit(bitId) ^ ((crc >> 31) & 1)
      crc = (crc << 1) ^ ((if(bit == 1) 0x04C11DB7 else 0))
    }
    val crcReversed = (0 until 32).map(i => ((crc >> i) & 1) << (31-i)).reduce(_ | _)
    ~crcReversed
  }

  def dataToFrameByte(data : Seq[Byte])= {
    val padded = data.map(_.toInt) ++ List.fill(Math.max(60 - data.size, 0))(0)
    val crc = SpinalSimMacTester.calcCrc32(padded)
    ((List.fill(7)(0x55) :+ 0xD5) ++ padded ++ List.tabulate(4)(i => ((crc >> i * 8) & 0xFF))).map(_.toByte)
  }
}

class SpinalSimMacTester extends SpinalAnyFunSuite{
  def hexStringToFrame(str : String) = {
    val spaceLess = str.replace(" ","")
    Seq.tabulate[Int](spaceLess.size/2)(i => Integer.parseInt(spaceLess.substring(i*2, i*2+2), 16))
  }
  val frameCorrectA = hexStringToFrame("33330000 0002000A CD2C1594 86DD600B DD410008 3AFFFE80 00000000 0000FC3B 9A3CE0E2 3955FF02 00000000 00000000 00000000 00028500 CC860000 00005901 A328")
  val frameCorrectB = hexStringToFrame("33330000 00FB000A CD2C1594 86DD600C 36DF0091 11FFFE80 00000000 0000FC3B 9A3CE0E2 3955FF02 00000000 00000000 00000000 00FB14E9 14E90091 C6390000 84000000 00020000 00000135 01350139 01330132 01650130 01650163 01330161 01390162 01330163 01660130 01300130 01300130 01300130 01300130 01300130 01300130 01380165 01660369 70360461 72706100 000C8001 00000078 000D0572 61777272 056C6F63 616C00C0 60001C80 01000000 780010FE 80000000 000000FC 3B9A3CE0 E239550D 5BA667")

  test("Crc32"){
//    println(frameCorrectA.map(v => f"0x$v%02X").mkString(","))
    SimConfig.compile(Crc(kind = CrcKind.Crc32, dataWidth = 4)).doSim(seed = Random.nextInt) { dut =>
      dut.clockDomain.forkStimulus(10)


      dut.io.input.valid #= false
      dut.io.flush #= false

      def feed(that : Seq[Int], expected : BigInt) = {

        for(i <- 0 until that.size; shift <- List(0, 4)) {
          dut.io.input.valid #= true
          dut.io.input.payload #= (that(i) >> shift) & 0xF
          dut.clockDomain.waitSampling()
        }
        dut.io.input.valid #= false
        dut.clockDomain.waitSampling()
        assert(dut.io.result.toBigInt === expected)
        dut.io.flush #= true
        dut.clockDomain.waitSampling()
        assert(dut.io.result.toBigInt === expected)
        dut.io.flush #= false
      }

      dut.clockDomain.waitSampling(10)
      feed(frameCorrectA, 0x2144DF1C)
      feed(frameCorrectA, 0x2144DF1C)
      feed(frameCorrectA, 0x2144DF1C)
      feed(frameCorrectA, 0x2144DF1C)
    }
  }

  test("MacRxPreamble") {
    val frameA = List(0x00, 0x55, 0x55, 0x55, 0xD5, 0x00, 0x21, 0x43, 0x65, 0x87, 0xA9)
    val frameARef = List(0x0, 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA)
    SimConfig.compile(MacRxPreamble(dataWidth = 4)).doSim(seed = Random.nextInt()) { dut => //-295653402
      dut.clockDomain.forkStimulus(10)

      StreamReadyRandomizer(dut.io.output, dut.clockDomain)

      val refs = mutable.Queue[Int]()

      def drive(frame : Seq[Int], ref : Seq[Int], shift : Int): Unit ={
        def getBit(id : Int) = (frame(id/8) >> ((id % 8))) & 1

        refs ++= ref
        val transferCount = (frame.size*8 - shift)/dut.dataWidth
        dut.io.input.valid #= true
        for(transferId <- 0 until transferCount){
          var data = 0
          for(i <- 0 until dut.dataWidth){
            data |= getBit(shift + transferId*dut.dataWidth + i) << i
          }
          dut.io.input.data #= data
          dut.io.input.error #= false
          dut.io.input.last #= transferId == transferCount-1
          dut.clockDomain.waitSamplingWhere(dut.io.input.ready.toBoolean)
        }
        dut.io.input.valid #= false
        dut.clockDomain.waitSampling(10)
      }

      StreamMonitor(dut.io.output, dut.clockDomain){ p =>
        assert(p.data.toInt == refs.dequeue())
      }

      dut.io.input.valid #= false
      dut.clockDomain.waitSampling(10)
      drive(frameA, frameARef , 0)
      drive(frameA, frameARef , 0)
      drive(frameA, frameARef,  0)
      drive(frameA, frameARef,  0)
      drive(frameA, frameARef,  0)
      assert(refs.isEmpty)
    }
  }


  test("MacRxChecker") {
    val frame = Seq(0x00, 0x21, 0x43, 0x65, 0x87, 0x00)


    val frameOk = hexStringToFrame("33330000 0002000A CD2C1594 86DD600B DD410008 3AFFFE80 00000000 0000FC3B 9A3CE0E2 3955FF02 00000000 00000000 00000000 00028500 CC860000 00005901 A328")
    SimConfig.compile(MacRxChecker(dataWidth = 4)).doSim(seed = Random.nextInt) { dut =>
      dut.clockDomain.forkStimulus(10)

      def drive(frame : Seq[Int]): Unit ={
        def getBit(id : Int) = (frame(id/8) >> ((7-id % 8))) & 1

//        var crc = -1
//        for(bitId <- 0 until frame.size*8){
//          val bit = getBit(bitId) ^ ((crc >> 31) & 1)
//          crc = (crc << 1) ^ ((if(bit == 1) 0x04C11DB7 else 0))
//        }
//        val crcReversed = (0 until 32).map(i => ((crc >> i) & 1) << (31-i)).reduce(_ | _)

        val transferCount = (frame.size*8)/dut.dataWidth
        dut.io.input.valid #= true
        for(transferId <- 0 until transferCount){
          var data = 0
          for(i <- 0 until dut.dataWidth){
            data |= getBit(transferId*dut.dataWidth + i) << i
          }
          dut.io.input.data #= data
          dut.io.input.error #= false
          dut.io.input.last #= transferId == transferCount-1
          dut.clockDomain.waitSampling()
        }
        dut.io.input.valid #= false
        dut.clockDomain.waitSampling(10)
      }

      dut.io.input.valid #= false
      dut.clockDomain.waitSampling(10)
//      drive(List(0x47))
      drive(frameOk)
      drive(frame)
      drive(frame)
    }
  }


  def testMacEth(aligned : Boolean, tid : Int = 1): Unit = {
    test(s"MacMii_aligned=${aligned}_$tid") {
      val header = Seq(0x55, 0x55, 0xD5)
      SimConfig.compile(MacEth(
        p = MacEthParameter(
          phy = PhyParameter(
            txDataWidth = 4,
            rxDataWidth = 4
          ),
          rxDataWidth = 32,
          rxBufferByteSize = 512,
          txDataWidth = 32,
          txBufferByteSize = 512
        ),
        txCd = ClockDomain.external("txCd", withReset = false),
        rxCd = ClockDomain.external("rxCd", withReset = false)
      )).doSim(seed=tid){ dut =>
        dut.clockDomain.forkStimulus(40)
//        dut.rxCd.forkStimulus(40)
        dut.txCd.forkStimulus(40)
        SimTimeout(1000000*40)

        {
          val clk = dut.rxCd.clockSim
          var value = clk.toBoolean
          var period = 40
          var counter = 0
          def t : Unit = {
            value = !value
            clk  #= value
            delayed(period >> 1)(t)
            counter += 1
            if(counter == 10000){
              counter = 0
              period = 2 + Random.nextInt(20)
            }
          }
          t
        }

        dut.io.ctrl.tx.alignerEnable #= aligned
        dut.io.ctrl.rx.alignerEnable #= aligned

        val refsUncomited = mutable.Queue[Seq[Int]]()
        val refs = mutable.Queue[Seq[Int]]()
        var started = false

        val phyRxQueue = mutable.Queue[() => Unit]()
        StreamDriver(dut.io.phy.rx, dut.rxCd) { p =>
          if (phyRxQueue.nonEmpty) {
            phyRxQueue.dequeue().apply()
            true
          } else {
            false
          }
        }

        def drive(frame: Seq[Int]): Unit = {
          started = true
          refsUncomited += ((if(aligned) List(-1,-1) else List[Int]()) ++ frame)

          val realFrame = header ++ frame

          def getBit(id: Int) = (realFrame(id / 8) >> ((id % 8))) & 1

          val transferCount = (realFrame.size * 8) / dut.p.phy.rxDataWidth

          for (transferId <- 0 until transferCount) {
            var data = 0
            for (i <- 0 until dut.p.phy.rxDataWidth) {
              data |= getBit(transferId * dut.p.phy.rxDataWidth + i) << i
            }
            phyRxQueue += { () =>
              dut.io.phy.rx.data #= data
              dut.io.phy.rx.error #= false
              dut.io.phy.rx.last #= transferId == transferCount - 1
            }
          }
        }


        StreamReadyRandomizer(dut.io.ctrl.rx.stream, dut.clockDomain).factor = 0.1f
        val pushQueue = mutable.Queue[Long]()
        val popQueue = mutable.Queue[Int]()
        StreamDriver(dut.io.ctrl.tx.stream, dut.clockDomain) { p =>
          if (pushQueue.nonEmpty) {
            p #= pushQueue.dequeue()
            true
          } else {
            false
          }
        }


        def doTx(that: Seq[Int]): Unit = {
          val headed = (if(aligned) List(0,0) else List[Int]()) ++ that
          val trueThat = headed ++ List.fill(3 - (headed.size - 1) % 4)(0)
          pushQueue += that.size * 8 + (if(aligned) 16 else 0)
          for (i <- 0 until trueThat.size / 4) {
            var build = 0l
            for (byteId <- 0 until 4) {
              build |= trueThat(i * 4 + byteId).toLong << 8 * byteId
            }
            pushQueue += build
          }

          val padded = that ++ List.fill(Math.max(60 - that.size, 0))(0)
          //        println(padded.map(v => f"$v%02X").mkString(""))
          val crc = SpinalSimMacTester.calcCrc32(padded)
          for (byte <- (List.fill(7)(0x55) :+ 0xD5) ++ padded ++ List.tabulate(4)(i => (crc >> i * 8) & 0xFF)) {
            popQueue += byte & 0xF
            popQueue += (byte >> 4) & 0xF
          }
        }


        var task: Seq[Int] = Nil


        dut.io.ctrl.tx.flush #= true
        dut.io.ctrl.rx.flush #= true
        dut.io.ctrl.rx.stream.ready #= false
        dut.rxCd.waitSampling(100)
        dut.io.ctrl.tx.flush #= false
        dut.io.ctrl.rx.flush #= false
        dut.rxCd.waitSampling(100)

        dut.rxCd.onSamplings{
          if(dut.io.sim.commit.toBoolean){
            val r = refsUncomited.dequeue()
            if(dut.io.sim.drop.toBoolean) {
//              println("Drop " + r.size * 8 + " at " + simTime())
            }else if(dut.io.sim.error.toBoolean) {
//              println("CrcE " + r.size * 8 + " at " + simTime())
            } else {
              refs += r
//              println("Keep " + r.size*8 + " at " + simTime())
            }
          }
        }
        StreamMonitor(dut.io.ctrl.rx.stream, dut.clockDomain) { p =>
          if (task.isEmpty) {
            task = refs.dequeue()
            assert(p.toLong == task.length * 8)
//            println(task.map(e => f"$e%02x").mkString(" "))
          } else {
            val data = p.toLong
            for (i <- 0 until Math.min(task.size, 4)) {
              assert(task.head == -1 | ((data >> i * 8) & 0xFF) == task.head)
              task = task.tail
            }
          }
        }

        StreamReadyRandomizer(dut.io.phy.tx, dut.txCd)
        StreamMonitor(dut.io.phy.tx, dut.txCd) { p =>
          assert(popQueue.dequeue() === dut.io.phy.tx.data.toInt)
        }

        dut.rxCd.waitSampling(100)

        val txThread = fork {
          doTx(0 to 15)
          doTx(0 to 0)
          doTx(0 to 1)
          doTx(0 to 2)
          doTx(0 to 3)
          for (i <- 0 until 500) {
            doTx(List.fill(Random.nextInt(128 + 1) + 1)(Random.nextInt(256)))
            if (Random.nextFloat() < 0.1) dut.clockDomain.waitSamplingWhere(popQueue.isEmpty)
          }
        }

        drive(frameCorrectA)
        drive(frameCorrectB)
        drive(frameCorrectA)
        drive(frameCorrectB)
        for (i <- 0 until 1000) {
          val frame = Seq.fill(Random.nextInt(256 + 1) + 1)(Random.nextInt(256))
          val crc = SpinalSimMacTester.calcCrc32(frame) ^ (if(Random.nextFloat() < 0.8) 0x0 else Random.nextInt())
          drive(frame ++ List.tabulate(4)(i => (crc >> i * 8) & 0xFF))
        }
        waitUntil(phyRxQueue.isEmpty)
        dut.rxCd.waitSampling(20000)
        onSimEnd{
          println(task.size + " " + refs.size)
        }
        assert(task.isEmpty && refs.isEmpty)
        txThread.join()
      }
    }
  }

  for(i <- 0 until 1) {
      testMacEth(true,i)
      testMacEth(false,i)
  }

  test("MacTxBuffer") {
    SimConfig.compile(MacTxBuffer(
      pushCd    = ClockDomain.external("pushCd"),
      popCd     = ClockDomain.external("popCd"),
      pushWidth = 32,
      popWidth  = 8,
      byteSize  = 16*4
    )).doSim(seed = Random.nextInt) { dut =>
      dut.pushCd.forkStimulus(10)
      dut.popCd.forkStimulus(40)

      val pushQueue = mutable.Queue[Long]()
      val popQueue = mutable.Queue[Int]()
      StreamDriver(dut.io.push.stream, dut.pushCd){p =>
        if(pushQueue.nonEmpty){
          p #= pushQueue.dequeue()
          true
        }else{
          false
        }
      }

      StreamReadyRandomizer(dut.io.pop.stream, dut.popCd)
      StreamMonitor(dut.io.pop.stream, dut.popCd){ p =>
        assert(popQueue.dequeue() == p.data.toInt)
      }

      def push(that : Seq[Int]): Unit ={
        val trueThat = that ++ List.fill(3-(that.size-1) % 4)(0)
        pushQueue += that.size*8
        for(i <- 0 until trueThat.size/4){
          var build = 0l
          for(byteId <- 0 until 4){
            build |= trueThat(i*4+byteId).toLong << 8*byteId
          }
          pushQueue += build
        }
        popQueue ++= that
      }


      dut.io.pop.commit #= false
      dut.io.pop.redo #= false
      dut.popCd.waitSampling(10)
      dut.pushCd.waitSampling(10)
      dut.popCd.waitSampling(10)
      dut.pushCd.waitSampling(10)

      fork{
        while(true) {
          dut.popCd.waitSamplingWhere(dut.io.pop.stream.valid.toBoolean)
          dut.popCd.waitSamplingWhere(!dut.io.pop.stream.valid.toBoolean)
          dut.popCd.waitSampling(Random.nextInt(5))
          dut.io.pop.commit #= true
          dut.popCd.waitSampling()
          dut.io.pop.commit #= false
        }
      }

      def sync(): Unit ={
        waitUntil(popQueue.isEmpty); dut.pushCd.waitSampling(100); assert(dut.io.push.availability.toInt == 1 << widthOf(dut.io.push.availability)-1)
      }
      push(0 to 15)
      sync()

      push(0 to 7)
      sync()

      push(0 to 7)
      push(0 to 7)
      push(0 to 7)
      sync()

      push(0 to 0)
      push(0 to 1)
      push(0 to 2)
      push(0 to 3)
      sync()

      for(i <- 0 until 100){
        push(Seq.fill(Random.nextInt(dut.byteSize-4) + 1)(Random.nextInt(256)))
        sync()
      }

      for(i <- 0 until 1000){
        push(Seq.fill(Random.nextInt(dut.byteSize-4) + 1)(Random.nextInt(256)))
        if(Random.nextFloat() < 0.1) sync()
      }

      sync()
      dut.pushCd.waitSampling(10)

    }
  }


  test("MacTxLso") {
    val compiled = SimConfig.withFstWave.compile(new MacTxLso(bufferBytes = 2048, mtuMax = 200))
    compiled.doSim(seed = 52){dut =>
      dut.clockDomain.forkStimulus(10)
      val (inputDriver, inputQueue) = StreamDriver.queue(dut.io.input, dut.clockDomain)
      val outputReady = StreamReadyRandomizer(dut.io.output, dut.clockDomain)

      val ref = mutable.Queue[(Byte, Boolean)]()
      val outputMonitor = StreamMonitor(dut.io.output, dut.clockDomain){ p =>
        val head = ref.dequeue()
        assert(p.data.toInt.toByte == head._1)
        assert(p.last.toBoolean == head._2)
      }

      for(i <- 0 until 10){
        val segmentArray = NetworkRef.icmpRef
        val inputArray = segmentArray.clone()
//        inputArray(0x24) = 0x54
//        inputArray(0x25) = 0x66
        for(i <- inputArray.indices){
          val last = i == inputArray.size-1
          inputQueue.enqueue{ p =>
            p.data #= inputArray(i).toInt & 0xFF
            p.last #= last
          }
        }

        for(i <- segmentArray.indices){
          val last = i == segmentArray.length-1
          ref.enqueue(segmentArray(i) -> last)
        }

        dut.clockDomain.waitSamplingWhere(ref.isEmpty)
      }

      for(i <- 0 until 10){
        val ip4Options = simRandom.nextInt(16-5)
        val headerSize = 14+(5+ip4Options)*4+8
        val dataSize = 1358 //simRandom.nextInt(60)

        val eth = new Eth()
        simRandom.nextBytes(eth.destination)
        simRandom.nextBytes(eth.source)
        eth.ethType = 0x800
        val ip4 = new Ip4
        ip4.IHL = 5+ip4Options
        ip4.DSCP = simRandom.nextInt(1 << 6)
        ip4.ECN = simRandom.nextInt(1 << 2)
        ip4.totalLength = headerSize + dataSize - 14
        ip4.identification = simRandom.nextInt(1 << 16)
        ip4.flags = simRandom.nextInt(1 << 3)
        ip4.fragmentOffset = simRandom.nextInt(1 << 13)
        ip4.ttl = simRandom.nextInt(1 << 8)
        ip4.protocol = 0x01
        ip4.headerChecksum = simRandom.nextInt(1 << 16)
        simRandom.nextBytes(ip4.sourceAddress)
        simRandom.nextBytes(ip4.destinationAddress)
        ip4.options = Array.fill(ip4Options)(simRandom.nextInt)

        val icmp = new Icmp
        icmp.randomize()

        val inputData = new Array[Byte](dataSize)
        simRandom.nextBytes(inputData)

        val inputArray = new Array[Byte](headerSize+dataSize)
        val inputBuffer = ByteBuffer.wrap(inputArray);
        eth.to(inputBuffer)
        ip4.to(inputBuffer)
        icmp.to(inputBuffer)
        inputBuffer.put(inputData)

        for(i <- inputArray.indices){
          val last = i == inputArray.size-1
          inputQueue.enqueue{ p =>
            p.data #= inputArray(i).toInt & 0xFF
            p.last #= last
          }
        }


        val offset = 0
        val segmentDataSize = dataSize
        val segmentArray = new Array[Byte](headerSize + segmentDataSize)
        val segmentBuffer = ByteBuffer.wrap(segmentArray);
        val segmentData = inputData.slice(offset, offset + segmentDataSize)

        icmp.checksum = 0
        ip4.headerChecksum = 0
        ip4.totalLength = segmentArray.length-14

        val cs = new Checksummer()
        cs.push(ip4.toArray())
        ip4.headerChecksum = cs.result()

        cs.clear()
        cs.push(icmp.toArray())
        cs.push(segmentData)
        icmp.checksum = cs.result()

        eth.to(segmentBuffer)
        ip4.to(segmentBuffer)
        icmp.to(segmentBuffer)
        segmentBuffer.put(segmentData)

        for(i <- segmentArray.indices){
          val last = i == segmentArray.length-1
          ref.enqueue(segmentArray(i) -> last)
        }


        dut.clockDomain.waitSamplingWhere(ref.isEmpty)
      }

      for(i <- 0 until 10){
        val ip4Options = simRandom.nextInt(16-5)
        val tcpOptions = simRandom.nextInt(16-5)
        val headerSize = 14+(5+ip4Options)*4+(5+tcpOptions)*4
        val dataSize = 1358 +simRandom.nextInt(60)

        val eth = new Eth()
        simRandom.nextBytes(eth.destination)
        simRandom.nextBytes(eth.source)
        eth.ethType = 0x800
        val ip4 = new Ip4
        ip4.IHL = 5+ip4Options
        ip4.DSCP = simRandom.nextInt(1 << 6)
        ip4.ECN = simRandom.nextInt(1 << 2)
        ip4.totalLength = headerSize + dataSize - 14
        ip4.identification = simRandom.nextInt(1 << 16)
        ip4.flags = simRandom.nextInt(1 << 3)
        ip4.fragmentOffset = simRandom.nextInt(1 << 13)
        ip4.ttl = simRandom.nextInt(1 << 8)
        ip4.protocol = 0x06
        ip4.headerChecksum = simRandom.nextInt(1 << 16)
        simRandom.nextBytes(ip4.sourceAddress)
        simRandom.nextBytes(ip4.destinationAddress)
        ip4.options = Array.fill(ip4Options)(simRandom.nextInt)

        val tcp = new Tcp
        tcp.sourcePort = simRandom.nextInt & 0xFFFF
        tcp.destinationPort = simRandom.nextInt & 0xFFFF
        tcp.sequenceNumber = simRandom.nextInt
        tcp.acknowledgementNumber = simRandom.nextInt
        tcp.dataOffset = 5+tcpOptions
        tcp.FIN = simRandom.nextBoolean()
        tcp.SYN = simRandom.nextBoolean()
        tcp.RST = simRandom.nextBoolean()
        tcp.PSH = simRandom.nextBoolean()
        tcp.ACK = simRandom.nextBoolean()
        tcp.URG = simRandom.nextBoolean()
        tcp.ECE = simRandom.nextBoolean()
        tcp.CWR = simRandom.nextBoolean()
        tcp.window = simRandom.nextInt(1 << 16)
        tcp.checksum = simRandom.nextInt(1 << 16)
        tcp.urgentPointer = simRandom.nextInt(1 << 16)
        tcp.options = Array.fill(tcpOptions)(simRandom.nextInt)

        val inputData = new Array[Byte](dataSize)
        simRandom.nextBytes(inputData)

        val inputArray = new Array[Byte](headerSize+dataSize)
        val inputBuffer = ByteBuffer.wrap(inputArray);
        eth.to(inputBuffer)
        ip4.to(inputBuffer)
        tcp.to(inputBuffer)
        inputBuffer.put(inputData)

        for(i <- inputArray.indices){
          val last = i == inputArray.size-1
          inputQueue.enqueue{ p =>
            p.data #= inputArray(i).toInt & 0xFF
            p.last #= last
          }
        }

        val FIN = tcp.FIN
        val SYN = tcp.SYN
        val RST = tcp.RST
        val PSH = tcp.PSH
        val ACK = tcp.ACK
        val URG = tcp.URG
        val ECE = tcp.ECE
        val CWR = tcp.CWR
        var left = dataSize
        var offset = 0
        do{
          val segmentDataSize = left min (dut.mtuMax - headerSize)
          val segmentArray = new Array[Byte](headerSize + segmentDataSize)
          val segmentBuffer = ByteBuffer.wrap(segmentArray);
          val segmentData = inputData.slice(offset, offset + segmentDataSize)

          tcp.checksum = 0
          ip4.headerChecksum = 0
          ip4.totalLength = segmentArray.length-14

          val isFirst = offset == 0
          val isLast = left == segmentDataSize
          tcp.FIN = FIN & isLast
          tcp.SYN = SYN & isFirst
          tcp.RST = RST & isFirst
          tcp.PSH = PSH & isLast
          tcp.ACK = ACK
          tcp.URG = URG & false
          tcp.ECE = ECE & isFirst
          tcp.CWR = CWR & isFirst

          val cs = new Checksummer()
          cs.push(ip4.toArray())
          ip4.headerChecksum = cs.result()

          cs.clear()
          cs.push(ip4.sourceAddress)
          cs.push(ip4.destinationAddress)
          cs.pushShort(ip4.protocol)
          cs.pushShort(ip4.totalLength - ip4.IHL*4)
          cs.push(tcp.toArray())
          cs.push(segmentData)
          tcp.checksum = cs.result()

          eth.to(segmentBuffer)
          ip4.to(segmentBuffer)
          tcp.to(segmentBuffer)
          segmentBuffer.put(segmentData)

          for(i <- segmentArray.indices){
            val last = i == segmentArray.length-1
            ref.enqueue(segmentArray(i) -> last)
          }

          tcp.sequenceNumber += segmentDataSize
          left -= segmentDataSize
          offset += segmentDataSize
        } while(left != 0)


        dut.clockDomain.waitSamplingWhere(ref.isEmpty)


      }


      for(i <- 0 until 10){
        val ip4Options = simRandom.nextInt(16-5)
        val headerSize = 14+(5+ip4Options)*4+8
        val dataSize = 1358 //simRandom.nextInt(60)

        val eth = new Eth()
        simRandom.nextBytes(eth.destination)
        simRandom.nextBytes(eth.source)
        eth.ethType = 0x800
        val ip4 = new Ip4
        ip4.IHL = 5+ip4Options
        ip4.DSCP = simRandom.nextInt(1 << 6)
        ip4.ECN = simRandom.nextInt(1 << 2)
        ip4.totalLength = headerSize + dataSize - 14
        ip4.identification = simRandom.nextInt(1 << 16)
        ip4.flags = simRandom.nextInt(1 << 3)
        ip4.fragmentOffset = simRandom.nextInt(1 << 13)
        ip4.ttl = simRandom.nextInt(1 << 8)
        ip4.protocol = 0x11
        ip4.headerChecksum = simRandom.nextInt(1 << 16)
        simRandom.nextBytes(ip4.sourceAddress)
        simRandom.nextBytes(ip4.destinationAddress)
        ip4.options = Array.fill(ip4Options)(simRandom.nextInt)

        val udp = new Udp
        udp.randomize()
        udp.length = dataSize

        val inputData = new Array[Byte](dataSize)
        simRandom.nextBytes(inputData)

        val inputArray = new Array[Byte](headerSize+dataSize)
        val inputBuffer = ByteBuffer.wrap(inputArray);
        eth.to(inputBuffer)
        ip4.to(inputBuffer)
        udp.to(inputBuffer)
        inputBuffer.put(inputData)

        for(i <- inputArray.indices){
          val last = i == inputArray.size-1
          inputQueue.enqueue{ p =>
            p.data #= inputArray(i).toInt & 0xFF
            p.last #= last
          }
        }


        val offset = 0
        val segmentDataSize = dataSize
        val segmentArray = new Array[Byte](headerSize + segmentDataSize)
        val segmentBuffer = ByteBuffer.wrap(segmentArray);
        val segmentData = inputData.slice(offset, offset + segmentDataSize)

        udp.checksum = 0
        ip4.headerChecksum = 0
        ip4.totalLength = segmentArray.length-14

        val cs = new Checksummer()
        cs.push(ip4.toArray())
        ip4.headerChecksum = cs.result()

        cs.clear()
        cs.push(ip4.sourceAddress)
        cs.push(ip4.destinationAddress)
        cs.pushShort(ip4.protocol)
        cs.pushShort(ip4.totalLength - ip4.IHL*4)
        cs.push(udp.toArray())
        cs.push(segmentData)
        udp.checksum = cs.result()

        eth.to(segmentBuffer)
        ip4.to(segmentBuffer)
        udp.to(segmentBuffer)
        segmentBuffer.put(segmentData)

        for(i <- segmentArray.indices){
          val last = i == segmentArray.length-1
          ref.enqueue(segmentArray(i) -> last)
        }


        dut.clockDomain.waitSamplingWhere(ref.isEmpty)
      }

      for(i <- 0 until 10){
        val ip4Options = simRandom.nextInt(16-5)
        val tcpOptions = simRandom.nextInt(16-5)
        val headerSize = 14+(5+ip4Options)*4+(5+tcpOptions)*4
        val dataSize = 612 + simRandom.nextInt(32)

        val eth = new Eth()
        simRandom.nextBytes(eth.destination)
        simRandom.nextBytes(eth.source)
        eth.ethType = 0x800
        val ip4 = new Ip4
        ip4.IHL = 5+ip4Options
        ip4.DSCP = simRandom.nextInt(1 << 6)
        ip4.ECN = simRandom.nextInt(1 << 2)
        ip4.totalLength = headerSize + dataSize - 14
        ip4.identification = simRandom.nextInt(1 << 16)
        ip4.flags = simRandom.nextInt(1 << 3)
        ip4.fragmentOffset = simRandom.nextInt(1 << 13)
        ip4.ttl = simRandom.nextInt(1 << 8)
        ip4.protocol = 0x42
        ip4.headerChecksum = simRandom.nextInt(1 << 16)
        simRandom.nextBytes(ip4.sourceAddress)
        simRandom.nextBytes(ip4.destinationAddress)
        ip4.options = Array.fill(ip4Options)(simRandom.nextInt)

        val inputData = new Array[Byte](dataSize)
        simRandom.nextBytes(inputData)

        val inputArray = new Array[Byte](headerSize+dataSize)
        val inputBuffer = ByteBuffer.wrap(inputArray);
        eth.to(inputBuffer)
        ip4.to(inputBuffer)
        inputBuffer.put(inputData)

        for(i <- inputArray.indices){
          val last = i == inputArray.size-1
          inputQueue.enqueue{ p =>
            p.data #= inputArray(i).toInt & 0xFF
            p.last #= last
          }
        }

        {
          val segmentDataSize = dataSize
          val segmentArray = new Array[Byte](headerSize + segmentDataSize)
          val segmentBuffer = ByteBuffer.wrap(segmentArray);
          val segmentData = inputData

          ip4.headerChecksum = 0
          ip4.totalLength = segmentArray.length-14

          val cs = new Checksummer()
          cs.push(ip4.toArray())
          ip4.headerChecksum = cs.result()

          cs.clear()
          cs.push(ip4.sourceAddress)
          cs.push(ip4.destinationAddress)
          cs.pushShort(ip4.protocol)
          cs.pushShort(ip4.totalLength - ip4.IHL*4)
          cs.push(segmentData)

          eth.to(segmentBuffer)
          ip4.to(segmentBuffer)
          segmentBuffer.put(segmentData)

          for(i <- segmentArray.indices){
            val last = i == segmentArray.length-1
            ref.enqueue(segmentArray(i) -> last)
          }
        }


        dut.clockDomain.waitSamplingWhere(ref.isEmpty)


      }
    }
  }





  test("MacRxChecksumChecker") {
    SimConfig.compile(MacRxCheckSumChecker()).doSim(seed = Random.nextInt) { dut =>
      dut.clockDomain.forkStimulus(10)

      val (inputDriver, inputQueue) = StreamDriver.queue(dut.io.input, dut.clockDomain)
      val outputReady = StreamReadyRandomizer(dut.io.output, dut.clockDomain)

      val ref = mutable.Queue[(Byte, Boolean, Boolean)]()
      val outputMonitor = StreamMonitor(dut.io.output, dut.clockDomain){ p =>
        val head = ref.dequeue()
        assert(p.data.toInt.toByte == head._1)
        assert(p.last.toBoolean == head._2)
        assert(p.error.toBoolean == head._3)
      }

      for(i <- 0 until 100) {
        def serialize(array : Array[Byte], code : Int): Unit = {
          for (i <- array.indices) {
            val last = i == array.size - 1
            inputQueue.enqueue { p =>
              p.data #= array(i).toInt & 0xFF
              p.error #= false
              p.last #= last
            }
            ref.enqueue((array(i).toByte, false, false))
          }
          ref.enqueue((code.toByte, true, false))
        }
        simRandom.nextInt(5) match {
          case 0 => {
            val eth = new Eth()
            val ip4 = new Ip4
            val tcp = new Tcp

            eth.randomize()
            ip4.randomize()
            tcp.randomize()
            val transportData = Array.fill(simRandom.nextInt(200) + 1)(simRandom.nextInt(256) toByte)

            eth.ethType = 0x800
            ip4.protocol = 0x06
            ip4.totalLength = ip4.headerSize + tcp.headerSize + transportData.size
            ip4.updateChecksum()
            tcp.updateChecksum(ip4, transportData)

            val ok = simRandom.nextBoolean()
            if(!ok) simRandom.nextInt(3) match {
              case 0 => ip4.headerChecksum += 1
              case 1 => tcp.checksum += 1
              case 2 => ip4.headerChecksum += 1; tcp.checksum += 1
            }

            val inputArray = new Array[Byte](14 + ip4.totalLength)
            val inputBuffer = ByteBuffer.wrap(inputArray);
            eth.to(inputBuffer)
            ip4.to(inputBuffer)
            tcp.to(inputBuffer)
            inputBuffer.put(transportData)

            serialize(inputArray, ok.toInt)
          }
          case 1 => {
            val eth = new Eth()
            val ip4 = new Ip4()
            val udp = new Udp()

            eth.randomize()
            ip4.randomize()
            udp.randomize()
            val transportData = Array.fill(simRandom.nextInt(200) + 1)(simRandom.nextInt(256) toByte)

            eth.ethType = 0x800
            ip4.protocol = 0x11
            ip4.totalLength = ip4.headerSize + udp.headerSize + transportData.size
            ip4.updateChecksum()
            udp.updateChecksum(ip4, transportData)

            val ok = simRandom.nextBoolean()
            if(!ok) {
              simRandom.nextInt(3) match {
                case 0 => ip4.headerChecksum += 1
                case 1 => udp.checksum += 1
                case 2 => ip4.headerChecksum += 1; udp.checksum += 1
              }
              if(udp.checksum == 0) udp.checksum += 0x1234
            } else {
              if(simRandom.nextBoolean()) udp.checksum = 0
            }

            val inputArray = new Array[Byte](14 + ip4.totalLength)
            val inputBuffer = ByteBuffer.wrap(inputArray);
            eth.to(inputBuffer)
            ip4.to(inputBuffer)
            udp.to(inputBuffer)
            inputBuffer.put(transportData)

            serialize(inputArray, ok.toInt)
          }
          case 2 => {
            val eth = new Eth()
            val ip4 = new Ip4()
            val icmp = new Icmp()

            eth.randomize()
            ip4.randomize()
            icmp.randomize()
            val transportData = Array.fill(simRandom.nextInt(200) + 1)(simRandom.nextInt(256) toByte)

            eth.ethType = 0x800
            ip4.protocol = 0x01
            ip4.totalLength = ip4.headerSize + icmp.headerSize + transportData.size
            ip4.updateChecksum()
            icmp.updateChecksum(transportData)

            val ok = simRandom.nextBoolean()
            if(!ok) {
              simRandom.nextInt(3) match {
                case 0 => ip4.headerChecksum += 1
                case 1 => icmp.checksum += 1
                case 2 => ip4.headerChecksum += 1; icmp.checksum += 1
              }
            }

            val inputArray = new Array[Byte](14 + ip4.totalLength)
            val inputBuffer = ByteBuffer.wrap(inputArray);
            eth.to(inputBuffer)
            ip4.to(inputBuffer)
            icmp.to(inputBuffer)
            inputBuffer.put(transportData)

            serialize(inputArray, ok.toInt)
          }
          case 3 => {
            val eth = new Eth()

            eth.randomize()
            val transportData = Array.fill(simRandom.nextInt(200) + 1)(simRandom.nextInt(256) toByte)

            eth.ethType = 0x3938

            val inputArray = new Array[Byte](14 + transportData.size)
            val inputBuffer = ByteBuffer.wrap(inputArray);
            eth.to(inputBuffer)
            inputBuffer.put(transportData)

            serialize(inputArray, 0)
          }
          case 4 => {
            val eth = new Eth()
            val ip4 = new Ip4()

            eth.randomize()
            ip4.randomize()
            val transportData = Array.fill(simRandom.nextInt(200) + 1)(simRandom.nextInt(256) toByte)

            eth.ethType = 0x800
            ip4.protocol = 0x87
            ip4.totalLength = ip4.headerSize + transportData.size
            ip4.updateChecksum()

            val ok = simRandom.nextBoolean()
            if(!ok) {
              ip4.headerChecksum += 1
            }

            val inputArray = new Array[Byte](14 + ip4.totalLength)
            val inputBuffer = ByteBuffer.wrap(inputArray);
            eth.to(inputBuffer)
            ip4.to(inputBuffer)
            inputBuffer.put(transportData)

            serialize(inputArray, 0)
          }
        }
      }

      dut.clockDomain.waitSamplingWhere(ref.isEmpty)
      dut.clockDomain.waitSampling(100)
    }
  }
}
