package spinal.lib.com.eth

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{FlowMonitor, StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable
import scala.util.Random

class SpinalSimMacTester extends SpinalAnyFunSuite{
  def hexStringToFrame(str : String) = {
    val spaceLess = str.replace(" ","")
    Seq.tabulate[Int](spaceLess.size/2)(i => Integer.parseInt(spaceLess.substring(i*2, i*2+2), 16))
  }
  val frameCorrectA = hexStringToFrame("33330000 0002000A CD2C1594 86DD600B DD410008 3AFFFE80 00000000 0000FC3B 9A3CE0E2 3955FF02 00000000 00000000 00000000 00028500 CC860000 00005901 A328")
  val frameCorrectB = hexStringToFrame("33330000 00FB000A CD2C1594 86DD600C 36DF0091 11FFFE80 00000000 0000FC3B 9A3CE0E2 3955FF02 00000000 00000000 00000000 00FB14E9 14E90091 C6390000 84000000 00020000 00000135 01350139 01330132 01650130 01650163 01330161 01390162 01330163 01660130 01300130 01300130 01300130 01300130 01300130 01300130 01380165 01660369 70360461 72706100 000C8001 00000078 000D0572 61777272 056C6F63 616C00C0 60001C80 01000000 780010FE 80000000 000000FC 3B9A3CE0 E239550D 5BA667")

  def calcCrc32(that : Seq[Int]): Int ={
    def getBit(id : Int) = (that(id/8) >> ((id % 8))) & 1
    var crc = -1
    for(bitId <- 0 until that.size*8){
      val bit = getBit(bitId) ^ ((crc >> 31) & 1)
      crc = (crc << 1) ^ ((if(bit == 1) 0x04C11DB7 else 0))
    }
    val crcReversed = (0 until 32).map(i => ((crc >> i) & 1) << (31-i)).reduce(_ | _)
    ~crcReversed
  }

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
          val crc = calcCrc32(padded)
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
              assert(task.head == -1 || ((data >> i * 8) & 0xFF) == task.head)
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
          val crc = calcCrc32(frame) ^ (if(Random.nextFloat() < 0.8) 0x0 else Random.nextInt())
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
}
