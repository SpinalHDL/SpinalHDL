package spinal.lib.bus.regif

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.amba4.axilite.sim.{AxiLite4Driver, AxiLite4ReadOnlyMonitor, AxiLite4WriteOnlyMonitor}
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable
import scala.math.{BigInt}
import scala.util.Random

class RegIfTestFixture extends Component {
  //RO generate wire not register fixed in commit "adfda73" , so here should not test RO as Register else Latch detect
  val ACCESS_LIST = List(
    AccessType.RW, AccessType.RW, AccessType.RC, AccessType.RS, // 0 - 3
    AccessType.WRC, AccessType.WRS, AccessType.WC, AccessType.WS, // 4 - 7
    AccessType.WSRC, AccessType.WCRS, AccessType.W1C, AccessType.W1S, // 8 - 11
    AccessType.W1T, AccessType.W0C, AccessType.W0S, AccessType.W0T, // 12 - 15
    AccessType.W1SRC, AccessType.W1CRS, AccessType.W0SRC, AccessType.W0CRS,
    AccessType.WO, AccessType.W0C, AccessType.W0S, AccessType.W1,
    AccessType.WO1, AccessType.NA /*, AccessType.W1P, AccessType.W0P */
  )
  val NUM_REGISTERS = ACCESS_LIST.size

  var io = new Bundle {
    val bus = slave(AxiLite4(AxiLite4Config(addressWidth = log2Up(NUM_REGISTERS)+2, dataWidth = 32)))
    val o = out(Bits(ACCESS_LIST.size*32 bit))
    val o_z = out(Bits(ACCESS_LIST.size*32 bit))
    val i = in(Bits(ACCESS_LIST.size*32 bit))
    val t = in(Bool())
  }

  val regIf = BusInterface(io.bus, (0, NUM_REGISTERS*32))

  for((acc, i) <- ACCESS_LIST.zipWithIndex) {
    val reg = regIf.newRegAt(i*4, s"R${i}")
    val b = reg.field(Bits(32 bit), acc)
    when(io.t) {
      b := io.i.subdivideIn(32 bit)(i)
    }
    io.o.subdivideIn(32 bit)(i) := b
  }

  io.o_z := RegNextWhen(io.o, io.bus.b.fire || io.bus.r.fire) init(0)
}

class RegIfAxiLite4Tester extends SpinalAnyFunSuite {

  def bitWidth = 32

  // Result is the NEXT state of the register after the read
  // Null results are read errors
  private def onRead(accType: AccessType, curState: BigInt): BigInt = accType match {
    // 0 - 3
    case AccessType.RO => curState
    case AccessType.RW => curState
    case AccessType.RC => 0
    case AccessType.RS => (BigInt(1) << bitWidth) - 1
    // 4 - 7
    case AccessType.WRC => 0
    case AccessType.WRS => (BigInt(1) << bitWidth) - 1
    case AccessType.WC => curState
    case AccessType.WS => curState
    // 8 - 11
    case AccessType.WSRC => 0
    case AccessType.WCRS => (BigInt(1) << bitWidth) - 1
    case AccessType.W1C => curState
    case AccessType.W1S => curState
    // 12 - 15
    case AccessType.W1T => curState
    case AccessType.W0C => curState
    case AccessType.W0S => curState
    case AccessType.W0T => curState
    // 16 - 19
    case AccessType.W1SRC => 0
    case AccessType.W1CRS => (BigInt(1) << bitWidth) - 1
    case AccessType.W0SRC => 0
    case AccessType.W0CRS => (BigInt(1) << bitWidth) - 1
    // 20 - 23
    case AccessType.WO => null
    case AccessType.WOC => null
    case AccessType.WOS => null
    case AccessType.W1 => curState
    // 24 - 27
    case AccessType.WO1 => null
    case AccessType.NA => curState
    case AccessType.W1P => curState
    case AccessType.W0P => curState

    case _ => null
  }

  // Result is the NEXT state of the register
  private def onWrite(accType: AccessType, curState: BigInt, in: BigInt, firstWrite: Boolean): BigInt = accType match {
    // 0 - 3
    case AccessType.RO => curState
    case AccessType.RW => in
    case AccessType.RC => curState
    case AccessType.RS => curState
    // 4 - 7
    case AccessType.WRC => in
    case AccessType.WRS => in
    case AccessType.WC => 0
    case AccessType.WS => (BigInt(1) << bitWidth) - 1
    // 8 - 11
    case AccessType.WSRC => (BigInt(1) << bitWidth) - 1
    case AccessType.WCRS => 0
    case AccessType.W1C => (((BigInt(1) << bitWidth) - 1) ^ in) & curState
    case AccessType.W1S => in | curState
    // 12 - 15
    case AccessType.W1T => in ^ curState
    case AccessType.W0C => in & curState
    case AccessType.W0S => (((BigInt(1) << bitWidth) - 1) ^ in) | curState
    case AccessType.W0T => (((BigInt(1) << bitWidth) - 1) ^ in) ^ curState
    // 16 - 19
    case AccessType.W1SRC => in | curState
    case AccessType.W1CRS => (((BigInt(1) << bitWidth) - 1) ^ in) & curState
    case AccessType.W0SRC => (((BigInt(1) << bitWidth) - 1) ^ in) | curState
    case AccessType.W0CRS => in & curState
    // 20 - 23
    case AccessType.WO => in
    case AccessType.WOC => 0
    case AccessType.WOS => (BigInt(1) << bitWidth) - 1
    case AccessType.W1 => if (firstWrite) in else curState
    // 24 - 27
    case AccessType.WO1 => if (firstWrite) in else curState
    case AccessType.NA => in
    case AccessType.W1P => in
    case AccessType.W0P => in

    case _ => curState
  }

  test("random") {
    SimConfig
      .compile(new RegIfTestFixture()).doSim { dut =>
      dut.clockDomain.forkStimulus(10 ns)

      dut.io.t #= false
      dut.io.i.randomize()

      val driver = AxiLite4Driver(dut.io.bus, dut.clockDomain)
      driver.reset()

      val writeHistory = mutable.HashSet[BigInt]()

      val readMonitor = new AxiLite4ReadOnlyMonitor(dut.io.bus.ar, dut.io.bus.r, dut.clockDomain) {

        var beatData: BigInt = 0

        override def onReadStart(addr: BigInt): Unit = {
          beatData = 0
        }

        override def onReadByte(addr: BigInt, data: Byte): Unit = {
          val addrAlign = addr >> log2Up(busConfig.bytePerWord) << log2Up(busConfig.bytePerWord)
          var biData = BigInt(data)
          if (biData < 0)
            biData = biData + 256
          beatData |= biData << ((addr - addrAlign) * 8).toInt
        }

        override def onResponse(addr: BigInt, resp: Byte): Unit = {
          val regData = (dut.io.o.toBigInt >> (addr*8).toInt) & ((BigInt(1) << busConfig.dataWidth) - 1)
          val lastRegData = (dut.io.o_z.toBigInt >> (addr*8).toInt) & ((BigInt(1) << busConfig.dataWidth) - 1)
          val readExpected = onRead(dut.ACCESS_LIST((addr >> 2).toInt), lastRegData)

          if (dut.ACCESS_LIST((addr >> 2).toInt) != AccessType.NA) {
            assert((resp == 0) == (readExpected != null), s"Read error flag did not match expected read error flag, addr = ${addr.hexString(32)}")

            if (readExpected != null) {
              assert(beatData == lastRegData, s"Bus read data did not match registered read data, addr = 0x${addr.hexString(32)}")
              assert(regData == readExpected, s"Registered read data did not match expected post read result, addr = 0x${addr.hexString(32)}")
            }
          }
        }
      }

      val writeMonitor = new AxiLite4WriteOnlyMonitor(dut.io.bus.aw, dut.io.bus.w, dut.io.bus.b, dut.clockDomain) {

        val beatsMap = mutable.HashMap[BigInt, BigInt]()

        override def onWriteStart(addr: BigInt): Unit = {
        }

        override def onWriteByte(addr: BigInt, data: Byte): Unit = {
          val addrAlign = addr >> log2Up(busConfig.bytePerWord) << log2Up(busConfig.bytePerWord)
          if (!beatsMap.contains(addrAlign))
            beatsMap(addrAlign) = 0
          var beatData = beatsMap(addrAlign)
          var biData = BigInt(data)
          if (biData < 0)
            biData = biData + 256
          beatData |= biData << ((addr - addrAlign) * 8).toInt
          beatsMap(addrAlign) = beatData
        }

        override def onResponse(addr: BigInt, resp: Byte): Unit = {
          val addrAlign = addr >> log2Up(busConfig.bytePerWord) << log2Up(busConfig.bytePerWord)
          val regData = (dut.io.o.toBigInt >> (addr * 8).toInt) & ((BigInt(1) << busConfig.dataWidth) - 1)
          val lastRegData = (dut.io.o_z.toBigInt >> (addr * 8).toInt) & ((BigInt(1) << busConfig.dataWidth) - 1)

          val beatData = beatsMap(addrAlign)
          val writeExpected = onWrite(dut.ACCESS_LIST((addr >> 2).toInt), lastRegData, beatData, !writeHistory.contains(addrAlign))

          if (dut.ACCESS_LIST((addr >> 2).toInt) != AccessType.NA) {
//            assert((resp == 0) == (writeExpected != null), s"Write error flag did not match expected write error flag, addr = ${addr}")

            if (writeExpected != null) {
              assert(regData == writeExpected, s"Registered write data did not match expected post write result, addr = ${addr}")
            }
          }

          writeHistory.add(addrAlign)
          beatsMap.remove(addr)
        }
      }

      val rand = Random

      for(_ <- 0 until 1000) {
        // TODO: Support unaligned addresses
        val addr = rand.nextInt(dut.NUM_REGISTERS) << 2
        if (rand.nextBoolean())
          driver.read(addr)
        else
          driver.writeRandom(addr)
        dut.clockDomain.waitSampling()
      }
    }
  }
}
