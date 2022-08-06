package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.sim.{ScoreboardInOrder, StreamDriver}


case class UnpackTestBundle() extends Bundle {
  val r = UInt(5 bits)
  val g = UInt(6 bits)
  val b = UInt(5 bits)
  val a = UInt(16 bits)
}

case class StreamUnpackFixture(bitWidth : Int, offset : Int = 0) extends Component {
  val io = new Bundle {
    val inStream = slave(new Stream(Bits(bitWidth bits)))
    val outData  = out(UnpackTestBundle())
    // Dones
    val dones    = out Bits(5 bits)
    val allDone  = out Bool()
  }

  val output = Reg(UnpackTestBundle())
  val stream = io.inStream.stage()

  val unpacker = StreamUnpacker[Bits](
    stream,
    List(
      output.r -> (0  + offset),
      output.g -> (8  + offset),
      output.b -> (16 + offset),
      output.a( 7 downto 0) -> 24,
      output.a(15 downto 8) -> 32
    )
  )

  io.outData := output

  // Done signals
  io.dones   := RegNext(unpacker.io.dones)
  io.allDone := RegNext(unpacker.io.allDone)
}


class StreamUnpackTester extends AnyFunSuite {

  case class UnpackTestUnit() {
    var r: BigInt = 0
    var g: BigInt = 0
    var b: BigInt = 0
    var a: BigInt = 0
  }

  def bitsTest(dut: StreamUnpackFixture): Unit = {
    dut.clockDomain.forkStimulus(10 )

    val scoreboard = ScoreboardInOrder[UnpackTestUnit]()
    var currentUnit = UnpackTestUnit()
    var currentByte = 0

    StreamDriver(dut.io.inStream, dut.clockDomain)(p => {
      currentByte match {
        case 0 =>
          currentUnit = UnpackTestUnit()
          currentUnit.r = p.toBigInt >> dut.offset
        case 1 =>
          currentUnit.g = p.toBigInt >> dut.offset
        case 2 =>
          currentUnit.b = p.toBigInt >> dut.offset
        case 3 =>
          currentUnit.a = p.toBigInt & 0xFF
        case 4 =>
          currentUnit.a += (p.toBigInt & 0xFF) << 8
          scoreboard.pushRef(currentUnit)
      }

      currentByte = (currentByte + 1) % 5
      true
    })

    for(i <- 0 to 100) {
      dut.clockDomain.waitSampling()
      if (dut.io.allDone.toBoolean) {
        val dutUnit = UnpackTestUnit()
        dutUnit.r = dut.io.outData.r.toBigInt
        dutUnit.g = dut.io.outData.g.toBigInt
        dutUnit.b = dut.io.outData.b.toBigInt
        dutUnit.a = dut.io.outData.a.toBigInt

        scoreboard.pushDut(dutUnit)
        scoreboard.check()
      }
    }

    simSuccess()
  }

  test("word aligned, bits") {
    SimConfig.compile(StreamUnpackFixture(8))
      .doSim("test")(bitsTest)
  }

  test("unaligned, bits") {
    SimConfig.compile(StreamUnpackFixture(8, 2))
      .doSim("test")(bitsTest)
  }
}
