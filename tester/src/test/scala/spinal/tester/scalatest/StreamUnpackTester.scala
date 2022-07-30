package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.sim.{ScoreboardInOrder, StreamDriver}

import spinal.lib.graphic.Rgb


case class StreamUnpackFixture(bitWidth : Int) extends Component {
  val io = new Bundle {
    val inStream = slave(new Stream(Bits(bitWidth bits)))
    val outRGB   = out(Rgb(5, 6, 5))
    val outAlpha = out UInt(16 bits)
    // Dones
    val dones    = out Bits(5 bits)
    val allDone  = out Bool()
  }

  // RGB should be as most 8 bits per channel
  assert(io.outRGB.r.getBitsWidth <= io.inStream.getBitsWidth)
  assert(io.outRGB.g.getBitsWidth <= io.inStream.getBitsWidth)
  assert(io.outRGB.b.getBitsWidth <= io.inStream.getBitsWidth)

  val rgb = Reg(Rgb(io.outRGB.c))
  val alpha = Reg(io.outAlpha.clone())
  val stream = io.inStream.stage()

  val unpacker = StreamUnpacker[Bits](
    stream,
    List(
      rgb.r -> 0,
      rgb.g -> 8,
      rgb.b -> 16,
      alpha(7 downto 0)  -> 24,
      alpha(15 downto 8) -> 32
    )
  )

  io.outRGB   := rgb
  io.outAlpha := alpha

  // Done signals
  io.dones   := unpacker.dones
  io.allDone := unpacker.allDone
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
          currentUnit.r = p.toBigInt
        case 1 =>
          currentUnit.g = p.toBigInt
        case 2 =>
          currentUnit.b = p.toBigInt
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
        dutUnit.r = dut.io.outRGB.r.toBigInt
        dutUnit.g = dut.io.outRGB.g.toBigInt
        dutUnit.b = dut.io.outRGB.b.toBigInt
        dutUnit.a = dut.io.outAlpha.toBigInt

        scoreboard.pushDut(dutUnit)
        scoreboard.check()
      }
    }

    simSuccess()
  }

  test("bits") {
    SimConfig.compile(StreamUnpackFixture(8))
      .doSim("test")(bitsTest)
  }
}
