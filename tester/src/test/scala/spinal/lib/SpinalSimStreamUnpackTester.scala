package spinal.lib

import sim.{ScoreboardInOrder, StreamDriver, StreamMonitor}

import spinal.core._
import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite
import scala.util.Random

class SpinalSimStreamUnpackTester extends SpinalAnyFunSuite {

  case class UnpackTestBundle() extends Bundle {
    val r = UInt(5 bits)
    val g = UInt(6 bits)
    val b = UInt(5 bits)
    val a = UInt(16 bits)
  }

  case class StreamUnpackFixture(offset: Int = 0, contiguousLayout : Boolean = false) extends Component {

    val io = new Bundle {
      val inStream = slave(new Stream(Bits(8 bits)))
      val outData = out(UnpackTestBundle())
      val start = in Bool()

      // Non contiguous layout breaks `a` into two elements
      val dones = out Bits(if(contiguousLayout) 4 bits else 5 bits)
      val allDone = out Bool()
    }

    val output = UnpackTestBundle()
    val stream = io.inStream.stage()

    val layout = List(
      output.r -> (0 + offset),
      output.g -> (8 + offset),
      output.b -> (16 + offset)
    ) ++ {
      if (contiguousLayout) {
        List(
          output.a -> 24
        )
      } else {
        List(
          output.a( 7 downto 0) -> 24,
          output.a(15 downto 8) -> 32
        )
      }
    }

    val unpacker = StreamUnpacker[Bits](
      stream,
      layout
    )

    unpacker.io.start := io.start
    io.outData := output

    io.dones := unpacker.io.dones
    io.allDone := unpacker.io.allDone
  }

  case class UnpackTestUnit(var r: Int, var g: Int, var b: Int, var a: Int)

  def simDriver(doRandom : Boolean)(dut: StreamUnpackFixture): Unit = {
    dut.clockDomain.forkStimulus(10 )

    dut.io.start #= false

    val scoreboard = ScoreboardInOrder[UnpackTestUnit]()

    // Wait until out of reset
    dut.clockDomain.waitSampling()

    // Input generator
    fork {
      // Generate the single-bit only cases
      for (i <- 0 to 40) {
        val allBundleBits = if(i == 0) BigInt(0) else BigInt(1) << (i-1)

        // Send bytes to the DUT
        for (b <- 0 to 4) {
          val byteToSend = (allBundleBits >> (b * 8)) & 0xFF

          // Wait some random number of cycles if we need to
          // but not on the first byte
          if (doRandom && b > 0) dut.clockDomain.waitSampling(Random.nextInt(4))

          dut.io.inStream.payload #= byteToSend
          dut.io.inStream.valid #= true
          dut.io.start #= b == 0  // Issue Start on the first byte

          dut.clockDomain.waitSampling()

          dut.io.inStream.payload #= 0
          dut.io.inStream.valid #= false
          dut.io.start #= false
        }

        // Wait until the DUT is done
        dut.clockDomain.waitSamplingWhere(dut.io.allDone.toBoolean)
        // Space out the cases by a few cycles
        dut.clockDomain.waitSampling(5)
      }
    }

    // Output checker
    {
      // Stream Monitor
      var curWord = 0
      var curR = 0
      var curG = 0
      var curB = 0
      var curA = 0
      StreamMonitor(dut.io.inStream, dut.clockDomain)(p => {
        curWord match {
          case 0 =>
            curR = (p.toInt >> dut.offset) & 0x1F
          case 1 =>
            curG = (p.toInt >> dut.offset) & 0x3F
          case 2 =>
            curB = (p.toInt >> dut.offset) & 0x1F
          case 3 =>
            curA = p.toInt & 0xFF
          case 4 =>
            curA += (p.toInt & 0xFF) << 8
            scoreboard.pushRef(UnpackTestUnit(curR, curG, curB, curA))
        }

        curWord = (curWord + 1) % 5
      })

      // Unpacked output
      for(i <- 0 to 40) {
        dut.clockDomain.waitSamplingWhere(dut.io.allDone.toBoolean)
        scoreboard.pushDut(UnpackTestUnit(
          dut.io.outData.r.toInt,
          dut.io.outData.g.toInt,
          dut.io.outData.b.toInt,
          dut.io.outData.a.toInt
        ))
        scoreboard.check()
      }
    }

    // Wait a bit before ending
    dut.clockDomain.waitSampling(10)

    simSuccess()
  }

  test("aligned, always ready") {
    SimConfig.compile(StreamUnpackFixture())
      .doSim(simDriver(doRandom = false) _)
  }

  test("aligned, random ready") {
    SimConfig.compile(StreamUnpackFixture())
      .doSim(simDriver(doRandom = true) _)
  }

  test("unaligned, always ready") {
    SimConfig.compile(StreamUnpackFixture(2))
      .doSim(simDriver(doRandom = false) _)
  }

  test("unaligned, random ready") {
    SimConfig.compile(StreamUnpackFixture(2))
      .doSim(simDriver(doRandom = true) _)
  }

  test("contiguous") {
    SimConfig.compile(StreamUnpackFixture(contiguousLayout = true))
      .doSim(simDriver(doRandom = false) _)
  }
}
