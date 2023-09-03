package spinal.lib

import sim.{ScoreboardInOrder, StreamReadyRandomizer, StreamMonitor}

import spinal.core._
import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite


class SpinalSimStreamPackTester extends SpinalAnyFunSuite {

  case class PackTestBundle() extends Bundle {
    val r = UInt(5 bits)
    val g = UInt(6 bits)
    val b = UInt(5 bits)
    val a = UInt(16 bits)
  }

  case class StreamPackFixture(offset: Int = 0, contiguousLayout : Boolean = false) extends Component {
    val io = new Bundle {
      val inData = in(PackTestBundle())
      val start = in Bool()
      val outStream = master(new Stream(Bits(8 bits)))
      val done = out Bool()
    }

    val input = io.inData

    val layout = List(
      input.r -> (0 + offset),
      input.g -> (8 + offset),
      input.b -> (16 + offset)
    ) ++ {
      if (contiguousLayout) {
        List(
          input.a -> 24
        )
      } else {
        List(
          input.a( 7 downto 0) -> 24,
          input.a(15 downto 8) -> 32
        )
      }
    }

    val packer = StreamPacker[Bits](
      io.outStream,
      layout
    )

    packer.io.start := io.start
    io.done := packer.io.done
  }

  case class PackTestUnit(var r: Int, var g: Int, var b: Int, var a: Int)

  def simDriver(doRandom : Boolean)(dut: StreamPackFixture): Unit = {
    dut.clockDomain.forkStimulus(10)

    val scoreboard = ScoreboardInOrder[PackTestUnit]()

    // Set Start low
    dut.io.start #= false

    dut.clockDomain.waitSampling()

    // Output checker
    {
      var curWord = 0
      var curR, curG, curB, curA = 0

      StreamMonitor(dut.io.outStream, dut.clockDomain)(p => {
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
        }

        curWord = (curWord + 1) % 5

        if (dut.io.done.toBoolean) {
          scoreboard.pushDut(
            PackTestUnit(
              curR, curG, curB, curA
            )
          )
        }
      })

      if (doRandom)
        StreamReadyRandomizer(dut.io.outStream, dut.clockDomain)
      else
       dut.io.outStream.ready #= true
    }

    // Input driver
    for (i <- 0 to 40) {
      // Generate the next test value
      val fullNum = if (i == 0) BigInt(0) else BigInt(1) << (i-1)

      val nextR = (fullNum >> ( 0 + dut.offset) & 0x1F  ) toInt
      val nextG = (fullNum >> ( 8 + dut.offset) & 0x3F  ) toInt
      val nextB = (fullNum >> (16 + dut.offset) & 0x1F  ) toInt
      val nextA = (fullNum >>  24               & 0xFFFF) toInt

      scoreboard.pushRef(
        PackTestUnit(
          nextR,
          nextG,
          nextB,
          nextA
        )
      )

      // Drive the next values to the DUT
      dut.io.inData.r #= nextR
      dut.io.inData.g #= nextG
      dut.io.inData.b #= nextB
      dut.io.inData.a #= nextA

      // Strobe the start
      dut.io.start #= true
      dut.clockDomain.waitSampling()
      dut.io.start #= false

      // Zero out the input to see if the DUT is registering correctly
      dut.io.inData.r #= 0
      dut.io.inData.g #= 0
      dut.io.inData.b #= 0
      dut.io.inData.a #= 0

      // Wait for the DUT to finish
      dut.clockDomain.waitSamplingWhere({
        val ready = dut.io.outStream.ready.toBoolean
        val valid = dut.io.outStream.valid.toBoolean
        val done = dut.io.done.toBoolean

        // outStream.isFire && done
        (ready && valid) && done
      })

      // Space out the cases by a few cycles
      dut.clockDomain.waitSampling(5)
    }

    // Wait a bit before ending
    dut.clockDomain.waitSampling(10)

    simSuccess()
  }

  test("aligned, always ready") {
    SimConfig.compile(StreamPackFixture())
      .doSim(simDriver(doRandom = false) _)
  }

  test("aligned, random ready") {
    SimConfig.compile(StreamPackFixture())
      .doSim(simDriver(doRandom = true) _)
  }

  test("unaligned, always ready") {
    SimConfig.compile(StreamPackFixture(2))
      .doSim(simDriver(doRandom = false) _)
  }

  test("unaligned, random ready") {
    SimConfig.compile(StreamPackFixture(2))
      .doSim(simDriver(doRandom = true) _)
  }

  test("contiguous") {
    SimConfig.compile(StreamPackFixture(contiguousLayout = true))
      .doSim(simDriver(doRandom = false) _)
  }
}
