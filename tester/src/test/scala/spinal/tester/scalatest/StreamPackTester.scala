package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.sim.{ScoreboardInOrder, StreamReadyRandomizer, StreamMonitor}


case class PackTestBundle() extends Bundle {
  val r = UInt(5 bits)
  val g = UInt(6 bits)
  val b = UInt(5 bits)
  val a = UInt(16 bits)
}

case class StreamPackFixture(bitWidth : Int, offset : Int = 0) extends Component {
  val io = new Bundle {
    val inData    = in(PackTestBundle())
    val start     = in Bool()
    val outStream = master(new Stream(Bits(bitWidth bits)))
    val done      = out Bool()
  }

  val input = io.inData

  val packer = StreamPacker[Bits](
    io.outStream,
    List(
      input.r -> (0  + offset),
      input.g -> (8  + offset),
      input.b -> (16 + offset),
      input.a( 7 downto 0) -> 24,
      input.a(15 downto 8) -> 32
    )
  )

  // IO
  packer.io.start := io.start
  io.done := packer.io.done
}

class StreamPackTester extends AnyFunSuite {
  case class PackTestUnit(var r: Int, var g: Int, var b: Int, var a: Int)

  def bitsTest(dut: StreamPackFixture): Unit = {
    dut.clockDomain.forkStimulus(10)

    val scoreboard = ScoreboardInOrder[PackTestUnit]()

    // Set Start low
    dut.io.start #= false

    dut.clockDomain.waitSampling()

    {
      // Output checker
      var curWord = 0
      var curR = 0
      var curG = 0
      var curB = 0
      var curA = 0

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
      })

      StreamReadyRandomizer(dut.io.outStream, dut.clockDomain)

      // Rising edge detection of Done signal
      var pDone = false
      dut.clockDomain.onSamplings {
        // Detect rising edge
        if (!pDone && dut.io.done.toBoolean) {
          scoreboard.pushDut(
            PackTestUnit(
              curR, curG, curB, curA
            )
          )
        }

        // Update the previous value of Done
        pDone = dut.io.done.toBoolean
      }
    }

    // Input driver
    for (i <- 0 to 100) {
      dut.io.inData.r.randomize()
      dut.io.inData.g.randomize()
      dut.io.inData.b.randomize()
      dut.io.inData.a.randomize()

      // Strobe the start
      dut.io.start #= true
      dut.clockDomain.waitSampling()
      scoreboard.pushRef(
        PackTestUnit(
          dut.io.inData.r.toInt,
          dut.io.inData.g.toInt,
          dut.io.inData.b.toInt,
          dut.io.inData.a.toInt
        )
      )

      dut.io.start #= false

      // Wait for Done strobe
      dut.clockDomain.waitSamplingWhere(dut.io.done.toBoolean === true)
      dut.clockDomain.waitSampling()

      // Check only 1 cycle strobe
      assert(dut.io.done.toBoolean === false)

      // Wait before starting the next loop
      dut.clockDomain.waitSampling(10)
    }

    simSuccess()
  }

  test("word aligned, bits") {
    SimConfig.compile(StreamPackFixture(8))
      .doSim("test")(bitsTest)
  }

  test("unaligned, bits") {
    SimConfig.compile(StreamPackFixture(8, 2))
      .doSim("test")(bitsTest)
  }
}
