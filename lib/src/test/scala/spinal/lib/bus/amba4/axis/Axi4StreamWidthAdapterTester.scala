package spinal.lib.bus.amba4.axis

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import Axi4Stream.{Axi4Stream, Axi4StreamBundle}
import spinal.lib.sim.{ScoreboardInOrder, StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinal.tester.SpinalAnyFunSuite

import scala.BigInt
import scala.collection.mutable.ListBuffer

case class Axi4StreamWidthAdapterFixture(inSize: Int, outSize: Int, compact: Boolean) extends Component {
  var inputConfig = Axi4StreamConfig(dataWidth = inSize,
    useLast = true,
    useKeep = true,
    useStrb = true,
    useId = true,
    idWidth = 5,
    useDest = true,
    destWidth = 5,
    useUser = true,
    userWidth = 2)
  var outputConfig = inputConfig.copy(dataWidth = outSize)

  val io = new Bundle {
    val axis_s = slave(Axi4Stream(inputConfig))
    val axis_m = master(Axi4Stream(outputConfig))
  }

  Axi4StreamWidthAdapter(io.axis_s, io.axis_m, compact = compact)
}

class Axi4StreamWidthAdapterTester extends SpinalAnyFunSuite {

  case class Axi4CheckByte(val data: BigInt = 0,
                           val strb: Boolean = false,
                           val keep: Boolean = false,
                           val last: Boolean = false,
                           val id: BigInt = -1,
                           val dest: BigInt = -1,
                           val user: BigInt = -1) {
    override def equals(that: Any): Boolean = {
      that match {
        case that: Axi4CheckByte => {
          that.data == data &&
          that.strb == strb &&
          that.keep == keep &&
          that.last == last &&
//          that.id == id &&
//          that.dest == dest &&
          that.user == user
        }
        case _ => false
      }
    }
  }

  def widthAdapterTest(dut: Axi4StreamWidthAdapterFixture): Unit = {
    dut.clockDomain.forkStimulus(10)
    SimTimeout(1000000L)

    val scoreboard = ScoreboardInOrder[Seq[Axi4CheckByte]]()

    val INPUT_BEATS = 100
    var inputBeats = 0
    var lastBeat = false
    var lastBeatDone = false

      StreamDriver(dut.io.axis_s, dut.clockDomain)(p => {
        if (lastBeat) {
          p.last #= true
          lastBeatDone = true
          true
        } else {
          !lastBeatDone
        }
      })

    def copyCheckByte(axisBundle: Axi4StreamBundle, idx: Int): Axi4CheckByte = {
      val data = (axisBundle.data.toBigInt >> 8*idx) & BigInt(0xFF)
      val strb = if (axisBundle.config.useStrb) axisBundle.strb.toBigInt.testBit(idx) else true
      val keep = if (axisBundle.config.useKeep) axisBundle.keep.toBigInt.testBit(idx) else true
      val last = if (axisBundle.config.useLast) axisBundle.last.toBoolean else false
      val id   = if (axisBundle.config.useId) axisBundle.id.toBigInt else BigInt(-1)
      val dest = if (axisBundle.config.useDest) axisBundle.dest.toBigInt else BigInt(-1)
      val user = if (axisBundle.config.useUser)
        (axisBundle.user.toBigInt >> axisBundle.config.userWidth*idx) & (BigInt(2).pow(axisBundle.config.userWidth)-1)
      else
        BigInt(-1)
      Axi4CheckByte(data = data, strb = strb, keep = keep, last = last, id = id, dest = dest, user = user)
    }

    def streamByteTransactionMonitor(stream: Axi4Stream, clockDomain: ClockDomain)(callback: (Seq[Axi4CheckByte]) => Unit) = {
      var currentTransaction = ListBuffer[Axi4CheckByte]()

      StreamMonitor(stream, clockDomain)(p => {
        // Test flow control
        inputBeats = inputBeats + 1
        if (inputBeats >= INPUT_BEATS) {
          lastBeat = true
        }

        // Build scoreboard reference
        for (idx <- 0 until p.config.dataWidth) {
          if (!p.config.useKeep || p.keep.toBigInt.testBit(idx)) {
            currentTransaction += copyCheckByte(p, idx).copy(last = false)
          }
        }

        if (!p.config.useLast) {
          if (p.config.useKeep) {
            currentTransaction = currentTransaction.filter(_.keep)
          }

          currentTransaction.foreach(b => callback(List(b)))
          currentTransaction = new ListBuffer[Axi4CheckByte]()
        } else if (p.last.toBoolean) {
          if (p.config.useKeep) {
            currentTransaction = currentTransaction.filter(_.keep)
          }

          currentTransaction += Axi4CheckByte(keep = true, last = true)
          callback(currentTransaction.toList)
          currentTransaction = new ListBuffer[Axi4CheckByte]()
        }
      })
    }

      streamByteTransactionMonitor(dut.io.axis_s, dut.clockDomain)(txn => {
        scoreboard.pushRef(txn)
      })

      streamByteTransactionMonitor(dut.io.axis_m, dut.clockDomain)(txn => {
        scoreboard.pushDut(txn)
        scoreboard.check()
      })

      StreamReadyRandomizer(dut.io.axis_m, dut.clockDomain)

    while(!lastBeatDone) {
      dut.clockDomain.waitSampling()
    }


    simSuccess()
  }

  /*
     Test with KEEP compacting
   */
  test("downsize_coprime_compact") {
    SimConfig.compile(Axi4StreamWidthAdapterFixture(4, 3, compact = true)).doSim("test")(widthAdapterTest)
  }

  test("upsize_coprime_compact") {
    SimConfig.compile(Axi4StreamWidthAdapterFixture(3, 4, compact = true)).doSim("test")(widthAdapterTest)
  }

  test("downsize_even_compact") {
    SimConfig.compile(Axi4StreamWidthAdapterFixture(8, 4, compact = true)).doSim("test")(widthAdapterTest)
  }

  test("upsize_even_compact") {
    SimConfig.compile(Axi4StreamWidthAdapterFixture(4, 8, compact = true)).doSim("test")(widthAdapterTest)
  }

  test("equal_sizes_compact") {
    SimConfig.compile(Axi4StreamWidthAdapterFixture(4, 4, compact = true)).doSim("test")(widthAdapterTest)
  }

  /*
   Test without KEEP compacting
   */
  test("downsize_coprime") {
    SimConfig.compile(Axi4StreamWidthAdapterFixture(4, 3, compact = false)).doSim("test")(widthAdapterTest)
  }

  test("upsize_coprime") {
    SimConfig.compile(Axi4StreamWidthAdapterFixture(3, 4, compact = false)).doSim("test")(widthAdapterTest)
  }

  test("downsize_even") {
    SimConfig.compile(Axi4StreamWidthAdapterFixture(8, 4, compact = false)).doSim("test")(widthAdapterTest)
  }

  test("upsize_even") {
    SimConfig.compile(Axi4StreamWidthAdapterFixture(4, 8, compact = false)).doSim("test")(widthAdapterTest)
  }

  test("equal_sizes") {
    SimConfig.compile(Axi4StreamWidthAdapterFixture(4, 4, compact = false)).doSim("test")(widthAdapterTest)
  }

}
