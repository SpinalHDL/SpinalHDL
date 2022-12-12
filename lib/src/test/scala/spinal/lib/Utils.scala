package spinal.lib

import spinal.core._
import spinal.core.sim._
import spinal.core.formal._
import spinal.lib.formal._

import spinal.tester.SpinalTesterCocotbBase
import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random

class FormalHistoryModifyableTester extends SpinalFormalFunSuite {
  test("pop_any") {
    FormalConfig
      .withBMC(10)
      .withProve(10)
      .withCover(10)
      .withDebug
      .doVerify(new Component {
        val outOnly = false
        val depth = 4
        val input = anyseq(Flow(UInt(6 bits)))
        val dut = HistoryModifyable(input, depth)
        val results = Vec(master(Stream(input.payloadType)), depth)
        val controls = Vec(slave(Stream(input.payloadType)), depth)
        dut.io.outStreams.zip(results).map { case (from, to) => from >> to }
        dut.io.inStreams.zip(controls).map { case (to, from) => from >> to }

        def outCount(data: UInt) = { results.sCount(x => x.valid && x.payload === data) }
        def outExists(data: UInt) = { results.sExist(x => x.valid && x.payload === data) }

        val reset = ClockDomain.current.isResetActive
        assumeInitial(reset)

        when(input.valid) {
          assume(!outExists(input.payload))
        }
        when(pastValid & past(!reset & input.valid)) { assert(results(0).valid && results(0).payload === past(input.payload)) }

        val dataOverflow = anyconst(cloneOf(input.payload))
        assert(
          (input.valid && results.sCount(_.valid) === depth && results.sCount(_.fire) === 0) === dut.io.willOverflow
        )
        val overflowModify = controls.sExist(x => x.fire && x.payload === dataOverflow)
        val overflowCount = outCount(dataOverflow)
        when(past(dut.io.willOverflow && results.last.payload === dataOverflow && !overflowModify)) {
          assert(results.last.valid && past(overflowCount) > overflowCount)
        }

        val dataOut = anyconst(cloneOf(input.payload))
        val dataIn = anyconst(cloneOf(input.payload))

        if (outOnly) {
          controls.map(x => assume(x.valid === False))
        } else {
          assume(controls.map(x => x.payload =/= dataOut).reduce(_ && _))

          controls
            .zip(results)
            .map {
              case (in, out) => {
                val inputFire = if (in == controls.last) input.valid else False
                when(past(in.payload === dataIn && in.fire && !out.fire && !inputFire) && past(!outExists(dataIn))) {
                  assert(outExists(dataIn))
                }
              }
            }

          controls.map(x => cover(x.fire))
          results.zip(controls).map { case (out, in) => cover(in.fire && out.fire) }
        }

        val inputCount = U(input.valid && input.payload === dataOut)
        val validCount = outCount(dataOut)

        val overflowCondition = dut.io.willOverflow && results.last.payload === dataOut
        def modifying(in: Stream[UInt], out: Stream[UInt]) = {
          out.valid && !out.ready && out.payload === dataOut && in.fire
        }
        val modifyCount = CountOne(
          controls
            .filter(_ != controls.last)
            .zip(results.filter(_ != results.last))
            .map { case (in, out) => modifying(in, out) }
        ) +^ U(modifying(controls.last, results.last) && !overflowCondition)
        val outOverflowCount = U(overflowCondition)
        val fireCount = results.sCount(x => x.fire && x.payload === dataOut)

        when(pastValid & !past(reset)) {
          assert(past(validCount - fireCount + inputCount - outOverflowCount - modifyCount) === validCount)
        }
        results.map(x => cover(x.fire))
        cover(results(0).fire && results(2).fire)
      })
  }
}

class SpinalSimLibTester extends AnyFunSuite {
  for (bitCount <- 0 until 12) {
    test("CountOnes" + bitCount) {
      LutInputs(Random.nextInt(5) + 2).on {
        SimConfig.noOptimisation
          .compile(new Component {
            val input = in Bits (bitCount bits)
            val output = out(CountOne(input))
          })
          .doSim(seed = 42) { dut =>
            for (_ <- 0 until 100 + (1 << bitCount) * 4) {
              dut.input.randomize()
              sleep(1)
              assert(dut.output.toInt === dut.input.toBigInt.bitCount)
            }
          }
      }
    }
  }

  for (bitCount <- 0 until 12) {
    test("CountOneOnEach" + bitCount) {
      SimConfig.noOptimisation
        .compile(new Component {
          val input = in Bits (bitCount bits)
          val output = out Vec (CountOneOnEach(input))
        })
        .doSim(seed = 42) { dut =>
          for (_ <- 0 until 100 + (1 << bitCount) * 4) {
            dut.input.randomize()
            sleep(1)
            val input = dut.input.toBigInt
            for (i <- 0 until bitCount; mask = ((1 << i + 1) - 1))
              assert(dut.output(i).toInt === (input & mask).bitCount)
          }
        }
    }
  }
}


class TraversableOncePimpedTester extends AnyFunSuite {
  test("reverse_by_shuffle") {
    SimConfig
      .compile(new Component {
        val data    = in  port Bits(64 bits)
        val outData = out port UInt(64 bits)

        val outSet = data.subdivideIn(8 bits)

        outData := outSet.shuffle(outSet.length - 1 - _).asBits.asUInt
      })
      .doSim(seed = 42) { dut =>
        for (j <- 0 until 1000) {
          val data = dut.data.randomize()

          sleep(1)
          val outData = dut.outData.toBigInt

          for (i <- 0 until 8) {
            val in  = (data >> i * 8) & 0xff
            val out = (outData >> (7 - i) * 8) & 0xff
            assert(in == out)
          }
        }
      }
  }

  test("reverse_by_shuffle_with_size") {
    SimConfig
      .compile(new Component {
        val data    = in  port Bits(64 bits)
        val outData = out port UInt(64 bits)

        outData := U(
          B(data.subdivideIn(8 bits).shuffleWithSize((n, i) => n - 1 - i))
        )
      })
      .doSim(seed = 42) { dut =>
        for (j <- 0 until 1000) {
          val data = dut.data.randomize()

          sleep(1)
          val outData = dut.outData.toBigInt

          for (i <- 0 until 8) {
            val in  = (data >> i * 8) & 0xff
            val out = (outData >> (7 - i) * 8) & 0xff
            assert(in == out)
          }
        }
      }
  }
}

object LatencyAnalysisTester{
  import spinal.lib.math.SIntMath

  class LibTester extends Component {
    val io = new Bundle {
      val inSIntA = in SInt (16 bit)
      val inSIntB = in SInt (16 bit)
      val outSInt = out SInt (32 bit)
      val outSIntRef = out SInt (32 bit)
    }
    io.outSInt := SIntMath.mul(io.inSIntA, io.inSIntB, 4, 0,1,(s,l) => RegNext(s))
    io.outSIntRef := Delay(io.inSIntA * io.inSIntB, LatencyAnalysis(io.inSIntA, io.outSInt))
  }
}

class LibTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "LibTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/LibTester"
  override def createToplevel: Component = new LatencyAnalysisTester.LibTester
  withWaveform = true
}
