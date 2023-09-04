package spinal.lib

import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.tester.SpinalAnyFunSuite

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class SpinalSimStreamFifoMultiChannelSharedSpaceTester extends SpinalAnyFunSuite {
  test("t1") {
    SimConfig.compile(new StreamFifoMultiChannelSharedSpace(Bits(32 bits), 4, 16)).doSimUntilVoid(seed = 42) { dut =>
      val queueModel = ArrayBuffer.fill(4)(mutable.Queue[Long]())

      SimTimeout(1000000)
      dut.clockDomain.forkStimulus(2)
      //Push data randomly and fill the queueModel with pushed transactions
      dut.io.push.stream.valid #= false
      dut.io.pop.stream.ready #= true

      val successCount = Array.fill(4)(0)
      dut.clockDomain.onSamplings {
        assert(!(dut.io.push.full.toBoolean && (dut.io.availability.toInt > 1)))

        if (dut.io.push.stream.valid.toBoolean && dut.io.push.stream.ready.toBoolean) {
          queueModel(log2Up(dut.io.push.channel.toInt)).enqueue(dut.io.push.stream.payload.toLong)
        }
        if (dut.io.pop.stream.valid.toBoolean && dut.io.pop.stream.ready.toBoolean) {
          val channel = log2Up(dut.io.pop.channel.toInt)
          assert(dut.io.pop.stream.payload.toLong == queueModel(channel).dequeue())
          successCount(channel) += 1
          if (successCount.forall(_ > 20000)) simSuccess()
        }
        dut.io.push.stream.valid.randomize()
        dut.io.push.stream.payload.randomize()
        dut.io.push.channel #= (1 << Random.nextInt(dut.channelCount))
        dut.io.pop.stream.ready.randomize()
        dut.io.pop.channel #= (1 << Random.nextInt(dut.channelCount))
      }
    }
  }
}
