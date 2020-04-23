package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.{StreamFifoCC, StreamFifoMultiChannel}
import spinal.tester

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class SpinalSimStreamFifoMultiChannelTester extends FunSuite {
  SimConfig.withWave.compile(new StreamFifoMultiChannel(Bits(32 bits), 4, 16)).doSimUntilVoid(seed = 42){dut =>
    val queueModel = ArrayBuffer.fill(4)(mutable.Queue[Long]())

    SimTimeout(1000000)
    dut.clockDomain.forkStimulus(2)


    //Push data randomly and fill the queueModel with pushed transactions
    dut.io.push.stream.valid #= false
    dut.io.pop.stream.ready #= true

    var successCount = 0
    dut.clockDomain.onSamplings{
      if(dut.io.push.stream.valid.toBoolean && dut.io.push.stream.ready.toBoolean){
        queueModel(dut.io.push.channel.toInt).enqueue(dut.io.push.stream.payload.toLong)
      }
      if(dut.io.pop.stream.valid.toBoolean && dut.io.pop.stream.ready.toBoolean){
        assert(dut.io.pop.stream.payload.toLong == queueModel(dut.io.pop.channel.toInt).dequeue())
        successCount += 1
        if(successCount == 100000) simSuccess()
      }
      dut.io.push.stream.valid.randomize()
      dut.io.push.stream.payload.randomize()
      dut.io.push.channel.randomize()
      dut.io.pop.stream.ready.randomize()
      dut.io.pop.channel.randomize()
    }
  }
}
