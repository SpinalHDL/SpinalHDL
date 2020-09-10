package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.sim.BmbDriver
import spinal.lib.bus.bmb.{BmbAccessCapabilities, BmbParameter}
import spinal.lib.bus.bsb.sim.{BsbBridgeTester, BsbDriver, BsbPacket}
import spinal.lib.bus.bsb.{Bsb, BsbDownSizerSparse, BsbParameter, BsbUpSizerDense, BsbUpSizerSparse}
import spinal.lib.misc.analog.{BmbBsbToDeltaSigma, BsbToDeltaSigmaParameter}
import spinal.lib.sim.StreamDriver
import spinal.lib.system.dma.sg.DmaSg

import scala.collection.mutable

class SpinalSimSigmaDeltaTester extends FunSuite{
  test("a"){
    SimConfig.compile(
      BmbBsbToDeltaSigma(
        p = BsbToDeltaSigmaParameter(
          channels = 2,
          channelWidth = 16,
          rateWidth = 12
        ),
        inputParameter = BsbParameter(
          byteCount = 4,
          sourceWidth = 0,
          sinkWidth = 0,
          withMask = true
        ),
        bmbParameter = BmbParameter(
          addressWidth = BmbBsbToDeltaSigma.addressWidth,
          dataWidth    = 32,
          sourceWidth  = 0,
          contextWidth = 4,
          lengthWidth  = 2
        )
      )
    ).doSim(seed = 42){dut =>
      dut.clockDomain.forkStimulus(10)

      val queue = mutable.Queue[Long]()
//      val bsb = BsbDriver(dut.io.input, dut.clockDomain)
      val bsb = StreamDriver(dut.io.input, dut.clockDomain){ p =>
        if(queue.isEmpty){
          false
        } else {
          p.data #= queue.dequeue()
          true
        }
      }

      val ctrl = BmbDriver(dut.io.ctrl, dut.clockDomain)
      ctrl.write(0x100, 0x14)
      ctrl.write(0x001, 0x10) //start

      for(i <- 0 to 64000 by 1024){
        val ch0 = i
        val ch1 = 64000 - i
        val word = (ch1 << 16) | (ch0 << 0)
        queue.enqueue(word & 0xFFFFFFFFl)
//        val packet = BsbPacket(0,0, (0 to 3).map(byteId => (word >> byteId*8).toByte))

      }
      waitUntil(queue.isEmpty)
    }
  }
}
