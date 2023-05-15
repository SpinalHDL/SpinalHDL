package spinal.lib.bus.tilelink

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.fiber.{Elab, hardFork}
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.sim._
import spinal.lib.sim.SparseMemory
import spinal.lib.system.tag.MemoryConnection



class WidthAdapterTester extends AnyFunSuite{
  def bp(dataWidth : Int)={
    BusParameter(
      addressWidth = 10,
      dataWidth    = dataWidth,
      sizeBytes    = 64,
      sourceWidth  = 4,
      sinkWidth    = 0,
      withBCE      = false, //TODO
      withDataA    = true,
      withDataB    = true,
      withDataD    = true,
      node = null
    )
  }

  def testOn(inputWidth : Int, outputWidth : Int): Unit ={
    test(s"$inputWidth-$outputWidth"){
      SimConfig.withFstWave.compile(new WidthAdapter(
        ip = bp(inputWidth),
        op = bp(outputWidth),
        ctxBuffer = ContextAsyncBufferFull
      )).doSim{dut =>
        dut.clockDomain.forkStimulus(10)
      }
    }
  }

  testOn(16, 16)
  testOn(16, 64)
  testOn(64, 16)

}
