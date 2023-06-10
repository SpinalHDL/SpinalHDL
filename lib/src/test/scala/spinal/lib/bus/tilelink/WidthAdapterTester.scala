package spinal.lib.bus.tilelink

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.fiber.{Fiber, hardFork}
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.sim._
import spinal.lib.sim.SparseMemory
import spinal.lib.system.tag.MemoryConnection

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



class WidthAdapterTester extends AnyFunSuite{
  def bp(dataWidth : Int)={
    NodeParameters(
      m = M2sParameters(
        addressWidth = 10,
        dataWidth = dataWidth,
        masters = List(
          M2sAgent(
            name = null,
            mapping = List(
              M2sSource(
                id = SizeMapping(0, 16),
                emits = M2sTransfers(
                  get = SizeRange.upTo(256),
                  putFull = SizeRange.upTo(256),
                  putPartial = SizeRange.upTo(256)
                )
              )
            )
          )
        )
      ),
      s = S2mParameters(
        slaves = Nil
      )
    ).toBusParameter()
  }

  def testOn(inputWidth : Int, outputWidth : Int): Unit ={
    test(s"$inputWidth-$outputWidth"){
      tilelink.DebugId.setup(16)
      SimConfig.withFstWave.compile(new WidthAdapter(
        ip = bp(inputWidth),
        op = bp(outputWidth),
        ctxBuffer = ContextAsyncBufferFull
      )).doSim{dut =>
        new BridgeTestbench(
          dut.io.up,
          dut.io.down,
          dut.clockDomain
        ).testPerSource(100)
      }
    }
  }


  for(
    iw <- List(16, 32, 64);
    ow <- List(16, 32, 64);
    if !(iw == ow && iw != 32)
  ){
    testOn(iw, ow)
  }
}
