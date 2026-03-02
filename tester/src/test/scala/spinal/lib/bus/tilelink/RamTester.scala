package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import tilelink._
import tilelink.fabric.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.Component
import spinal.lib.StreamPipe
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.fabric.{MasterBus, RamFiber, SlaveBus}
import spinal.lib.bus.tilelink.sim.{Block, MasterAgent, MasterTester}
import spinal.lib.sim.SparseMemory
import spinal.lib.system.tag.PMA
import spinal.sim.SimThread

import scala.collection.mutable.ArrayBuffer

class RamTester extends AnyFunSuite{
  def doTest(): Unit = {
    val tester = new TilelinkTester(
      simConfig = SimConfig,
      cGen = new Component {
        val m0 = new MasterBus(
          M2sParameters(
            addressWidth = 32,
            dataWidth = 128,
            masters = List.tabulate(4)(mid => M2sAgent(
              name = null,
              mapping = List(M2sSource(
                id = SizeMapping(mid * 4, 4),
                emits = M2sTransfers(
                  get = SizeRange(1, 64),
                  putFull = SizeRange(1, 64),
                  putPartial = SizeRange(1, 64)
                )
              ))
            ))
          )
        )

        // Note that the testbench automaticaly use val ordering = Flow(OrderingCmd(p.sizeBytes))
        // from the spinal.lib.bus.tilelink.Ram component to figure out the global memory ordering
        // If you want to test your memory component, you also need to implement that val ordering thing
        val ram = new RamFiber(4096)
        ram.up at 0x0 of m0.node
        ram.up.setUpConnection(a = StreamPipe.FULL)

        // The initial memory content need to be initialized
        fiber.hardFork {
          val mem = ram.thread.logic.mem
          val rand = SparseMemory(seed =42)

          val array = ArrayBuffer[BigInt]()
          var offset = 0
          for(wordId <- 0 until mem.wordCount){
            var acc = BigInt(0)
            for(byteId <- 0 until mem.width/8){
              acc |= BigInt(rand.read(offset).toInt & 0xFF) << (byteId * 8)
              offset += 1
            }
            array += acc
          }
          ram.thread.logic.mem.initBigInt(array)
        }
      }
    )

    tester.doSim("manual") { tb =>
      periodicaly(1000) {
        tb.mastersStuff.foreach(_.agent.driver.driver.randomizeStallRate())
        tb.slavesStuff.foreach(_.model.driver.driver.randomizeStallRate())
      }

      val testers = (tb.masterSpecs, tb.mastersStuff).zipped.map((s, t) => new MasterTester(s, t.agent))
      //      val globalLock = Some(SimMutex()) //for test only
      val globalLock = Option.empty[SimMutex]
      testers.foreach(_.startPerSource(10000, globalLock))
      testers.foreach(_.join())
      tb.waitCheckers()
      tb.assertCoverage()
    }

    tester.doSimDirected("get"){_.coverGet(32)}
    tester.doSimDirected("putFull") {_.coverPutFullData(32)}
    tester.doSimDirected("putPartial") {_.coverPutPartialData(32)}
    tester.checkErrors()
  }


  test("default"){
    doTest()
  }


}
