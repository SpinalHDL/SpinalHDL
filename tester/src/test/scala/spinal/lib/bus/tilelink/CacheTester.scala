package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink
import tilelink._
import tilelink.fabric.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.Component
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.coherent.CacheFiber
import spinal.lib.bus.tilelink.fabric.{MasterBus, SlaveBus}
import spinal.lib.bus.tilelink.sim.MasterTester
import spinal.lib.system.tag.PMA
import spinal.sim.SimThread

import scala.collection.mutable.ArrayBuffer

class CacheTester extends AnyFunSuite{


  test("directed"){
    val tester = new TilelinkTester(
      simConfig = SimConfig.withFstWave,
      cGen = new Component {
        val m0 = new MasterBus(
          M2sParameters(
            addressWidth = 32,
            dataWidth = 64,
            masters = List.tabulate(4)(mid => M2sAgent(
              name = null,
              mapping = List(M2sSource(
                id = SizeMapping(mid * 4, 4),
                emits = M2sTransfers(
                  acquireT = SizeRange(64),
                  acquireB = SizeRange(64),
                  get = SizeRange(1, 64),
                  putFull = SizeRange(1, 64),
                  putPartial = SizeRange(1, 64)
                )
              ))
            ))
          )
        )

        val directory = new CacheFiber()
        directory.parameter.cacheWays = 4
        directory.parameter.cacheBytes = 4096
        directory.parameter.allocateOnMiss = (op, src, addr, size) => addr(6)
        directory.up << m0.node

        val s0 = new SlaveBus(
          M2sSupport(
            transfers = M2sTransfers.all,
            dataWidth = 64,
            addressWidth = 13
          ),
          S2mParameters.none()
        )
        s0.node at 0x10000 of directory.down
        s0.node.addTag(PMA.MAIN)

        val s1 = new SlaveBus(
          M2sSupport(
            transfers = M2sTransfers.all,
            dataWidth = 64,
            addressWidth = 10
          ),
          S2mParameters.none()
        )
        s1.node at 0x20000 of directory.down
      }
    )

//    tester.noStall = true //for test only


    tester.doSim("manual") { tb =>
      disableSimWave()

      periodicaly(10000){
        tb.mastersStuff.foreach(_.agent.driver.driver.randomizeStallRate())
        tb.slavesStuff.foreach(_.model.driver.driver.randomizeStallRate())
      }

//      delayed(2147898761l-1000000)(enableSimWave())
      val testers = (tb.masterSpecs, tb.mastersStuff).zipped.map((s, t) => new MasterTester(s, t.agent))
//      val globalLock = Some(SimMutex()) //for test only
      val globalLock = Option.empty[SimMutex]
      testers.foreach(_.startPerSource(10000, globalLock))
      testers.foreach(_.join())
      tb.waitCheckers()
      tb.assertCoverage()
    }

//    tester.doSim("manual") { tb =>
//      val agent = tb.mastersStuff(0).agent
//      agent.
//      for(i <- 64 until 4096 by 128) {
//        agent.putFullData(0, 0x10000, Array.fill(16)(simRandom.nextInt.toByte))
//      }
//      tb.waitCheckers()
//      tb.assertCoverage()
//    }

//    tester.doSimDirected("manual"){tb =>
//      tb.coverAcquirePerm(32)
//    }
//
//    tester.doSim("manual2"){tb =>
//      val agent = tb.mastersStuff.head.agent
//      val threads = ArrayBuffer[SimThread]()
//      def doFork(body : => Unit) = threads += fork(body)
//      def doJoin() = threads.foreach(_.join())
//      def doBlock(name : String)(body : => Unit): Unit = { println(s"test $name"); body; doJoin(); agent.cd.waitSampling(20) }
//
//      for(address <- List(0x10000, 0x10040)) {
//        doBlock("multiGet") {
//          for (i <- 0 to 7) doFork(agent.get(i, address, 16))
//        }
//        doBlock("multiGetPut") {
//          for (i <- 0 to 3) doFork(agent.get(i, address, 16))
//          for (i <- 4 to 8) doFork(agent.putPartialData(i, address, Array.fill(16)(simRandom.nextInt.toByte), Array.fill(16)(simRandom.nextBoolean())))
//        }
//      }
//    }
//
    tester.doSimDirected("get"){_.coverGet(32)}
    tester.doSimDirected("putFull") {_.coverPutFullData(32)}
    tester.doSimDirected("putPartial") {_.coverPutPartialData(32)}
    tester.doSimDirected("acquireB")(_.coverAcquireB(32))
    tester.doSimDirected("acquireT")(_.coverAcquireT(32))
    tester.doSimDirected("acquireBT")(_.coverAcquireBT(32))
    tester.doSimDirected("acquireTB")(_.coverAcquireTB(32))
    tester.doSimDirected("acquirePerm")(_.coverAcquirePerm(32))
    tester.doSimDirected("coherencyBx2")(_.coverCoherencyBx2(32))
    tester.doSimDirected("coherencyTx2")(_.coverCoherencyTx2(32))
    tester.doSimDirected("coherencyT_B")(_.coverCoherencyT_B(32))
    tester.doSimDirected("coherencyBx2_T_Bx2")(_.coverCoherencyBx2_T_Bx2(32))



    tester.checkErrors()
  }
}
