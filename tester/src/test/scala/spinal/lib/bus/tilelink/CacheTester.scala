package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.tilelink
import tilelink._
import tilelink.fabric.sim._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.Component
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.coherent.{CacheFiber, CacheParam, SelfFLush}
import spinal.lib.bus.tilelink.fabric.{MasterBus, SlaveBus}
import spinal.lib.bus.tilelink.sim.{Block, MasterAgent, MasterTester}
import spinal.lib.system.tag.PMA
import spinal.sim.SimThread

import scala.collection.mutable.ArrayBuffer

class CacheTester extends AnyFunSuite{


  def doTest(cp : CacheParam => Unit): Unit = {
    val tester = new TilelinkTester(
      simConfig = SimConfig,
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


        val ctrl = new MasterBus(
          M2sParameters(
            addressWidth = 32,
            dataWidth = 32,
            masters = List.tabulate(4)(mid => M2sAgent(
              name = null,
              mapping = List(M2sSource(
                id = SizeMapping(mid, 1),
                emits = M2sTransfers(
                  get = SizeRange(4),
                  putFull = SizeRange(4)
                )
              ))
            ))
          )
        )
        ctrl.node.addTag(tilelinkTesterExcluded)

        val directory = new CacheFiber(withCtrl = true)
        directory.parameter.cacheWays = 4
        directory.parameter.cacheBytes = 4096
        directory.parameter.allocateOnMiss = (op, src, addr, size, param) => addr(6)
        directory.parameter.selfFlush = SelfFLush(0x10000, 0x10000+0x400, 2000)
        directory.parameter.flushCompletionsCount = 4
        cp(directory.parameter)
        directory.up << m0.node
        directory.ctrl at (0x00, 0x100) of ctrl.node

        val ctrlInterrupt = out Bool()
        ctrlInterrupt := directory.interrupt.flag

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


    def doFlush(ctrl : MasterAgent, sourceId : Int, base : Int, size : Int, interrupt : Bool = null): Unit = {
      while(ctrl.getInt(sourceId, 0x08) != 0){ } // Reserve the flush hardware
      if(interrupt != null) ctrl.putInt(sourceId, 0x38, 1);
      ctrl.putInt(sourceId, 0x10, base);
      ctrl.putInt(sourceId, 0x18, base + size - 1);
      ctrl.putInt(sourceId, 0x08, 3 | (sourceId << 8)); // Start the flush with completion ID = sourceId
      if(interrupt != null){
        ctrl.cd.waitSamplingWhere(interrupt.toBoolean)
      } else {
        while ((ctrl.getInt(sourceId, 0x00) & (1 << sourceId)) == 0) { // Wait until the sourceId completion register is high
          ctrl.cd.waitSampling(simRandom.nextInt(50))
        }
      }
      if(interrupt != null) ctrl.putInt(sourceId, 0x38, 0);
    }

    tester.doSim("manual") { tb =>
//      disableSimWave()

      periodicaly(10000) {
        tb.mastersStuff.foreach(_.agent.driver.driver.randomizeStallRate())
        tb.slavesStuff.foreach(_.model.driver.driver.randomizeStallRate())
      }


      val ctrl = new MasterAgent(tb.dut.ctrl.node.bus, tb.dut.ctrl.node.clockDomain)(tb.idAllocator)
      ctrl.bus.p.node.m.masters.indices.foreach(sourceId =>  fork {
        val cd = tb.dut.ctrl.node.clockDomain
        while(true) {
          cd.waitSampling(simRandom.nextInt(2000))
          val (base, size) = simRandom.nextInt(3) match{
            case 0 => (simRandom.nextInt(1000), simRandom.nextInt(1024))
            case 1 => (0x10000 + simRandom.nextInt(8192), simRandom.nextInt(1024))
            case 2 => (0x20000 + simRandom.nextInt(1024), simRandom.nextInt(1024))
          }
          doFlush(ctrl, sourceId, base, size)
        }
      })

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


    tester.doSim("flush") { tb =>
      val ctrl = new MasterAgent(tb.dut.ctrl.node.bus, tb.dut.ctrl.node.clockDomain)(tb.idAllocator)
      val m0 = tb.mastersStuff(0).agent
      for(address <- List(0x10000, 0x10040)) {
        val offset = address - 0x10000
        var block : Block = null

        // Dirty
        m0.putInt(0, address, 0x1)
        assert(tb.slavesStuff(0).model.mem.readInt(offset) != 0x1)
        doFlush(ctrl, 0, address, 0x40)
        assert(tb.slavesStuff(0).model.mem.readInt(offset) == 0x1)

        // Clean
        m0.getInt(0, address)
        assert(tb.slavesStuff(0).model.mem.readInt(offset) == 0x1)
        doFlush(ctrl, 0, address, 0x40)
        assert(tb.slavesStuff(0).model.mem.readInt(offset) == 0x1)

        // Clean probe
        block = m0.acquireBlock(0, Param.Grow.NtoT, address, 0x40)
        doFlush(ctrl, 0, address, 0x40, tb.dut.ctrlInterrupt)
        assert(tb.slavesStuff(0).model.mem.readInt(offset) == 0x1)

        // Clean probe2
        block = m0.acquireBlock(0, Param.Grow.NtoT, address, 0x40)
        m0.release(0, Param.Cap.toB, block)
        doFlush(ctrl, 0, address, 0x40, tb.dut.ctrlInterrupt)
        assert(tb.slavesStuff(0).model.mem.readInt(offset) == 0x1)

        // Dirty probe
        block = m0.acquireBlock(0, Param.Grow.NtoT, address, 0x40)
        block.data(0) = 0x02
        block.dirty = true
        doFlush(ctrl, 0, address, 0x40, tb.dut.ctrlInterrupt)
        assert(tb.slavesStuff(0).model.mem.readInt(offset) == 0x2)


        assert(m0.getInt(0, address) == 0x2)
      }
    }

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

  test("dp 1bank"){
    doTest{p => }
  }
  test("dp 1bank non-pow2"){
    doTest{p => p.generalSlotCount = 9}
  }
  test("dp 2bank"){
    doTest{p => p.cacheBanks = 2}
  }
  test("sp 1bank"){
    doTest{p => p.withDualPortRam = false}
  }
  test("sp 2bank"){
    doTest{p => p.withDualPortRam = false; p.cacheBanks = 2}
  }
  test("sp 4bank"){
    doTest{p => p.withDualPortRam = false; p.cacheBanks = 4}
  }
}
