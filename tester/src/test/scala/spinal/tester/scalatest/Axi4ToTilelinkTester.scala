package spinal.tester.scalatest

import spinal.core.sim._
import spinal.lib.StreamPipe
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axi.sim._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.sim.{MonitorSubscriber, TransactionA}

import scala.collection.mutable

class Axi4ToTilelinkTester extends SpinalAnyFunSuite{
  test("writeOnly"){
    val compiled = SimConfig.compile(new Axi4WriteOnlyToTilelinkFull(
      Axi4Config(
        16,
        64,
        4,
        useRegion = false,
        useBurst = false,
        useLock = false,
        useCache = false,
        useQos = false,
        useProt = false
      ),
      64,
      4,
      upPipe = StreamPipe.M2S
    ))

    compiled.doSimUntilVoid(42){ dut =>
      val cd = dut.clockDomain
      cd.forkStimulus(10)
//      SimTimeout(1000000)


      val writesInflights = mutable.LinkedHashMap[Int, Byte]()
      val regions = mutable.LinkedHashSet[SizeMapping]()

      var coverage = 0
      val inflightIds = Array.fill(1 << dut.config.idWidth)(0)

      val upAgent = new Axi4WriteOnlyMasterAgent(dut.io.up, cd) {
        override def genCmd() = {
          super.genCmd()
//          allowGen = false //REMOVE ME
        }

        override def genAddress(): BigInt = simRandom.nextInt(1 << dut.io.up.config.addressWidth)
//        override def genAddress(): BigInt = 0x2
//        override def sizes = List(3)
//        override def lens = List(0)
        override def bursts: List[Int] = List(1)
        override def mappingAllocate(mapping: SizeMapping): Boolean = {
          if (regions.exists(_.overlap(mapping))) return false
          regions += mapping
          true
        }
        override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
      }

      val upMonitor = new Axi4WriteOnlyMonitor (dut.io.up, cd) {
        override def onWriteStart(address: BigInt, id: Int, size: Int, len: Int, burst: Int): Unit = {
          inflightIds(id) += 1
        }
        override def onWriteByte(address: BigInt, data: Byte, id: Int): Unit = {
//          println(f"UP W $address%x")
          writesInflights(address.toInt) = data
        }
        override def onResponse(id: Int, resp: Byte): Unit = {
          inflightIds(id) -= 1
          assert(inflightIds(id) >= 0)
          coverage += 1
//          upAgent.allowGen = true   //REMOVE ME
//          assert(writesInflights.isEmpty)  //REMOVE ME
//          assert(inflightIds.forall(_ == 0))  //REMOVE ME
          if(coverage == 10000) {
            upAgent.allowGen = false
            fork{
              dut.clockDomain.waitSampling(2000)
              assert(writesInflights.isEmpty)
              assert(inflightIds.forall(_ == 0))
              simSuccess()
            }
          }
        }
      }

      val downAgent = new tilelink.sim.MemoryAgent(dut.io.down, cd)(null)
      val downChecker = tilelink.sim.Checker(downAgent.monitor)
      val writeChecker = downAgent.monitor.add(new MonitorSubscriber{
        override def onA(a: TransactionA) = {
          val addr = a.address.toInt
          for((value, i) <- a.data.zipWithIndex){
            val local = addr + i
            if(a.mask(i)) writesInflights.get(local) match {
              case Some(x) => assert(x == value)
              case None => simFailure(f"Spawned a write out of nothing on $local%x")
            }
            writesInflights.remove(local)
          }
        }
      })
    }
  }

  test("readOnly") {
    val compiled = SimConfig.compile(new Axi4ReadOnlyToTilelinkFull(
      Axi4Config(
        16,
        32,
        4,
        useRegion = false,
        useBurst = false,
        useLock = false,
        useCache = false,
        useQos = false,
        useProt = false
      ),
      64,
      4,
      upPipe = StreamPipe.M2S
    ))

    compiled.doSimUntilVoid(42) { dut =>
      val cd = dut.clockDomain
      cd.forkStimulus(10)
//      SimTimeout(100000)


      val regions = mutable.LinkedHashSet[SizeMapping]()

      var coverage = 0
      val inflightIds = Array.fill(1 << dut.config.idWidth)(0)

      val downAgent = new tilelink.sim.MemoryAgent(dut.io.down, cd)(null)
      val downChecker = tilelink.sim.Checker(downAgent.monitor)

      val upAgent = new Axi4ReadOnlyMasterAgent(dut.io.up, cd) {
        override def genCmd() = {
          super.genCmd()
//          allowGen = false //REMOVE ME
        }

        override def genAddress(): BigInt = simRandom.nextInt(1 << dut.io.up.config.addressWidth)

//        override def genAddress(): BigInt = simRandom.nextInt(1024*16) & 0xFF00
//        override def sizes = List(2)
//        override def lens = List(0,1,2,3)
//        override def bursts: List[Int] = List(1)

        override def mappingAllocate(mapping: SizeMapping): Boolean = {
          if (regions.exists(_.overlap(mapping))) return false
          regions += mapping
          true
        }

        override def mappingFree(mapping: SizeMapping): Unit = regions.remove(mapping)
      }

      val upMonitor = new Axi4ReadOnlyMonitor(dut.io.up, cd) {
        override def onReadStart(address: BigInt, id: Int, size: Int, len: Int, burst: Int): Unit = {
          inflightIds(id) += 1
        }

        override def onReadByte(address: BigInt, data: Byte, id: Int) = {
          assert(downAgent.mem.read(address.toInt) == data)
        }

        override def onResponse(address: BigInt, id: Int, last: Boolean, resp: Byte): Unit = {
          if(!last) return
          inflightIds(id) -= 1
          assert(inflightIds(id) >= 0)
          coverage += 1
//          upAgent.allowGen = true  //REMOVE ME
          //          assert(writesInflights.isEmpty)  //REMOVE ME
          //          assert(inflightIds.forall(_ == 0))  //REMOVE ME
          if (coverage == 10000) {
            upAgent.allowGen = false
            fork {
              dut.clockDomain.waitSampling(1000)
              assert(inflightIds.forall(_ == 0))
              assert(!upAgent.pending)
              simSuccess()
            }
          }
        }
      }


    }
  }
}
