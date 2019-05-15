package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core.sim._
import spinal.lib.sim._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbOnChipRam, BmbParameter}
import spinal.lib.bus.bmb.sim.{BmbMasterAgent, BmbMemoryAgent, BmbRegionAllocator}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.graphic.Rgb

import scala.collection.mutable
import scala.util.Random

class SpinalSimBmbOnChipRamTester extends FunSuite{
  test("test1"){

    val memInit = new Array[Byte](64*1024)
    Random.nextBytes(memInit)

    SimConfig.compile{
      val dut = BmbOnChipRam(
        p = BmbOnChipRam.busCapabilities(size = 64 KiB, dataWidth = 32).copy(
          sourceWidth   = 0,
          contextWidth = 0
        ),
        size = 64 KiB
      )
      //Initialize the ram with memInit
      dut.rework(dut.ram.initBigInt(Seq.tabulate(dut.size.toInt/4)(i => BigInt(((memInit(i*4+0).toLong & 0xFF) << 0) | ((memInit(i*4+1).toLong & 0xFF) << 8) | ((memInit(i*4+2).toLong & 0xFF) << 16) | ((memInit(i*4+3).toLong & 0xFF) << 24)))))
      dut
    }doSimUntilVoid{ dut =>
      Phase.boot()
      Phase.setup {
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.forkSimSpeedPrinter()

        //Memory agent which will spy the accesses to mimic the content of the dut ram
        val memory = new BmbMemoryAgent(dut.size)
        for(i <- 0 until memInit.length) memory.setByte(i, memInit(i))
        memory.addPort(
          bus = dut.io.bus,
          busAddress = 0x0000,
          clockDomain = dut.clockDomain,
          withDriver = false
        )

        //Regions used to avoid having write to read hazard
        val regions = BmbRegionAllocator()

        val masterAgent = new BmbMasterAgent(dut.io.bus, dut.clockDomain){
          val busP = dut.io.bus.p
            override def onRspRead(address: BigInt, data: Byte): Unit = assert(data == memory.getByte(address.toLong))
            override def getCmd(): () => Unit = if(Phase.stimulus.isActive || cmdQueue.nonEmpty) super.getCmd() else null
            override def regionAllocate(sizeMax : Int): SizeMapping = regions.allocate(Random.nextInt(dut.size.toInt), sizeMax, busP)
            override def regionFree(region: SizeMapping): Unit = regions.free(region)
            override def regionIsMapped(region: SizeMapping, opcode : Int): Boolean = true
          }

          //Retain the flush phase until all Bmb rsp are received
          Phase.flush.retain()
          Phase.flush(fork{
            while(masterAgent.rspQueue.exists(_.nonEmpty)) {
              dut.clockDomain.waitSampling(1000)
            }
            dut.clockDomain.waitSampling(1000)
            Phase.flush.release()
          })

          //Retain the stimulus phase until at least 30000 transaction are completed
          val retainers = List.fill(1 << dut.p.sourceWidth)(Phase.stimulus.retainer(30000))
          masterAgent.rspMonitor.addCallback{payload =>
          if(payload.last.toBoolean){
            retainers(payload.fragment.source.toInt).release()
          }
        }
      }
    }
  }
}

