package spinal.lib.bus.bmb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb.Bmb
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim.Phase

import scala.util.Random


class BmbMemoryTester(bmb : Bmb,
                      cd : ClockDomain) {

  val memory = new BmbMemoryAgent(BigInt(1) << bmb.p.addressWidth)
  Phase.boot()
  Phase.setup {
    cd.forkStimulus(10)
    cd.forkSimSpeedPrinter()

    //Memory agent which will spy the accesses to mimic the content of the dut ram
    memory.addPort(
      bus = bmb,
      busAddress = 0,
      clockDomain = cd,
      withDriver = false
    )

    //Regions used to avoid having write to read hazard
    val regions = BmbRegionAllocator()

    val masterAgent = new BmbMasterAgent(bmb, cd){
      val busP = bmb.p
      override def onRspRead(address: BigInt, data: Byte): Unit = assert(data == memory.getByte(address.toLong))
      override def getCmd(): () => Unit = if(Phase.stimulus.isActive || cmdQueue.nonEmpty) super.getCmd() else null
      override def regionAllocate(sizeMax : Int): SizeMapping = regions.allocate(Random.nextInt(1 << bmb.p.addressWidth), sizeMax, busP)
      override def regionFree(region: SizeMapping): Unit = regions.free(region)
      override def regionIsMapped(region: SizeMapping, opcode : Int): Boolean = true
    }

    //Retain the flush phase until all Bmb rsp are received
    Phase.flush.retain()
    Phase.flush{
      fork{
        while(masterAgent.rspQueue.exists(_.nonEmpty)) {
          cd.waitSampling(1000)
        }
        cd.waitSampling(1000)
        Phase.flush.release()
      }
    }

    //Retain the stimulus phase until at least 30000 transaction are completed
    val retainers = List.fill(1 << bmb.p.sourceWidth)(Phase.stimulus.retainer(30000))
    masterAgent.rspMonitor.addCallback{payload =>
      if(payload.last.toBoolean){
        retainers(payload.fragment.source.toInt).release()
      }
    }
  }
}