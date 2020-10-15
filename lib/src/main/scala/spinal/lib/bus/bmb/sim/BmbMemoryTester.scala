package spinal.lib.bus.bmb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.bmb.Bmb
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim.Phase

import scala.util.Random


class BmbMemoryTester(bmb : Bmb,
                      cd : ClockDomain,
                      rspCounterTarget : Int = 30000) {

  val memory = new BmbMemoryAgent(BigInt(1) << bmb.p.access.addressWidth)
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
      override def regionAllocate(sizeMax : Int): SizeMapping = regions.allocate(Random.nextInt(1 << bmb.p.access.addressWidth), sizeMax, busP)
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

    //Retain the stimulus phase until at least rspCounterTarget transaction are completed
    val retainers = List.tabulate(1 << bmb.p.access.sourceWidth)(source => Phase.stimulus.retainer(if(bmb.p.access.sources.contains(source)) rspCounterTarget else 0))
    masterAgent.rspMonitor.addCallback{payload =>
      if(payload.last.toBoolean){
        retainers(payload.fragment.source.toInt).release()
      }
    }
  }
}


case class BmbMemoryMultiPort(bmb : Bmb,
                              cd : ClockDomain)

class BmbMemoryMultiPortTester(ports : Seq[BmbMemoryMultiPort], cmdFactor : Float = 0.5f, rspFactor : Float = 0.5f, forkClocks : Boolean = true) {
  def addressGen(bmb : Bmb) = Random.nextInt(1 << bmb.p.access.addressWidth)
  def transactionCountTarget = 30000

  val memory = new BmbMemoryAgent(BigInt(1) << ports.head.bmb.p.access.addressWidth)
  Phase.boot()
  Phase.setup {
    //Regions used to avoid having write to read hazard
    val regions = BmbRegionAllocator()

    if(forkClocks) ports.map(_.cd).distinct.foreach(_.forkStimulus(10))
    //ports.map(_.cd).distinct.foreach(cd => cd.forkStimulus(if(cd.frequency != null) HertzNumber(1e9) / cd.frequency.getValue))

    for(port <- ports) {
      import port._

      //Memory agent which will spy the accesses to mimic the content of the dut ram
      memory.addPort(
        bus = bmb,
        busAddress = 0,
        clockDomain = cd,
        withDriver = false
      )


      val masterAgent = new BmbMasterAgent(bmb, cd, cmdFactor, rspFactor) {
        val busP = bmb.p
        override def onRspRead(address: BigInt, data: Byte): Unit = assert(data == memory.getByte(address.toLong))
        override def getCmd(): () => Unit = if (Phase.stimulus.isActive || cmdQueue.nonEmpty) super.getCmd() else null
        override def regionAllocate(sizeMax: Int): SizeMapping = regions.allocate(addressGen(bmb), sizeMax, busP)
        override def regionFree(region: SizeMapping): Unit = regions.free(region)
        override def regionIsMapped(region: SizeMapping, opcode: Int): Boolean = true
      }

      //Retain the flush phase until all Bmb rsp are received
      Phase.flush.retain()
      Phase.flush {
        fork {
          while (masterAgent.rspQueue.exists(_.nonEmpty)) {
            cd.waitSampling(1000)
          }
          cd.waitSampling(1000)
          Phase.flush.release()
        }
      }

      //Retain the stimulus phase until at least transactionCountTarget transaction are completed
      val retainers = List.tabulate(1 << bmb.p.access.sourceWidth)(source => Phase.stimulus.retainer(if(bmb.p.access.sources.contains(source)) transactionCountTarget else 0))
      masterAgent.rspMonitor.addCallback { payload =>
        if (payload.last.toBoolean) {
          retainers(payload.fragment.source.toInt).release()
        }
      }
    }
  }
}