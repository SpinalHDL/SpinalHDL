package spinal.lib.bus.bmb.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.bus.bmb.sim._
import spinal.lib.sim.{Phase, StreamDriver, StreamMonitor, StreamReadyRandomizer}
import scala.collection.mutable
import scala.util.Random
import spinal.lib.bus.misc.SizeMapping




class BmbBridgeTester(master : Bmb,
                      masterCd : ClockDomain,
                      slave : Bmb,
                      slaveCd : ClockDomain,
                      alignmentMinWidth : Int = 0) {
  Phase.boot()
  Phase.setup {
    masterCd.forkStimulus(10)
    assert(masterCd == slaveCd)
    //        dut.clockDomain.forkSimSpeedPrinter()

    val memorySize = 1 << master.p.addressWidth
    val allowedWrites = mutable.HashMap[Long, Byte]()
    val memory = new BmbMemoryAgent(memorySize) {
      override def setByte(address: Long, value: Byte): Unit = {
        val option = allowedWrites.get(address)
        assert(option.isDefined)
        assert(option.get == value)
        super.setByte(address, value)
        allowedWrites.remove(address)
      }
    }

    memory.addPort(
      bus = slave,
      busAddress = 0,
      clockDomain = slaveCd,
      withDriver = true
    )

    val regions = BmbRegionAllocator(alignmentMinWidth = alignmentMinWidth)
    val agent = new BmbMasterAgent(master, masterCd) {
      override def onRspRead(address: BigInt, data: Seq[Byte]): Unit = {
        val ref = (0 until data.length).map(i => memory.getByte(address.toLong + i))
        if (ref != data) {
          simFailure(s"Read missmatch on $master\n  REF=$ref\n  DUT=$data")
        }
      }

      override def getCmd(): () => Unit = if (Phase.stimulus.isActive || cmdQueue.nonEmpty) super.getCmd() else null

      override def onCmdWrite(address: BigInt, data: Byte): Unit = {
        val addressLong = address.toLong
        assert(!allowedWrites.contains(addressLong))
        allowedWrites(addressLong) = data
      }

      override def regionAllocate(sizeMax: Int): SizeMapping = regions.allocate(Random.nextInt(memorySize), sizeMax, master.p)

      override def regionFree(region: SizeMapping): Unit = regions.free(region)

      override def regionIsMapped(region: SizeMapping, opcode: Int): Boolean = true
    }

    //Retain the flush phase until all Bmb rsp are received
    Phase.flush.retain()
    Phase.flush(fork {
      while (agent.rspQueue.exists(_.nonEmpty)) {
        masterCd.waitSampling(1000)
      }
      masterCd.waitSampling(1000)
      Phase.flush.release()
    })

    //Retain the stimulus phase until at least 300 transaction completed on each Bmb source id
    val retainers = List.fill(1 << master.p.sourceWidth)(Phase.stimulus.retainer(300)) //TODO
    agent.rspMonitor.addCallback { _ =>
      if (master.rsp.last.toBoolean) {
        retainers(master.rsp.source.toInt).release()
      }
    }
  }
}
