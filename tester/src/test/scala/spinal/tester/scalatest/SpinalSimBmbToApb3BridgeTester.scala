package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.Apb3Config
import spinal.lib.bus.amba3.apb.sim.Apb3Monitor
import spinal.lib.bus.bmb.{BmbOnChipRam, BmbToApb3Bridge}
import spinal.lib.bus.bmb.sim.{BmbMasterAgent, BmbMemoryAgent, BmbRegionAllocator}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.sim._

import scala.collection.mutable
import scala.util.Random

class SpinalSimBmbToApb3BridgeTester extends SpinalSimFunSuite{
  test("test1") {

    val memInit = new Array[Byte](64 * 1024)
    Random.nextBytes(memInit)

    SimConfig.compile {
      BmbToApb3Bridge(
        apb3Config = Apb3Config(16, 32),
        bmbParameter = BmbToApb3Bridge.busCapabilities(addressWidth = 16, dataWidth = 32).copy(
          sourceWidthMax = 4,
          contextWidthMax = 4
        ).toBmbParameter,
        pipelineBridge = false
      )
    }.doSimUntilVoid("test") { dut =>
      Phase.boot()
      Phase.setup {
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.forkSimSpeedPrinter()

        //Memory agent which will spy the accesses to mimic the content of the dut ram
        val memory = new BmbMemoryAgent(1 << dut.apb3Config.addressWidth)
        for (i <- 0 until memInit.length) memory.setByte(i, memInit(i))
        memory.addPort(
          bus = dut.io.input,
          busAddress = 0x0000,
          clockDomain = dut.clockDomain,
          withDriver = false
        )


        //Regions used to avoid having write to read hazard
        val regions = BmbRegionAllocator()
        val allowedWrites = mutable.HashMap[Long, Byte]()
        val masterAgent = new BmbMasterAgent(dut.io.input, dut.clockDomain) {
          val busP = dut.io.input.p

          override def onRspRead(address: BigInt, data: Byte): Unit = assert(data == memory.getByte(address.toLong))

          override def getCmd(): () => Unit = if (Phase.stimulus.isActive || cmdQueue.nonEmpty) super.getCmd() else null

          override def regionAllocate(sizeMax: Int): SizeMapping = regions.allocate(Random.nextInt((1 << dut.apb3Config.addressWidth)) & ~0x3, 4, busP, sizeMin = 4)

          override def maskRandom(): Boolean = true

          override def regionFree(region: SizeMapping): Unit = regions.free(region)

          override def regionIsMapped(region: SizeMapping, opcode: Int): Boolean = true

          override def onCmdWrite(address: BigInt, data: Byte): Unit = {
            val addressLong = address.toLong
            assert(!allowedWrites.contains(addressLong))
            allowedWrites(addressLong) = data
          }
        }


        val apbMonitor = new Apb3Monitor(dut.io.output, dut.clockDomain) {
          def onRead(address: BigInt): Byte = memory.getByte(address.toInt)

          def onWrite(address: BigInt, value: Byte): Unit = {
            val option = allowedWrites.get(address.toLong)
            assert(option.isDefined)
            assert(option.get == value)
            allowedWrites.remove(address.toLong)
          }
        }


        //Retain the flush phase until all Bmb rsp are received
        Phase.flush.retain()
        Phase.flush {
          fork {
            while (masterAgent.rspQueue.exists(_.nonEmpty)) {
              dut.clockDomain.waitSampling(1000)
            }
            dut.clockDomain.waitSampling(1000)
            Phase.flush.release()
          }
        }

        //Retain the stimulus phase until at least 30000 transaction are completed
        val retainers = List.tabulate(1 << dut.bmbParameter.access.sourceWidth)(source => Phase.stimulus.retainer(if(dut.bmbParameter.access.sources.contains(source)) (10000*durationFactor).toInt else 0))
        masterAgent.rspMonitor.addCallback { payload =>
          if (payload.last.toBoolean) {
            retainers(payload.fragment.source.toInt).release()
          }
        }
      }
    }
  }
}

