package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.sim._
import spinal.lib.bus.bmb._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.generator._
import spinal.lib.sim.Phase

import scala.collection.mutable
import scala.util.Random



//TODO handle generation when a master has no slave
class SpinalSimBmbInterconnectGeneratorTester  extends FunSuite{
  def f() = {
    new GeneratorComponent(new Generator {
      val interconnect = BmbInterconnectGenerator()


      def addMaster(requirements : BmbParameter) = new Generator {
        val busHandle = Handle[Bmb]
        interconnect.addMaster(requirements, busHandle)

        val logic = add task new Area{
          val bus = slave(Bmb(requirements))
          busHandle.load(bus)
        }
      }

      def addSlave(address : BigInt, capabilities : BmbParameter) = new Generator{
        val requirements = Handle[BmbParameter]
        val busHandle = Handle[Bmb]
        interconnect.addSlaveAt(capabilities, requirements, busHandle, address)
        dependencies += requirements
        val logic = add task new Area{
          val bus = master(Bmb(requirements))
          busHandle.load(bus)
        }
      }


      val mA = addMaster(BmbParameter(
        addressWidth = 20,
        dataWidth = 32,
        lengthWidth = 8,
        sourceWidth = 4,
        contextWidth = 4,
        canRead = true,
        canWrite = true,
        alignment     = BmbParameter.BurstAlignement.BYTE,
        maximumPendingTransactionPerId = Int.MaxValue
      ))

      val mB = addMaster(BmbParameter(
        addressWidth = 20,
        dataWidth = 32,
        lengthWidth = 8,
        sourceWidth = 4,
        contextWidth = 4,
        canRead = true,
        canWrite = true,
        alignment     = BmbParameter.BurstAlignement.WORD,
        maximumPendingTransactionPerId = Int.MaxValue
      ))

      val mC = addMaster(BmbParameter(
        addressWidth = 20,
        dataWidth = 32,
        lengthWidth = 6,
        sourceWidth = 0,
        contextWidth = 5,
        canRead = true,
        canWrite = true,
        alignment     = BmbParameter.BurstAlignement.LENGTH,
        maximumPendingTransactionPerId = Int.MaxValue
      ))


      val mD = addMaster(BmbParameter(
        addressWidth = 20,
        dataWidth = 32,
        lengthWidth = 2,
        sourceWidth = 4,
        contextWidth = 3,
        canRead = true,
        canWrite = true,
        alignment     = BmbParameter.BurstAlignement.LENGTH,
        maximumPendingTransactionPerId = Int.MaxValue
      ))

      val sA = addSlave(0x00000, BmbParameter(
        addressWidth = 18,
        dataWidth = 32,
        lengthWidth = 10,
        sourceWidth = Int.MaxValue,
        contextWidth = Int.MaxValue,
        canRead = true,
        canWrite = true,
        alignment     = BmbParameter.BurstAlignement.BYTE,
        maximumPendingTransactionPerId = Int.MaxValue
      ))

      val sB = addSlave(0x40000, BmbParameter(
        addressWidth = 17,
        dataWidth = 32,
        lengthWidth = Int.MaxValue,
        sourceWidth = 8,
        contextWidth = Int.MaxValue,
        canRead = true,
        canWrite = true,
        alignment     = BmbParameter.BurstAlignement.BYTE,
        maximumPendingTransactionPerId = Int.MaxValue
      ))

      val sB2 = addSlave(0x60000, BmbParameter(
        addressWidth = 17,
        dataWidth = 32,
        lengthWidth = 2,
        sourceWidth = Int.MaxValue,
        contextWidth = Int.MaxValue,
        canRead = true,
        canWrite = true,
        alignment     = BmbParameter.BurstAlignement.LENGTH,
        maximumPendingTransactionPerId = Int.MaxValue
      ))

      // write only
      val sC = addSlave(0x80000, BmbParameter(
        addressWidth = 16,
        dataWidth = 32,
        lengthWidth = Int.MaxValue,
        sourceWidth = Int.MaxValue,
        contextWidth = 9,
        canRead = false,
        canWrite = true,
        alignment     = BmbParameter.BurstAlignement.BYTE,
        maximumPendingTransactionPerId = Int.MaxValue
      ))

      //read only
      val sD = addSlave(0x90000, BmbParameter(
        addressWidth = 16,
        dataWidth = 32,
        lengthWidth = Int.MaxValue,
        sourceWidth = Int.MaxValue,
        contextWidth = Int.MaxValue,
        canRead = true,
        canWrite = false,
        alignment     = BmbParameter.BurstAlignement.BYTE,
        maximumPendingTransactionPerId = Int.MaxValue
      ))

      //Read only and write only mapped at the same address
      val sE = addSlave(0xA0000, BmbParameter(
        addressWidth = 17,
        dataWidth = 32,
        lengthWidth = Int.MaxValue,
        sourceWidth = Int.MaxValue,
        contextWidth = Int.MaxValue,
        canRead = false,
        canWrite = true,
        alignment     = BmbParameter.BurstAlignement.BYTE,
        maximumPendingTransactionPerId = Int.MaxValue
      ))

      val sE2 = addSlave(0xA0000, BmbParameter(
        addressWidth = 17,
        dataWidth = 32,
        lengthWidth = Int.MaxValue,
        sourceWidth = Int.MaxValue,
        contextWidth = Int.MaxValue,
        canRead = true,
        canWrite = false,
        alignment     = BmbParameter.BurstAlignement.BYTE,
        maximumPendingTransactionPerId = Int.MaxValue
      ))

      // down sizer
      val sF = addSlave(0xC0000, BmbParameter(
        addressWidth = 17,
        dataWidth = 16,
        lengthWidth = Int.MaxValue,
        sourceWidth = Int.MaxValue,
        contextWidth = Int.MaxValue,
        canRead = true,
        canWrite = true,
        alignment = BmbParameter.BurstAlignement.LENGTH,
        maximumPendingTransactionPerId = Int.MaxValue
      ))

      def fullAccess(bus : Handle[Bmb]) = interconnect.slaves.keys.foreach(s => interconnect.addConnection(bus, s))
      fullAccess(mA.busHandle)
      fullAccess(mB.busHandle)
      fullAccess(mC.busHandle)
      fullAccess(mD.busHandle)
      //      interconnect.addConnection(mA.busHandle, List(sA.busHandle))
      //      interconnect.addConnection(mB, List(sA))
    })
  }
  test("test1"){
    SimConfig.allOptimisation.compile(f).doSimUntilVoid("test1", 42){dut => //TODO remove seed
      Phase.boot()
      Phase.setup {
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.forkSimSpeedPrinter()

        val memorySize = 0x100000
        val allowedWrites = mutable.HashMap[Long, Byte]()
        val memory = new BmbMemoryAgent(memorySize){
          override def setByte(address: Long, value: Byte): Unit = {
            val option = allowedWrites.get(address)
            assert(option.isDefined)
            assert(option.get == value)
            super.setByte(address, value)
            allowedWrites.remove(address)
          }
        }
        for((bus, model) <- dut.generator.interconnect.slaves){
          memory.addPort(
            bus = bus,
            busAddress = model.mapping.lowerBound.toLong,
            clockDomain = dut.clockDomain,
            withDriver = true
          )
        }

        val regions = BmbRegionAllocator()
        for((bus, model) <- dut.generator.interconnect.masters){
          val agent = new BmbMasterAgent(bus, dut.clockDomain){
            override def onRspRead(address: BigInt, data: Seq[Byte]): Unit = {
              val ref = (0 until data.length).map(i => memory.getByte(address.toLong + i))
              if(ref != data){
                simFailure(s"Read missmatch on $bus\n  REF=$ref\n  DUT=$data")
              }
            }

            override def getCmd(): () => Unit = if(Phase.stimulus.isActive || cmdQueue.nonEmpty) super.getCmd() else null
            override def onCmdWrite(address: BigInt, data: Byte): Unit = {
              val addressLong = address.toLong
              assert(!allowedWrites.contains(addressLong))
              allowedWrites(addressLong) = data
            }

            override def regionAllocate(sizeMax : Int): SizeMapping = regions.allocate(Random.nextInt(memorySize), sizeMax, bus.p)
            override def regionFree(region: SizeMapping): Unit = regions.free(region)
            override def regionIsMapped(region: SizeMapping, opcode : Int): Boolean = {
              dut.generator.interconnect.slaves.values.exists{model =>
                val opcodeOk = opcode match {
                  case Bmb.Cmd.Opcode.WRITE => model.requirements.canWrite
                  case Bmb.Cmd.Opcode.READ => model.requirements.canRead
                }
                val addressOk = model.mapping.lowerBound <= region.end && model.mapping.get.asInstanceOf[SizeMapping].end >= region.base

                addressOk && opcodeOk
              }
            }
          }

          //Retain the flush phase until all Bmb rsp are received
          Phase.flush.retain()
          Phase.flush(fork{
            while(agent.rspQueue.exists(_.nonEmpty)) {
              dut.clockDomain.waitSampling(1000)
            }
            dut.clockDomain.waitSampling(1000)
            Phase.flush.release()
          })

          //Retain the stimulus phase until at least 300 transaction completed on each Bmb source id
          val retainers = List.fill(1 << bus.p.sourceWidth)(Phase.stimulus.retainer(300)) //TODO
          agent.rspMonitor.addCallback{_ =>
            if(bus.rsp.last.toBoolean){
              retainers(bus.rsp.source.toInt).release()
            }
          }
        }
      }
    }
  }
}
