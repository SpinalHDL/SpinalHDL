package spinal.lib.bus.bmb.sim

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.bus.bmb.Bmb
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.sim._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class BmbInterconnectTester {
  case class SlaveModel(bus : Bmb, mapping : AddressMapping, offset : Int, cd : ClockDomain)
  case class MasterModel(bus : Bmb, cd : ClockDomain)

  val masters = ArrayBuffer[MasterModel]()
  val slaves = ArrayBuffer[SlaveModel]()

  def addSlave(bus : Bmb, mapping : AddressMapping, offset : Int, cd : ClockDomain) = slaves += SlaveModel(bus, mapping,offset,cd)
  def addMaster(bus : Bmb, cd : ClockDomain) = masters += MasterModel(bus,cd)

  var perSourceRspCountTarget = 300

  Phase.boot()
  Phase.setup {
    val allowedWrites = mutable.HashMap[Long, Byte]()
    val memory = new BmbMemoryAgent(){
      override def setByte(address: Long, value: Byte): Unit = {
        val option = allowedWrites.get(address)
        assert(option.isDefined)
        assert(option.get == value)
        super.setByte(address, value)
        allowedWrites.remove(address)
      }
    }
    for(s <- slaves){
      memory.addPort(
        bus = s.bus,
        busAddress = s.offset,
        clockDomain = s.cd,
        withDriver = true
      )
    }

    val regions = BmbRegionAllocator()
    for(m <- masters){
      val agent = new BmbMasterAgent(m.bus, m.cd){
        override def onRspRead(address: BigInt, data: Seq[Byte]): Unit = {
          val ref = (0 until data.length).map(i => memory.getByte(address.toLong + i))
          if(ref != data){
            simFailure(s"Read mismatch on $m.bus\n  REF=$ref\n  DUT=$data")
          }
        }

        override def getCmd(): () => Unit = if(Phase.stimulus.isActive || cmdQueue.nonEmpty) super.getCmd() else null
        override def onCmdWrite(address: BigInt, data: Byte): Unit = {
          val addressLong = address.toLong
          assert(!allowedWrites.contains(addressLong))
          allowedWrites(addressLong) = data
        }

        override def regionAllocate(sizeMax : Int): SizeMapping = regions.allocate(Random.nextInt(1 << m.bus.p.access.addressWidth), sizeMax, m.bus.p)
        override def regionFree(region: SizeMapping): Unit = regions.free(region)
        override def regionIsMapped(region: SizeMapping, opcode : Int): Boolean = {
          slaves.exists{model =>
            val opcodeOk = opcode match {
              case Bmb.Cmd.Opcode.WRITE => model.bus.p.access.canWrite
              case Bmb.Cmd.Opcode.READ => model.bus.p.access.canRead
            }
            val addressOk = model.mapping == DefaultMapping || model.mapping.lowerBound <= region.end && model.mapping.asInstanceOf[SizeMapping].end >= region.base

            addressOk && opcodeOk
          }
        }
      }

      //Retain the flush phase until all Bmb rsp are received
      Phase.flush.retain()
      Phase.flush(fork{
        waitUntil(agent.rspQueue.forall(_.isEmpty))
        sleep(10000)
        Phase.flush.release()
      })

      //Retain the stimulus phase until at least perSourceRspCountTarget transaction completed on each Bmb source id
      val retainers = List.tabulate(1 << m.bus.p.access.sourceWidth)(source => Phase.stimulus.retainer(if(m.bus.p.access.sources.contains(source)) perSourceRspCountTarget else 0))
      agent.rspMonitor.addCallback{_ =>
        if(m.bus.rsp.last.toBoolean){
          retainers(m.bus.rsp.source.toInt).release()
        }
      }
    }
  }
}