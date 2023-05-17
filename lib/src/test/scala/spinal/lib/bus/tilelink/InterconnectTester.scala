package spinal.lib.bus.tilelink

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.core.fiber.{Elab, hardFork}
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.sim._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.sim.SparseMemory
import spinal.lib.system.tag.MemoryConnection
import spinal.sim.SimThread

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

//While create a interconnect master as an io of the toplevel
class MasterBus(p : M2sParameters)(implicit ic : Interconnect) extends Area{
  val node = ic.createMaster()
  val logic = Elab build new Area{
    node.m2s.parameters.load(p)
    node.m2s.setProposedFromParameters() //Here, we just ignore the negotiation phase
    slave(node.bus)
  }
}

//While create a interconnect slave as an io of the toplevel
class SlaveBus(m2sSupport : M2sSupport)(implicit ic : Interconnect) extends Area{
  val node = ic.createSlave()
  val logic = Elab build new Area {
    node.s2m.none() //We do not want to implement memory coherency
    node.m2s.supported.load(m2sSupport)
    master(node.bus)
  }
}



class InterconnectTester extends AnyFunSuite{


  test("Simple"){
    tilelink.DebugId.setup(16)

    SimConfig.withFstWave.compile(new Component{
      implicit val interconnect = new Interconnect()

      val cdA = ClockDomain.external("cdA")

      val m0, m1 = new MasterBus(
        M2sParameters(
        addressWidth = 32,
        dataWidth    = 32,
        masters = List(M2sAgent(
          name = this,
          mapping = List(M2sSource(
            id = SizeMapping(0, 4),
            emits = M2sTransfers(
              get = SizeRange.upTo(0x40),
              putFull = SizeRange.upTo(0x40),
              putPartial = SizeRange.upTo(0x40)
            )
          ))
        ))
      ))

//      val s0 = new SlaveBus(
//        M2sSupport(
//          transfers = M2sTransfers(
//            get     = SizeRange.upTo(0x1000),
//            putFull = SizeRange.upTo(0x1000),
//            putPartial = SizeRange.upTo(0x1000)
//          ),
//          dataWidth    = 32,
//          addressWidth = 8,
//          allowExecute = false
//        )
//      )

//      val b0 = interconnect.createNode()
//
//      b0 << m0.node
//      b0 << m1.node
//
//      s0.node at 0x200 of b0

      //Define intermediate buses
      val b0,b1,b2 = interconnect.createNode()
      b0 << m0.node  //Simple connection
      b0 << m1.node
      b1 at(0x30000, 0x10000) of b0 //Make b1 accessible from b0 bus from address [0x30000, 0x30FFF]

      //Create to memory slaves
      val s0,s1,s2 = new SlaveBus(
        M2sSupport(
          transfers = M2sTransfers(
            get     = SizeRange.upTo(0x1000),
            putFull = SizeRange.upTo(0x1000),
            putPartial = SizeRange.upTo(0x1000)
          ),
          dataWidth    = 32,
          addressWidth = 8,
          allowExecute = false
        )
      )

      val r0 = new SlaveBus(
        M2sSupport(
          transfers = M2sTransfers(
            get     = SizeRange.upTo(0x1000)
          ),
          dataWidth    = 32,
          addressWidth = 8,
          allowExecute = false
        )
      )

      val w0 = new SlaveBus(
        M2sSupport(
          transfers = M2sTransfers(
            putFull = SizeRange.upTo(0x1000),
            putPartial = SizeRange.upTo(0x1000)
          ),
          dataWidth    = 32,
          addressWidth = 8,
          allowExecute = false
        )
      )

      //Connect those memory slaves at different memory addresses
      s0.node at 0x200 of b1
      s1.node at 0x400 of b1
      b2 at(0x2000, 0x1000) of b1
      s2.node at 0x600 of b2
      r0.node at 0x700 of b2
      w0.node at 0x800 of b2

      val zoneA = cdA on new Area{
        val m0, m1 = new MasterBus(
          M2sParameters(
            addressWidth = 32,
            dataWidth = 32,
            masters = List(M2sAgent(
              name = this,
              mapping = List(M2sSource(
                id = SizeMapping(0, 4),
                emits = M2sTransfers(
                  get = SizeRange.upTo(0x40),
                  putFull = SizeRange.upTo(0x40),
                  putPartial = SizeRange.upTo(0x40)
                )
              ))
            ))
          )
        )
        val ba0 = interconnect.createNode()
        ba0 << m0.node
        ba0 << m1.node

        val r0 = new SlaveBus(
          M2sSupport(
            transfers = M2sTransfers(
              get     = SizeRange.upTo(0x1000)
            ),
            dataWidth    = 32,
            addressWidth = 8,
            allowExecute = false
          )
        )

        val w0 = new SlaveBus(
          M2sSupport(
            transfers = M2sTransfers(
              putFull = SizeRange.upTo(0x1000),
              putPartial = SizeRange.upTo(0x1000)
            ),
            dataWidth    = 32,
            addressWidth = 8,
            allowExecute = false
          )
        )

        r0.node at 0x900 of b2
        w0.node at 0xA00 of b2
        r0.node at 0xC00 of ba0
        w0.node at 0xD00 of ba0


      }


      val m2 = new MasterBus(
        M2sParameters(
          addressWidth = 32,
          dataWidth    = 64,
          masters = List(M2sAgent(
            name = this,
            mapping = List(M2sSource(
              id = SizeMapping(0, 4),
              emits = M2sTransfers(
                get = SizeRange.upTo(0x100),
                putFull = SizeRange.upTo(0x100),
                putPartial = SizeRange.upTo(0x100)
              )
            ))
          ))
        )
      )

      val s4, s5 = new SlaveBus(
        M2sSupport(
          transfers = M2sTransfers(
            get     = SizeRange.upTo(0x1000),
            putFull = SizeRange.upTo(0x1000),
            putPartial = SizeRange.upTo(0x1000)
          ),
          dataWidth    = 256,
          addressWidth = 10,
          allowExecute = false
        )
      )
      s4.node at 0x400 of m2.node
      s5.node at 0x800 of m2.node

    }).doSim(seed = 42){dut =>
      dut.clockDomain.forkStimulus(10)
      dut.cdA.forkStimulus(25)


      val nodeToGmem = mutable.LinkedHashMap[InterconnectNode, SparseMemory]()
      val slaveNodes = dut.interconnect.nodes.filter(_.isSlaveOnly())
      val masterNodes = dut.interconnect.nodes.filter(_.isMasterOnly())

      for(node <-slaveNodes) new SlaveAgent(node.bus, node.clockDomain){
        val gMem = SparseMemory()
        val sMem = SparseMemory(gMem.seed)
        nodeToGmem(node) = gMem

        override def onGet(debugId : Long, source: Int, address: Long, bytes: Int) = {
          accessAckData(source, address, sMem.readBytes(address, bytes))
        }

        override def onPutPartialData(source: Int, address: Long, size: Int, mask: Array[Boolean], data: Array[Byte]) = {
          val burstBytes = (1 << size)
          val isLast = (address & (burstBytes-1)) >= burstBytes - node.bus.p.dataBytes
          sMem.write(address, data, mask)
          if(isLast) accessAck(source, size)
        }
      }

      val threads = ArrayBuffer[SimThread]()
      for(m <- masterNodes){
        case class Mapping(node : InterconnectNode, mapping : SizeMapping)
        val slaves = ArrayBuffer[Mapping]()
        MemoryConnection.walk(m){(s, addr, size) =>
          if(slaveNodes.contains(s)) {
            slaves += Mapping(s.asInstanceOf[InterconnectNode], SizeMapping(addr, size))
          }
        }
        val agent = new MasterAgent(m.bus, m.clockDomain)

        for(masterParam <- m.m2s.parameters.masters) {
          for (source <- masterParam.mapping) {
            source.id.foreach { sourceIdBI =>
              val sourceId = sourceIdBI.toInt
              threads += fork {
                val distribution = new WeightedDistribution[Unit]()
                val slavesWithGet = slaves.filter(_.node.m2s.parameters.emits.get.some)
                distribution(10){
                  val s = slavesWithGet.randomPick()
                  val sizeMax = s.mapping.size.toInt
                  val bytes = source.emits.get.random(randMax = sizeMax)
                  val addressLocal = bytes*Random.nextInt(sizeMax/bytes)
                  val address = s.mapping.base.toLong + addressLocal
                  val gMem = nodeToGmem(s.node)
                  var ref = new Array[Byte](bytes)

                  val orderingCompletion = new OrderingCtrl(bytes)
                  val data = agent.get(sourceId, address, bytes) { args =>
                    gMem.readBytes(args.address toLong, args.bytes, ref, args.address-addressLocal toInt)
                    orderingCompletion -=(args.address-addressLocal toInt, args.bytes)
                  }
                  assert(orderingCompletion.empty)
                  assert((data, ref).zipped.forall(_ == _))
                }
                val slavesWithPutPartial = slaves.filter(_.node.m2s.parameters.emits.putPartial.some)
                distribution(10){
                  val s = slavesWithPutPartial.randomPick()
                  val sizeMax = s.mapping.size.toInt
                  val bytes = source.emits.putPartial.random(randMax = sizeMax)
                  val addressLocal = bytes*Random.nextInt(sizeMax/bytes)
                  val address = s.mapping.base.toLong + addressLocal
                  val data = Array.fill[Byte](bytes)(Random.nextInt().toByte)
                  val mask = Array.fill[Boolean](bytes)(Random.nextInt(2).toBoolean)
                  val gMem = nodeToGmem(s.node)
                  val orderingCompletion = new OrderingCtrl(bytes)
                  assert(!agent.putPartialData(sourceId, address, data, mask){ args =>
                    gMem.write(args.address toLong, data, mask, args.bytes, args.address-addressLocal toInt)
                    orderingCompletion -=(args.address-addressLocal toInt, args.bytes)
                  })
                  assert(orderingCompletion.empty)
                }


                for (i <- 0 until 100) {
                  distribution.randomExecute()
                }
              }
            }
          }
        }
      }

      threads.foreach(_.join())
    }
  }
}
