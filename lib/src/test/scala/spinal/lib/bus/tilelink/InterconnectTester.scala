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





class InterconnectTester extends AnyFunSuite{
  def testInterconnect(interconnect: Interconnect): Unit ={
    val nodeToModel = mutable.LinkedHashMap[InterconnectNode, SparseMemory]()
    val slaveNodes = interconnect.nodes.filter(_.isSlaveOnly())
    val masterNodes = interconnect.nodes.filter(_.isMasterOnly())

    for(node <- slaveNodes) {
      val model = new SlaveRam(node.bus, node.clockDomain)
      nodeToModel(node) = SparseMemory(model.mem.seed)
    }

    val masterSpecs = masterNodes.map(n => {
      val mappings = ArrayBuffer[Mapping]()
      MemoryConnection.walk(n) { (s, addr, size) =>
        s match {
          case n: InterconnectNode => {
            nodeToModel.get(n) match {
              case Some(m) => mappings += Mapping(n.m2s.supported, SizeMapping(addr, size), m)
              case None =>
            }
          }
        }
      }
      MasterSpec(n.bus, n.clockDomain, mappings)
    })

    val mastersStuff = for(m <- masterSpecs) yield new Area{
      val agent = new MasterAgent(m.bus, m.cd)
      val tester = new MasterTester(m, agent)
      tester.startPerSource(100)
    }

    mastersStuff.foreach(_.tester.join())
  }

  def readWrite = M2sTransfers(
    get = SizeRange.upTo(0x40),
    putFull = SizeRange.upTo(0x40),
    putPartial = SizeRange.upTo(0x40)
  )

  def readOnly = M2sTransfers(
    get = SizeRange.upTo(0x40)
  )

  def writeOnly = M2sTransfers(
    putFull = SizeRange.upTo(0x40),
    putPartial = SizeRange.upTo(0x40)
  )

  def coherentOnly = M2sTransfers(
    acquireT = SizeRange(0x40),
    acquireB = SizeRange(0x40)
  )

  def simpleMaster(emit : M2sTransfers, dataWidth : Int = 32)(implicit ic : Interconnect) = new MasterBus(
    M2sParameters(
      addressWidth = 32,
      dataWidth = dataWidth,
      masters = List(M2sAgent(
        name = null,
        mapping = List(M2sSource(
          id = SizeMapping(0, 4),
          emits = emit
        ))
      ))
    )
  )

  def simpleSlave(addressWidth : Int, dataWidth : Int = 32) (implicit ic : Interconnect) = new SlaveBus(
    M2sSupport(
      transfers = M2sTransfers.unknownEmits,
      dataWidth = dataWidth,
      addressWidth = addressWidth,
      allowExecute = false
    )
  )

  def simpleReadOnlySlave() (implicit ic : Interconnect) = new SlaveBus(
    M2sSupport(
      transfers = M2sTransfers(
        get = SizeRange.upTo(0x1000)
      ),
      dataWidth = 32,
      addressWidth = 8,
      allowExecute = false
    )
  )

  def simpleWriteOnlySlave() (implicit ic : Interconnect) = new SlaveBus(
    M2sSupport(
      transfers = M2sTransfers(
        putFull = SizeRange.upTo(0x1000),
        putPartial = SizeRange.upTo(0x1000)
      ),
      dataWidth = 32,
      addressWidth = 8,
      allowExecute = false
    )
  )

  test("Simple"){
    tilelink.DebugId.setup(16)
    SimConfig.withFstWave.compile(new Component{
      implicit val interconnect = new Interconnect()

      val m0, m1 = simpleMaster(readWrite)
      val s0, s1 = simpleSlave(8)

      val b0 = interconnect.createNode()

      b0 << m0.node
      b0 << m1.node

      s0.node at 0x200 of b0
      s1.node at 0x400 of b0
    }).doSim(seed = 42){dut =>
      dut.clockDomain.forkStimulus(10)
      testInterconnect(dut.interconnect)
    }
  }

  test("Buffering"){
    tilelink.DebugId.setup(16)
    SimConfig.withFstWave.compile(new Component{
      implicit val interconnect = new Interconnect()

      val m0 = simpleMaster(readWrite)
      val s0 = simpleSlave(8)

      val b0 = interconnect.createNode()
      b0 << m0.node


      b0.setDecoderConnection { (s, m) =>
        new Area {
          println("wuff")
          val miaou = False
          s.connectFrom(m)(a = StreamPipe.M2S, d = StreamPipe.FULL)
        }
      }

      s0.node at 0x1000 of b0

      val c = interconnect.getConnection(s0.node, b0)

    }).doSim(seed = 42){dut =>
      dut.clockDomain.forkStimulus(10)
      testInterconnect(dut.interconnect)
    }
  }

  test("WidthAdapter_A"){
    tilelink.DebugId.setup(16)
    SimConfig.withFstWave.compile(new Component{
      implicit val interconnect = new Interconnect()
      val m0 = simpleMaster(readWrite, 128)
      val s0 = simpleSlave(8, 32)
      val b0, b1, b2 = interconnect.createNode()
      b0 << m0.node
      b1 << b0
      b2 << b1
      s0.node at 0x1000 of b2
      Elab check{
        assert(m0.node.bus.p.dataWidth == 128)
        assert(b0.bus.p.dataWidth == 32)
        assert(b1.bus.p.dataWidth == 32)
        assert(b2.bus.p.dataWidth == 32)
        assert(s0.node.bus.p.dataWidth == 32)
      }
    }).doSim(seed = 42){dut =>
      dut.clockDomain.forkStimulus(10)
      testInterconnect(dut.interconnect)
    }
  }

  test("WidthAdapter_B"){
    tilelink.DebugId.setup(16)
    SimConfig.withFstWave.compile(new Component{
      implicit val interconnect = new Interconnect()
      val m0 = simpleMaster(readWrite, 128)
      val s0 = simpleSlave(8, 32)
      val b0, b1, b2 = interconnect.createNode()
      b0 << m0.node
      b1 << b0
      b2 << b1
      s0.node at 0x1000 of b2

      b1.forceDataWidth(64)

      Elab check{
        assert(m0.node.bus.p.dataWidth == 128)
        assert(b0.bus.p.dataWidth == 64)
        assert(b1.bus.p.dataWidth == 64)
        assert(b2.bus.p.dataWidth == 32)
        assert(s0.node.bus.p.dataWidth == 32)
      }
    }).doSim(seed = 42){dut =>
      dut.clockDomain.forkStimulus(10)
      testInterconnect(dut.interconnect)
    }
  }

  test("Coherent_A"){
    tilelink.DebugId.setup(16)
    SimConfig.withFstWave.compile(new Component{
      implicit val interconnect = new Interconnect()
      val m0, m1 = simpleMaster(coherentOnly)
      val s0 = simpleSlave(addressWidth = 12)
      val b0 = interconnect.createNode()
      b0 << m0.node
      b0 << m1.node
      s0.node at 0x1000 of b0
//      s1.node at 0x2000 of b0
    }).doSim(seed = 42){dut =>
//      dut.clockDomain.forkStimulus(10)
//      testInterconnect(dut.interconnect)
    }
  }

  test("Advanced"){
    tilelink.DebugId.setup(16)

    SimConfig.withFstWave.compile(new Component{
      implicit val interconnect = new Interconnect()

      val cdA = ClockDomain.external("cdA")

      val m0, m1 = simpleMaster(readWrite)

      //Define intermediate buses
      val b0,b1,b2 = interconnect.createNode()
      b0 << m0.node  //Simple connection
      b0 << m1.node
      b1 at(0x30000, 0x10000) of b0 //Make b1 accessible from b0 bus from address [0x30000, 0x30FFF]

      //Create to memory slaves
      val s0,s1,s2 = simpleSlave(8)

      val r0 = simpleReadOnlySlave()
      val w0 = simpleWriteOnlySlave()

      //Connect those memory slaves at different memory addresses
      s0.node at 0x200 of b1
      s1.node at 0x400 of b1
      b2 at(0x2000, 0x1000) of b1
      s2.node at 0x600 of b2
      r0.node at 0x700 of b2
      w0.node at 0x800 of b2

      val zoneA = cdA on new Area{
        val m0, m1 = simpleMaster(readWrite)
        val ba0 = interconnect.createNode()
        ba0 << m0.node
        ba0 << m1.node

        val r0 = simpleReadOnlySlave()
        val w0 = simpleWriteOnlySlave()

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
      testInterconnect(dut.interconnect)
    }
  }
}
