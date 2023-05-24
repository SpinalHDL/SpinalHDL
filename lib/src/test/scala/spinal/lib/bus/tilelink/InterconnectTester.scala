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
import spinal.lib.system.tag.{MemoryConnection, PMA}
import spinal.sim.SimThread

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random





class InterconnectTester extends AnyFunSuite{
  def testInterconnect(interconnect: Interconnect): Unit ={
    val nodeToModel = mutable.LinkedHashMap[InterconnectNode, SparseMemory]()
    val slaveNodes = interconnect.nodes.filter(_.bus.isMasterInterface)
    val masterNodes = interconnect.nodes.filter(_.bus.isSlaveInterface)

    for(node <- slaveNodes) {
      val model = new SlaveRam(node.bus, node.clockDomain)
      nodeToModel(node) = SparseMemory(model.mem.seed)
    }

    val masterSpecs = masterNodes.map(n => {
      val mappings = ArrayBuffer[Mapping]()
      val nodes = MemoryConnection.nodes(n)
      for(s <- nodes){
        s.node match {
          case n: InterconnectNode => {
            nodeToModel.get(n) match {
              case Some(m) => mappings += Mapping(n.m2s.supported, SizeMapping(s.address, s.size), m)
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

  def coherentOnlySlave = S2mParameters(
    List.tabulate(4)(i =>
      S2mAgent(
        name = null,
        sinkId = SizeMapping(i*8, 8),
        emits = S2mTransfers(
          probe = SizeRange(0x40)
        )
      )
    )
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

  def simpleSlave(addressWidth : Int, dataWidth : Int = 32, s2mParameters: S2mParameters = S2mParameters.none(null)) (implicit ic : Interconnect) = new SlaveBus(
    M2sSupport(
      transfers = M2sTransfers.unknownEmits,
      dataWidth = dataWidth,
      addressWidth = addressWidth,
      allowExecute = false
    ),
    s2mParameters
  )

  def simpleReadOnlySlave(addressWidth : Int, dataWidth : Int = 32) (implicit ic : Interconnect) = new SlaveBus(
    M2sSupport(
      transfers = M2sTransfers(
        get = SizeRange.upTo(0x1000)
      ),
      dataWidth = dataWidth,
      addressWidth = addressWidth,
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
      val m0 = simpleMaster(coherentOnly)
      val s0 = simpleSlave(12, 32, coherentOnlySlave)
      val b0 = interconnect.createNode()
      b0 << m0.node
      s0.node at 0x1000 of b0
    }).doSim(seed = 42){dut =>
//      dut.clockDomain.forkStimulus(10)
//      testInterconnect(dut.interconnect)
    }
  }

  test("Coherent_B"){
    tilelink.DebugId.setup(16)
    SimConfig.withFstWave.compile(new Component{
      implicit val interconnect = new Interconnect()
      val m0, m1 = simpleMaster(coherentOnly)
      val s0, s1 = simpleSlave(12, 32, coherentOnlySlave)
      val b0 = interconnect.createNode()
      b0 << m0.node
      b0 << m1.node
      s0.node at 0x1000 of b0
      s1.node at 0x2000 of b0
    }).doSim(seed = 42){dut =>
//      dut.clockDomain.forkStimulus(10)
//      testInterconnect(dut.interconnect)
    }
  }

  test("Coherent_C"){
    tilelink.DebugId.setup(16)
    SimConfig.withFstWave.compile(new Component{
      implicit val interconnect = new Interconnect()
      val m0, m1 = simpleMaster(coherentOnly)
      val s0, s1, s2 = simpleSlave(12, 32, coherentOnlySlave)
      val b0, b1 = interconnect.createNode()
      b0 << m0.node
      b0 << m1.node
      s0.node at 0x1000 of b0
      s1.node at 0x2000 of b0
      b1 at (0x10000) of b0
      s2.node at 0x3000 of b1
    }).doSim(seed = 42){dut =>
      //      dut.clockDomain.forkStimulus(10)
      //      testInterconnect(dut.interconnect)
    }
  }

  test("Coherent_D"){
    tilelink.DebugId.setup(16)
    SimConfig.withFstWave.compile(new Component{
      implicit val interconnect = new Interconnect()
      val m0 = simpleMaster(coherentOnly)
      val b0 = interconnect.createNode()
      b0 << m0.node

      val hub = new CoherencyHubIntegrator()
      val h0 = hub.createPort()
      h0 << b0

      val s0 = simpleSlave(16, 32)
      s0.node << hub.memGet
      s0.node << hub.memPut
    }).doSim(seed = 42){dut =>
//      dut.clockDomain.forkStimulus(10)
//      testInterconnect(dut.interconnect)
    }
  }

  test("scanRegions"){
    tilelink.DebugId.setup(16)
    SimConfig.withFstWave.compile(new Component{
      implicit val interconnect = new Interconnect()
      val m0 = simpleMaster(readWrite)
      val s0, s1 = simpleSlave(addressWidth = 10)
      val b0 = interconnect.createNode()
      b0 at 0xE0000 of m0.node
      s0.node at 0x1000 of b0
      s1.node at 0x2000 of b0

      s0.node.addTag(PMA.VOLATILE)
      s1.node.addTag(PMA.CACHED)

      Elab build {
        var hits = 0
        MemoryConnection.walk(m0.node) { w =>
          w.node match {
            case s0.node =>
              hits += 1
              assert(w.address == 0xE0000 + 0x1000)
              assert(w.size == 0x400)
              assert(w.node.hasTag(PMA.VOLATILE))
            case s1.node =>
              hits += 1
              assert(w.address == 0xE0000 + 0x2000)
              assert(w.size == 0x400)
              assert(w.node.hasTag(PMA.CACHED))
            case _ =>
          }
        }
        assert(hits == 2)
      }
    }).doSim(seed = 42){dut =>
      //      dut.clockDomain.forkStimulus(10)
      //      testInterconnect(dut.interconnect)
    }
  }


  test("Soc_A"){
    tilelink.DebugId.setup(16)
    SimConfig.withFstWave.compile(new Component{
      implicit val interconnect = new Interconnect()
      val cpu = new Area{
        val main = simpleMaster(coherentOnly)
        val io = simpleMaster(readWrite)
      }

      val hub = new CoherencyHubIntegrator()
      val p0 = hub.createPort()
      p0 << cpu.main.node
      p0 << cpu.io.node

      val memory = simpleSlave(20, 32)
      memory.node.addTag(PMA.MAIN)
      memory.node at 0x80000000l of hub.memGet
      memory.node at 0x80000000l of hub.memPut

      val peripheral = new Area{
        val bus = interconnect.createNode()
        bus at 0x20000000 of hub.memGet
        bus at 0x20000000 of hub.memPut

        val gpio = simpleSlave(12, 32)
        val uart = simpleSlave(12, 32)
        val spi = simpleSlave(12, 32)
        val memory = simpleSlave(16, 32)
        memory.node.addTag(PMA.MAIN)
        val rom = simpleReadOnlySlave(12, 32)
        rom.node.addTag(PMA.MAIN)

        gpio.node at 0x1000 of bus
        uart.node at 0x2000 of bus
        spi.node at 0x3000 of bus
        memory.node at 0x10000 of bus
        rom.node at 0x20000 of bus
      }

      Elab check new Area{
        val mainSupport = MemoryConnection.getSupportedTransfers(cpu.main.node)
        println("cpu.main.node can access : ")
        println(mainSupport.map("- " + _).mkString("\n"))
        val ioSupport = MemoryConnection.getSupportedTransfers(cpu.io.node)
        println("cpu.io.node can access : ")
        println(ioSupport.map("- " + _).mkString("\n"))
      }
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

      val r0 = simpleReadOnlySlave(8)
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

        val r0 = simpleReadOnlySlave(8)
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
