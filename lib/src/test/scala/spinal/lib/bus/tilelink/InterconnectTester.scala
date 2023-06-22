package spinal.lib.bus.tilelink

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.core.fiber.{Fiber, hardFork}
import spinal.lib.bus.misc.{AddressMapping, InterleavedMapping, OffsetTransformer, OrMapping, SizeMapping, SizeMappingInterleaved}
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.sim.{OrderingArgs, _}
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.coherent.HubFabric
import spinal.lib.bus.tilelink.fabric._
import spinal.lib.sim.SparseMemory
import spinal.lib.system.tag.{MemoryConnection, PMA}
import spinal.sim.{SimError, SimThread}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.compat.Platform.EOL
import scala.util.{Failure, Random, Success, Try}





class InterconnectTester extends AnyFunSuite{
  def testInterconnectAll(cGen : => Component) : Unit = {
    tilelink.DebugId.setup(16)
    val c = SimConfig.withFstWave.compile(cGen)
    val nodes = ArrayBuffer[Node]()
    val orderings = ArrayBuffer[OrderingTag]()
    c.report.toplevel.walkComponents(_.foreachTag {
      case t: Node => nodes += t
      case t: OrderingTag => orderings += t
      case _ =>
    })

    val separator = "\n" + "-"*80 + "\n"
    val errors = new StringBuilder()
    def doSim(name : String)(body : TestbenchBase => Unit): Unit ={
      Try{
        c.doSim(name, 42) { dut =>
          implicit val idAllocator = new IdAllocator(DebugId.width)
          implicit val idCallback = new IdCallback
          for(i <- 0 until DebugId.space.reserved) idAllocator.allocate(i)
          val tb = new TestbenchBase(nodes, orderings)
          val cds = nodes.map(_.clockDomain).distinct
          cds.foreach(_.forkStimulus(Random.nextInt(40)+10))
          var timeout = 0
          cds.head.onSamplings{
            timeout += 1
            if(timeout == 10000){
              SimError("Timeout")
            }
          }
          tb.mastersStuff.foreach(_.monitor.add(new MonitorSubscriber {
            override def onA(a: TransactionA) = {
              timeout = 0
            }
          }))
          body(tb)
        }
      } match {
        case Success(_) =>
        case Failure(e) => errors ++= s"$separator$name FAILED !!! with :\n" + e + EOL +  e.getStackTrace().mkString("", EOL, EOL) + separator
      }
    }

    def doSimDirected(name : String)(body : MasterDebugTester => Unit): Unit ={
      doSim(name){tb =>
        val tester = new MasterDebugTester((tb.masterSpecs, tb.mastersStuff).zipped.map((s, t) => new MasterDebugTesterElement(s, t.agent)))
        body(tester)
      }
    }


    val emits = nodes.map(_.bus.p.node.m.emits).reduce(_ mincover _)
    if(emits.get.some) doSimDirected("get")(_.coverGet(2))
    if(emits.putFull.some) doSimDirected("putf")(_.coverPutFullData(2))
    if(emits.putPartial.some) doSimDirected("putp")(_.coverPutPartialData(2))
    if(emits.get.some) doSimDirected("getPut"){t => t.coverPutFullData(2); t.coverPutPartialData(2); t.coverGet(2)}
    if(emits.acquireB.some) doSimDirected("acquireB")(_.coverAcquireB(8))
    if(emits.acquireT.some) doSimDirected("acquireT")(_.coverAcquireT(8))
    if(emits.withBCE) doSimDirected("acquireBT")(_.coverAcquireBT(8))
    if(emits.withBCE) doSimDirected("acquireTB")(_.coverAcquireTB(8))
    if(emits.withBCE) doSimDirected("coherencyBx2")(_.coverCoherencyBx2(8))
    if(emits.withBCE) doSimDirected("coherencyTx2")(_.coverCoherencyTx2(8))
    if(emits.withBCE) doSimDirected("coherencyT_B")(_.coverCoherencyT_B(8))
    if(emits.withBCE) doSimDirected("coherencyBx2_T_Bx2")(_.coverCoherencyBx2_T_Bx2(8))
    doSim("randomized"){tb =>
      val testers = (tb.masterSpecs, tb.mastersStuff).zipped.map((s, t) => new MasterTester(s, t.agent))
      testers.foreach(_.startPerSource(100))
      testers.foreach(_.join())
      tb.waitCheckers()
      tb.assertCoverage()
    }

    if(errors.nonEmpty) {
      System.err.println(errors.toString())
      throw new Exception("Some tests failed")
    }
  }

  class TestbenchBase(nodes : Seq[Node], orderings : Seq[OrderingTag]) (implicit idAllocator : IdAllocator, idCallback : IdCallback) extends Area {
    val nodeToModel = mutable.LinkedHashMap[Node, SparseMemory]()
    val slaveNodes = nodes.filter(_.bus.isMasterInterface)
    val masterNodes = nodes.filter(_.bus.isSlaveInterface)

    for(o <- orderings) o.cd.onSamplings{
      if(o.cmd.valid.toBoolean){
        idCallback.call(o.cmd.debugId.toLong)(new OrderingArgs(0, o.cmd.bytes.toInt))
      }
    }

    for(node <- slaveNodes) {
      nodeToModel(node) = SparseMemory(Random.nextInt())
    }

    val masterSpecs = masterNodes.map(n => {
      val mappings = ArrayBuffer[Endpoint]()
      val suportedTransfers = MemoryConnection.getMemoryTransfers(n)
      for(e <- suportedTransfers){
        e.node match {
          case n: Node => {
            nodeToModel.get(n) match {
              case Some(m) => {
                e.transfers match {
                  case t : M2sTransfers => mappings += Endpoint(m, List(new Chunk(t, e.where.mapping, e.where.transformers)))
                }
              }
              case None =>
            }
          }
          case n if n.hasTag(TransferFilterTag) => {
            mappings += Endpoint(TransferFilterTag, List(new Chunk(M2sTransfers(), e.where.mapping, e.where.transformers)))
          }
        }
      }
      MasterSpec(n.bus, n.clockDomain, mappings)
    })

    val mastersStuff = for(m <- masterSpecs) yield new Area{
      val agent = new MasterAgent(m.bus, m.cd)
      val monitor = new Monitor(m.bus, m.cd)
      val checker = new Checker(monitor, m.endpoints)
    }

    val slavesStuff = for(node <- slaveNodes) yield new Area{
      val model = new MemoryAgent(node.bus, node.clockDomain, nodeToModel(node).seed)
    }

    def waitCheckers(): Unit ={
      mastersStuff.foreach(_.checker.waitEmpty())
    }
    def assertCoverage(): Unit ={
      for(s <- slavesStuff){
        assert(s.model.monitor.counterA > 100)
      }
    }
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

  def all = M2sTransfers(
    putFull = SizeRange.upTo(0x40),
    putPartial = SizeRange.upTo(0x40),
    get = SizeRange.upTo(0x40),
    acquireT = SizeRange(0x40),
    acquireB = SizeRange(0x40)
  )

  def coherentSlave = S2mParameters(
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

  def simpleMaster(emit : M2sTransfers, dataWidth : Int = 32) = new MasterBus(
    M2sParameters(
      addressWidth = 32,
      dataWidth = dataWidth,
      masters = List.tabulate(4)(mid => M2sAgent(
        name = null,
        mapping = List(M2sSource(
          id = SizeMapping(mid*4, 4),
          emits = emit
        ))
      ))
    )
  )

  def simpleSlave(addressWidth : Int, dataWidth : Int = 32, s2mParameters: S2mParameters = S2mParameters.none, m2sTransfers: M2sTransfers = M2sTransfers.unknownEmits)  = new SlaveBus(
    M2sSupport(
      transfers = m2sTransfers,
      dataWidth = dataWidth,
      addressWidth = addressWidth,
      allowExecute = false
    ),
    s2mParameters
  )

  def simpleReadOnlySlave(addressWidth : Int, dataWidth : Int = 32)  = new SlaveBus(
    M2sSupport(
      transfers = M2sTransfers(
        get = SizeRange.upTo(0x1000)
      ),
      dataWidth = dataWidth,
      addressWidth = addressWidth,
      allowExecute = false
    )
  )

  def simpleWriteOnlySlave()  = new SlaveBus(
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

  test("OneToOne"){
    tilelink.DebugId.setup(16)
    SimConfig.withFstWave.compile(new Component{
      val m0 = simpleMaster(readWrite)
      val s0 = simpleSlave(10)
      s0.node at 0x800 of m0.node
    }).doSim(seed = 42){dut =>
      dut.clockDomain.forkStimulus(10)

      implicit val idAllocator = new IdAllocator(DebugId.width)
      implicit val idCallback = new IdCallback

      val seed = Random.nextInt()
      val m0 = new Area{
        val driver = new MasterAgent(dut.m0.node.bus, dut.m0.node.clockDomain)
        val monitor = new Monitor(dut.m0.node.bus, dut.m0.node.clockDomain)
        val checker = Checker(monitor, List(Endpoint(
          chunks = List(Chunk(
            allowed = dut.m0.node.bus.p.node.m.emits,
            mapping = SizeMapping(0x800, 0x400),
            offset  = 0x800
          )),
          model   = SparseMemory(seed)
        )))
      }
      val s0 = new Area{
        val driver = new MemoryAgent(dut.s0.node.bus, dut.s0.node.clockDomain, seed)
      }

      m0.monitor.add(new MonitorSubscriber {
        override def onA(a: TransactionA) = println(a)
        override def onD(d: TransactionD) = println(d)
      })

      m0.driver.get(1, 0x840, 0x10)
      dut.clockDomain.waitSampling(20)

      m0.driver.get(1, 0x882, 0x2)
      dut.clockDomain.waitSampling(20)

      m0.driver.putPartialData(1, 0x820, (0 until 0x20).map(_.toByte), List(0x86, 0xAA, 0xFF, 0xF0).flatMap(e => (0 to 7).map(i => ((e >> i) & 1) != 0)))
      dut.clockDomain.waitSampling(20)

      m0.driver.get(1, 0x820, 0x20)
      dut.clockDomain.waitSampling(20)
    }
  }

  test("OneToOneAutomated"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(readWrite)
      val s0 = simpleSlave(10)
      s0.node at 0x800 of m0.node
    })
  }

  test("Simple"){
    testInterconnectAll(new Component{
      val m0, m1 = simpleMaster(readWrite)
      val s0, s1 = simpleSlave(8)

      val b0 = Node()

      b0 << m0.node
      b0 << m1.node

      s0.node at 0x200 of b0
      s1.node at 0x400 of b0
    })
  }

  test("unaligned mapping"){
    testInterconnectAll(new Component{
      val m0, m1 = simpleMaster(readWrite)
      val s0, s1, s2 = simpleSlave(8)

      val b0 = Node()

      b0 at 0x11C0 of m0.node
      b0 at 0x11C0 of m1.node

      s0.node at 0x240 of b0
      s1.node at 0x380 of b0
      s2.node at 0x480 of b0
    })
  }


  test("roWo"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(readWrite)
      val b0 = Node()

      b0 at 0x1000 of m0.node

      val r0 = simpleSlave(8, m2sTransfers = M2sTransfers(get = SizeRange.upTo(64)))
      val w0 = simpleSlave(8, m2sTransfers = M2sTransfers(putFull = SizeRange.upTo(64), putPartial = SizeRange.upTo(64)))
      r0.node at 0x200 of b0
      w0.node at 0x300 of b0
    })
  }

  test("roWoOverlapped"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(readWrite)
      val b0 = Node()

      b0 at 0x1000 of m0.node

      val r0 = simpleSlave(8, m2sTransfers = readOnly)
      val w0 = simpleSlave(8, m2sTransfers = writeOnly)
      r0.node at 0x200 of b0
      w0.node at 0x200 of b0
    })
  }


  test("roWoForkJoin"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(readWrite)
      val b0 = Node()

      b0 at 0x1000 of m0.node

      val r0 = Node()
      val w0 = Node()
      r0 at 0x400 of b0
      w0 at 0x400 of b0

      r0.m2s.addModifier(_.intersect(readOnly))
      w0.m2s.addModifier(_.intersect(writeOnly))

      val s0 = simpleSlave(8)
      s0.node at 0x200 of r0
      s0.node at 0x200 of w0
    })
  }


  test("interleaving"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(readWrite)
      val b0 = Node()

      b0 at 0x1000 of m0.node

      val s0,s1 = simpleSlave(8)
      s0.node at InterleavedMapping(SizeMapping(0x200, 0x100), 0x10, 4, 0) of b0
      s1.node at InterleavedMapping(SizeMapping(0x200, 0x100), 0x10, 4, 1) of b0
    })
  }

  test("interleaving2"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(readWrite)
      val b0, b1 = Node()

      b0 at InterleavedMapping(SizeMapping(0x1000, 0x400), 0x10, 4, 0) of m0.node
      b1 at InterleavedMapping(SizeMapping(0x1000, 0x400), 0x10, 4, 1) of m0.node

      val s0, s1 = simpleSlave(8)
      s0.node at 0x200 of b0
      s1.node at 0x200 of b1
    })
  }

  test("interleavingBridge"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(readWrite)

      val i1 = Interleaver(0x10, 4, 1)
      i1 at 0x1000 of m0.node

      val i2 = Interleaver(0x10, 4, 2)
      i2 at 0x1000 of m0.node

      val s1, s2 = simpleSlave(8)
      s1.node at 0x200 of i1.down
      s2.node at 0x200 of i2.down
    })
  }

  //  test("check no overlap mapping"){
//    tilelink.DebugId.setup(16)
//    SimConfig.withFstWave.compile(new Component{
//
//
//      val m0 = simpleMaster(readWrite)
//      val s0, s1, s2 = simpleSlave(8)
//
//      val b0 = InterconnectNode()
//
//      b0 << m0.node
//
//      s0.node at 0x240 of b0
//      s1.node at 0x340 of b0
//      s2.node at 0x400 of b0
//    }).doSim(seed = 42){dut =>
//      dut.clockDomain.forkStimulus(10)
//      testInterconnect(dut)
//    }
//  }


  test("MixedBurstSize"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(readWrite)
      val s0 = simpleSlave(8)
      val s1 = simpleSlave(8, m2sTransfers = M2sTransfers(get = SizeRange.apply(4), putFull = SizeRange.apply(4)))

      val b0 = Node()

      b0 << m0.node

      s0.node at 0x200 of b0
      s1.node at 0x400 of b0
    })
  }

  test("Buffering"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(readWrite)
      val s0 = simpleSlave(8)

      val b0 = Node()
      b0 << m0.node


      b0.setDecoderConnection { (s, m) =>
        new Area {
          println("wuff")
          val miaou = False
          s.connectFrom(m)(a = StreamPipe.M2S, d = StreamPipe.FULL)
        }
      }

      s0.node at 0x1000 of b0

    })
  }

  test("DefaultA"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(readWrite)
      val s0, s1 = simpleSlave(8)
      val d0 = simpleSlave(16)

      val b0 = Node()
      b0 << m0.node

      s0.node at 0x200 of b0
      s1.node at 0x400 of b0
      d0.node << b0

      Fiber build {
        val probed = MemoryConnection.getMemoryTransfers(m0.node)
        println(probed)
      }
    })
  }

  test("DefaultOverlap"){
    testInterconnectAll(new Component{
      val m0, m1 = simpleMaster(readWrite)
      val s0, s1, s2 = simpleSlave(8)

      val b0,b1,b2,b3 = Node()

      b0 << m0.node
      b0 << m1.node

      b1 << b0
      s2.node at 0x1000 of b1

      b2 << b1
      b3 << b2
      s0.node at 0x200 of b3
      s1.node at 0x400 of b3

      val rob = Node()
      rob << b3
      val ro = simpleReadOnlySlave(8)
      ro.node at 0x2000 of rob

      val wob = Node()
      wob << b3
      val wo = simpleWriteOnlySlave()
      wo.node at 0x3000 of wob //TODO catch when << is used instead

//      val ro = simpleReadOnlySlave(8)
//      ro.node << b3
//
//      val wo = simpleWriteOnlySlave()
//      wo.node << b3
    })
  }

  test("WidthAdapter_A"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(readWrite, 128)
      val s0 = simpleSlave(8, 32)
      val b0, b1, b2 = Node()
      b0 << m0.node
      b1 << b0
      b2 << b1
      s0.node at 0x1000 of b2
      Fiber check{
        assert(m0.node.bus.p.dataWidth == 128)
        assert(b0.bus.p.dataWidth == 32)
        assert(b1.bus.p.dataWidth == 32)
        assert(b2.bus.p.dataWidth == 32)
        assert(s0.node.bus.p.dataWidth == 32)
      }
    })
  }

  test("WidthAdapter_B"){
    testInterconnectAll(new Component{

      val m0 = simpleMaster(readWrite, 128)
      val s0 = simpleSlave(8, 32)
      val b0, b1, b2 = Node()
      b0 << m0.node
      b1 << b0
      b2 << b1
      s0.node at 0x1000 of b2

      b1.forceDataWidth(64)

      Fiber check{
        assert(m0.node.bus.p.dataWidth == 128)
        assert(b0.bus.p.dataWidth == 64)
        assert(b1.bus.p.dataWidth == 64)
        assert(b2.bus.p.dataWidth == 32)
        assert(s0.node.bus.p.dataWidth == 32)
      }
    })
  }

  test("WidthAdapter_C"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(readWrite, 128)
      val s0 = simpleSlave(8, 32)
      val widthAdapter = new fabric.WidthAdapter()
      val b0, b1 = Node()
      b0 << m0.node
      widthAdapter.up << b0
      b1 << widthAdapter.down
      s0.node at 0x1000 of b1
      Fiber check{
        assert(m0.node.bus.p.dataWidth == 128)
        assert(b0.bus.p.dataWidth == 128)
        assert(b1.bus.p.dataWidth == 32)
        assert(s0.node.bus.p.dataWidth == 32)
        assert(widthAdapter.up.bus.p.dataWidth == 128)
        assert(widthAdapter.down.bus.p.dataWidth == 32)
      }
    })
  }


  test("Coherent_noGetPut"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(coherentOnly, dataWidth = 128)
      val s0 = simpleSlave(12, 128, coherentSlave)
      val b0 = Node()
      b0 << m0.node
      s0.node at 0x1000 of b0
    })
  }

  test("Coherent_all"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(all, dataWidth = 128)
      val s0 = simpleSlave(12, 128, coherentSlave)
      val b0 = Node()
      b0 << m0.node
      s0.node at 0x1000 of b0
    })
  }

  test("Coherent_B"){
    testInterconnectAll(new Component{
      val m0, m1 = simpleMaster(all, dataWidth = 128)
      val s0, s1 = simpleSlave(12, 128, coherentSlave)
      val b0 = Node()
      b0 << m0.node
      b0 << m1.node
      s0.node at 0x1000 of b0
      s1.node at 0x2000 of b0
    })
  }

  test("Coherent_C"){
    testInterconnectAll(new Component{
      val m0, m1 = simpleMaster(coherentOnly, dataWidth = 128)
      val s0, s1, s2 = simpleSlave(12, 128, coherentSlave)
      val b0, b1 = Node()
      b0 << m0.node
      b0 << m1.node
      s0.node at 0x1000 of b0
      s1.node at 0x2000 of b0
      b1 at (0x10000) of b0
      s2.node at 0x3000 of b1
    })
  }

  test("Coherent_D"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(all, dataWidth = 128)
      val b0 = Node()
      b0 << m0.node

      val s0 = simpleSlave(12, 128, coherentSlave)
      s0.node at 0x1000 of b0
    })
  }

  test("Coherent_withHub"){
    testInterconnectAll(new Component{
      val m0 = simpleMaster(all, dataWidth = 128)

      val hub = new HubFabric()
      hub.up << m0.node

      val s0 = simpleSlave(16, 128)
      s0.node at 0x10000 of hub.down
      s0.node.addTag(PMA.MAIN)
    })
  }

//  test("Coherent_E"){
//    tilelink.DebugId.setup(16)
//    SimConfig.withFstWave.compile(new Component{
//      val m0 = simpleMaster(coherentOnly)
//      val b0 = Node()
//      b0 << m0.node
//
//      val hub = new HubCrossbar()
//      hub.up << b0
//
//      val s0 = simpleSlave(16, 32)
//      s0.node at 0x10000 of hub.down
//      s0.node.addTag(PMA.MAIN)
//    }).doSim(seed = 42){dut =>
////      dut.clockDomain.forkStimulus(10)
////      testInterconnect(dut)
//    }
//  }

  test("scanRegions"){
    tilelink.DebugId.setup(16)
    testInterconnectAll(new Component{
      val m0 = simpleMaster(readWrite)
      val s0, s1 = simpleSlave(addressWidth = 10)
      val s2 = simpleSlave(addressWidth = 15)
      val b0, b1 = Node()
      b0 at 0xE0000 of m0.node
      s0.node at 0x4000 of b0
      s1.node at 0x6000 of b0
      b1 << b0
      s2.node at 0x4000 of b1

      s0.node.addTag(PMA.VOLATILE)
      s1.node.addTag(PMA.CACHED)

      Fiber build {
        var hits = 0
        val probed = MemoryConnection.getMemoryTransfers(m0.node)
        probed.foreach{ w =>
          w.node match {
            case s0.node =>
              hits += 1
              assert(w.where.transformers == List(OffsetTransformer(0xE0000), OffsetTransformer(0x4000)))
              assert(w.node.hasTag(PMA.VOLATILE))
              assert(w.mapping == SizeMapping(0xE4000, 0x400))
            case s1.node =>
              hits += 1
              assert(w.where.transformers == List(OffsetTransformer(0xE0000), OffsetTransformer(0x6000)))
              assert(w.node.hasTag(PMA.CACHED))
              assert(w.mapping == SizeMapping(0xE6000, 0x400))
            case s2.node =>
              hits += 1
              assert(w.where.transformers == List(OffsetTransformer(0xE0000), OffsetTransformer(0x4000)))
              assert(w.mapping == OrMapping(
                List(
                  SizeMapping(0xE4400, 0x1C00),
                  SizeMapping(0xE6400, 0x5C00)
                )
              ))
            case _ =>
          }
        }
        assert(hits == 3)
      }
    })
  }


  test("Soc_A"){
    testInterconnectAll(new Component{
      //A fictive CPU which has 2 memory bus, one from the data cache and one for peripheral accesses
      val cpu = new Area{
        val main = simpleMaster(coherentOnly, dataWidth = 64)
        val io = simpleMaster(readWrite)
      }

      val dma = new Area{
        val main = simpleMaster(readWrite, dataWidth = 64)
        val filter = new fabric.TransferFilter()
        filter.up << main.node
      }

      val n0 = Node()
      n0 << cpu.main.node
      n0 << cpu.io.node
      n0 << dma.filter.down

      val something = simpleSlave(20, 32, m2sTransfers = readWrite)
      something.node at 0x82000000l of n0

      //Will manage memory coherency
      val hub = new HubFabric()
      hub.up << n0

      //Define the main memory of the SoC (ex : DDR)
      val memory = simpleSlave(28, 32)
      memory.node.addTag(PMA.MAIN)
      memory.node at 0x80000000l of hub.down

      //Define all the peripherals / low performance stuff, ex uart, scratch ram, rom, ..
      val peripheral = new Area{
        val bus = Node()
        bus at 0x10000000 of hub.down

        val gpio = simpleSlave(12, 32)
        val uart = simpleSlave(12, 16)
        val spi = simpleSlave(12, 32)
        val memory = simpleSlave(16, 32)
        val rom = simpleReadOnlySlave(12, 8)

        memory.node.addTag(PMA.MAIN)
        rom.node.addTag(PMA.MAIN)

        gpio.node at 0x1000 of bus
        uart.node at 0x2000 of bus
        spi.node at 0x3000 of bus
        memory.node at 0x10000 of bus
        rom.node at 0x20000 of bus
      }

      //Will analyse the access capabilities of the CPU buses
      Fiber build new Area{
        val mainSupport = MemoryConnection.getMemoryTransfers(cpu.main.node)
        println("cpu.main.node can access : ")
        println(mainSupport.map("- " + _).mkString("\n"))
        val ioSupport = MemoryConnection.getMemoryTransfers(cpu.io.node)
        println("cpu.io.node can access : ")
        println(ioSupport.map("- " + _).mkString("\n"))
        val dmaSupport = MemoryConnection.getMemoryTransfers(dma.main.node)
        println("dma.main.node can access : ")
        println(dmaSupport.map("- " + _).mkString("\n"))
      }
    })
  }


  test("Advanced"){
    testInterconnectAll(new Component{
      val cdA = ClockDomain.external("cdA")

      val m0, m1 = simpleMaster(readWrite)

      //Define intermediate buses
      val b0,b1,b2 = Node()
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
        val ba0 = Node()
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
    })
  }
}


//TODO directed tests
