package spinal.lib.bus.tilelink.fabric.sim


import spinal.core.sim._
import spinal.core._
import spinal.core.fiber.{Fiber, hardFork}
import spinal.lib.bus.misc.{AddressMapping, InterleavedMapping, OffsetTransformer, OrMapping, SizeMapping, SizeMappingInterleaved}
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.sim._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.coherent.HubFiber
import spinal.lib.bus.tilelink.fabric._
import spinal.lib.sim.SparseMemory
import spinal.lib.system.tag.{MemoryConnection, PMA}
import spinal.sim.{SimError, SimThread}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.compat.Platform.EOL
import scala.util.{Failure, Success, Try}
import scala.collection.Seq


class TilelinkTester(cGen: => Component, simConfig : SpinalSimConfig = SimConfig){
  tilelink.DebugId.setup(16)
  val c = simConfig.compile(cGen)
  val nodes = ArrayBuffer[Node]()
  val orderings = ArrayBuffer[OrderingTag]()
  c.report.toplevel.walkComponents(_.foreachTag {
    case t: Node => nodes += t
    case t: OrderingTag => orderings += t
    case _ =>
  })

  val separator = "\n" + "-" * 80 + "\n"
  val errors = new StringBuilder()
  var noStall = false

  def doSim(name: String)(body: TilelinkTestbenchBase => Unit): Unit = {
    Try {
      c.doSim(name, 42) { dut =>
        implicit val idAllocator = new IdAllocator(DebugId.width)
        implicit val idCallback = new IdCallback
        for (i <- 0 until DebugId.space.reserved) idAllocator.allocate(i)
        val tb = new TilelinkTestbenchBase(nodes, orderings)
        val cds = nodes.map(_.clockDomain).distinct
        cds.foreach(_.forkStimulus(simRandom.nextInt(40) + 10))
        var timeout = 0
        cds.head.onSamplings {
          timeout += 1
          if (timeout == 10000) {
              SimError("Timeout")
          }
        }
        tb.mastersStuff.foreach(_.monitor.add(new MonitorSubscriber {
          override def onA(a: TransactionA) = {
            timeout = 0
          }
        }))
        if(noStall){
          tb.mastersStuff.foreach(_.agent.driver.driver.noStall())
          tb.slavesStuff.foreach(_.model.driver.driver.noStall())
        }
        body(tb)
      }
    } match {
      case Success(_) =>
      case Failure(e) => errors ++= s"$separator$name FAILED !!! with :\n" + e + EOL + e.getStackTrace().mkString("", EOL, EOL) + separator
    }
  }

  def doSimDirected(name: String)(body: MasterDebugTester => Unit): Unit = {
    doSim(name) { tb =>
      val tester = new MasterDebugTester((tb.masterSpecs, tb.mastersStuff).zipped.map((s, t) => new MasterDebugTesterElement(s, t.agent)))
      body(tester)
    }
  }

  def checkErrors(): Unit = {
    if (errors.nonEmpty) {
      System.err.println(errors.toString())
      throw new Exception("Some tests failed")
    }
  }

  def testAll(){
    val emits = nodes.map(_.bus.p.node.m.emits).reduce(_ mincover _)
    if (emits.get.some) doSimDirected("get")(_.coverGet(2))
    if (emits.putFull.some) doSimDirected("putf")(_.coverPutFullData(2))
    if (emits.putPartial.some) doSimDirected("putp")(_.coverPutPartialData(2))
    if (emits.get.some) doSimDirected("getPut") { t => t.coverPutFullData(2); t.coverPutPartialData(2); t.coverGet(2) }
    if (emits.acquireB.some) doSimDirected("acquireB")(_.coverAcquireB(8))
    if (emits.acquireT.some) doSimDirected("acquireT")(_.coverAcquireT(8))
    if (emits.withBCE) doSimDirected("acquireBT")(_.coverAcquireBT(8))
    if (emits.withBCE) doSimDirected("acquireTB")(_.coverAcquireTB(8))
    if (emits.withBCE) doSimDirected("acquirePerm")(_.coverAcquirePerm(8))
    if (emits.withBCE) doSimDirected("coherencyBx2")(_.coverCoherencyBx2(8))
    if (emits.withBCE) doSimDirected("coherencyTx2")(_.coverCoherencyTx2(8))
    if (emits.withBCE) doSimDirected("coherencyT_B")(_.coverCoherencyT_B(8))
    if (emits.withBCE) doSimDirected("coherencyBx2_T_Bx2")(_.coverCoherencyBx2_T_Bx2(8))
    doSim("randomized") { tb =>
      val testers = (tb.masterSpecs, tb.mastersStuff).zipped.map((s, t) => new MasterTester(s, t.agent))
      testers.foreach(_.startPerSource(100))
      testers.foreach(_.join())
      tb.waitCheckers()
      tb.assertCoverage()
    }

    checkErrors()
  }
}

class TilelinkTestbenchBase(nodes: Seq[Node], orderings: Seq[OrderingTag])(implicit idAllocator: IdAllocator, idCallback: IdCallback) extends Area {
  val nodeToModel = mutable.LinkedHashMap[Node, SparseMemory]()
  val slaveNodes = nodes.filter(_.bus.isMasterInterface)
  val masterNodes = nodes.filter(_.bus.isSlaveInterface)

  for (o <- orderings) o.cd.onSamplings {
    if (o.cmd.valid.toBoolean) {
      idCallback.call(o.cmd.debugId.toLong)(new OrderingArgs(0, o.cmd.bytes.toInt))
    }
  }

  for (node <- slaveNodes) {
    nodeToModel(node) = SparseMemory(simRandom.nextInt())
  }

  for (node <- nodes) {
    node.refOwner match {
      case r: RamFiber => nodeToModel(node) = SparseMemory(42)
      case _ =>
    }
  }

  val masterSpecs = masterNodes.map(n => {
    val mappings = ArrayBuffer[Endpoint]()
    val suportedTransfers = MemoryConnection.getMemoryTransfers(n)
    for (e <- suportedTransfers) {
      e.node match {
        case n: Node => {
          nodeToModel.get(n) match {
            case Some(m) => {
              e.transfers match {
                case t: M2sTransfers => mappings += Endpoint(m, List(new Chunk(t, e.where.mapping, e.where.transformers)))
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

  val mastersStuff = for (m <- masterSpecs) yield new Area {
    val agent = new MasterAgent(m.bus, m.cd)
    val monitor = new Monitor(m.bus, m.cd)
    val checker = new Checker(monitor, m.endpoints)
  }

  val slavesStuff = for (node <- slaveNodes) yield new Area {
    val model = new MemoryAgent(node.bus, node.clockDomain, nodeToModel(node).seed)
  }

  def waitCheckers(): Unit = {
    mastersStuff.foreach(_.checker.waitEmpty())
  }

  def assertCoverage(): Unit = {
    for (s <- slavesStuff) {
      assert(s.model.monitor.counterA > 100)
    }
  }
}