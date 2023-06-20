package spinal.lib.bus.tilelink.sim

import spinal.core.ClockDomain
import spinal.lib.bus.tilelink._
import spinal.lib.sim.SparseMemory
import spinal.sim.SimError

import scala.collection.mutable

object Checker{
  def apply(m : Monitor, mappings : Seq[Endpoint] = null)(implicit ordering : IdCallback)  : Checker = {
    val c = new Checker(m.bus.p, mappings)
    m.add(c)
    c
  }
}

class Checker(p : BusParameter, mappings : Seq[Endpoint])(implicit idCallback : IdCallback) extends MonitorSubscriber {
  def this(m : Monitor, mappings : Seq[Endpoint])(implicit ordering : IdCallback) {
    this(m.bus.p, mappings)
    m.add(this)
  }

  def getMapping(address : BigInt, opcode : Any) : (Endpoint, Chunk) = {
    for(endpoint <- mappings; chunk <- endpoint.chunks){
      if(chunk.allowed.allow(opcode) && chunk.mapping.hit(address)){
        return (endpoint, chunk)
      }
    }
    ???
  }

  class InflightA(val a : TransactionA){
    val (endpoint, chunk) = getMapping(a.address, a.opcode)
    var ref : Array[Byte] = null
  }

  val inflightA = Array.fill[InflightA](1 << p.sourceWidth)(null)
  val inflightB = mutable.LinkedHashMap[(Int, BigInt), TransactionB]()
  val inflightC = mutable.LinkedHashMap[(Int, BigInt), TransactionC]()
  val inflightD = mutable.LinkedHashMap[BigInt, TransactionD]()

  case class Cap(var current : Int, var probed : Boolean)
  val mToBlockCap = mutable.LinkedHashMap[M2sAgent, mutable.LinkedHashMap[BigInt, Cap]]()
  val sourceToBlockCap = new Array[mutable.LinkedHashMap[BigInt, Cap]](1 << p.sourceWidth)
  for(m <- p.node.m.masters){
    val map = mutable.LinkedHashMap[BigInt, Cap]()
    mToBlockCap(m) = map
    for(source <- m.mapping){
      source.id.foreach{sourceId =>
        sourceToBlockCap(sourceId.toInt) = map
      }
    }
  }

  def getCap(source : Int, address : BigInt) = sourceToBlockCap(source).getOrElseUpdate(address, Cap(Param.Cap.toN, false))
  def checkGrow(source : Int, address : BigInt, param : Int): Unit = {
//    val (pFrom, pTo) = Param.Grow.fromTo(param)
//    val from = getCap(source, address)
//    assert(pFrom == from)
  }
  def doGrow(source : Int, address : BigInt, to : Int): Unit = {
//    val cap = getCap(source, address)
//    assert(cap.current >= to)
//    cap.current = to
//    cap.probed = false
  }
  def doShrink(source : Int, address : BigInt, param : Int): Unit = {
//    val (pFrom, pTo) = Param.reportPruneFromTo(param)
//    val cap = getCap(source, address)
//    assert(pFrom == cap.current || cap.probed)
//
//    if(pTo == Param.Cap.toN){
//      sourceToBlockCap(source).remove(address)
//    } else {
//      val cap = sourceToBlockCap(source)(address)
//      cap.probed = false
//      cap.current = pTo
//    }
  }

  override def onA(a: TransactionA) = {
    assert(inflightA(a.source) == null)
    assert((a.address & (a.bytes-1)) == 0, "Unaligned address :(")

    a.opcode match {
      case Opcode.A.PUT_FULL_DATA | Opcode.A.PUT_PARTIAL_DATA | Opcode.A.GET=>
      case Opcode.A.ACQUIRE_BLOCK | Opcode.A.ACQUIRE_PERM => //checkGrow(a.source, a.address, a.param)
    }

    val ctx = new InflightA(a)
    inflightA(a.source) = ctx
    idCallback.add(a.debugId, ctx){
      case o : OrderingArgs => {
        val address = ctx.chunk.globalToLocal(a.address + o.offset).toLong
        ctx.endpoint.model match {
          case mem : SparseMemory => a.opcode match {
            case Opcode.A.GET => {
              ctx.ref = mem.readBytes(address, o.bytes)
            }
            case Opcode.A.PUT_PARTIAL_DATA=> {
              mem.write(address, a.data, a.mask)
            }
            case Opcode.A.PUT_FULL_DATA=> {
              assert(a.mask.forall(v => v))
              mem.write(address, a.data, a.mask)
            }
            case Opcode.A.ACQUIRE_BLOCK => {
              ctx.ref = mem.readBytes(address, o.bytes)
            }
            case Opcode.A.ACQUIRE_PERM =>
          }
        }
      }
    }
  }

  override def onB(b: TransactionB) = {
    val key = b.source -> b.address
    assert(!inflightB.contains(key))
    inflightB(key) = b

    b.opcode match {
      case Opcode.B.PROBE_PERM | Opcode.B.PROBE_BLOCK => {
        val cap = getCap(b.source, b.address)
        cap.probed = true

      }
    }
  }

  override def onC(c: TransactionC) = {
    assert((c.address & (c.bytes-1)) == 0, "Unaligned address :(")
    val (endpoint, chunk) = getMapping(c.address, c.opcode)
    val localAddress = chunk.globalToLocal(c.address).toLong
    import Opcode.B._
    import Opcode.C._
    c.opcode match {
      case RELEASE_DATA | RELEASE => {
        doShrink(c.source, c.address, c.param)
        val key = c.source -> c.address
        assert(!inflightC.contains(key))
        inflightC(key) = c
      }
      case PROBE_ACK | PROBE_ACK_DATA => {
        doShrink(c.source, c.address, c.param)
        val key = c.source -> c.address
        inflightB.get(key) match {
          case Some(b) =>
            assert(b.opcode == PROBE_BLOCK || b.opcode == PROBE_PERM)
            if(c.opcode == PROBE_ACK_DATA) assert(b.opcode != PROBE_PERM)
          case None => SimError("spawned probe ack")
        }
        inflightB.remove(key)
      }
    }
    endpoint.model match {
      case mem : SparseMemory => c.opcode match {
        case PROBE_ACK_DATA | RELEASE_DATA => mem.write(localAddress, c.data)
        case PROBE_ACK | RELEASE =>
      }
    }
  }


  override def onD(d: TransactionD) = {
    d.opcode match{
      case Opcode.D.ACCESS_ACK | Opcode.D.ACCESS_ACK_DATA | Opcode.D.GRANT | Opcode.D.GRANT_DATA  =>  {
        val ctx = inflightA(d.source)
        assert(ctx != null)
        d.assertRspOf(ctx.a)
        if(d.withData) {
          assert(ctx.ref != null, s"No reference data was provided for :\n${ctx.a}to compare with :\n$d")
          assert((ctx.ref, d.data).zipped.forall(_ == _), s"Missmatch for :\n$ctx.a\n$d\n!=${ctx.ref.map(v => f"${v}%02x").mkString(" ")}")
        }
        assert(!d.denied)
        assert(!d.corrupt)
        if(d.opcode == Opcode.D.GRANT || d.opcode == Opcode.D.GRANT_DATA){
          doGrow(d.source, d.address, d.param)
        }
        inflightA(d.source) = null
        idCallback.remove(ctx.a.debugId, ctx)
      }
      case Opcode.D.RELEASE_ACK => {
        inflightC.remove(d.source -> d.address) match {
          case Some(c) =>
        }
      }
    }

    d.opcode match{
      case Opcode.D.ACCESS_ACK | Opcode.D.ACCESS_ACK_DATA | Opcode.D.RELEASE_ACK=>
      case Opcode.D.GRANT | Opcode.D.GRANT_DATA  =>  {
        assert(!inflightD.contains(d.sink))
        inflightD(d.sink) = d
      }
    }
  }

  override def onE(e: TransactionE) = {
    inflightD.remove(e.sink) match {
      case Some(e) =>
    }
  }

  def isEmpty() = {
    inflightA.forall(_ == null) && inflightB.isEmpty && inflightC.isEmpty && inflightD.isEmpty
  }
  def waitEmpty(delay : => Unit = spinal.core.sim.sleep(1000)): Unit ={
    while(!isEmpty()){
      delay
    }
  }
}
