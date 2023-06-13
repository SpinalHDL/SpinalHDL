package spinal.lib.bus.tilelink.sim

import spinal.core.ClockDomain
import spinal.lib.bus.tilelink._
import spinal.lib.sim.SparseMemory
import spinal.sim.SimError

import scala.collection.mutable

object Checker{
  def apply(m : Monitor, mappings : Seq[Mapping] = null)(implicit ordering : IdCallback)  : Checker = {
    val c = new Checker(m.bus.p, mappings)
    m.add(c)
    c
  }
}

class Checker(p : BusParameter, mappings : Seq[Mapping])(implicit idCallback : IdCallback) extends MonitorSubscriber {
  def this(m : Monitor, mappings : Seq[Mapping])(implicit ordering : IdCallback) {
    this(m.bus.p, mappings)
    m.add(this)
  }

  def getMapping(address : BigInt) = mappings.find(_.mapping.exists(_.hit(address))).get

  class InflightA(val a : TransactionA){
    val mapping = getMapping(a.address)
    var ref : Array[Byte] = null
  }

  val inflightA = Array.fill[InflightA](1 << p.sourceWidth)(null)
  val inflightB = mutable.LinkedHashMap[(Int, BigInt), TransactionB]()
  val inflightC = mutable.LinkedHashMap[(Int, BigInt), TransactionC]()
  val inflightD = mutable.LinkedHashMap[BigInt, TransactionD]()

  override def onA(a: TransactionA) = {
    assert(inflightA(a.source) == null)
    assert((a.address & (a.bytes-1)) == 0, "Unaligned address :(")
    val ctx = new InflightA(a)
    inflightA(a.source) = ctx
    idCallback.add(a.debugId, ctx){
      case o : OrderingArgs => ctx.mapping.model match {
        case mem : SparseMemory => a.opcode match {
          case Opcode.A.GET => {
            ctx.ref = mem.readBytes(o.address.toInt, o.bytes)
          }
          case Opcode.A.PUT_PARTIAL_DATA=> {
            mem.write(o.address.toInt, a.data, a.mask)
          }
          case Opcode.A.PUT_FULL_DATA=> {
            assert(a.mask.forall(v => v))
            mem.write(o.address.toInt, a.data, a.mask)
          }
          case Opcode.A.ACQUIRE_BLOCK => {
            ctx.ref = mem.readBytes(o.address.toInt, o.bytes)
          }
          case Opcode.A.ACQUIRE_PERM =>
        }
      }
    }
  }

  override def onB(b: TransactionB) = {
    val key = b.source -> b.address
    assert(!inflightB.contains(key))
    inflightB(key) = b
  }

  override def onC(c: TransactionC) = {
    assert((c.address & (c.bytes-1)) == 0, "Unaligned address :(")
    val mapping = getMapping(c.address)
    val base = mapping.mapping.map(_.base).min.toLong
    import Opcode.B._
    import Opcode.C._
    c.opcode match {
      case RELEASE_DATA | RELEASE => {
        val key = c.source -> c.address
        assert(!inflightC.contains(key))
        inflightC(key) = c
      }
      case PROBE_ACK | PROBE_ACK_DATA => {
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
    mapping.model match {
      case mem : SparseMemory => c.opcode match {
        case PROBE_ACK_DATA | RELEASE_DATA => mem.write(c.address.toLong-base, c.data)
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
        if(d.withData) assert((ctx.ref, d.data).zipped.forall(_ == _), s"Missmatch for :\n$ctx.a\n$d\n!=${ctx.ref.map(v => f"${v}%02x").mkString(" ")}")
        assert(!d.denied)
        assert(!d.corrupt)
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
