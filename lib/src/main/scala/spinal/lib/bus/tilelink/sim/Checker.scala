package spinal.lib.bus.tilelink.sim

import spinal.core.ClockDomain
import spinal.lib.bus.tilelink._
import spinal.lib.sim.SparseMemory

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
        }
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
      case Opcode.D.RELEASE_ACK => ???
    }
  }
  override def onB(b: TransactionB) = {
    //TODO
  }
  override def onC(c: TransactionC) = {
    //TODO check that inflight trafic is ok
    assert((c.address & (c.bytes-1)) == 0, "Unaligned address :(")
    val mapping = getMapping(c.address)
    val base = mapping.mapping.map(_.base).min.toLong
    mapping.model match {
      case mem : SparseMemory => c.opcode match {
        case Opcode.C.RELEASE_DATA => {
          mem.write(c.address.toLong-base, c.data)
        }
        case Opcode.C.PROBE_ACK_DATA=> {
          mem.write(c.address.toLong-base, c.data)
        }
        case Opcode.C.PROBE_ACK=> {

        }
        case Opcode.C.RELEASE=> {

        }
      }
    }
  }
  override def onE(e: TransactionE) = {
    //TODO
  }
}
