package spinal.lib.bus.tilelink.sim

import spinal.core.ClockDomain
import spinal.lib.bus.tilelink._

object Checker{
  def apply(m : Monitor) : Checker = {
    val c = new Checker(m.bus.p)
    m.add(c)
    c
  }
}

class Checker(p : BusParameter, mappings : Seq[Mapping] = null) extends MonitorSubscriber {
  def this(m : Monitor) {
    this(m.bus.p)
    m.add(this)
  }

  class InflightA(val a : TransactionA){

  }
  val inflightA = Array.fill[InflightA](1 << p.sourceWidth)(null)
  override def onA(a: TransactionA) = {
    assert(inflightA(a.source) == null)
//    inflightA(a.source) = new InflightA(a)
//    DebugId.manager.add(a.debugId) =
  }
  override def onB(b: TransactionB) = ???
  override def onC(c: TransactionC) = ???
  override def onD(d: TransactionD) = {
    d.opcode match{
      case Opcode.D.ACCESS_ACK | Opcode.D.ACCESS_ACK_DATA =>  {
        val ctx = inflightA(d.source)
        assert(ctx != null)
        d.assertRspOf(ctx.a)
        inflightA(d.source) = null
      }
      case Opcode.D.GRANT | Opcode.D.GRANT_DATA | Opcode.D.RELEASE_ACK => ???
    }
  }
  override def onE(e: TransactionE) = ???
}
