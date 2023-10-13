package spinal.lib.bus.tilelink.sim

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink.fabric.TransferFilterTag
import spinal.lib.sim.SparseMemory
import spinal.sim.SimError
import scala.collection.Seq

import scala.collection.mutable

object Checker{
  def apply(m : Monitor, mappings : Seq[Endpoint] = null)(implicit ordering : IdCallback)  : Checker = {
    val c = new Checker(m.bus.p, mappings)
    m.add(c)
    c
  }
  def apply(m : Monitor)  : Checker = {
    val c = new Checker(m.bus.p, Nil, checkMapping = false)(null)
    m.add(c)
    c
  }
}

class Checker(p : BusParameter, mappings : Seq[Endpoint], checkMapping : Boolean = true)(implicit idCallback : IdCallback) extends MonitorSubscriber {
  def this(m : Monitor, mappings : Seq[Endpoint])(implicit ordering : IdCallback) {
    this(m.bus.p, mappings)
    m.add(this)
  }

  def getMapping(address : BigInt, opcode : Any) : (Endpoint, Chunk) = {
    if(!checkMapping) return (null, null)
    for(endpoint <- mappings; chunk <- endpoint.chunks){
      if(chunk.allowed.allow(opcode) && chunk.mapping.hit(address)){
        return (endpoint, chunk)
      }
    }
    for(endpoint <- mappings; if endpoint.model == TransferFilterTag; chunk <- endpoint.chunks){
      if(chunk.mapping.hit(address)){
        return (endpoint, chunk)
      }
    }
    println(f"Can't map $address%x $opcode")
    ???
  }

  class InflightA(val a : TransactionA){
    val (endpoint, chunk) = getMapping(a.address, a.opcode)
    var ref : Array[Byte] = null
    var denied = false
    var isSet = false
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

  def doGrow(source : Int, address : BigInt, to : Int): Unit = {
//    if(address == 0x24c0) println(f"$source $address%04x grow $to $simTime")
    val cap = getCap(source, address)
    assert(cap.current > to)
    cap.current = to
    cap.probed = false
  }
  def doShrink(source : Int, address : BigInt, param : Int): Unit = {
    val (pFrom, pTo) = Param.reportPruneFromTo(param)
    val cap = getCap(source, address)
//    if(address == 0x24c0) println(f"$source $address%04x shrink $param $pFrom $pTo $cap $simTime")
//    assert(pFrom == cap.current) This can be asserted, as a probe TtoB followed by a release BtoN can be swapped by the interconnect

    val targetCap = pTo max cap.current
    if(targetCap == Param.Cap.toN){
      sourceToBlockCap(source).remove(address)
    } else {
      val cap = sourceToBlockCap(source)(address)
      cap.probed = false
      cap.current = targetCap
    }
  }

  override def onA(a: TransactionA) = {
    assert(inflightA(a.source) == null)
    assert((a.address & (a.bytes-1)) == 0, "Unaligned address :(")

    a.opcode match {
      case Opcode.A.PUT_FULL_DATA | Opcode.A.PUT_PARTIAL_DATA | Opcode.A.GET=>
      case Opcode.A.ACQUIRE_BLOCK | Opcode.A.ACQUIRE_PERM =>
    }

    val ctx = new InflightA(a)
    inflightA(a.source) = ctx

    if(checkMapping) ctx.endpoint.model match {
      case mem : SparseMemory =>
      case TransferFilterTag => {
        ctx.isSet = true
        ctx.denied = true
      }
    }

    if(checkMapping) idCallback.add(a.debugId, ctx){
      case o : OrderingArgs => {
        val address = ctx.chunk.globalToLocal(a.address + o.offset).toLong
        ctx.isSet = true
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
    import Opcode.B._
    import Opcode.C._
    c.opcode match {
      case RELEASE_DATA | RELEASE => {
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
  }


  override def onBeatC(c: TransactionC) = {
    //The reason why the need to update the endpoint models on each beat instead than on the whole transaction
    //is because some coherent agent may loop back C to D, and in the case that loopback only need the few first beat
    //D will finish before C
    if (checkMapping) {
      import Opcode.C._
      val (endpoint, chunk) = getMapping(c.address, c.opcode)
      val localAddress = chunk.globalToLocal(c.address).toLong
      endpoint.model match {
        case mem: SparseMemory => c.opcode match {
          case PROBE_ACK_DATA | RELEASE_DATA => mem.write(localAddress, c.data)
          case PROBE_ACK | RELEASE =>
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
        if(checkMapping) {
          assert(ctx.isSet, s"No reference was provided for :\n${ctx.a}to compare with :\n$d")
          if (d.withData && !ctx.denied) {
            assert(ctx.ref != null, s"No reference data was provided for :\n${ctx.a}to compare with :\n$d")
            assert((ctx.ref, d.data).zipped.forall(_ == _), s"Missmatch for :\n${ctx.a}\n$d\n!=${ctx.ref.map(v => f"${v}%02x").mkString(" ")}")
          }
          assert(d.denied == ctx.denied)
          assert(!d.corrupt)
        }
        if(d.opcode == Opcode.D.GRANT || d.opcode == Opcode.D.GRANT_DATA){
          doGrow(d.source, d.address, d.param)
        }
        inflightA(d.source) = null
        if(checkMapping) idCallback.remove(ctx.a.debugId, ctx)
      }
      case Opcode.D.RELEASE_ACK => {
        inflightC.remove(d.source -> d.address) match {
          case Some(c) => doShrink(d.source, d.address, c.param)
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
