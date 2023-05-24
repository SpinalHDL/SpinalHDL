package spinal.lib.system.tag

import spinal.core._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.bus.tilelink.InterconnectNode

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait SupportedTransfers extends SpinalTag{
  type T <: SupportedTransfers
  def mincover(rhs: T) : T
  def intersect(rhs: T) : T
}

trait MemoryConnection extends SpinalTag {
  def m : Nameable with SpinalTagReady
  def s : Nameable with SpinalTagReady
  def mapping :  AddressMapping
  def sToM(downs : SupportedTransfers) : SupportedTransfers

  def populate(): Unit ={
    m.addTag(this)
    s.addTag(this)
  }
}


object MemoryConnection{
  def WalkArgs(m : InterconnectNode) : WalkArgs = WalkArgs(m, 0, BigInt(1) << m.bus.p.addressWidth)
  case class WalkArgs(node : Nameable with SpinalTagReady, address : BigInt, size : BigInt){
    override def toString = f"$node $address%x $size%x"

    def foreachSlave(body : (WalkArgs, MemoryConnection) => Unit): Unit ={
      node.foreachTag{
        case c : MemoryConnection if c.m == node => {
          val (a,s) = c.mapping match {
            case DefaultMapping => (address, size)
            case m => (m.lowerBound, m.highestBound+1-m.lowerBound)
          }
          body(new WalkArgs(c.s, address+a, s), c)
        }
        case _ =>
      }
    }
  }



  def foreachSlave(m : InterconnectNode)(body : (WalkArgs, MemoryConnection) => Unit): Unit = WalkArgs(m, 0, BigInt(1) << m.bus.p.addressWidth).foreachSlave(body)

  def nodes(m : InterconnectNode): Seq[WalkArgs] = nodes(m, 0, BigInt(1) << m.bus.p.addressWidth)
  def nodes(m : Nameable with SpinalTagReady, address : BigInt, size : BigInt): Seq[WalkArgs] ={
    val l = mutable.LinkedHashMap[Nameable with SpinalTagReady, WalkArgs]()
    walk(m, address, size){ args =>
      l.get(args.node) match {
        case Some(e) => assert(e == args)
        case None => l(args.node) = args
      }
    }
    l.values.toList
  }
  def walk(m : InterconnectNode)(body : WalkArgs => Unit): Unit = walk(m, 0, BigInt(1) << m.bus.p.addressWidth)(body)
  def walk(m : Nameable with SpinalTagReady, address : BigInt, size : BigInt)(body : WalkArgs => Unit): Unit ={
    //println(m.getName() + f" at 0x$address%x over 0x$size%x")
    body(WalkArgs(m, address, size))
    m.foreachTag{
      case c : MemoryConnection if c.m == m => {
        val (a,s) = c.mapping match {
          case DefaultMapping => (address, size)
          case m => (m.lowerBound, m.highestBound+1-m.lowerBound)
        }
        walk(c.s, address+a, s)(body)
      }
      case _ =>
    }
  }

  def getSupportedTransfers(args : WalkArgs): ArrayBuffer[(WalkArgs, SupportedTransfers)] ={
    // Stop on leafs
    if(!args.node.existsTag{
      case c : MemoryConnection if c.m == args.node => true
      case _ => false
    }) {
      return args.node.findTag(_.isInstanceOf[SupportedTransfers]) match {
        case Some(x) => ArrayBuffer(args -> x.asInstanceOf[SupportedTransfers])
      }
    }

    val ret = ArrayBuffer[(WalkArgs, SupportedTransfers)]()
    val unfiltred = mutable.LinkedHashMap[WalkArgs, SupportedTransfers]()
    args.foreachSlave{ (s, c) =>
      val spec = getSupportedTransfers(s)
      val transformed = spec.map(e => e._1 -> c.sToM(e._2))
      for((who, what) <- transformed){
        unfiltred.get(who) match {
          case None => unfiltred(who) = what
          case Some(x) => unfiltred(who) = what.mincover(x.asInstanceOf[what.T])
        }
      }
    }
    
    args.node.findTag(_.isInstanceOf[SupportedTransfers]) match {
      case None => unfiltred.foreach(e => ret += e._1 -> e._2)
      case Some(x) => unfiltred.foreach(e => ret += e._1 -> e._2.intersect(x.asInstanceOf[e._2.T]))
    }

    ret
  }
  def getSupportedTransfers(m : InterconnectNode) : mutable.ArrayBuffer[(WalkArgs, SupportedTransfers)] = getSupportedTransfers(WalkArgs(m))
}

trait PMA extends SpinalTag

object PMA {
  case object CACHED        extends PMA // an intermediate agent may have cached a copy of the region for you
  case object TRACKED       extends PMA // the region may have been cached by another master, but coherence is being provided
  case object UNCACHED      extends PMA // the region has not been cached yet, but should be cached when possible
  case object IDEMPOTENT    extends PMA // reads return most recently put content, but content should not be cached
  case object VOLATILE      extends PMA // content may change without a write
  case object WRITE_EFFECTS extends PMA // writes produce side effects and so must not be combined/delayed
  case object READ_EFFECTS  extends PMA // reads produce side effects and so must not be issued speculatively
  case object EXECUTABLE    extends PMA // Allows an agent to fetch code from this region
}
