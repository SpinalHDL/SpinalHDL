package spinal.lib.system.tag

import spinal.core._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.bus.tilelink.{InterconnectNode, M2sTransfers}
import spinal.lib.system.tag.MemoryConnection.WalkArgs

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait SupportedTransfers extends SpinalTag{
  def mincover(rhs: SupportedTransfers) : SupportedTransfers
  def intersect(rhs: SupportedTransfers) : SupportedTransfers
  def isEmpty : Boolean
  def nonEmpty : Boolean
  def getter() : SupportedTransfers = this
}

class SupportedTransfersLanda(val landa : () => SupportedTransfers) extends SupportedTransfers{
  def mincover(rhs: SupportedTransfers) : SupportedTransfers = landa().mincover(rhs)
  def intersect(rhs: SupportedTransfers) : SupportedTransfers = landa().intersect(rhs)
  def isEmpty : Boolean = landa().isEmpty
  def nonEmpty : Boolean = landa().nonEmpty
  override def getter() = landa()
}

trait MemoryConnection extends SpinalTag {
  def m : Nameable with SpinalTagReady
  def s : Nameable with SpinalTagReady
  def mapping :  AddressMapping
  def sToM(downs : SupportedTransfers, args : WalkArgs) : SupportedTransfers

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
        case Some(x) => ArrayBuffer(args -> x.asInstanceOf[SupportedTransfers].getter())
      }
    }

    //Collect slaves supports
    val ret = ArrayBuffer[(WalkArgs, SupportedTransfers)]()
    val unfiltred = mutable.LinkedHashMap[WalkArgs, SupportedTransfers]()
    args.foreachSlave{ (s, c) =>
      val spec = getSupportedTransfers(s)
      val transformed = spec.map(e => e._1 -> c.sToM(e._2, e._1))
      for((who, what) <- transformed){
        unfiltred.get(who) match {
          case None => unfiltred(who) = what
          case Some(x) => unfiltred(who) = what.mincover(x)
        }
      }
    }

    //Filter the agregated slave supports with the current node capabilities
    args.node.findTag(_.isInstanceOf[SupportedTransfers]) match {
      case None => unfiltred.foreach(e => ret += e._1 -> e._2)
      case Some(x) => unfiltred.foreach(e => ret += e._1 -> e._2.intersect(x.asInstanceOf[SupportedTransfers].getter()))
    }

    ret.filter(_._2.nonEmpty)
  }
  def getSupportedTransfers(m : InterconnectNode) : mutable.ArrayBuffer[(WalkArgs, M2sTransfers)] = getSupportedTransfers(WalkArgs(m)).map{
    case e : (WalkArgs, M2sTransfers) => e
  }
}

trait PMA extends SpinalTag

object PMA {
  object MAIN          extends PMA
  object IO            extends PMA
  object CACHED        extends PMA // an intermediate agent may have cached a copy of the region for you
  object TRACKED       extends PMA // the region may have been cached by another master, but coherence is being provided
  object UNCACHED      extends PMA // the region has not been cached yet, but should be cached when possible
  object IDEMPOTENT    extends PMA // reads return most recently put content, but content should not be cached
  object VOLATILE      extends PMA // content may change without a write
  object WRITE_EFFECTS extends PMA // writes produce side effects and so must not be combined/delayed
  object READ_EFFECTS  extends PMA // reads produce side effects and so must not be issued speculatively
  object EXECUTABLE    extends PMA // Allows an agent to fetch code from this region
}
