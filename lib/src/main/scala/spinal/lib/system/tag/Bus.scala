package spinal.lib.system.tag

import spinal.core._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.bus.tilelink.{InterconnectNode, M2sTransfers}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object MemoryTransfers{
  def of(tags : SpinalTagReady) : Option[MemoryTransfers] = {
    tags.getTags().foreach{
      case e : MemoryTransferTag => return Some(e.get)
      case _ =>
    }
    None
  }
}

trait MemoryTransferTag extends SpinalTag{
  def get : MemoryTransfers
}

trait MemoryTransfers {
  def mincover(rhs: MemoryTransfers) : MemoryTransfers
  def intersect(rhs: MemoryTransfers) : MemoryTransfers
  def isEmpty : Boolean
  def nonEmpty : Boolean
}

trait MemoryConnection extends SpinalTag {
  def m : Nameable with SpinalTagReady
  def s : Nameable with SpinalTagReady
  def mapping :  AddressMapping
  def sToM(downs : MemoryTransfers, args : MappedNode) : MemoryTransfers

  def populate(): Unit ={
    m.addTag(this)
    s.addTag(this)
  }
}

object MappedNode{
  def apply(m : InterconnectNode) : MappedNode = MappedNode(m, 0, BigInt(1) << m.bus.p.addressWidth)
  def apply(node : Nameable with SpinalTagReady, address : BigInt, size : BigInt) : MappedNode = MappedNode(node, SizeMapping(address, size))
}

case class MappedNode(node : Nameable with SpinalTagReady, mapping : SizeMapping){
  def address = mapping.base
  def size = mapping.size

  override def toString = f"$node $address%x $size%x"

  def foreachSlave(body : (MappedNode, MemoryConnection) => Unit): Unit ={
    node.foreachTag{
      case c : MemoryConnection if c.m == node => {
        val (a,s) = c.mapping match {
          case DefaultMapping => (address, size)
          case m => (m.lowerBound, m.highestBound+1-m.lowerBound)
        }
        body(MappedNode(c.s, address+a, s), c)
      }
      case _ =>
    }
  }
}

object MemoryConnection{
  def getMemoryTransfers(m : InterconnectNode) : mutable.ArrayBuffer[(MappedNode, M2sTransfers)] = {
    m.await()
    getMemoryTransfers(MappedNode(m)).map{
      case e : (MappedNode, M2sTransfers) => e
    }
  }

  def getMemoryTransfers(args : MappedNode): ArrayBuffer[(MappedNode, MemoryTransfers)] ={
    // Stop on leafs
    if(!args.node.existsTag{
      case c : MemoryConnection if c.m == args.node => true
      case _ => false
    }) {
      return ArrayBuffer(args -> MemoryTransfers.of(args.node).get)
    }

    //Collect slaves supports
    val ret = ArrayBuffer[(MappedNode, MemoryTransfers)]()
    val unfiltred = mutable.LinkedHashMap[MappedNode, MemoryTransfers]()
    args.foreachSlave{ (s, c) =>
      val spec = getMemoryTransfers(s)
      val transformed = spec.map(e => e._1 -> c.sToM(e._2, e._1))
      for((who, what) <- transformed){
        unfiltred.get(who) match {
          case None => unfiltred(who) = what
          case Some(x) => unfiltred(who) = what.mincover(x)
        }
      }
    }

    //Filter the agregated slave supports with the current node capabilities
    MemoryTransfers.of(args.node) match {
      case None => unfiltred.foreach(e => ret += e._1 -> e._2)
      case Some(x) => unfiltred.foreach(e => ret += e._1 -> e._2.intersect(x))
    }

    ret.filter(_._2.nonEmpty)
  }

  def foreachSlave(m : InterconnectNode)(body : (MappedNode, MemoryConnection) => Unit): Unit = {
    m.await()
    MappedNode(m, 0, BigInt(1) << m.bus.p.addressWidth).foreachSlave(body)
  }

  def walk(m : InterconnectNode)(body : MappedNode => Unit): Unit = {
    m.await()
    walk(MappedNode(m, 0, BigInt(1) << m.bus.p.addressWidth))(body)
  }
  def walk(m : MappedNode)(body : MappedNode => Unit): Unit ={
    body(m)
    m.foreachSlave{(s,c) =>
      walk(s)(body)
    }
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
