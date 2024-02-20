package spinal.lib.system.tag

import spinal.core._
import spinal.lib.bus.misc.{AddressMapping, AddressTransformer, DefaultMapping, OffsetTransformer, SizeMapping}
import spinal.lib.bus.tilelink.M2sTransfers
import spinal.lib.bus.tilelink.fabric.{Node, TransferFilterTag}

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

//Address seen by the slave slave are mapping.foreach(_.base-offset)
trait MemoryConnection extends SpinalTag {
  def up : Nameable with SpinalTagReady
  def down : Nameable with SpinalTagReady
  def mapping : AddressMapping //Specify the memory mapping of the slave from the master address (before transformers are applied)
  def transformers : List[AddressTransformer]  //List of alteration done to the address on this connection (ex offset, interleaving, ...)
  def sToM(downs : MemoryTransfers, args : MappedNode) : MemoryTransfers = downs//Convert the slave MemoryTransfers capabilities into the master ones

  def populate(): Unit ={
    up.addTag(this)
    down.addTag(this)
  }
}

object MappedNode{
  def apply(m : Node) : MappedNode = MappedNode(m, Nil, 0, BigInt(1) << m.bus.p.addressWidth)
  def apply(node : Nameable with SpinalTagReady, transformers: List[AddressTransformer], address : BigInt, size : BigInt) : MappedNode = MappedNode(node, SizeMapping(address, size), transformers)
}

/**
 *
 * @param node What is the endpoint
 * @param localOffset What is the master address to access address 0 of that endpoint
 * @param mapping Range of master address which can access the node
 */
case class MappedNode(node : Nameable with SpinalTagReady, mapping : AddressMapping, transformers : List[AddressTransformer]){
//  def address = mapping.base
//  def size = mapping.size

  def remap(transformers : List[AddressTransformer]) : MappedNode = MappedNode(node, transformers.foldRight(mapping)((t,m) => m.withOffsetInvert(t)), transformers ++ this.transformers)//
  def remap(offset : BigInt) : MappedNode = MappedNode(node, mapping.withOffset(offset), OffsetTransformer(offset) :: transformers)
  override def toString = f"$node mapped=$mapping through=$transformers "

  def foreachSlave(body : (MappedNode, MemoryConnection) => Unit): Unit ={
    node.foreachTag{
      case c : MemoryConnection if c.up == node => {
        val remaped = c.transformers.foldRight(c.mapping)((t, a) => a.withOffset(t))
        body(MappedNode(c.down, remaped, Nil), c)
      }
      case _ =>
    }
  }
}

case class MappedTransfers(where : MappedNode, transfers: MemoryTransfers){
  def node = where.node
  def mapping = where.mapping
}

object MemoryConnection{
  def getMemoryTransfers(m : Node) : mutable.ArrayBuffer[MappedTransfers] = {
    m.await()
    getMemoryTransfers(MappedNode(m))
  }

  def getMemoryTransfers(args : MappedNode): ArrayBuffer[MappedTransfers] ={
    // Stop on leafs
    if(!args.node.existsTag{
      case c : MemoryConnection if c.up == args.node => true
      case _ => false
    }) {
      val elem = MemoryTransfers.of(args.node).get
      return ArrayBuffer(MappedTransfers(args, elem))
    }

    //Collect slaves supports
    val ret = ArrayBuffer[MappedTransfers]()
    val unfiltred = mutable.LinkedHashMap[MappedNode, MemoryTransfers]() //The HashMap will allow handle a bus to fork RO WO and join later do a RW join. Will only work for exactly similar mappings
    args.foreachSlave{ (s, c) =>
      val spec = getMemoryTransfers(s)
      val transformed = for(e <- spec) yield {
        val remapped = e.where.remap(c.transformers) // c.offset Give the same address view point as the "args" (master)
        val filtred = remapped.copy( // Will handle partial mapping and stuff as InterleavedMapping
          mapping = c.mapping.intersect(remapped.mapping)
        )
        val mt = c.sToM(e.transfers, e.where)
        filtred -> mt
      }
      for((who, what) <- transformed){
        unfiltred.get(who) match {
          case None => unfiltred(who) = what
          case Some(x) => unfiltred(who) = what.mincover(x)
        }
      }
    }

    //Filter the agregated slave supports with the current node capabilities
    MemoryTransfers.of(args.node) match {
      case None => unfiltred.foreach(e => ret += MappedTransfers(e._1, e._2))
      case Some(x) => unfiltred.foreach(e => ret += MappedTransfers(e._1, e._2.intersect(x)))
    }

    ret.filter(e => e.transfers.nonEmpty || e.where.node.hasTag(TransferFilterTag))
  }

  def foreachSlave(m : Node)(body : (MappedNode, MemoryConnection) => Unit): Unit = {
    m.await()
    MappedNode(m, Nil, 0, BigInt(1) << m.bus.p.addressWidth).foreachSlave(body)
  }

//  def walk(m : InterconnectNode)(body : MappedNode => Unit): Unit = {
//    m.await()
//    walk(MappedNode(m, 0, BigInt(1) << m.bus.p.addressWidth))(body)
//  }
//  def walk(m : MappedNode)(body : MappedNode => Unit): Unit ={
//    body(m)
//    m.foreachSlave{(s,c) =>
//      walk(s)(body)
//    }
//  }
}

trait PMA extends SpinalTag

object PMA {
  object MAIN          extends PMA
  object IO            extends PMA
  object CACHABLE      extends PMA // an intermediate agent may have cached a copy of the region for you
  object TRACEABLE     extends PMA // the region may have been cached by another master, but coherence is being provided
  object UNCACHABLE    extends PMA // the region has not been cached yet, but should be cached when possible
  object IDEMPOTENT    extends PMA // reads return most recently put content, but content should not be cached
  object EXECUTABLE    extends PMA // Allows an agent to fetch code from this region
  object VOLATILE      extends PMA // content may change without a write
  object WRITE_EFFECTS extends PMA // writes produce side effects and so must not be combined/delayed
  object READ_EFFECTS  extends PMA // reads produce side effects and so must not be issued speculatively
}
