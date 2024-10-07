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

trait MemoryEndpoint extends SpinalTag{
  def mapping: AddressMapping
}

class MemoryEndpointTag(body : => AddressMapping) extends MemoryEndpoint{
  override def mapping: AddressMapping = body
}

class VirtualEndpoint(val up : Nameable with SpinalTagReady, val mapping : AddressMapping) extends Area with SpinalTagReady  {
  val self = this
  new MemoryConnection {
    override def up = self.up
    override def down = self
    override def transformers = Nil
    populate()
  }
  addTag(new MemoryTransferTag{
    override def get: MemoryTransfers = MemoryTransfers.of(up).get
  })
  addTag(new MemoryEndpoint {
    override def mapping = self.mapping
  })
}

trait PmaRegion{
  def mapping : AddressMapping
  def transfers: MemoryTransfers
  def isMain : Boolean
  def isIo : Boolean = !isMain
  def isExecutable : Boolean
}

case class PmaRegionImpl(mapping: AddressMapping,
                         transfers: MemoryTransfers,
                         isMain: Boolean,
                         isExecutable: Boolean) extends PmaRegion {
}


//Address seen by the slave slave are mapping.foreach(_.base-offset)
trait MemoryConnection extends SpinalTag {
  def up : Nameable with SpinalTagReady
  def down : Nameable with SpinalTagReady
  def transformers : List[AddressTransformer]  //List of alteration done to the address on this connection (ex offset, interleaving, ...)
  def sToM(downs : MemoryTransfers, args : MappedNode) : MemoryTransfers = downs//Convert the slave MemoryTransfers capabilities into the master ones
  def sToM(down : AddressMapping) : AddressMapping = down //Convert the slave MemoryMapping capabilities into the master ones

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

  def isMain = node.hasTag(PMA.MAIN)
  def isExecutable = node.hasTag(PMA.EXECUTABLE)
}

case class MappedTransfers(where : MappedNode, transfers: MemoryTransfers) extends PmaRegion{
  def node = where.node
  def mapping = where.mapping
  def isMain = where.isMain
  def isExecutable = where.isExecutable
}

object MemoryConnection{
  def getMemoryTransfers(m: Node): mutable.ArrayBuffer[MappedTransfers] = {
    m.await()
    getMemoryTransfers(m.asInstanceOf[Nameable with SpinalTagReady])
  }

  def getMemoryTransfers(up : Nameable with SpinalTagReady): ArrayBuffer[MappedTransfers] = {
    val mc = up.getTags().collect {
      case c: MemoryConnection if c.up == up => c
    }
    getMemoryTransfers(up, mc.toSeq)
  }


  def getMemoryTransfers(up : Nameable with SpinalTagReady, downs : Seq[MemoryConnection]): ArrayBuffer[MappedTransfers] = {
    val ret = ArrayBuffer[MappedTransfers]()

    // Stop on leafs
    if (downs.isEmpty) {
      val mapping = up.getTags().collectFirst { case t: MemoryEndpoint => t } match {
        case Some(ep) => ep.mapping
        case None => {
          up match {
            case up: Node => SizeMapping(0, BigInt(1) << up.m2s.parameters.addressWidth) //backward compatibility
            case _ => throw new Exception(s"Missing enpoint on $up")
          }
        }
      }
      ret += new MappedTransfers(
        where = new MappedNode(up, mapping, Nil),
        transfers = MemoryTransfers.of(up).get
      )
      return ret
    }

    //Collect slaves supports
    for(c <- downs) {
      val dmt = getMemoryTransfers(c.down)
      val invertTransform = c.transformers.reverse
      val remapped = dmt.map { e =>
        val transformed = invertTransform.foldRight(e.mapping)((t, a) => a.withOffsetInvert(t))
        val filtred = c.sToM(transformed)
        val where = new MappedNode(
          e.node,
          filtred,
          c.transformers ++ e.where.transformers
        )
        val transfers = c.sToM(e.transfers, e.where)
        new MappedTransfers(where, transfers)
      }

      //Let's merge entries which target the same endpoint
      val aggreged = mutable.LinkedHashMap[MappedNode, MemoryTransfers]()
      for (e <- remapped) {
        aggreged.get(e.where) match {
          case None => aggreged(e.where) = e.transfers
          case Some(value) => aggreged(e.where) = {
            assert(e.transfers.intersect(value).isEmpty, "Same transfers can go to two different busses")
            e.transfers.mincover(value)
          }
        }
      }
      ret ++= aggreged.map(e => new MappedTransfers(e._1, e._2))
    }
    ret
  }
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
