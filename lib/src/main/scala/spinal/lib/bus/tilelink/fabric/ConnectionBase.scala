package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, OffsetTransformer, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer


class ConnectionBase(val m : NodeBase, val s : NodeBase) extends Area {
  setLambdaName(m.isNamed && s.isNamed)(s"${m.getName()}_to_${s.getName()}")

  //Specify how the connection is memory mapped to the decoder
  val mapping = new Area{
    var automatic = Option.empty[Any]
    val value = Handle[AddressMapping]
  }

  //Handles used for negociation
  val up, down = new Area{
    val bus = Handle[Bus]()
    val m2s = new Area{
      val parameters = Handle[M2sParameters]()
    }
    val s2m = new Area{
      val parameters = Handle[S2mParameters]()
    }
  }

  //Document the memory connection in a agnostic way for further usages
  val tag = new MemoryConnection{
    override def m = ConnectionBase.this.m
    override def s = ConnectionBase.this.s
    override def mapping = getMapping()
    override def transformers = ConnectionBase.this.mapping.automatic match {
      case Some(DefaultMapping) => Nil
      case _ => List(OffsetTransformer(mapping.lowerBound))
    }
    override def sToM(down: MemoryTransfers, args: MappedNode) = down
    populate()
  }

  def getMapping() : AddressMapping = mapping.value

  def decoderAddressWidth() : Int = {
    def full = s.m2s.supported.addressWidth
    mapping.automatic match {
      case Some(v : BigInt) => log2Up(v + (BigInt(1) << s.m2s.supported.addressWidth))
      case Some(DefaultMapping) => full
      case Some(m : AddressMapping) => log2Up(m.highestBound+1)
      case None => log2Up(mapping.value.highestBound+1)
    }
  }

  override def toString = if(this.isNamed) getName() else s"${m}_to_${s}"
}

