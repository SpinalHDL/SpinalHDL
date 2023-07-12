package spinal.lib.bus.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, OffsetTransformer, SizeMapping}
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer

/**
  * provide some software interface to connect 2 NodeBase
  */
abstract class ConnectionMapped[N <: Node](val m : N, val s : N) extends Area {
  setLambdaName(m.isNamed && s.isNamed)(s"${m.getName()}_to_${s.getName()}")

  //Specify how the connection is memory mapped to the decoder
  val mapping = new Area{
    var automatic = Option.empty[Any]
    val value = Handle[AddressMapping]
  }


  //Document the memory connection in a agnostic way for further usages
  val tag = new MemoryConnection{
    override def m = ConnectionMapped.this.m
    override def s = ConnectionMapped.this.s
    override def mapping = getMapping()
    override def transformers = ConnectionMapped.this.mapping.automatic match {
      case Some(DefaultMapping) => Nil
      case _ => List(OffsetTransformer(mapping.lowerBound))
    }
    override def sToM(down: MemoryTransfers, args: MappedNode) = down
    populate()
  }

  def getMapping() : AddressMapping = mapping.value

  def decoderAddressWidth() : Int = {
    def full = getSlaveAddressWidth
    mapping.automatic match {
      case Some(v : BigInt) => log2Up(v + (BigInt(1) << full))
      case Some(DefaultMapping) => full
      case Some(m : AddressMapping) => log2Up(m.highestBound+1)
      case None => log2Up(mapping.value.highestBound+1)
    }
  }

  def getSlaveAddressWidth() : Int

  override def toString = if(this.isNamed) getName() else s"${m}_to_${s}"
}