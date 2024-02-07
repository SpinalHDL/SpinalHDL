package spinal.lib.bus.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, OffsetTransformer, SizeMapping}
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer

/**
  * provide some software interface to connect 2 NodeBase
  */
abstract class MappedConnection[N <: Node](val m : N, val s : N) extends Area {
  setLambdaName(m.isNamed && s.isNamed)(s"${m.getName()}_to_${s.getName()}")

  //Specify how the connection is memory mapped to the decoder
  val mapping = new Area{
    var automatic = Option.empty[Any]
  }

  def mEmits : MemoryTransfers

  //Document the memory connection in a agnostic way for further usages
  val tag = new MemoryConnection{
    override def up = MappedConnection.this.m
    override def down = MappedConnection.this.s
    override def transformers = MappedConnection.this.mapping.automatic match {
      case None => Nil
      case Some(DefaultMapping) => Nil
      case Some(v: BigInt) =>  List(OffsetTransformer(v))
      case Some(v: AddressMapping) => List(OffsetTransformer(v.lowerBound))
    }
    override def sToM(downs: MemoryTransfers, args: MappedNode) = {
      downs.intersect(mEmits)
    }
    populate()
  }


  def decoderAddressWidth(full : Int) : Int = {
    mapping.automatic match {
      case Some(v: BigInt) => log2Up(v+(BigInt(1) << full))
      case Some(DefaultMapping) => full //you can remove this
      case Some(v: AddressMapping) => v.width
      case None => full
    }
  }

  def proposalAddressWidth(full: Int): Int = {
    mapping.automatic match {
      case Some(v: BigInt) => log2Up((BigInt(1) << full)-v)
      case Some(DefaultMapping) => full  //you can remove this
      case Some(v: AddressMapping) => log2Up(v.highestBound+1-v.lowerBound)
      case None => full
    }
  }

  override def toString = if(this.isNamed) getName() else s"${m}_to_${s}"
}