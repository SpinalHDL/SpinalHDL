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
    val value = Handle[AddressMapping]
  }


  //Document the memory connection in a agnostic way for further usages
  val tag = new MemoryConnection{
    override def up = MappedConnection.this.m
    override def down = MappedConnection.this.s
    override def mapping = getMapping()
    override def transformers = MappedConnection.this.mapping.automatic match {
      case Some(DefaultMapping) => Nil
      case _ => List(OffsetTransformer(mapping.lowerBound))
    }
    populate()
  }

  def getMapping() : AddressMapping = mapping.value

  def decoderAddressWidth(full : Int) : Int = {
    mapping.automatic match {
      case Some(v : BigInt) => log2Up(v + (BigInt(1) << full))
      case Some(DefaultMapping) => full
      case None => log2Up(mapping.value.highestBound+1)
    }
  }

  def proposalAddressWidth(full: Int): Int = {
    mapping.automatic match {
      case Some(_) => {
        full
      }
      case None => {
        log2Up(mapping.value.highestBound - mapping.value.lowerBound + 1)
      }
    }
  }

  override def toString = if(this.isNamed) getName() else s"${m}_to_${s}"
}