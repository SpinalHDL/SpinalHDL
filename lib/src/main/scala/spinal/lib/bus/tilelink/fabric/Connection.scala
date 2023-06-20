package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.{AddressMapping, OffsetTransformer, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer


class Connection(val m : Node, val s : Node) extends Area {
  //Register this connection in the connected nodes
  m.downs += this
  s.ups += this
  setLambdaName(m.isNamed && s.isNamed)(s"${m.getName()}_to_${s.getName()}")

  //Specify how the connection is memory mapped to the decoder
  val mapping = new Area{
    var addressSpec = Option.empty[BigInt]
    var mappingSpec = Option.empty[AddressMapping]
    var defaultSpec = Option.empty[Unit]
    val value = Handle[AddressMapping]
  }

  //Document the memory connection in a agnostic way for further usages
  val tag = new MemoryConnection{
    override def m = Connection.this.m
    override def s = Connection.this.s
    override def mapping = getMapping()
    override def transformers = Connection.this.mapping.defaultSpec match {
      case None => List(OffsetTransformer(mapping.lowerBound))
      case Some(_) => Nil
    }
    override def sToM(down: MemoryTransfers, args: MappedNode) = down
    populate()
  }

  //Handles used for negociation
  val decoder, arbiter = new Area{
    val bus = Handle[Bus]()
    val m2s = new Area{
      val parameters = Handle[M2sParameters]()
    }
    val s2m = new Area{
      val parameters = Handle[S2mParameters]()
    }
  }

  //Will negociate the parameters and then connect the ends through the required adapters
  val thread = Fiber build new Area{
    soon(arbiter.m2s.parameters)
    soon(decoder.s2m.parameters)

    arbiter.m2s.parameters.load(s.m2s.supported join decoder.m2s.parameters)
    decoder.s2m.parameters.load(m.s2m.supported join arbiter.s2m.parameters)

    var ptr = decoder.bus.get
    for(adapter <- adapters){
      if(adapter.isRequired(Connection.this)){
        ptr = adapter.build(Connection.this)(ptr)
      }
    }
    ptr >> arbiter.bus
  }

  
  val adapters = ArrayBuffer[InterconnectAdapter]()
  adapters += new InterconnectAdapterCc()
  adapters += new InterconnectAdapterWidth()

  def getMapping() : AddressMapping = {
    mapping.value
  }

  def decoderAddressWidth() : Int = {
    def full = s.m2s.supported.addressWidth
    mapping.addressSpec match {
      case Some(v) => return  log2Up(v + (BigInt(1) << s.m2s.supported.addressWidth))
      case None =>
    }

    mapping.defaultSpec match {
      case Some(x) => return full
      case None =>
    }

    mapping.mappingSpec match {
      case Some(m) => log2Up(m.highestBound+1)
      case None => log2Up(mapping.value.highestBound+1)
    }
  }

  override def toString = if(this.isNamed) getName() else s"${m}_to_${s}"
}





