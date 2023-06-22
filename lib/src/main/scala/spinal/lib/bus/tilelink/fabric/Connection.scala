package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, OffsetTransformer, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer


class Connection(val m : NodeRaw, val s : NodeRaw) extends Area {
  setLambdaName(m.isNamed && s.isNamed)(s"${m.getName()}_to_${s.getName()}")

  //Specify how the connection is memory mapped to the decoder
  val mapping = new Area{
    var automatic = Option.empty[Any]
    val value = Handle[AddressMapping]
  }

  //Document the memory connection in a agnostic way for further usages
  val tag = new MemoryConnection{
    override def m = Connection.this.m
    override def s = Connection.this.s
    override def mapping = getMapping()
    override def transformers = Connection.this.mapping.automatic match {
      case Some(DefaultMapping) => Nil
      case _ => List(OffsetTransformer(mapping.lowerBound))
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
    mapping.automatic match {
      case Some(v : BigInt) => log2Up(v + (BigInt(1) << s.m2s.supported.addressWidth))
      case Some(DefaultMapping) => full
      case Some(m : AddressMapping) => log2Up(m.highestBound+1)
      case None => log2Up(mapping.value.highestBound+1)
    }
  }

  override def toString = if(this.isNamed) getName() else s"${m}_to_${s}"
}


class InterconnectAdapterWidth extends InterconnectAdapter{
  var adapter = Option.empty[tilelink.WidthAdapter]

  override def isRequired(c : Connection) = c.m.m2s.parameters.dataWidth != c.s.m2s.parameters.dataWidth
  override def build(c : Connection)(m: Bus) : Bus = {
    val adapter = new tilelink.WidthAdapter(
      ip = m.p,
      op = m.p.copy(dataWidth = c.s.m2s.parameters.dataWidth),
      ctxBuffer = ContextAsyncBufferFull
    )
    adapter.setLambdaName(c.m.isNamed && c.s.isNamed)(s"${c.m.getName()}_to_${c.s.getName()}_widthAdapter")
    this.adapter = Some(adapter)
    adapter.io.up << m
    adapter.io.down
  }
}


trait InterconnectAdapter {
  def isRequired(c : Connection) : Boolean
  def build(c : Connection)(m : Bus) : Bus
}

class InterconnectAdapterCc extends InterconnectAdapter{
  var aDepth = 8
  var bDepth = 8
  var cDepth = 8
  var dDepth = 8
  var eDepth = 8

  var cc = Option.empty[FifoCc]
  override def isRequired(c : Connection) = c.m.clockDomain.clock != c.s.clockDomain.clock
  override def build(c : Connection)(m: Bus) : Bus = {
    val cc = FifoCc(
      busParameter = m.p,
      inputCd      = c.m.clockDomain,
      outputCd     = c.s.clockDomain,
      aDepth       = aDepth,
      bDepth       = bDepth,
      cDepth       = cDepth,
      dDepth       = dDepth,
      eDepth       = eDepth
    )
    cc.setLambdaName(c.m.isNamed && c.s.isNamed)(s"${c.m.getName()}_to_${c.s.getName()}_cc")
    this.cc = Some(cc)
    cc.io.input << m
    cc.io.output
  }
}


