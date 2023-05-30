package spinal.lib.bus.tilelink.crossbar

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer


class InterconnectConnection(val m : Node, val s : Node) extends Area {
  m.downs += this
  s.ups += this
  setLambdaName(m.isNamed && s.isNamed)(s"${m.getName()}_to_${s.getName()}")

  val tag = new MemoryConnection{
    override def m = InterconnectConnection.this.m
    override def s = InterconnectConnection.this.s
    override def mapping = getMapping()
    override def offset = InterconnectConnection.this.mapping.defaultSpec match {
      case None => mapping.map(_.base).min
      case Some(_) => 0
    }
    override def sToM(down: MemoryTransfers, args: MappedNode) = down
  }

  m.addTag(tag)
  s.addTag(tag)

  val mapping = new Area{
    var addressSpec = Option.empty[BigInt]
    var mappingSpec = Option.empty[SizeMapping]
    var defaultSpec = Option.empty[Unit]
    val value = Handle[Seq[SizeMapping]]
  }


  val adapters = ArrayBuffer[InterconnectAdapter]()
  adapters += new InterconnectAdapterCc()
  adapters += new InterconnectAdapterWidth()

  def getMapping() : Seq[SizeMapping] = {
    mapping.value
  }

  def proposedAddressWidth() : Int = {
    def full = m.m2s.proposed.addressWidth
    mapping.addressSpec match {
      case Some(v) => return full
      case None =>
    }

    mapping.defaultSpec match {
      case Some(x) => return full
      case None =>
    }

    mapping.mappingSpec.get match {
      case m : SizeMapping => log2Up(m.size)
    }
  }

  val decoder, arbiter = new Area{
    val bus = Handle[Bus]()
    val m2s = new Area{
      val parameters = Handle[M2sParameters]()
    }
    val s2m = new Area{
      val parameters = Handle[S2mParameters]()
    }
  }

  val thread = hardFork on new Area{
    soon(arbiter.m2s.parameters)
    soon(decoder.s2m.parameters)

    arbiter.m2s.parameters.load(s.m2s.supported join decoder.m2s.parameters)
    decoder.s2m.parameters.load(m.s2m.supported join arbiter.s2m.parameters)

    var ptr = decoder.bus.get
    for(adapter <- adapters){
      if(adapter.isRequired(InterconnectConnection.this)){
        ptr = adapter.build(InterconnectConnection.this)(ptr)
      }
    }
    ptr >> arbiter.bus
  }

  override def toString = if(this.isNamed) getName() else s"${m}_to_${s}"
}





