package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.{AddressMapping, InvertMapping, OrMapping, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer

abstract class NodeRaw extends Area with SpinalTagReady with SpinalTag {
  val clockDomain = ClockDomain.currentHandle
  val bus = Handle[Bus]()
  val lock = Lock() //Allow to hold the generation of this node

  def m2s : NodeRawM2s
  def s2m : NodeRawS2m

  //Document the current component being host for this node
  Component.current.addTag(this)

  //Document the memory transfer capabilities of the current node
  this.addTag(new MemoryTransferTag {
    override def get = m2s.parameters.emits
  })
}

//Negotiation handles for master to slave requests
class NodeRawM2s extends Area{
  val proposed = Handle[M2sSupport]()
  val supported = Handle[M2sSupport]()
  val parameters = Handle[M2sParameters]()

  def setProposedFromParameters(): Unit ={
    proposed load M2sSupport(parameters)
  }
}

//Negotiation handles for slave to master requests (memory coherency only)
class NodeRawS2m extends Area{
  val proposed = Handle[S2mSupport]()
  val supported = Handle[S2mSupport]()
  val parameters = Handle[S2mParameters]()

  def none() = {
    proposed.load(S2mSupport.none)
    parameters.load(S2mParameters.none)
  }
  def setProposedFromParameters(): Unit ={
    proposed load S2mSupport(parameters)
  }
  def from(s : NodeRawS2m) ={
    proposed.load(s.proposed)
    s.supported.load(supported)
    parameters.load(s.parameters)
  }
}
