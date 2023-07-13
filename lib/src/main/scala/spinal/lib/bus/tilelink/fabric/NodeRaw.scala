package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.{AddressMapping, InvertMapping, OrMapping, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink
import spinal.lib._
import spinal.lib.bus.fabric
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer

/**
 * Specify the software interface of nodes. Mostly negociation Handles
 * Threadless so far
 */
abstract class NodeRaw extends bus.fabric.Node {
  val bus = Handle[Bus]()

  //Negociation handles
  val m2s = new NodeM2s()
  val s2m = new NodeS2m()

  //Document the memory transfer capabilities of the current node
  this.addTag(new MemoryTransferTag {
    override def get = m2s.parameters.emits
  })
}

/**
  * Extends NodeRaw with the capability of connecting masters / slaves
  */
class NodeUpDown() extends NodeRaw with bus.fabric.MappedUpDown[NodeUpDown, ConnectionRaw]{
  override def connectFrom(m: NodeUpDown): ConnectionRaw = Node.connect(m, NodeUpDown.this)
}


/**
 * Negotiation handles for master to slave requests
 */
class NodeM2s extends bus.fabric.NegotiateSP[M2sSupport, M2sParameters] {
  override def setProposedFromParameters(): Unit ={
    proposed load M2sSupport(parameters)
  }
}

/**
 * Negotiation handles for slave to master requests (memory coherency only)
 */
class NodeS2m extends bus.fabric.NegotiateSP[S2mSupport, S2mParameters]{
  def none() = {
    proposed.load(S2mSupport.none)
    parameters.load(S2mParameters.none)
  }
  override def setProposedFromParameters(): Unit ={
    proposed load S2mSupport(parameters)
  }
}
