package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.fabric.{MappedUpDown, NegotiateSP}
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, InvertMapping, OrMapping, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink
import spinal.lib._
import spinal.lib.system.tag._

import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

object Node{
  def apply() : Node = new Node()
  def slave() : Node = apply().setSlaveOnly()
  def master() : Node = apply().setMasterOnly()
  def up(): Node = apply().setSlaveOnly()
  def down(): Node = apply().setMasterOnly()
  def connect(m : NodeUpDown, s : NodeUpDown) = {
    val c = new Connection(m, s)
    m.downs += c
    s.ups += c
    c
  }
}


/**
 * Implement the elaboration thread to handle the negociation and hardware generation of a NodeUpDown
 */
class Node() extends NodeUpDown{
  var arbiterConnector : (Bus, Bus) => Any = (s, m) => s << m
  var decoderConnector : (Bus, Bus) => Any = (s, m) => s << m

  //Allows to customize how the node is connected to its arbiter / decoder components (pipelining)
  def setUpConnection(body : (Bus, Bus) => Any) : Unit  = arbiterConnector = body
  def setDownConnection(body : (Bus, Bus) => Any) : Unit  = decoderConnector = body

  def setUpConnection(a: StreamPipe = StreamPipe.NONE,
                      b: StreamPipe = StreamPipe.NONE,
                      c: StreamPipe = StreamPipe.NONE,
                      d: StreamPipe = StreamPipe.NONE,
                      e: StreamPipe = StreamPipe.NONE) : Unit = {
    setUpConnection(_.connectFrom(_)(a, b, c, d, e))
  }

  def setDownConnection(a: StreamPipe = StreamPipe.NONE,
                        b: StreamPipe = StreamPipe.NONE,
                        c: StreamPipe = StreamPipe.NONE,
                        d: StreamPipe = StreamPipe.NONE,
                        e: StreamPipe = StreamPipe.NONE) : Unit  = {
    setDownConnection(_.connectFrom(_)(a, b, c, d, e))
  }


  def forceDataWidth(dataWidth : Int): Unit ={
    m2s.proposedModifiers += { s =>
      s.copy(dataWidth = dataWidth)
    }
    m2s.supportedModifiers += { s =>
      s.copy(dataWidth = dataWidth)
    }
  }

  //Will negociate the m2s/s2m handles, then generate the arbiter / decoder required to connect the ups / downs connections
  val thread = Fiber build new Composite(this, weak = false) {
    // Specify which Handle will be loaded by the current thread, as this help provide automated error messages
    soon(ups.map(_.down.bus))
    soon(downs.map(_.up.bus))
    soon(downs.map(_.up.m2s.parameters))
    soon(ups.map(_.down.s2m.parameters))
    soon(bus)
    m2s.soons(Node.this)
    s2m.soonsInv(Node.this)

    await()
    assertUpDown()

    // Start of the m2s negociation
    // m2s.proposed <- ups.m2s.proposed
    if(withUps) {
      val fromUps = ups.map(_.m.m2s.proposed).reduce(_ mincover _)
      val addressConstrained = fromUps.copy(
        addressWidth = fromUps.addressWidth min ups.map(up => up.proposalAddressWidth(up.m.m2s.proposed.addressWidth)).max
      )
      val modified = m2s.applyProposedModifiersOn(addressConstrained)
      m2s.proposed load modified
    }

    // m2s.supported <- downs.m2s.supported
    if(withDowns) {
      val fromDowns = downs.map(_.s.m2s.supported.get).reduce(_ mincover _)
      val addressConstrained = fromDowns.copy(
        addressWidth = downs.map(down => down.decoderAddressWidth(down.s.m2s.supported.addressWidth)).max
      )
      val modified = m2s.applySupportedModifiersOn(addressConstrained)
      m2s.supported load modified
    }

    // m2s.parameters <- ups.arbiter.m2s.parameters
    if(withUps) {
      val p = Arbiter.downMastersFrom(
        ups.map(_.down.m2s.parameters.get)
      )
      val modified = m2s.applyParametersModifiersOn(p)
      m2s.parameters load modified
    }

    //down.decoder.m2s.parameters <- m2s.parameters + down.s.m2s.supported
    for (down <- downs) {
      down.up.m2s.parameters.load(Decoder.downMastersFrom(m2s.parameters, down.s.m2s.supported))
    }

    //Generate final connections mapping
    generateMapping(_.up.m2s.parameters.addressWidth)

    //Start of the s2m negociation
    m2s.parameters.withBCE match {
      case true =>{
        // s2m.proposed <- downs.s2m.proposed
        if(withDowns) {
          val fromDowns = downs.map(_.s.s2m.proposed.get).reduce(_ mincover _)
          val modified = s2m.applyProposedModifiersOn(fromDowns)
          s2m.proposed load modified
        }

        // s2m.supported <- ups.s2m.supported
        if(withUps) {
          val fromUps = ups.map(_.m.s2m.supported.get).reduce(_ mincover _)
          val modified = s2m.applySupportedModifiersOn(fromUps)
          s2m.supported load modified
        }

        // s2m.parameters <- downs.decoder.s2m.parameters
        if(withDowns){
          val p = Decoder.upSlavesFrom(downs.map(_.up.s2m.parameters.get))
          val modified = s2m.applyParametersModifiersOn(p)
          s2m.parameters.load(p)
        }

        //ups.arbiter.s2m.parameters <- s2m.parameters
        for(up <- ups){
          up.down.s2m.parameters.load(Arbiter.upSlaveFrom(s2m.parameters, up.m.s2m.supported))
        }
      }
      case false => {
        if(withDowns) s2m.proposed load S2mSupport.none()
        if(withUps) s2m.supported load S2mSupport.none()
        if(withDowns) s2m.parameters.load(S2mParameters.none())
        for(up <- ups) up.down.s2m.parameters.load(S2mParameters.none())
      }
    }

    // Start hardware generation from that point
    // Generate the node bus
    val p = NodeParameters(m2s.parameters, s2m.parameters).toBusParameter()
    bus.load(Bus(p))

    val arbiter = (withUps && ups.size > 1) generate new Area {
      val core = Arbiter(
        ups.map(up => NodeParameters(
          m = up.down.m2s.parameters,
          s = up.down.s2m.parameters
        )),
        NodeParameters(
          m2s.parameters,
          s2m.parameters
        )
      )
      for((up, arbitred) <- (ups, core.io.ups).zipped){
        up.down.bus.load(arbitred.fromCombStage())
      }
      val connection = arbiterConnector(bus, core.io.down)
    }

    val noArbiter = (withUps && ups.size == 1) generate new Area {
      ups.head.down.bus.load(cloneOf(bus.get))
      val connection = arbiterConnector(bus, ups.head.down.bus)
    }

    val decoder = (withDowns && downs.size > 1) generate new Area {
      val core = Decoder(bus.p.node, downs.map(_.s.m2s.supported), downs.map(_.up.s2m.parameters), downs.map(_.getMapping()), downs.map(_.tag.transformers))
      for((down, decoded) <- (downs, core.io.downs).zipped){
        down.up.bus.load(decoded.combStage())
      }
      val connection = decoderConnector(core.io.up, bus)
    }

    val noDecoder = (withDowns && downs.size == 1) generate new Area {
      val c = downs.head
      val toDown = cloneOf(bus.get)
      val connection = decoderConnector(toDown, bus)
      c.up.bus.load(Bus(NodeParameters(c.up.m2s.parameters, c.up.s2m.parameters)))
      val target = c.up.bus
      target << toDown
      target.a.size.removeAssignments() := toDown.a.size.resized
      toDown.d.size.removeAssignments() := target.d.size.resized
      target.a.address.removeAssignments() := c.tag.transformers(toDown.a.address).resized
      if(toDown.p.withBCE) {
        toDown.b.address.removeAssignments() := c.tag.transformers.invert(target.b.address).resized
        target.c.address.removeAssignments() := c.tag.transformers(toDown.c.address).resized
      }
    }
  }
}




/*
    def negociate[S <: spinal.lib.bus.misc.LogicalOp[S],
                  P,
                  NSP <: NegociateSP[S, P],
                  N  <: spinal.lib.bus.fabric.Node ,
                  C <: spinal.lib.bus.fabric.MappedConnection[N]]
                  (ups : Seq[C], self : NSP, downs : Seq[C])
                  (nToNsp : N => NSP): Unit ={

      // self.proposed <- ups.proposed
      if(ups.nonEmpty) {
        val fromUps = ups.map(e => nToNsp(e.m).proposed).reduce(_ mincover _)
        val modified = self.applyProposedModifiersOn(fromUps)
        self.proposed load modified
      }

      // self.supported <- downs.supported
      if(withDowns) {
        val fromDowns = downs.map(e => nToNsp(e.s).supported.get).reduce(_ mincover _)
        val addressConstrained = fromDowns.withAddressWidth(
          downs.map(down => down.decoderAddressWidth()).max
        )
        val modified = self.applySupportedModifiersOn(addressConstrained)
        self.supported load modified
      }
    }


    negociate[M2sSupport, M2sParameters, NodeM2s, NodeUpDown, ConnectionBase](
      ups,
      m2s,
      downs
    )(e => e.m2s)
 */