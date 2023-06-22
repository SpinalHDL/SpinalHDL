package spinal.lib.bus.tilelink.fabric

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, InvertMapping, OrMapping, SizeMapping}
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer

object Node{
  def apply() : Node = new Node()
  def slave() : Node = apply().setSlaveOnly()
  def master() : Node = apply().setMasterOnly()
  def connect(m : Node, s : Node) = {
    val c = new Connection(m, s)
    m.downs += c
    s.ups += c
    c
  }
}


class Node() extends NodeRaw {
  var withUps, withDowns = true //Used for assertion
  val ups = ArrayBuffer[Connection]()
  val downs = ArrayBuffer[Connection]()

  val m2s = new NodeM2s
  val s2m = new NodeRawS2m

  var arbiterConnector : (Bus, Bus) => Any = (s, m) => s << m
  var decoderConnector : (Bus, Bus) => Any = (s, m) => s << m

  //Will negociate the m2s/s2m handles, then generate the arbiter / decoder required to connect the ups / downs connections
  val thread = Fiber build new Composite(this, weak = false) {
    // Specify which Handle will be loaded by the current thread, as this help provide automated error messages
    soon(ups.map(_.down.bus) :_*)
    soon(downs.map(_.up.bus) :_*)
    soon(downs.map(_.up.m2s.parameters) :_*)
    soon(ups.map(_.down.s2m.parameters) :_*)
    soon(ups.map(_.down.bus) :_*)
    soon(
      bus
    )
    if(withUps) soon(
      m2s.proposed,
      m2s.parameters,
      s2m.supported
    )
    if(withDowns) soon(
      m2s.supported,
      s2m.parameters,
      s2m.proposed
    )

    await()
    if(withDowns && downs.isEmpty) SpinalError(s"${getName()} has no slave")
    if(!withDowns && downs.nonEmpty) SpinalError(s"${getName()} has slaves")

    if(withUps && ups.isEmpty) SpinalError(s"${getName()} has no master")
    if(!withUps && ups.nonEmpty) SpinalError(s"${getName()} has masters")

    // m2s.proposed <- ups.m2s.proposed
    if(withUps) {
      val fromUps = ups.map(_.m.m2s.proposed).reduce(_ mincover _)
      val modified = m2s.proposedModifiers.foldLeft(fromUps)((e, f) => f(e))
      m2s.proposed load modified
    }

    // m2s.supported <- downs.m2s.supported
    if(withDowns) {
      val fromDowns = downs.map(_.s.m2s.supported.get).reduce(_ mincover _)
      val addressConstrained = fromDowns.copy(
        addressWidth = downs.map(down => down.decoderAddressWidth()).max
      )
      val modified = m2s.supportedModifiers.foldLeft(addressConstrained)((e, f) => f(e))
      m2s.supported load modified
    }

    // m2s.parameters <- ups.arbiter.m2s.parameters
    if(withUps) {
      m2s.parameters load Arbiter.downMastersFrom(
        ups.map(_.down.m2s.parameters.get)
      )
    }

    //down.decoder.m2s.parameters <- m2s.parameters + down.s.m2s.supported
    for (down <- downs) {
      down.up.m2s.parameters.load(Decoder.downMastersFrom(m2s.parameters, down.s.m2s.supported))
    }

    //Generate final connections mapping
    if(withDowns) {
      var dc = ArrayBuffer[Connection]()
      downs.foreach{ c =>
        c.mapping.automatic match {
          case Some(v : BigInt) => c.mapping.value load SizeMapping(v, BigInt(1) << c.up.m2s.parameters.get.addressWidth)
          case Some(DefaultMapping) => dc += c
          case Some(x : AddressMapping) => c.mapping.value load x
          case None => c.mapping.value.get
        }
      }
      for(c <- dc){
        val spec = ArrayBuffer[SizeMapping]()
        val others = downs.filter(_.mapping.automatic.exists(_ != DefaultMapping)).flatMap(_.mapping.value.get match {
          case m : SizeMapping => List(m)
          case m : OrMapping => m.conds.map(_.asInstanceOf[SizeMapping]) //DefaultMapping only supported if all others are sizeMapping
        })
        val sorted = others.sortWith(_.base < _.base)
        var address = BigInt(0)
        val endAt = BigInt(1) << c.up.m2s.parameters.addressWidth
        for(other <- sorted){
          val size = other.base - address
          if(size != 0) spec += SizeMapping(address, size)
          address = other.base + other.size
        }
        val lastSize = endAt - address
        if(lastSize != 0) spec += SizeMapping(address, lastSize)
        c.mapping.value.load(spec.size match {
          case 0 => ???
          case 1 => spec.head
          case _ => OrMapping(spec)
        })
      }
    }

    m2s.parameters.withBCE match {
      case true =>{
        // s2m.proposed <- downs.s2m.proposed
        if(withDowns) {
          val fromDowns = downs.map(_.s.s2m.proposed.get).reduce(_ mincover _)
          //        val modified = s2m.proposedModifiers.foldLeft(fromDowns)((e, f) => f(e))
          s2m.proposed load fromDowns
        }

        // s2m.supported <- ups.s2m.supported
        if(withUps) {
          val fromUps = ups.map(_.m.s2m.supported.get).reduce(_ mincover _)
          //        val modified = m2s.supportedModifiers.foldLeft(addressConstrained)((e, f) => f(e))
          s2m.supported load fromUps
        }

        // s2m.parameters <- downs.decoder.s2m.parameters
        if(withDowns){
          s2m.parameters.load(Decoder.upSlavesFrom(downs.map(_.up.s2m.parameters.get)))
        }

        //ups.arbiter.s2m.parameters <- s2m.parameters
        for(up <- ups){
          //        up.arbiter.s2m.parameters.load(s2m.parameters)
          up.down.s2m.parameters.load(Arbiter.upSlaveFrom(s2m.parameters, up.m.s2m.supported))
        }
      }
      case false => {
        if(withDowns) {
          s2m.proposed load S2mSupport.none
        }
        if(withUps) {
          s2m.supported load S2mSupport.none
        }
        if(withDowns){
          s2m.parameters.load(S2mParameters.none())
        }
        for(up <- ups){
          up.down.s2m.parameters.load(S2mParameters.none())
        }
      }
    }

    // Start hardware generation from that point
    // Generate the node bus
    val p = NodeParameters(m2s.parameters, s2m.parameters).toBusParameter()
    bus.load(Bus(p))

    val arbiter = (withUps && ups.size > 1) generate new Area {
      val core = Arbiter(ups.map(up => NodeParameters(
        m = up.down.m2s.parameters,
        s = up.down.s2m.parameters
      )))
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

  //Allows to customize how the node is connected to its arbiter / decoder components (pipelining)
  def setArbiterConnection(body : (Bus, Bus) => Any) = arbiterConnector = body
  def setDecoderConnection(body : (Bus, Bus) => Any) = decoderConnector = body

  def setSlaveOnly() = {
    assert(withUps)
    withDowns = false
    this
  }
  def setMasterOnly() = {
    assert(withDowns)
    withUps = false
    this
  }

  def <<(m : Node): Connection = {
    val c = Node.connect(m, this)
    c.mapping.automatic = Some(DefaultMapping)
    c
  }

  def <<(m : Seq[Node]): Seq[Connection] = m.map(this << _)

  class At(body : Connection => Unit){
    def of(m : Node): Connection = {
      val c = Node.connect(m, Node.this)
      body(c)
      c
    }
  }
  def at(address : BigInt) = new At(_.mapping.automatic = Some(address))
  def at(address : BigInt, size : BigInt) : At = at(SizeMapping(address, size))
  def at(mapping : AddressMapping) = new At(_.mapping.value load mapping)

  def forceDataWidth(dataWidth : Int): Unit ={
    m2s.proposedModifiers += { s =>
      s.copy(dataWidth = dataWidth)
    }
    m2s.supportedModifiers += { s =>
      s.copy(dataWidth = dataWidth)
    }
  }

  override def toString =  (if(component != null)component.getPath() + "/"  else "") + getName()
}

class NodeM2s extends NodeRawM2s{
  val proposedModifiers, supportedModifiers = ArrayBuffer[M2sSupport => M2sSupport]()
  val parametersModifiers = ArrayBuffer[M2sParameters => M2sParameters]()

  def addModifier(f : M2sSupport => M2sSupport) = {
    proposedModifiers += f
    supportedModifiers += f
  }
}



