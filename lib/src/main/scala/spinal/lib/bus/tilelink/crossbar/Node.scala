package spinal.lib.bus.tilelink.crossbar

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink._
import spinal.lib.system.tag._

import scala.collection.mutable.ArrayBuffer



class NodeMode extends Nameable
object NodeMode extends AreaRoot {
  val BOTH, MASTER, SLAVE = new NodeMode
}

object Node{
  def apply() : Node = new Node()
  def slave() : Node = apply().setSlaveOnly()
  def master() : Node = apply().setMasterOnly()
}
class Node() extends Area with SpinalTagReady with SpinalTag {
  val bus = Handle[Bus]()
  val ups = ArrayBuffer[InterconnectConnection]()
  val downs = ArrayBuffer[InterconnectConnection]()
  val lock = Lock()
  val clockDomain = ClockDomain.currentHandle

  Component.current.addTag(this)

  var arbiterConnector : (Bus, Bus) => Any = (s, m) => s << m
  var decoderConnector : (Bus, Bus) => Any = (s, m) => s << m
  def setArbiterConnection(body : (Bus, Bus) => Any) = arbiterConnector = body
  def setDecoderConnection(body : (Bus, Bus) => Any) = decoderConnector = body

  def await() = {
    lock.await()
  }

  this.addTag(new MemoryTransferTag {
    override def get = m2s.parameters.emits
  })

  val m2s = new Area{
    val proposed = Handle[M2sSupport]()
    val supported = Handle[M2sSupport]()
    val parameters = Handle[M2sParameters]()

    val proposedModifiers, supportedModifiers = ArrayBuffer[M2sSupport => M2sSupport]()
    val parametersModifiers = ArrayBuffer[M2sParameters => M2sParameters]()

    def setProposedFromParameters(): Unit ={
      proposed load M2sSupport(parameters)
    }
  }
  val s2m = new Area{
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
  }

  var mode = NodeMode.BOTH
  def setSlaveOnly() = {mode = NodeMode.SLAVE; this}
  def setMasterOnly() = {mode = NodeMode.MASTER; this}
  def isSlaveOnly() = mode == NodeMode.SLAVE
  def isMasterOnly() = mode == NodeMode.MASTER

  def <<(m : Node): InterconnectConnection = {
    val c = new InterconnectConnection(m, this)
    c.mapping.defaultSpec = Some(Unit)
    c
  }

  class At(body : InterconnectConnection => Unit){
    def of(m : Node): InterconnectConnection = {
      val c = new InterconnectConnection(m, Node.this)
      body(c)
      c
    }
  }
  def at(address : BigInt) = new At(_.mapping.addressSpec = Some(address))
  def at(address : BigInt, size : BigInt) : At = at(SizeMapping(address, size))
  def at(mapping : SizeMapping) = new At(_.mapping.mappingSpec = Some(mapping))

  def forceDataWidth(dataWidth : Int): Unit ={
    m2s.proposedModifiers += { s =>
      s.copy(dataWidth = dataWidth)
    }
    m2s.supportedModifiers += { s =>
      s.copy(dataWidth = dataWidth)
    }
  }

  override def toString =  (if(component != null)component.getPath() + "/"  else "") + getName()




  val thread = Elab build new Composite(this, weak = false) {
    // Specify which Handle will be loaded by the current thread, as this help provide automated error messages
    soon(ups.map(_.arbiter.bus) :_*)
    soon(downs.map(_.decoder.bus) :_*)
    soon(downs.map(_.decoder.m2s.parameters) :_*)
    soon(ups.map(_.arbiter.s2m.parameters) :_*)
    soon(ups.map(_.arbiter.bus) :_*)
    soon(
      bus
    )
    if(mode != NodeMode.MASTER) soon(
      m2s.proposed,
      m2s.parameters,
      s2m.supported
    )
    if(mode != NodeMode.SLAVE) soon(
      m2s.supported,
      s2m.parameters,
      s2m.proposed
    )


    await()
    mode match {
      case NodeMode.MASTER =>
        if(ups.nonEmpty)  { SpinalError(s"${getName()} has masters") }
        if(downs.isEmpty) { SpinalError(s"${getName()} has no slave") }
      case NodeMode.BOTH =>
        if(ups.isEmpty)   { SpinalError(s"${getName()} has no master") }
        if(downs.isEmpty) { SpinalError(s"${getName()} has no slave") }
      case NodeMode.SLAVE =>
        if(ups.isEmpty)    { SpinalError(s"${getName()} has no master") }
        if(downs.nonEmpty) { SpinalError(s"${getName()} has slaves") }
    }

    // m2s.proposed <- ups.m2s.proposed
    if(mode != NodeMode.MASTER) {
      val fromUps = ups.map(_.m.m2s.proposed).reduce(_ mincover _)
      val addressConstrained = fromUps.copy(
        addressWidth = ups.map(up => up.proposedAddressWidth()).max
      )
      val modified = m2s.proposedModifiers.foldLeft(addressConstrained)((e, f) => f(e))
      m2s.proposed load modified
    }

    // m2s.supported <- downs.m2s.supported
    if(mode != NodeMode.SLAVE) {
      val fromDowns = downs.map(_.s.m2s.supported.get).reduce(_ mincover _)
      val addressConstrained = fromDowns.copy(
        addressWidth = m2s.proposed.addressWidth
      )
      val modified = m2s.supportedModifiers.foldLeft(addressConstrained)((e, f) => f(e))
      m2s.supported load modified
    }

    // m2s.parameters <- ups.arbiter.m2s.parameters
    if(mode != NodeMode.MASTER) {
      m2s.parameters load Arbiter.downMastersFrom(
        ups.map(_.arbiter.m2s.parameters.get)
      )
    }

    //down.decoder.m2s.parameters <- m2s.parameters + down.s.m2s.supported
    for (down <- downs) {
      down.decoder.m2s.parameters.load(Decoder.downMastersFrom(m2s.parameters, down.s.m2s.supported))
    }

    //Generate final connections mapping
    if(mode != NodeMode.SLAVE) {
      var dc = ArrayBuffer[InterconnectConnection]()
      downs.foreach{ c =>
        c.mapping.addressSpec match {
          case Some(v) => c.mapping.value load List(SizeMapping(v, BigInt(1) << c.decoder.m2s.parameters.get.addressWidth))
          case None =>
        }
        c.mapping.mappingSpec match {
          case Some(x) => c.mapping.value load List(x)
          case None =>
        }
        c.mapping.defaultSpec match {
          case Some(_) => {
            //              assert(c.decoder.m2s.parameters.addressWidth == m2s.parameters.addressWidth, s"Default connection $c addressWidth doesn't match\n ${ m2s.parameters.addressWidth} bits >> ${c.decoder.m2s.parameters.addressWidth} bits")
            dc += c
          }
          case None => assert(c.mapping.value.isLoaded)
        }
      }
      for(c <- dc){
        val spec = ArrayBuffer[SizeMapping]()
        val others = downs.filter(_.mapping.defaultSpec.isEmpty).flatMap(_.mapping.value.get)
        val sorted = others.sortWith(_.base < _.base)
        var address = BigInt(0)
        val endAt = BigInt(1) << c.decoder.m2s.parameters.addressWidth
        for(other <- sorted){
          val size = other.base - address
          if(size != 0) spec += SizeMapping(address, size)
          address += other.base + other.size
        }
        val lastSize = endAt - address
        if(lastSize != 0) spec += SizeMapping(address, lastSize)
        c.mapping.value.load(spec)
      }
    }

    m2s.parameters.withBCE match {
      case true =>{
        // s2m.proposed <- downs.s2m.proposed
        if(mode != NodeMode.SLAVE) {
          val fromDowns = downs.map(_.s.s2m.proposed.get).reduce(_ mincover _)
          //        val modified = s2m.proposedModifiers.foldLeft(fromDowns)((e, f) => f(e))
          s2m.proposed load fromDowns
        }

        // s2m.supported <- ups.s2m.supported
        if(mode != NodeMode.MASTER) {
          val fromUps = ups.map(_.m.s2m.supported.get).reduce(_ mincover _)
          //        val modified = m2s.supportedModifiers.foldLeft(addressConstrained)((e, f) => f(e))
          s2m.supported load fromUps
        }

        // s2m.parameters <- downs.decoder.s2m.parameters
        if(mode != NodeMode.SLAVE){
          s2m.parameters.load(Decoder.upSlavesFrom(downs.map(_.decoder.s2m.parameters.get)))
        }

        //ups.arbiter.s2m.parameters <- s2m.parameters
        for(up <- ups){
          //        up.arbiter.s2m.parameters.load(s2m.parameters)
          up.arbiter.s2m.parameters.load(Arbiter.upSlaveFrom(s2m.parameters, up.m.s2m.supported))
        }
      }
      case false => {
        if(mode != NodeMode.SLAVE) {
          s2m.proposed load S2mSupport.none
        }
        if(mode != NodeMode.MASTER) {
          s2m.supported load S2mSupport.none
        }
        if(mode != NodeMode.SLAVE){
          s2m.parameters.load(S2mParameters.none())
        }
        for(up <- ups){
          up.arbiter.s2m.parameters.load(S2mParameters.none())
        }
      }
    }



    // Start hardware generation from that point
    // Generate the node bus
    val p = NodeParameters(m2s.parameters, s2m.parameters).toBusParameter()
    bus.load(Bus(p))

    val arbiter = (mode != NodeMode.MASTER && ups.size > 1) generate new Area {
      val core = Arbiter(ups.map(up => NodeParameters(
        m = up.arbiter.m2s.parameters,
        s = up.arbiter.s2m.parameters
      )))
      (ups, core.io.ups.map(_.fromCombStage())).zipped.foreach(_.arbiter.bus.load(_))
      val connection = arbiterConnector(bus, core.io.down)
    }

    val noArbiter = (mode != NodeMode.MASTER && ups.size == 1) generate new Area {
      ups.head.arbiter.bus.load(cloneOf(bus.get))
      val connection = arbiterConnector(bus, ups.head.arbiter.bus)
    }

    val decoder = (mode != NodeMode.SLAVE && downs.size > 1) generate new Area {
      val core = Decoder(bus.p.node, downs.map(_.s.m2s.supported), downs.map(_.decoder.s2m.parameters), downs.map(_.getMapping()), downs.map(_.tag.offset), downs.map(_.mapping.defaultSpec.nonEmpty))
      (downs, core.io.downs.map(_.combStage())).zipped.foreach(_.decoder.bus.load(_))
      val connection = decoderConnector(core.io.up, bus)
    }

    val noDecoder = (mode != NodeMode.SLAVE && downs.size == 1) generate new Area {
      val c = downs.head
      val toDown = cloneOf(bus.get)
      val connection = decoderConnector(toDown, bus)
      c.decoder.bus.load(Bus(NodeParameters(c.decoder.m2s.parameters, c.decoder.s2m.parameters)))
      val target = c.decoder.bus
      target << toDown
      target.a.address.removeAssignments() := (toDown.a.address - c.tag.offset).resized
      if(toDown.p.withBCE) {
        toDown.b.address.removeAssignments() := (target.b.address + c.tag.offset).resized
        target.c.address.removeAssignments() := (toDown.c.address - c.tag.offset).resized
      }
    }
  }
}

trait InterconnectAdapter {
  def isRequired(c : InterconnectConnection) : Boolean
  def build(c : InterconnectConnection)(m : Bus) : Bus
}

class InterconnectAdapterCc extends InterconnectAdapter{
  var aDepth = 8
  var bDepth = 8
  var cDepth = 8
  var dDepth = 8
  var eDepth = 8

  var cc = Option.empty[FifoCc]
  override def isRequired(c : InterconnectConnection) = c.m.clockDomain.clock != c.s.clockDomain.clock
  override def build(c : InterconnectConnection)(m: Bus) : Bus = {
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

class InterconnectAdapterWidth extends InterconnectAdapter{
  var adapter = Option.empty[WidthAdapter]

  override def isRequired(c : InterconnectConnection) = c.m.m2s.parameters.dataWidth != c.s.m2s.parameters.dataWidth
  override def build(c : InterconnectConnection)(m: Bus) : Bus = {
    val adapter = new WidthAdapter(
      ip = m.p,
      op = m.p.copy(dataWidth = c.s.m2s.parameters.dataWidth),
      ctxBuffer = ContextAsyncBufferFull
    )
    adapter.setLambdaName(c.m.isNamed && c.s.isNamed)(s"${c.m.getName()}_to_${c.s.getName()}_widthAdapter")
    this.adapter = Some(adapter)
    adapter.io.input << m
    adapter.io.output
  }
}






