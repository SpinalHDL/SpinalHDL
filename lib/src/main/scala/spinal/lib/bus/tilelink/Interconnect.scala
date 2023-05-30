package spinal.lib.bus.tilelink

import spinal.core._
import spinal.core.fiber._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.system.tag._
import spinal.lib.{master, slave}

import scala.collection.mutable.ArrayBuffer

object M2sSupport{
  def apply(p : M2sParameters) : M2sSupport = M2sSupport(
    transfers    = p.emits,
    addressWidth = p.addressWidth,
    dataWidth    = p.dataWidth,
    allowExecute = p.withExecute
  )
}

case class M2sSupport(transfers : M2sTransfers,
                      addressWidth : Int,
                      dataWidth : Int,
                      allowExecute : Boolean = false){
  def mincover(that : M2sSupport): M2sSupport ={
    M2sSupport(
      transfers = transfers.mincover(that.transfers),
      dataWidth = dataWidth max that.dataWidth,
      addressWidth = addressWidth max that.addressWidth,
      allowExecute = this.allowExecute && that.allowExecute
    )
  }

  def join(p: M2sParameters): M2sParameters ={
    M2sParameters(
      addressWidth = addressWidth,
      dataWidth    = dataWidth,
      masters = p.masters.map(e => e) //TODO
    )
  }
}


object S2mSupport{
  def apply(p : S2mParameters) : S2mSupport = S2mSupport(
    transfers    = p.emits
  )

  def none = S2mSupport(
    transfers = S2mTransfers.none
  )
}

case class S2mSupport(transfers : S2mTransfers){
  def mincover(that : S2mSupport): S2mSupport ={
    S2mSupport(
      transfers = transfers.mincover(that.transfers)
    )
  }

  def join(p: S2mParameters): S2mParameters ={
    S2mParameters(
      slaves = p.slaves.map(e => e) //TODO
    )
  }
}

class InterconnectNodeMode extends Nameable
object InterconnectNodeMode extends AreaRoot {
  val BOTH, MASTER, SLAVE = new InterconnectNodeMode
}

object InterconnectNode{
  def apply() : InterconnectNode = new InterconnectNode()
  def slave() : InterconnectNode = apply().setSlaveOnly()
  def master() : InterconnectNode = apply().setMasterOnly()
}
class InterconnectNode() extends Area with SpinalTagReady with SpinalTag {
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

  var mode = InterconnectNodeMode.BOTH
  def setSlaveOnly() = {mode = InterconnectNodeMode.SLAVE; this}
  def setMasterOnly() = {mode = InterconnectNodeMode.MASTER; this}
  def isSlaveOnly() = mode == InterconnectNodeMode.SLAVE
  def isMasterOnly() = mode == InterconnectNodeMode.MASTER

  def <<(m : InterconnectNode): InterconnectConnection = {
    val c = new InterconnectConnection(m, this)
    c.mapping.defaultSpec = Some(Unit)
    c
  }

  class At(body : InterconnectConnection => Unit){
    def of(m : InterconnectNode): InterconnectConnection = {
      val c = new InterconnectConnection(m, InterconnectNode.this)
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
  
  
  
  
  val thread = Elab build new Area {
    // Specify which Handle will be loaded by the current thread, as this help provide automated error messages
    soon(ups.map(_.arbiter.bus) :_*)
    soon(downs.map(_.decoder.bus) :_*)
    soon(downs.map(_.decoder.m2s.parameters) :_*)
    soon(ups.map(_.arbiter.s2m.parameters) :_*)
    soon(ups.map(_.arbiter.bus) :_*)
    soon(
      bus
    )
    if(mode != InterconnectNodeMode.MASTER) soon(
      m2s.proposed,
      m2s.parameters,
      s2m.supported
    )
    if(mode != InterconnectNodeMode.SLAVE) soon(
      m2s.supported,
      s2m.parameters,
      s2m.proposed
    )


    await()
    mode match {
      case InterconnectNodeMode.MASTER =>
        if(ups.nonEmpty)  { SpinalError(s"${getName()} has masters") }
        if(downs.isEmpty) { SpinalError(s"${getName()} has no slave") }
      case InterconnectNodeMode.BOTH =>
        if(ups.isEmpty)   { SpinalError(s"${getName()} has no master") }
        if(downs.isEmpty) { SpinalError(s"${getName()} has no slave") }
      case InterconnectNodeMode.SLAVE =>
        if(ups.isEmpty)    { SpinalError(s"${getName()} has no master") }
        if(downs.nonEmpty) { SpinalError(s"${getName()} has slaves") }
    }

    // m2s.proposed <- ups.m2s.proposed
    if(mode != InterconnectNodeMode.MASTER) {
      val fromUps = ups.map(_.m.m2s.proposed).reduce(_ mincover _)
      val addressConstrained = fromUps.copy(
        addressWidth = ups.map(up => up.proposedAddressWidth()).max
      )
      val modified = m2s.proposedModifiers.foldLeft(addressConstrained)((e, f) => f(e))
      m2s.proposed load modified
    }

    // m2s.supported <- downs.m2s.supported
    if(mode != InterconnectNodeMode.SLAVE) {
      val fromDowns = downs.map(_.s.m2s.supported.get).reduce(_ mincover _)
      val addressConstrained = fromDowns.copy(
        addressWidth = m2s.proposed.addressWidth
      )
      val modified = m2s.supportedModifiers.foldLeft(addressConstrained)((e, f) => f(e))
      m2s.supported load modified
    }

    // m2s.parameters <- ups.arbiter.m2s.parameters
    if(mode != InterconnectNodeMode.MASTER) {
      m2s.parameters load Arbiter.downMastersFrom(
        ups.map(_.arbiter.m2s.parameters.get)
      )
    }

    //down.decoder.m2s.parameters <- m2s.parameters + down.s.m2s.supported
    for (down <- downs) {
      down.decoder.m2s.parameters.load(Decoder.downMastersFrom(m2s.parameters, down.s.m2s.supported))
    }

    //Generate final connections mapping
    if(mode != InterconnectNodeMode.SLAVE) {
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
        if(mode != InterconnectNodeMode.SLAVE) {
          val fromDowns = downs.map(_.s.s2m.proposed.get).reduce(_ mincover _)
          //        val modified = s2m.proposedModifiers.foldLeft(fromDowns)((e, f) => f(e))
          s2m.proposed load fromDowns
        }

        // s2m.supported <- ups.s2m.supported
        if(mode != InterconnectNodeMode.MASTER) {
          val fromUps = ups.map(_.m.s2m.supported.get).reduce(_ mincover _)
          //        val modified = m2s.supportedModifiers.foldLeft(addressConstrained)((e, f) => f(e))
          s2m.supported load fromUps
        }

        // s2m.parameters <- downs.decoder.s2m.parameters
        if(mode != InterconnectNodeMode.SLAVE){
          s2m.parameters.load(Decoder.upSlavesFrom(downs.map(_.decoder.s2m.parameters.get)))
        }

        //ups.arbiter.s2m.parameters <- s2m.parameters
        for(up <- ups){
          //        up.arbiter.s2m.parameters.load(s2m.parameters)
          up.arbiter.s2m.parameters.load(Arbiter.upSlaveFrom(s2m.parameters, up.m.s2m.supported))
        }
      }
      case false => {
        if(mode != InterconnectNodeMode.SLAVE) {
          s2m.proposed load S2mSupport.none
        }
        if(mode != InterconnectNodeMode.MASTER) {
          s2m.supported load S2mSupport.none
        }
        if(mode != InterconnectNodeMode.SLAVE){
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

    val arbiter = (mode != InterconnectNodeMode.MASTER && ups.size > 1) generate new Area {
      val core = Arbiter(ups.map(up => NodeParameters(
        m = up.arbiter.m2s.parameters,
        s = up.arbiter.s2m.parameters
      )))
      (ups, core.io.ups.map(_.fromCombStage())).zipped.foreach(_.arbiter.bus.load(_))
      val connection = arbiterConnector(bus, core.io.down)
    }

    val noArbiter = (mode != InterconnectNodeMode.MASTER && ups.size == 1) generate new Area {
      ups.head.arbiter.bus.load(cloneOf(bus.get))
      val connection = arbiterConnector(bus, ups.head.arbiter.bus)
    }

    val decoder = (mode != InterconnectNodeMode.SLAVE && downs.size > 1) generate new Area {
      val core = Decoder(bus.p.node, downs.map(_.s.m2s.supported), downs.map(_.decoder.s2m.parameters), downs.map(_.getMapping()), downs.map(_.tag.offset), downs.map(_.mapping.defaultSpec.nonEmpty))
      (downs, core.io.downs.map(_.combStage())).zipped.foreach(_.decoder.bus.load(_))
      val connection = decoderConnector(core.io.up, bus)
    }

    val noDecoder = (mode != InterconnectNodeMode.SLAVE && downs.size == 1) generate new Area {
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

class InterconnectConnection(val m : InterconnectNode, val s : InterconnectNode) extends Area {
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



class CPU() extends Area{
  val node = InterconnectNode.master()

  val logic = Elab build new Area{
    node.m2s.parameters.load(
      M2sParameters(
        addressWidth = 32,
        dataWidth    = 32,
        masters = List(M2sAgent(
          name = this,
          mapping = List(M2sSource(
            id = SizeMapping(0, 4),
            emits = M2sTransfers(
              get = SizeRange.upTo(0x40),
              putFull = SizeRange.upTo(0x40)
            )
          ))
        ))
      )
    )
    node.m2s.setProposedFromParameters()
    slave(node.bus)
  }
}


class Adapter(canCrossClock : Boolean = false,
              canUpsize : Boolean = false,
              canDownSize : Boolean = false,
              canSplit : Boolean = false) extends Area{
  val input  = InterconnectNode.slave()
  val output = InterconnectNode.master()

  hardFork{
    output.m2s.proposed.load(input.m2s.proposed)
    input.m2s.supported.load(output.m2s.supported)
    output.m2s.parameters.load(input.m2s.parameters)
    input.s2m.parameters.load(output.s2m.parameters)
    output.bus << input.bus
  }
}


class VideoIn() extends Area{
  val node = InterconnectNode.master()
  node.m2s.parameters.load(
    M2sParameters(
      addressWidth = 32,
      dataWidth    = 32,
      masters = List(M2sAgent(
        name = this,
        mapping = List(M2sSource(
          id = SizeMapping(0, 4),
          emits = M2sTransfers(
            putFull = SizeRange.upTo(0x40)
          )
        ))
      ))
    )
  )
  hardFork(slave(node.bus))
}

class VideoOut() extends Area{
  val node = InterconnectNode.master()
  node.m2s.parameters.load(
    M2sParameters(
      addressWidth = 32,
      dataWidth    = 32,
      masters = List(M2sAgent(
        name = this,
        mapping = List(M2sSource(
          id = SizeMapping(0, 4),
          emits = M2sTransfers(
            get = SizeRange.upTo(0x40)
          )
        ))
      ))
    )
  )
  hardFork(slave(node.bus))
}



//class Bridge() extends Area{
//  val node = ic.createNode()
//}

class UART() extends Area{
  val node = InterconnectNode.slave()
  node.s2m.parameters.load(
    S2mParameters.none
  )
  node.m2s.supported.loadAsync(
    M2sSupport(
      transfers = node.m2s.proposed.transfers.intersect(
        M2sTransfers(
          get = SizeRange.upTo(0x1000),
          putFull = SizeRange.upTo(0x1000)
        )
      ),
      dataWidth    = 32,
      addressWidth = 8,
      allowExecute = false
    )
  )
  hardFork(master(node.bus))
}

class ROM() extends Area{
  val node = InterconnectNode.slave()
  node.s2m.parameters.load(
    S2mParameters.none
  )
  node.m2s.supported.loadAsync(
    M2sSupport(
      transfers = node.m2s.proposed.transfers.intersect(
        M2sTransfers(
          get = SizeRange.upTo( 0x1000)
        )
      ),
      dataWidth    = 32,
      addressWidth = 7,
      allowExecute = false
    )
  )
  hardFork(master(node.bus))
}

class StreamOut() extends Area{
  val node = InterconnectNode.slave()
  node.s2m.parameters.load(
    S2mParameters.none
  )
  node.m2s.supported.loadAsync(
    M2sSupport(
      transfers = node.m2s.proposed.transfers.intersect(
        M2sTransfers(
          putFull = SizeRange.upTo( 0x1000)
        )
      ),
      dataWidth    = 32,
      addressWidth = 6,
      allowExecute = false
    )
  )
  hardFork(master(node.bus))
}

class CoherentCpu() extends Area{
  val node = InterconnectNode.master()
  node.m2s.parameters.load(
    M2sParameters(
      addressWidth = 32,
      dataWidth    = 32,
      masters = List(M2sAgent(
        name = this,
        mapping = List(M2sSource(
          id = SizeMapping(0, 4),
          emits = M2sTransfers(
            acquireT = SizeRange(0x40),
            acquireB = SizeRange(0x40),
            probeAckData = SizeRange(0x40)
          )
        ))
      ))
    )
  )
  hardFork(slave(node.bus))
}

class CoherencyHubIntegrator() extends Area{
  val memPut = InterconnectNode.master()
  val memGet = InterconnectNode.master()
  val coherents = ArrayBuffer[InterconnectNode]()
  val blockSize = 64

  def addressWidth = coherents.map(_.m2s.proposed.addressWidth).max

  def createPort() ={
    val ret = InterconnectNode.slave()
    coherents += ret

    new MemoryConnection{
      override def m = ret
      override def s = internalConnection
      override def offset = 0
      override def mapping = List(SizeMapping(0, BigInt(1) << addressWidth))
      populate()
      override def sToM(down: MemoryTransfers, args: MappedNode) = {
        down match{
          case t : M2sTransfers => {
            val canGet = t.get.contains(blockSize)
            val canPut = t.putFull.contains(blockSize)
            val isMain = args.node.hasTag(PMA.MAIN)
            t.copy(
              acquireT = if(isMain && canGet && canPut) SizeRange(blockSize) else SizeRange.none,
              acquireB = if(isMain && canGet) SizeRange(blockSize) else SizeRange.none
            )
          }
        }
      }
    }
    ret
  }

  val internalConnection = new Nameable with SpinalTagReady {
    override type RefOwnerType = this.type
  }

  List(memGet, memPut).foreach(node => new MemoryConnection {
    override def m = internalConnection
    override def s = node
    override def offset = 0
    override def mapping = List(SizeMapping(0, BigInt(1) << addressWidth))
    override def sToM(downs: MemoryTransfers, args : MappedNode) = downs
    populate()
  })

  val logic = Elab build new Area{
    val slotsCount = 4
    val cSourceCount = 4
    val dataWidth = coherents.map(_.m2s.proposed.dataWidth).max
    for(node <- coherents){
      node.m2s.supported.loadAsync(
        M2sSupport(
          transfers = node.m2s.proposed.transfers.intersect(
            M2sTransfers(
              acquireT = SizeRange(blockSize),
              acquireB = SizeRange(blockSize),
              get = SizeRange.upTo(blockSize),
              putFull = SizeRange.upTo(blockSize),
              putPartial = SizeRange.upTo(blockSize),
              probeAckData = SizeRange(blockSize)
            )
          ),
          dataWidth = dataWidth,
          addressWidth = addressWidth,
          allowExecute = true
        )
      )

      node.s2m.parameters.load(
        node.m2s.proposed.transfers.withBCE match {
          case false =>  S2mParameters.none
          case true => S2mParameters(List(S2mAgent(
            name = this,
            emits = S2mTransfers(
              probe = SizeRange(blockSize)
            ),
            sinkId = SizeMapping(0, slotsCount)
          )))
        }
      )
      node.s2m.setProposedFromParameters()
    }

    memPut.m2s.parameters.load(
      CoherentHub.downPutM2s(
        name           = this,
        addressWidth   = addressWidth,
        dataWidth      = dataWidth,
        blockSize      = blockSize,
        slotCount      = slotsCount,
        cSourceCount   = cSourceCount
      )
    )
    memPut.m2s.setProposedFromParameters()
    memPut.s2m.supported.load(S2mSupport.none)

    memGet.m2s.parameters.load(
      CoherentHub.downGetM2s(
        name           = this,
        addressWidth   = addressWidth,
        dataWidth      = dataWidth,
        blockSize      = blockSize,
        slotCount      = slotsCount
      )
    )
    memGet.m2s.setProposedFromParameters()
    memGet.s2m.supported.load(S2mSupport.none)


    val hub = new CoherentHub(
      CoherentHubParameters(
        nodes     = coherents.map(_.bus.p.node),
        slotCount = slotsCount,
        cacheSize = 1024,
        wayCount  = 2,
        lineSize  = blockSize,
        cSourceCount = cSourceCount
      )
    )
    for((s, m) <- hub.io.ups zip coherents) s << m.bus
    hub.io.downGet >> memGet.bus
    hub.io.downPut >> memPut.bus
  }
}






object TopGen extends App{
  SpinalVerilog(new Component{
    val slowCd = ClockDomain.external("slow")
    val fastCd = ClockDomain.external("fast")

    val system = fastCd on new Area {
      val cpu0 = new CPU()
      val cpu1 = new CPU()

      val busA = InterconnectNode()
      busA << cpu0.node
      busA << cpu1.node
    }
    val peripheral = slowCd on new Area {
      val peripheralBus = InterconnectNode()
      peripheralBus at(0x10000000, 16 MiB) of system.busA

      val adapter = new Adapter()
      adapter.input << peripheralBus

      val uart0 = new UART()
      uart0.node at 0x100 of adapter.output

      val uart1 = new UART()
      uart1.node at 0x200 of adapter.output
    }

//    uart0.node.mapping.load(SizeMapping(0x100, 0x100))

//    val uart1 = new UART()
//    interconnect.connect(bridgeA.node, uart1.node)
//    uart1.node.mapping.load(SizeMapping(0x200, 0x100))


//    val cpu0 = new CPU()
//    val cpu1 = new CPU()
//
//    val bridgeA = new Bridge()
//    bridgeA.node.mapping.load(DefaultMapping)
//    interconnect.connect(cpu0.node, bridgeA.node)
//    interconnect.connect(cpu1.node, bridgeA.node)
//
//    val bridgeB = new Bridge()
//    bridgeB.node.mapping.load(SizeMapping(0x100, 0x300))
//    interconnect.connect(bridgeA.node, bridgeB.node)

//    val videoIn0 = new VideoIn()
//    interconnect.connect(videoIn0.node, bridgeA.node)
//
//    val videoOut0 = new VideoOut()
//    interconnect.connect(videoOut0.node, bridgeA.node)

//    val uart0 = new UART()
//    interconnect.connect(bridgeB.node, uart0.node)
//    uart0.node.mapping.load(SizeMapping(0x100, 0x100))
//
//    val uart1 = new UART()
//    interconnect.connect(bridgeB.node, uart1.node)
//    uart1.node.mapping.load(SizeMapping(0x200, 0x100))

//    val bridgeC = new Bridge()
//    bridgeC.node.mapping.load(DefaultMapping)
//    interconnect.connect(bridgeB.node, bridgeC.node)

//    val bridgeD = new Bridge()
//    bridgeD.node.mapping.load(DefaultMapping)
//    interconnect.connect(bridgeC.node, bridgeD.node)
//
//    val rom0 = new ROM()
//    rom0.node.mapping.load(SizeMapping(0x300, 0x100))
//    interconnect.connect(bridgeD.node, rom0.node)
//
//    val streamOut = new StreamOut()
//    streamOut.node.mapping.load(SizeMapping(0x400, 0x100))
//    interconnect.connect(bridgeD.node, streamOut.node)

//    val hub = new CoherencyHubIntegrator()
//    interconnect.connect(hub.memGet, bridgeA.node)
//    interconnect.connect(hub.memPut, bridgeA.node)
//
//    val ccpu0 = new CoherentCpu()
//    val c0 = hub.createCoherent()
//    interconnect.connect(ccpu0.node, c0)
//
//    val ccpu1 = new CoherentCpu()
//    val c1 = hub.createCoherent()
//    interconnect.connect(ccpu1.node, c1)

//    val rom0 = new ROM()
//    rom0.node.mapping.load(SizeMapping(0x300, 0x100))
//    interconnect.connect(cpu0.node, rom0.node)

//    val streamOut = new StreamOut()
//    streamOut.node.mapping.load(SizeMapping(0x400, 0x100))
//    interconnect.connect(cpu0.node, streamOut.node)

//    interconnect.connect(cpu0.node, uart0.node)
//    interconnect.connect(cpu1.node, uart0.node)
//    interconnect.connect(cpu0.node, uart1.node)
//    interconnect.connect(cpu1.node, uart1.node)
//    uart0.node.mapping.load(SizeMapping(0x100, 0x100))
//    uart1.node.mapping.load(SizeMapping(0x200, 0x100))
  })
}


