package spinal.lib.bus.tilelink.meta

import spinal.core._

import spinal.core.fiber.{Fiber, Handle, Lock, soon}
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, OrMapping, SizeMapping}

import scala.collection.mutable.ArrayBuffer

abstract class ConnectionBase[T <: Bundle, C, DC <: DeferredConfig[C], N <: NodeBase[T, C, DC, N, CB], CB <: ConnectionBase[T, C, DC, N, CB]](val m: N, val s: N) extends Area {
  val mapping = new Area {
    var automatic = Option.empty[Any]
    val value = Handle[AddressMapping]
  }

  val up, down = new Area {
    val bus = new Handle[T]
    val m2s = new Handle[DC]
    val s2m = new Handle[DC]
  }

  def decoderAddressWidth(): BigInt
}

abstract class DeferredConfig[C] extends Area {
  val proposed = new Handle[C]
  val supported = new Handle[C]
  val parameters = new Handle[C]

  val proposedModifiers = new ArrayBuffer[C => C]()
  val supportedModifiers = new ArrayBuffer[C => C]()

  def mincover(l: C, r: C): C
  def mergeUpDown(up: C, down: C): C

  def addProposedModifier(modifier: C => C): Unit = proposedModifiers += modifier
  def addSupportedModifier(modifier: C => C): Unit = supportedModifiers += modifier

  def setAddressWidth(c: C, width: BigInt): C
  def getAddressWidth(c: C): BigInt

  def mergeMasters(c: Seq[C]): C
  def downFromMaster(c: C, masters: Int): C
}

abstract class NodeBase[T <: Bundle, C, DC <: DeferredConfig[C], N <: NodeBase[T, C, DC, N, CB], CB <: ConnectionBase[T, C, DC, N, CB]] extends Area with SpinalTagReady with SpinalTag {
  var withUps, withDowns = true //Used for assertion
  val ups = ArrayBuffer[CB]()
  val downs = ArrayBuffer[CB]()

  val clockDomain = ClockDomain.currentHandle
  val bus = Handle[T]()
  val lock = Lock() //Allow to hold the generation of this node

  val m2s: DC
  val s2m: DC

  val thread = Fiber build new Composite(this, weak = false) {
    soon(ups.map(_.down.bus))
    soon(downs.map(_.up.bus))
    soon(downs.map(_.up.m2s))
    soon(ups.map(_.down.s2m))
    soon(bus)

    if (withUps) soon(
      m2s.proposed,
      m2s.parameters,
      s2m.supported
    )
    if (withDowns) soon(
      m2s.supported,
      s2m.parameters,
      s2m.proposed
    )

    lock.await()
    if (withDowns && downs.isEmpty) SpinalError(s"${getName()} has no slave")
    if (!withDowns && downs.nonEmpty) SpinalError(s"${getName()} has slaves")

    if (withUps && ups.isEmpty) SpinalError(s"${getName()} has no master")
    if (!withUps && ups.nonEmpty) SpinalError(s"${getName()} has masters")

    // m2s.proposed <- ups.m2s.proposed
    if (withUps) {
      val fromUps = ups.map(_.m.m2s.proposed.get).reduce(m2s.mincover)
      val modified = m2s.proposedModifiers.foldLeft(fromUps)((e, f) => f(e))
      m2s.proposed load modified
    }

    // m2s.supported <- downs.m2s.supported
    if (withDowns) {
      val fromDowns = downs.map(_.s.m2s.supported.get).reduce(m2s.mincover)
      val addressConstrained = m2s.setAddressWidth(fromDowns, downs.map(_.decoderAddressWidth()).max)
      val modified = m2s.supportedModifiers.foldLeft(addressConstrained)((e, f) => f(e))
      m2s.supported load modified
    }

    // m2s.parameters <- ups.arbiter.m2s.parameters
    if (withUps) {
      m2s.parameters load m2s.mergeMasters(ups.map(_.down.m2s.parameters.get))
    }

    //down.decoder.m2s.parameters <- m2s.parameters + down.s.m2s.supported
    for (down <- downs) {
      down.up.m2s.parameters load m2s.downFromMaster(m2s.parameters, downs.size)
    }

    //Generate final connections mapping
    if (withDowns) {
      val dc = ArrayBuffer[CB]()
      downs.foreach{ c =>
        c.mapping.automatic match {
          case Some(v: BigInt) => c.mapping.value load SizeMapping(v, BigInt(1) << c.up.m2s.getAddressWidth(c.up.m2s.parameters.get).toInt)
          case Some(DefaultMapping) => dc += c
          case Some(x: AddressMapping) => c.mapping.value load x
          case None => c.mapping.value.get
        }
      }
      for (c <- dc) {
        val spec = ArrayBuffer[SizeMapping]()
        val others = downs.filter(_.mapping.automatic.exists(_ != DefaultMapping)).flatMap(_.mapping.value.get match {
          case m: SizeMapping => List(m)
          case m: OrMapping => m.conds.map(_.asInstanceOf[SizeMapping]) //DefaultMapping only supported if all others are sizeMapping
        })
        val sorted = others.sortWith(_.base < _.base)
        var address = BigInt(0)
        val endAt = BigInt(1) <<c.up.m2s.getAddressWidth(c.up.m2s.parameters).toInt
        for (other <- sorted) {
          val size = other.base - address
          if (size != 0) spec += SizeMapping(address, size)
          address = other.base + other.size
        }
        val lastSize = endAt - address
        if (lastSize != 0) spec += SizeMapping(address, lastSize)
        c.mapping.value.load(spec.size match {
          case 0 => ???
          case 1 => spec.head
          case _ => OrMapping(spec)
        })
      }
    }

    // Start hardware generation from that point
    // Generate the node bus
    bus.load(makeBus(m2s.mergeUpDown(m2s.parameters, s2m.parameters)))

    val arbiter = (withUps && ups.size > 1) generate new Area {
      buildArbiter(ups)
    }

    val noArbiter = (withUps && ups.size == 1) generate new Area {
      ups.head.down.bus.load(cloneOf(bus.get))
      arbiterConnection(bus, ups.head.down.bus)
    }

    val decoder = (withDowns && downs.size > 1) generate new Area {
      buildDecoder(downs)
    }

    val noDecoder = (withDowns && downs.size == 1) generate new Area {
      downs.head.up.bus.load(cloneOf(bus.get))
      decoderConnection(downs.head.up.bus, bus)
    }
  }

  def buildArbiter(connections: Seq[CB]): Unit
  def buildDecoder(connections: Seq[CB]): Unit

  def makeBus(config: C): T

  def setSlaveOnly() = {
    assert(withDowns)
    withUps = false
    this.asInstanceOf[N]
  }

  def setMasterOnly() = {
    assert(withUps)
    withDowns = false
    this.asInstanceOf[N]
  }

  def makeConnection(m: N, s: N): CB

  def connect(m: N, s: N): CB = {
    val c = makeConnection(m, s)
    m.downs += c
    s.ups += c
    c
  }

  def <<(m: N): CB = {
    val c = connect(m, this.asInstanceOf[N])
    c.mapping.automatic = Some(DefaultMapping)
    c
  }

  def <<(m: Seq[N]): Seq[CB] = m.map(this << _)

  def arbiterConnection(m: T, s: T): Unit
  def decoderConnection(m: T, s: T): Unit

  class At(body: CB => Unit) {
    def of(m: N): CB = {
      val c = connect(m, NodeBase.this.asInstanceOf[N])
      body(c)
      c
    }
  }

  def at(address: BigInt) = new At(_.mapping.automatic = Some(address))

  def at(address: BigInt, size: BigInt): At = at(SizeMapping(address, size))

  def at(mapping: AddressMapping) = new At(_.mapping.value load mapping)
}
