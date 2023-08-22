package spinal.lib.bus.fabric

import spinal.core.{Nameable, SpinalError}
import spinal.lib.bus.misc._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer


trait UpDown[C] extends Nameable{
  var withUps, withDowns = true //Used for assertion
  val ups = ArrayBuffer[C]()
  val downs = ArrayBuffer[C]()

  def setSlaveOnly(): this.type = {
    assert(withUps)
    withDowns = false
    this
  }

  def setMasterOnly(): this.type = {
    assert(withDowns)
    withUps = false
    this
  }

  def assertUpDown(): Unit = {
    if (withDowns && downs.isEmpty) SpinalError(s"${getName()} has no slave")
    if (!withDowns && downs.nonEmpty) SpinalError(s"${getName()} has slaves")

    if (withUps && ups.isEmpty) SpinalError(s"${getName()} has no master")
    if (!withUps && ups.nonEmpty) SpinalError(s"${getName()} has masters")
  }
}

trait MappedUpDown[N <: bus.fabric.Node, C <: MappedConnection[N]] extends UpDown[C]{
  def <<(m : N): C = {
    val c = connectFrom(m)
    c.mapping.automatic = Some(DefaultMapping)
    c
  }

  def <<(m : Seq[N]): Seq[C] = m.map(this << _)
  def <<(first : N, m : N*): Seq[C] = this <<(first :: m.toList)

  class At(body : C => Unit){
    def of(m : N): C = {
      val c = connectFrom(m)
      body(c)
      c
    }
    def of(m : N, others : N*) : Unit = {
      of(m)
      others.foreach(of)
    }
  }
  def at(address : BigInt) = new At(_.mapping.automatic = Some(address))
  def at(address : BigInt, size : BigInt) : At = at(SizeMapping(address, size))
  def at(mapping : AddressMapping) = new At(_.mapping.value load mapping)
  def connectFrom(m : N) : C


  def generateMapping(cToAddressWidth : C => Int): Unit ={
    if(withDowns) {
      var dc = ArrayBuffer[C]()
      downs.foreach{ c =>
        c.mapping.automatic match {
          case Some(v : BigInt) => c.mapping.value load SizeMapping(v, BigInt(1) << cToAddressWidth(c))
          case Some(DefaultMapping) => dc += c
          case Some(x : AddressMapping) => c.mapping.value load x
          case None => c.mapping.value.get
        }
      }
      for(c <- dc){
        val spec = ArrayBuffer[SizeMapping]()
        val others = downs.filter(e => e.mapping.automatic.isEmpty || e.mapping.automatic.get != DefaultMapping).flatMap(_.mapping.value.get match {
          case m : SizeMapping => List(m)
          case m : OrMapping => m.conds.map(_.asInstanceOf[SizeMapping]) //DefaultMapping only supported if all others are sizeMapping
        })
        val sorted = others.sortWith(_.base < _.base)
        var address = BigInt(0)
        val endAt = BigInt(1) << cToAddressWidth(c)
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
  }
}
