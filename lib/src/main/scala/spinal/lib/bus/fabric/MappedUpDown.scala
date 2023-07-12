package spinal.lib.bus.fabric

import spinal.core.{Nameable, SpinalError}
import spinal.lib.bus.misc._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer


trait MappedUpDown[N <: bus.fabric.Node, C <: MappedConnection[N]] extends Nameable {
  var withUps, withDowns = true //Used for assertion
  val ups = ArrayBuffer[C]()
  val downs = ArrayBuffer[C]()

  def setSlaveOnly() : this.type = {
    assert(withUps)
    withDowns = false
    this
  }
  def setMasterOnly() : this.type = {
    assert(withDowns)
    withUps = false
    this
  }

  def <<(m : N): C = {
    val c = connectFrom(m)
    c.mapping.automatic = Some(DefaultMapping)
    c
  }

  def <<(m : Seq[N]): Seq[C] = m.map(this << _)

  class At(body : C => Unit){
    def of(m : N): C = {
      val c = connectFrom(m)
      body(c)
      c
    }
  }
  def at(address : BigInt) = new At(_.mapping.automatic = Some(address))
  def at(address : BigInt, size : BigInt) : At = at(SizeMapping(address, size))
  def at(mapping : AddressMapping) = new At(_.mapping.value load mapping)
  def connectFrom(m : N) : C

  def assertUpDown(): Unit ={
    if(withDowns && downs.isEmpty) SpinalError(s"${getName()} has no slave")
    if(!withDowns && downs.nonEmpty) SpinalError(s"${getName()} has slaves")

    if(withUps && ups.isEmpty) SpinalError(s"${getName()} has no master")
    if(!withUps && ups.nonEmpty) SpinalError(s"${getName()} has masters")
  }
}
