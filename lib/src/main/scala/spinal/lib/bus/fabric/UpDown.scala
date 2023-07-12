package spinal.lib.bus.fabric

import spinal.lib.bus.misc._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer


trait UpDown[N <: bus.fabric.Node, C <: ConnectionMapped[N]] {
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
}
