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
    connectFrom(m)
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
  def at(address : BigInt) = new At(_.userMapping = Some(address))
  def at(address : BigInt, size : BigInt) : At = at(SizeMapping(address, size))
  def at(mapping : AddressMapping) = new At(_.userMapping = Some(mapping))
  def connectFrom(m : N) : C


  def generateMapping(cToAddressWidth : C => Int): Unit ={
    if(withDowns) {

    }
  }
}
