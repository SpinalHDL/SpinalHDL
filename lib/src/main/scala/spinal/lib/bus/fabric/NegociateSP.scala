package spinal.lib.bus.fabric

import spinal.core._
import spinal.core.fiber.Handle

import scala.collection.mutable.ArrayBuffer

/**
  * Negotiation handles for master to slave requests
  */
abstract class NegociateSP[S, P] extends Area{
  val proposed = Handle[S]()
  val supported = Handle[S]()
  val parameters = Handle[P]()

  val proposedModifiers = ArrayBuffer[S => S]()
  val supportedModifiers = ArrayBuffer[S => S]()
  val parametersModifiers = ArrayBuffer[P => P]()

  def addModifier(f : S => S) = {
    proposedModifiers += f
    supportedModifiers += f
  }

  def setProposedFromParameters(): Unit

  def from(m : NegociateSP[S, P]) ={
    proposed.load(m.proposed)
    m.supported.load(supported)
    parameters.load(m.parameters)
  }
}
