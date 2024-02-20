package spinal.lib.bus.fabric

import spinal.core._
import spinal.core.fiber.{Handle, soon}

import scala.collection.mutable.ArrayBuffer

/**
  * Negotiation handles for master to slave requests
  */
abstract class NegotiateSP[S, P] extends Area{
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
  def forceParameters(p : P): Unit ={
    parameters.load(p)
    setProposedFromParameters()
  }

  def from(m : NegotiateSP[S, P]) ={
    proposed.load(m.proposed)
    m.supported.load(supported)
    parameters.load(m.parameters)
  }

  def applyProposedModifiersOn(that : S) = {
    proposedModifiers.foldLeft(that)((e, f) => f(e))
  }
  def applySupportedModifiersOn(that : S) = {
    supportedModifiers.foldLeft(that)((e, f) => f(e))
  }
  def applyParametersModifiersOn(that : P) = {
    parametersModifiers.foldLeft(that)((e, f) => f(e))
  }

  def soons(that : MappedUpDown[_,_]): Unit ={
    if(that.withUps) soon(proposed, parameters)
    if(that.withDowns) soon(supported)
  }
  def soonsInv(that : MappedUpDown[_,_]): Unit ={
    if(that.withUps) soon(supported)
    if(that.withDowns) soon(parameters, proposed)
  }
}
