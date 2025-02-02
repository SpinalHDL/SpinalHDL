package spinal.lib.formal

import spinal.core._
import spinal.lib.IMasterSlave

trait FormalBundle {
  self =>
  type Self <: FormalBundle

  def formalIsValid() : Bool
  def formalAssertEquivalence(that : Self): Unit = {}
}

trait FormalMasterSlave extends IMasterSlave with FormalBundle {
  def formalIsProducerValid() : Bool
  def formalIsConsumerValid() : Bool = True
  override def formalIsValid() : Bool = formalIsConsumerValid() && formalIsProducerValid()
}
