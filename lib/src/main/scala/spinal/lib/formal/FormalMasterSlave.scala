package spinal.lib.formal

import spinal.core._
import spinal.lib.IMasterSlave

trait FormalBundle {
  def formalIsValid() : Bool
}

trait FormalMasterSlave extends IMasterSlave {
  def formalIsProducerValid() : Bool
  def formalIsConsumerValid() : Bool = True
  def formalIsValid() : Bool = formalIsConsumerValid() && formalIsProducerValid()
}
