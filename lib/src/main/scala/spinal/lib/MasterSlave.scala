package spinal.lib

import spinal.core.Data

trait Interface {
  def asMaster: this.type
  def asSlave: this.type
}

object master {
  def apply[T <: Interface](i: T) = {
    i.asMaster;
    i
  }

  object Flow {
    def apply[T <: Data](gen: T): Flow[T] = master.apply(new Flow(gen))
  }

  object Handshake {
    def apply[T <: Data](gen: T): Handshake[T] = master.apply(new Handshake(gen))
  }

}

object slave {
  def apply[T <: Interface](i: T) = {
    i.asSlave;
    i
  }

  object Flow {
    def apply[T <: Data](gen: T): Flow[T] = slave.apply(new Flow(gen))
  }

  object Handshake {
    def apply[T <: Data](gen: T): Handshake[T] = slave.apply(new Handshake(gen))
  }

}
