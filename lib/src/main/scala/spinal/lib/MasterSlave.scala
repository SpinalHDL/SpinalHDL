package spinal.lib

import spinal.core.Data

trait Interface {
  def asMaster: this.type
  def asSlave: this.type
}

trait MSFactory{
  def postApply(interface : Interface) : Unit = {}
}

trait MS{
  def apply[T <: Interface](i: T) : T

  object Flow extends FlowFactory{
    override def postApply(interface : Interface) : Unit = {
      super.postApply(interface)
      MS.this.apply(interface)
    }
  }

  object Handshake extends HandshakeFactory{
    override def postApply(interface : Interface) : Unit = {
      super.postApply(interface)
      MS.this.apply(interface)
    }
  }
}

object master extends MS{
  override def apply[T <: Interface](i: T) = {
    i.asMaster;
    i
  }
}

object slave extends MS {
  def apply[T <: Interface](i: T) = {
    i.asSlave;
    i
  }
}
