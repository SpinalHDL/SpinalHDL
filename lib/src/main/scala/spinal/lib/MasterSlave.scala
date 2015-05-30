package spinal.lib

import spinal.core.Data

trait IMasterSlave {
  def asMaster: this.type
  def asSlave: this.type
}

trait MSFactory{
  def postApply(interface : IMasterSlave) : Unit = {}
}

trait MS{
  def apply[T <: IMasterSlave](i: T) : T

  object Flow extends FlowFactory{
    override def postApply(interface : IMasterSlave) : Unit = {
      super.postApply(interface)
      MS.this.apply(interface)
    }
  }

  object Stream extends StreamFactory{
    override def postApply(interface : IMasterSlave) : Unit = {
      super.postApply(interface)
      MS.this.apply(interface)
    }
  }

  object event extends EventFactory{
    override def postApply(interface : IMasterSlave) : Unit = {
      super.postApply(interface)
      MS.this.apply(interface)
    }
  }

  def Event : Event = {
    val ret = spinal.lib.Event
    MS.this.apply(ret)
    ret
  }
}

object master extends MS{
  override def apply[T <: IMasterSlave](i: T) = {
    i.asMaster;
    i
  }
}

object slave extends MS {
  def apply[T <: IMasterSlave](i: T) = {
    i.asSlave;
    i
  }
}
