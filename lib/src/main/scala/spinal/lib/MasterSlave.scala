package spinal.lib

import spinal.core.Data

trait IMasterSlave {
  def asMaster(): this.type
  def asSlave(): this.type = asMaster.asInstanceOf[Data].flip.asInstanceOf[this.type]
}

//trait MasterSlave extends IMasterSlave{
//  def asMaster() : this.type = {
//    this
//  }
//}

//trait MasterSlave extends IMasterSlave{
//  def applyMaster() : Unit
//  def asMaseter() : this.type = {
//    applyMaster()
//    this
//  }
//}

trait MSFactory{
  def postApply(interface : IMasterSlave) : Unit = {}
}

trait MS{
  def apply[T <: IMasterSlave](i: T) : T
  def apply(a: IMasterSlave,b: IMasterSlave,c: IMasterSlave*) : Unit = {
    apply(a)
    apply(b)
    for(e <- c) apply(e)
  }

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
    this.apply(ret)
    ret
  }
}

object master extends MS{
  override def apply[T <: IMasterSlave](i: T) = {
    i.asMaster();
    i
  }
}

object slave extends MS {
  def apply[T <: IMasterSlave](i: T) = {
    i.asSlave();
    i
  }
}

object slaveWithNull extends MS {
  override def apply[T <: IMasterSlave](that: T): T = if(that != null) that.asSlave() else that
}
object masterWithNull extends MS {
  override def apply[T <: IMasterSlave](that: T): T = if(that != null) that.asMaster() else that
}
