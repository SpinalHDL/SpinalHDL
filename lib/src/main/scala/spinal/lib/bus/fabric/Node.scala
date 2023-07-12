package spinal.lib.bus.fabric

import spinal.core._
import spinal.core.fiber.{Handle, Lock}

class Node extends Area with SpinalTagReady with SpinalTag {
  val clockDomain = ClockDomain.currentHandle
  val lock = Lock() //Allow to hold the generation of this node
  def await() = {
    lock.await()
  }

  //Document the current component being host for this node
  Component.current.addTag(this)

  override def toString = (if(component != null) component.getPath() + "/"  else "") + getName()
}
