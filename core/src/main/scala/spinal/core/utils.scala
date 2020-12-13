package spinal.core

object flyWireFromTop{
  def apply[T <:BaseType](wire: Bits, name: String) = {
    val topLevel = Component.current.parents().head

    val topwire = if(topLevel.getAllIo.map(_.getName() == name).reduce(_ || _)){
      val t = topLevel.getAllIo.filter(_.getName() == name).head
      if(t.getBitsWidth < wire.getBitsWidth) {
        topLevel.getAllIo.remove(t)
        topLevel.rework(in(cloneOf(wire)).setName(name))
      } else t
    } else topLevel.rework(in(cloneOf(wire)).setName(name))

    Data.doPull(topwire, Component.current, useCache = true, propagateName = true)
  }
}

object flyWireToTop{

}
