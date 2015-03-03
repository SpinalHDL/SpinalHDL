package spinal.lib

import spinal._

class Fragment[T <: Data](dataType: T) extends Bundle {
  val last = Bool()
  val fragment = dataType.clone

  override def clone: this.type = { new Fragment(dataType).asInstanceOf[this.type]; }
}