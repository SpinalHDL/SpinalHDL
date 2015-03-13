package spinal.lib

import spinal.core.{Data, Bundle, Bool}

object Fragment{
  def apply[T <: Data](dataType: T) : Fragment[T] = new Fragment(dataType)
}

object FlowFragment{
  def apply[T <: Data](dataType: T) : Flow[Fragment[T]] = Flow(Fragment(dataType))
}
object HandshakeFragment{
  def apply[T <: Data](dataType: T) : Handshake[Fragment[T]] = Handshake(Fragment(dataType))
}

class Fragment[T <: Data](dataType: T) extends Bundle {
  val last = Bool()
  val fragment = dataType.clone

  override def clone: this.type = { new Fragment(dataType).asInstanceOf[this.type]; }
}