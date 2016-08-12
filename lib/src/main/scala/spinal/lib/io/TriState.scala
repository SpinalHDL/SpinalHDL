package spinal.lib.io

import spinal.core._
import spinal.lib.IMasterSlave

case class TriState[T <: Data](dataType : T) extends Bundle with IMasterSlave{
  val read,write : T = dataType.clone
  val writeEnable = Bool
  
  override def asMaster(): TriState.this.type = {
    out(write,writeEnable)
    in(read)
    this
  }
}

object TriStateArray{
  def apply(width : BitCount) = new TriStateArray(width.value)
}
case class TriStateArray(width : Int) extends Bundle with IMasterSlave{
  val read,write,writeEnable = Bits(width bits)

  override def asMaster(): TriStateArray.this.type = {
    out(write,writeEnable)
    in(read)
    this
  }
}
