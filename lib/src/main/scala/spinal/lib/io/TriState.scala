package spinal.lib.io

import spinal.core._
import spinal.lib.IMasterSlave

case class TriState[T <: Data](_dataType : T) extends Bundle with IMasterSlave{
  def dataType = cloneOf(_dataType)
  val read,write : T = dataType
  val writeEnable = Bool

  override def asMaster(): Unit = {
    out(write,writeEnable)
    in(read)
  }
}

object TriStateArray{
  def apply(width : BitCount) = new TriStateArray(width.value)
}
case class TriStateArray(width : Int) extends Bundle with IMasterSlave{
  val read,write,writeEnable = Bits(width bits)

  override def asMaster(): Unit = {
    out(write,writeEnable)
    in(read)
  }
}
