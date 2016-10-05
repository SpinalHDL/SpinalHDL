package spinal.lib.io

import spinal.core._
import spinal.lib.IMasterSlave

case class TriState[T <: Data](dataType : HardType[T]) extends Bundle with IMasterSlave{
  val read,write : T = dataType()
  val writeEnable = Bool

  override def asMaster(): Unit = {
    out(write,writeEnable)
    in(read)
  }
}


case class TriStateArray(width : BitCount) extends Bundle with IMasterSlave{
  val read,write,writeEnable = Bits(width)

  override def asMaster(): Unit = {
    out(write,writeEnable)
    in(read)
  }
}
