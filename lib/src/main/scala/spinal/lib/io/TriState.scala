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
