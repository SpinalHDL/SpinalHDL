package spinal.lib.io

import spinal.core._
import spinal.lib._

/**
  * Define an open drain readable port
  */
case class ReadableOpenDrain[T<: Data](dataType : HardType[T]) extends Bundle with IMasterSlave{
  val write,read : T = dataType()

  override def asMaster(): Unit = {
    out(write)
    in(read)
  }
}
