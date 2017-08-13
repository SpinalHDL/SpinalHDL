package spinal.lib.com.spi

import spinal.core._
import spinal.lib.{master, IMasterSlave}
import spinal.lib.io.{TriStateArray, TriState}


case class SpiKind() extends Bundle {
  val cpol = Bool
  val cpha = Bool
}

case class Spi(ssWidth : Int = 1,
               useSclk : Boolean = true) extends Bundle with IMasterSlave{
  val sclk = if(useSclk)Bool else null
  val mosi = Bool
  val miso = Bool
  val ss   = if(ssWidth != 0) Bits(ssWidth bits) else null

  override def asMaster(): Unit = {
    out(sclk, mosi)
    if(ssWidth != 0) out(ss)
    in(miso)
  }
}

case class SpiSio( sioCount : Int,
                   ssWidth : Int = 1,
                   useSclk : Boolean = true) extends Bundle with IMasterSlave{
  val sclk = if(useSclk)Bool else null
  val sio  = TriStateArray(sioCount)
  val ss   = if(ssWidth != 0) Bits(ssWidth bits) else null

  override def asMaster(): Unit = {
    out(sclk)
    master(sio)
    if(ssWidth != 0) out(ss)
  }

  override def asSlave(): Unit = {
    in(sclk)
    master(sio)
    if(ssWidth != 0) in(ss)
  }
}

