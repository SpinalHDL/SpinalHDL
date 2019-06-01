package spinal.lib.com.spi

import spinal.core._
import spinal.lib.{BufferCC, master, IMasterSlave}
import spinal.lib.io.{TriStateOutput, TriStateArray, TriState}


case class SpiKind() extends Bundle {
  val cpol = Bool
  val cpha = Bool
}

case class SpiMaster(ssWidth : Int = 1,
                     useSclk : Boolean = true) extends Bundle with IMasterSlave{
  val ss   = if(ssWidth != 0) Bits(ssWidth bits) else null
  val sclk = if(useSclk)Bool else null
  val mosi = Bool
  val miso = Bool

  override def asMaster(): Unit = {
    out(sclk, mosi)
    if(ssWidth != 0) out(ss)
    in(miso)
  }
}

case class SpiSlave(useSclk : Boolean = true) extends Bundle with IMasterSlave{
  val sclk = if(useSclk)Bool else null
  val mosi = Bool
  val miso = TriStateOutput(Bool)
  val ss   = Bool

  override def asMaster(): Unit = {
    in(sclk, mosi)
    in(ss)
    out(miso)
  }

  def slaveResync() : SpiSlave = {
    val ret = cloneOf(this)
    if(useSclk) ret.sclk := BufferCC(this.sclk)
    ret.ss   := BufferCC(this.ss)
    ret.mosi := BufferCC(this.mosi)
    this.miso.write := ret.miso.write
    this.miso.writeEnable := ret.miso.writeEnable
    ret
  }
}



case class SpiHalfDuplexMaster( dataWidth : Int = 2,
                                ssWidth : Int = 1,
                                useSclk : Boolean = true) extends Bundle with IMasterSlave{
  val ss   = if(ssWidth != 0) Bits(ssWidth bits) else null
  val sclk = if(useSclk)Bool else null
  val data = TriStateArray(dataWidth bits)

  override def asMaster(): Unit = {
    out(sclk)
    if(ssWidth != 0) out(ss)
    master(data)
  }
}

