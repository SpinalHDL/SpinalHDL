package spinal.lib.com.sio

import spinal.core._
import spinal.lib.{BufferCC, master, IMasterSlave}
import spinal.lib.io.TriStateArray

/**
 * Created by PIC32F_USER on 31/08/2017.
 */

case class Sio( sioCount : Int,
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

  def slaveResync() : Sio = {
    val ret = cloneOf(this)
    if(useSclk) ret.sclk := BufferCC(this.sclk)
    ret.ss   := BufferCC(this.ss)
    ret.sio.read := BufferCC(this.sio.read)
    this.sio.write := ret.sio.write
    this.sio.writeEnable := ret.sio.writeEnable
    ret
  }
}
