package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

/**
 * Created by PIC32F_USER on 12/08/2016.
 */

case class Axi4Arw(config: Axi4Config) extends Axi4Ax(config){
  val wr = Bool
}


case class Axi4Shared(config: Axi4Config) extends Bundle with IMasterSlave {

  val arw = Stream(Axi4Arw(config))
  val b = if (config.mode.write) Stream(Axi4B(config)) else null
  val ar = if (config.mode.read) Stream(Axi4Ar(config)) else null
  val r = if (config.mode.read) Stream(Axi4R(config)) else null

  def readWriteCmd = arw
  def writeRsp = b
  def readCmd = ar
  def readRsp = r

  //  def >> (that : Axi4) : Unit = {
  //    if(that.config.mode.write){
  //      this.writeCmd  >> that.writeCmd
  //      this.writeData >> that.writeData
  //      this.writeRsp  << that.writeRsp
  //    }
  //
  //    if(that.config.mode.read) {
  //      this.readCmd >> that.readCmd
  //      this.readRsp << that.readRsp
  //      assert(this.config.idWidth <= that.config.idWidth,s"$this idWidth > $that idWidth")
  //
  //      that.readCmd.id.removeAssignements()
  //      that.readCmd.id := this.readCmd.id.resized
  //
  //      this.readRsp.id.removeAssignements()
  //      this.readRsp.id := that.readRsp.id.resized
  //    }
  //  }

  //  def <<(that : Axi4) : Unit = that >> this

  override def asMaster(): this.type = {
    if (config.mode.write) {
      master(arw)
      slave(b)
    }
    if (config.mode.read) {
      master(ar)
      slave(r)
    }
    this
  }
}
