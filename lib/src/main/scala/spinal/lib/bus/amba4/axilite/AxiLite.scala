package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._

trait AxiLiteMode{
  def write = false
  def read = false
}
object WRITE_ONLY extends AxiLiteMode{
  override def write = true
}
object READ_ONLY extends AxiLiteMode{
  override def read = true
}
object READ_WRITE extends AxiLiteMode{
  override def write = true
  override def read = true
}

case class AxiLiteConfig(addressWidth: Int,
                         dataWidth: Int,
                         mode : AxiLiteMode = READ_WRITE){
  def dataByteCount = dataWidth/8
}

case class AxiLiteAw(config: AxiLiteConfig) extends Bundle {
  val addr = UInt(config.addressWidth bit)
  val prot = Bits(3 bit)

  def setUnprivileged : Unit = prot := 0
}

case class AxiLiteW(config: AxiLiteConfig) extends Bundle {
  val data = Bits(config.dataWidth bit)
  val strb = Bits(config.dataWidth / 8 bit)

  def setStrb : Unit = strb := (1 << widthOf(strb))-1
}

case class AxiLiteB(config: AxiLiteConfig) extends Bundle {
  val resp = Bits(2 bit)
}

case class AxiLiteAr(config: AxiLiteConfig) extends Bundle {
  val addr = UInt(config.addressWidth bit)
  val prot = Bits(3 bit)

  def setUnprivileged : Unit = prot := 0
}

case class AxiLiteR(config: AxiLiteConfig) extends Bundle {
  val data = Bits(config.addressWidth bit)
  val resp = Bits(2 bit)
}

case class AxiLite(config: AxiLiteConfig) extends Bundle with IMasterSlave {
  val aw = if(config.mode.write) Stream(AxiLiteAw(config)) else null
  val w = if(config.mode.write)  Stream(AxiLiteW(config)) else null
  val b = if(config.mode.write)  Stream(AxiLiteB(config)) else null
  val ar = if(config.mode.read)  Stream(AxiLiteAr(config)) else null
  val r = if(config.mode.read)   Stream(AxiLiteR(config)) else null

  //Because aw w b ar r are ... very lazy
  def writeCmd = aw
  def writeData = w
  def writeRet = b
  def readCmd = ar
  def readData = r


  def >> (that : AxiLite) : Unit = {
    assert(that.config == this.config)

    if(config.mode.write){
      this.writeCmd >> that.writeCmd
      this.writeData >> that.writeData
      this.writeRet << that.writeRet
    }

    if(config.mode.read) {
      this.readCmd >> that.readCmd
      this.readData << that.readData
    }
  }

  def <<(that : AxiLite) : Unit = that >> this

  override def asMaster(): this.type = {
    if(config.mode.write){
      master(aw,w)
      slave(b)
    }
    if(config.mode.read) {
      master(ar)
      slave(r)
    }
    this
  }
}
