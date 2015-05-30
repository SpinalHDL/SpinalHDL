package spinal.lib.bus.axilite

import spinal.core._
import spinal.lib._

case class AxiLiteConfig(addressWidth: Int, dataWidth: Int)

case class AxiLiteAw(config: AxiLiteConfig) extends Bundle {
  val addr = UInt(config.addressWidth bit)
  val prot = UInt(3 bit)
}

case class AxiLiteW(config: AxiLiteConfig) extends Bundle {
  val data = UInt(config.dataWidth bit)
  val strb = UInt(config.dataWidth / 8 bit)
}

case class AxiLiteB(config: AxiLiteConfig) extends Bundle {
  val resp = UInt(2 bit)
}

case class AxiLiteAr(config: AxiLiteConfig) extends Bundle {
  val addr = UInt(config.addressWidth bit)
  val prot = UInt(3 bit)
}

case class AxiLiteR(config: AxiLiteConfig) extends Bundle {
  val data = UInt(config.addressWidth bit)
  val resp = UInt(2 bit)
}

case class AxiLite(config: AxiLiteConfig) extends Bundle with IMasterSlave {
  val aw = Handshake(AxiLiteAw(config))
  val w = Handshake(AxiLiteW(config))
  val b = Handshake(AxiLiteB(config))
  val ar = Handshake(AxiLiteAr(config))
  val r = Handshake(AxiLiteR(config))

  //Because aw w b ar r are ... very lazy
  def writeCmd = aw
  def writeData = w
  def writeRet = b
  def readCmd = ar
  def readData = r


  def >> (that : AxiLite) : Unit = {
    assert(that.config == this.config)
    this.writeCmd >> that.writeCmd
    this.writeData >> that.writeData
    this.writeRet << that.writeRet
    this.readCmd >> that.readCmd
    this.readData << that.readData
  }

  def <<(that : AxiLite) : Unit = that >> this

  override def asMaster: this.type = {
    aw.asMaster
    w.asMaster
    b.asSlave
    ar.asMaster
    r.asSlave
    this
  }

  override def asSlave: this.type = asSlave.flip
}
