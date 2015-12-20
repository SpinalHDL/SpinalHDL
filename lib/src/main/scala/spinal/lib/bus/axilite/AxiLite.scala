package spinal.lib.bus.axilite

import spinal.core._
import spinal.lib._

case class AxiLiteConfig(addressWidth: Int, dataWidth: Int){
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

case class AxiLiteReadOnly(config: AxiLiteConfig) extends Bundle with IMasterSlave {
  val ar = Stream(AxiLiteAr(config))
  val r = Stream(AxiLiteR(config))

  def readCmd = ar
  def readData = r

  def >> (that : AxiLiteReadOnly) : Unit = {
    assert(that.config == this.config)
    this.readCmd >> that.readCmd
    this.readData << that.readData
  }

  def <<(that : AxiLiteReadOnly) : Unit = that >> this

  override def asMaster(): this.type = {
    ar.asMaster()
    r.asSlave()
    this
  }

  override def asSlave(): this.type = asSlave().flip()
}

case class AxiLiteWriteOnly(config: AxiLiteConfig) extends Bundle with IMasterSlave {
  val aw = Stream(AxiLiteAw(config))
  val w = Stream(AxiLiteW(config))
  val b = Stream(AxiLiteB(config))

  //Because aw w b ar r are ... very lazy
  def writeCmd = aw
  def writeData = w
  def writeRet = b


  def >> (that : AxiLiteWriteOnly) : Unit = {
    assert(that.config == this.config)
    this.writeCmd >> that.writeCmd
    this.writeData >> that.writeData
    this.writeRet << that.writeRet
  }

  def <<(that : AxiLiteWriteOnly) : Unit = that >> this

  override def asMaster(): this.type = {
    aw.asMaster()
    w.asMaster()
    b.asSlave()
    this
  }

  override def asSlave(): this.type = asSlave().flip()
}


case class AxiLite(config: AxiLiteConfig) extends Bundle with IMasterSlave {
  val aw = Stream(AxiLiteAw(config))
  val w = Stream(AxiLiteW(config))
  val b = Stream(AxiLiteB(config))
  val ar = Stream(AxiLiteAr(config))
  val r = Stream(AxiLiteR(config))

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

  override def asMaster(): this.type = {
    aw.asMaster()
    w.asMaster()
    b.asSlave()
    ar.asMaster()
    r.asSlave()
    this
  }

  override def asSlave(): this.type = asSlave().flip()
}
