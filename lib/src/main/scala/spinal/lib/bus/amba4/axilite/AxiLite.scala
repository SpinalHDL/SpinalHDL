package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._


/**
  * Definition of the constants used by the AXI Lite bus
  */
object AxiLiteCst {

  /**
    * Read Write response
    */
  object resp{
    val OKAY   = B"00" // Normal access success
    val EXOKAY = B"01" // Exclusive access okay
    val SLVERR = B"10" // Slave error
    val DECERR = B"11" // Decode error
  }

  /**
    * Access permissions
    */
  object prot{
    val UNPRIVILEGED_ACCESS = B"000"
    val PRIVILEGED_ACCESS   = B"001"
    val SECURE_ACCESS       = B"000"
    val NON_SECURE_ACCESS   = B"010"
    val DATA_ACCESS         = B"000"
    val INSTRUCTION_ACCESS  = B"100"
  }
}


/**
  * Define all access modes
  */
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


/**
  * Configuration class for the Axi Lite bus
  * @param addressWidth Width of the address bus
  * @param dataWidth    Width of the data bus
  * @param mode         Access mode : WRITE_ONLY, READ_ONLY, READ_WRITE
  */
case class AxiLiteConfig(addressWidth: Int,
                         dataWidth: Int,
                         mode : AxiLiteMode = READ_WRITE){
  def dataByteCount = dataWidth/8
}


/**
  * Definition of the Write/Read address channel
  * @param config Axi Lite configuration class
  */
case class AxiLiteAx(config: AxiLiteConfig) extends Bundle {

  val addr = UInt(config.addressWidth bit)
  val prot = Bits(3 bit)


  import AxiLiteCst.prot._

  def setUnprivileged : Unit = prot := UNPRIVILEGED_ACCESS | SECURE_ACCESS | DATA_ACCESS
  def setPermissions ( permission : Bits ) : Unit = prot := permission
}


/**
  * Definition of the Write data channel
  * @param config Axi Lite configuration class
  */
case class AxiLiteW(config: AxiLiteConfig) extends Bundle {
  val data = Bits(config.dataWidth bit)
  val strb = Bits(config.dataWidth / 8 bit)

  def setStrb : Unit = strb := (1 << widthOf(strb))-1
  def setStrb(bytesLane : Bits) : Unit = strb := bytesLane
}


/**
  * Definition of the Write response channel
  * @param config Axi Lite configuration class
  */
case class AxiLiteB(config: AxiLiteConfig) extends Bundle {
  val resp = Bits(2 bit)

  import AxiLiteCst.resp._

  def setOKAY()   : Unit = resp := OKAY
  def setEXOKAY() : Unit = resp := EXOKAY
  def setSLVERR() : Unit = resp := SLVERR
  def setDECERR() : Unit = resp := DECERR
}



/**
  * Definition of the Read data channel
  * @param config Axi Lite configuration class
  */
case class AxiLiteR(config: AxiLiteConfig) extends Bundle {
  val data = Bits(config.addressWidth bit)
  val resp = Bits(2 bit)

  import AxiLiteCst.resp._

  def setOKAY()   : Unit = resp := OKAY
  def setEXOKAY() : Unit = resp := EXOKAY
  def setSLVERR() : Unit = resp := SLVERR
  def setDECERR() : Unit = resp := DECERR
}


/**
  * Axi Lite interface definition
  * @param config Axi Lite configuration class
  */
case class AxiLite(val config: AxiLiteConfig) extends Bundle with IMasterSlave {

  val aw = if(config.mode.write)  Stream(AxiLiteAx(config)) else null
  val w  = if(config.mode.write)  Stream(AxiLiteW(config))  else null
  val b  = if(config.mode.write)  Stream(AxiLiteB(config))  else null
  val ar = if(config.mode.read)   Stream(AxiLiteAx(config)) else null
  val r  = if(config.mode.read)   Stream(AxiLiteR(config))  else null

  //Because aw w b ar r are ... very lazy
  def writeCmd  = aw
  def writeData = w
  def writeRsp  = b
  def readCmd   = ar
  def readRsp   = r


  def >> (that : AxiLite) : Unit = {
    assert(that.config == this.config)

    if(config.mode.write){
      this.writeCmd  >> that.writeCmd
      this.writeData >> that.writeData
      this.writeRsp  << that.writeRsp
    }

    if(config.mode.read) {
      this.readCmd  >> that.readCmd
      this.readRsp << that.readRsp
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
