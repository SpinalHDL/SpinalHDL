/******************************************************************************
  *  This file describes the Axi4 interface
  *
  *   _________________________________________________________________________
  *  | Global | Write Data | Write Addr | Write Resp | Read Data  | Read Addr  |
  *  |   -    |    w       |    aw      |      b     |     r      |     ar     |
  *  |-------------------------------------------------------------------------|
  *  |  aclk  |  wid       |  *awid     |  *bid      |  *arid     |  rid       |
  *  |  arstn |  wdata     |  awaddr    |  *bresp    |  araddr    |  rdata     |
  *  |        |  *wstrb    |  *awlen    |  buser     |  *arlen    |  rresp     |
  *  |        |  wlast     |  *awsize   |  bvalid    |  *arsize   |  rlast     |
  *  |        |  *wuser    |  *awburst  |  bready    |  *arburst  |  *ruser    |
  *  |        |  wvalid    |  *awlock   |            |  *arlock   |  rvalid    |
  *  |        |  wready    |  *awcache  |            |  *arcache  |  rready    |
  *  |        |            |  awprot    |            |  arprot    |            |
  *  |        |            |  *awqos    |            |  *arqos    |            |
  *  |        |            |  *awregion |            |  *arregion |            |
  *  |        |            |  *awuser   |            |  *aruser   |            |
  *  |        |            |  awvalid   |            |  arvalid   |            |
  *  |        |            |  awready   |            |  arready   |            |
  *  |________|____________|____________|____________|____________|____________|
  *   * Optional signal
  * @TODO add signals for the low power ???
  */


package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._


/**
  * Definition of the constants used by the Axi4 bus
  */
object Axi4{
  object size{
    def apply() = Bits(3 bits)
    def BYTE_1   = B"000"
    def BYTE_2   = B"001"
    def BYTE_4   = B"010"
    def BYTE_8   = B"011"
    def BYTE_16  = B"100"
    def BYTE_32  = B"101"
    def BYTE_64  = B"110"
    def BYTE_128 = B"111"
  }

  object awcache{
    def apply() = Bits(4 bits)
    def OTHER      = B"1000"
    def ALLOCATE   = B"0100"
    def MODIFIABLE = B"0010"
    def BUFFERABLE = B"0001"
  }

  object arcache{
    def apply() = Bits(4 bits)
    def ALLOCATE   = B"1000"
    def OTHER      = B"0100"
    def MODIFIABLE = B"0010"
    def BUFFERABLE = B"0001"
  }

  object burst{
    def apply() = Bits(2 bits)
    def FIXED    = B"00"
    def INCR     = B"01"
    def WRAP     = B"10"
    def RESERVED = B"11"
  }

  object lock{
    def apply() = Bits(2 bits)
    def NORMAL    = B"00"
    def EXCLUSIVE = B"01"
    def LOCKED    = B"10"
    def RESERVED  = B"11"
  }

  object resp{
    def apply() = Bits(2 bits)
    def OKAY   = B"00" // Normal access success
    def EXOKAY = B"01" // Exclusive access okay
    def SLVERR = B"10" // Slave error
    def DECERR = B"11" // Decode error
  }
}



/**
  * Define all access modes
  */
trait Axi4Mode{
  def write = false
  def read = false
}
object WRITE_ONLY extends Axi4Mode{
  override def write = true
}
object READ_ONLY extends Axi4Mode{
  override def read = true
}
object READ_WRITE extends Axi4Mode{
  override def write = true
  override def read = true
}

/**
  * Configuration class for the Axi4 bus
  */
case class Axi4Config( addressWidth : Int,
                      dataWidth    : Int,
                      useId        : Boolean = false,
                      useRegion    : Boolean = false,
                      useBurst     : Boolean = false,
                      useLock      : Boolean = false,
                      useCache     : Boolean = false,
                      useSize      : Boolean = false,
                      useQos       : Boolean = false,
                      useLen       : Boolean = false,
                      useResp      : Boolean = false,
                      useUser      : Boolean = false,
                      useStrb      : Boolean = false,
                      lenWidth     : Int = -1 ,
                      idWidth      : Int = -1,
                      userWidth    : Int = -1 ,
                      mode         : Axi4Mode = READ_WRITE ) {

  def dataByteCount = dataWidth/8

}


/**
  * Definition of the Write/Read address channel
  * @param config Axi4 configuration class
  */
case class Axi4Ax(config: Axi4Config) extends Bundle {
  val addr   = UInt(config.addressWidth bits)
  val id     = if(config.useId)     UInt(config.idWidth bits)   else null
  val region = if(config.useRegion) Bits(4 bits)                else null
  val len    = if(config.useLen)    UInt(config.lenWidth bits)  else null
  val size   = if(config.useSize)   Bits(3 bits)                else null
  val burst  = if(config.useBurst)  Bits(2 bits)                else null
  val lock   = if(config.useLock)   Bits(2 bits)                else null
  val cache  = if(config.useCache)  Bits(4 bits)                else null
  val qos    = if(config.useQos)    Bits(4 bits)                else null
  val user   = if(config.useUser)   Bits(config.userWidth bits) else null
  val prot   = Bits(3 bits)

  import Axi4.burst._

  def setBurstFIXED(): Unit = if(config.useBurst) burst := FIXED
  def setBurstWRAP() : Unit = if(config.useBurst) burst := WRAP
  def setBurstINCR() : Unit = if(config.useBurst) burst := INCR

  def setSize(sizeBurst :Bits) : Unit = if(config.useBurst) size := sizeBurst

  def setLock(lockType :Bits) : Unit = if(config.useLock) lock := lockType

  def setCache(cacheType : Bits) : Unit = if (config.useCache ) cache := cacheType

}


/**
  * Definition of the Write data channel
  * @param config Axi4 configuration class
  */
case class Axi4W(config: Axi4Config) extends Bundle {
  val data = Bits(config.addressWidth bits)
  val strb = if(config.useStrb) Bits(config.dataByteCount bits) else null
  val user = if(config.useUser) Bits(config.userWidth bits)     else null
  val last = if(config.useLen)  Bool                            else null

  def setStrb : Unit = if(config.useStrb) strb := (1 << widthOf(strb))-1
  def setStrb(bytesLane : Bits) : Unit = if(config.useStrb) strb := bytesLane
}


/**
  * Definition of the Write response channel
  * @param config Axi4 configuration class
  */
class Axi4B(config: Axi4Config) extends Bundle {
  val id   = if(config.useId)   UInt(config.idWidth bits)   else null
  val resp = if(config.useResp) Bits(2 bits)                else null
  val user = if(config.useUser) UInt(config.userWidth bits) else null

  import Axi4.resp._

  def setOKAY()   : Unit = resp := OKAY
  def setEXOKAY() : Unit = resp := EXOKAY
  def setSLVERR() : Unit = resp := SLVERR
  def setDECERR() : Unit = resp := DECERR
}


/**
  * Definition of the Read Data channel
  * @param config Axi4 configuration class
  */
class Axi4R(config: Axi4Config) extends Bundle {
  val data = Bits(config.addressWidth bits)
  val resp = if(config.useResp) Bits(2 bits)               else null
  val last = if(config.useLen)  Bool                       else null

  import Axi4.resp._

  def setOKAY()   : Unit = resp := OKAY
  def setEXOKAY() : Unit = resp := EXOKAY
  def setSLVERR() : Unit = resp := SLVERR
  def setDECERR() : Unit = resp := DECERR
}


/**
  * Axi4 interface definition
  * @param config Axi4 configuration class
  */
case class Axi4(config: Axi4Config) extends Bundle with IMasterSlave {
  val aw = if(config.mode.write) Stream(Axi4Ax(config))     else null
  val w  = if(config.mode.write) Stream(Axi4W(config))      else null
  val b  = if(config.mode.write) Stream(new Axi4B(config))  else null
  val ar = if(config.mode.read)  Stream(Axi4Ax(config))     else null
  val r  = if(config.mode.read)  Stream(new Axi4R(config))  else null

  def writeCmd  = aw
  def writeData = w
  def writeRsp  = b
  def readCmd   = ar
  def readRsp   = r

  def >> (that : Axi4) : Unit = {
    assert(that.config == this.config)

    if(config.mode.write){
      this.writeCmd  >> that.writeCmd
      this.writeData >> that.writeData
      this.writeRsp  << that.writeRsp
    }

    if(config.mode.read) {
      this.readCmd  >> that.readCmd
      this.readRsp  << that.readRsp
    }

  }

  def <<(that : Axi4) : Unit = that >> this

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

