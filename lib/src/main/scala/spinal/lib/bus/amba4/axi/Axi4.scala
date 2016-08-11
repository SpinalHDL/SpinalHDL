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
    def NORMAL    = B"0"
    def EXCLUSIVE = B"1"
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
case class Axi4Config(addressWidth : Int,
                      dataWidth    : Int,
                      idWidth      : Int,
                      useId        : Boolean = true,
                      useRegion    : Boolean = true,
                      useBurst     : Boolean = true,
                      useLock      : Boolean = true,
                      useCache     : Boolean = true,
                      useSize      : Boolean = true,
                      useQos       : Boolean = true,
                      useLen       : Boolean = true,
                      useResp      : Boolean = true,
                      useUser      : Boolean = true,
                      useStrb      : Boolean = true,
                      userWidth    : Int = 0,
                      mode         : Axi4Mode = READ_WRITE ) {

  def dataByteCount = dataWidth/8
  def isWriteOnly = mode == WRITE_ONLY
  def isReadOnly = mode == READ_ONLY

}


/**
  * Definition of the Write/Read address channel
  * @param config Axi4 configuration class
  */
class Axi4Ax(config: Axi4Config) extends Bundle {
  val addr   = UInt(config.addressWidth bits)
  val id     = if(config.useId)     UInt(config.idWidth bits)   else null
  val region = if(config.useRegion) Bits(4 bits)                else null
  val len    = if(config.useLen)    UInt(8 bits)  else null
  val size   = if(config.useSize)   Bits(3 bits)                else null
  val burst  = if(config.useBurst)  Bits(2 bits)                else null
  val lock   = if(config.useLock)   Bits(1 bits)                else null
  val cache  = if(config.useCache)  Bits(4 bits)                else null
  val qos    = if(config.useQos)    Bits(4 bits)                else null
  val user   = if(config.useUser)   Bits(config.userWidth bits) else null
  val prot   = Bits(3 bits)

  import Axi4.burst._

  def setBurstFIXED(): Unit = {assert(config.useBurst); burst := FIXED}
  def setBurstWRAP() : Unit = {assert(config.useBurst); burst := WRAP}
  def setBurstINCR() : Unit = {assert(config.useBurst); burst := INCR}

  def setSize(sizeBurst :Bits) : Unit = if(config.useBurst) size := sizeBurst
  def setLock(lockType :Bits) : Unit = if(config.useLock) lock := lockType
  def setCache(cacheType : Bits) : Unit = if (config.useCache ) cache := cacheType

}

case class Axi4Ar(config: Axi4Config) extends Axi4Ax(config)
case class Axi4Aw(config: Axi4Config) extends Axi4Ax(config)

/**
  * Definition of the Write data channel
  * @param config Axi4 configuration class
  */
case class Axi4W(config: Axi4Config) extends Bundle {
  val data = Bits(config.dataWidth bits)
  val strb = if(config.useStrb) Bits(config.dataByteCount bits) else null
  val user = if(config.useUser) Bits(config.userWidth bits)     else null
  val last = if(config.useLen)  Bool                            else null

  def setStrb() : Unit = if(config.useStrb) strb := (1 << widthOf(strb))-1
  def setStrb(bytesLane : Bits) : Unit = if(config.useStrb) strb := bytesLane
}


/**
  * Definition of the Write response channel
  * @param config Axi4 configuration class
  */
case class Axi4B(config: Axi4Config) extends Bundle {
  val id   = if(config.useId)   UInt(config.idWidth bits)   else null
  val resp = if(config.useResp) Bits(2 bits)                else null
  val user = if(config.useUser) UInt(config.userWidth bits) else null

  import Axi4.resp._

  def setOKAY()   : Unit = resp := OKAY
  def setEXOKAY() : Unit = resp := EXOKAY
  def setSLVERR() : Unit = resp := SLVERR
  def setDECERR() : Unit = resp := DECERR
  def isOKAY()   : Unit = resp === OKAY
  def isEXOKAY() : Unit = resp === EXOKAY
  def isSLVERR() : Unit = resp === SLVERR
  def isDECERR() : Unit = resp === DECERR
}


/**
  * Definition of the Read Data channel
  * @param config Axi4 configuration class
  */
case class Axi4R(config: Axi4Config) extends Bundle {
  val data = Bits(config.dataWidth bits)
  val id   = if(config.useId)     UInt(config.idWidth bits)   else null
  val resp = if(config.useResp) Bits(2 bits)               else null
  val last = if(config.useLen)  Bool                       else null

  import Axi4.resp._

  def setOKAY()   : Unit = resp := OKAY
  def setEXOKAY() : Unit = resp := EXOKAY
  def setSLVERR() : Unit = resp := SLVERR
  def setDECERR() : Unit = resp := DECERR
  def isOKAY()   : Unit = resp === OKAY
  def isEXOKAY() : Unit = resp === EXOKAY
  def isSLVERR() : Unit = resp === SLVERR
  def isDECERR() : Unit = resp === DECERR
}


/**
  * Axi4 interface definition
  * @param config Axi4 configuration class
  */
case class Axi4(config: Axi4Config) extends Bundle with IMasterSlave {

  val aw = if(config.mode.write) Stream(Axi4Aw(config))     else null
  val w  = if(config.mode.write) Stream(Axi4W(config))      else null
  val b  = if(config.mode.write) Stream(Axi4B(config))      else null
  val ar = if(config.mode.read)  Stream(Axi4Ar(config))     else null
  val r  = if(config.mode.read)  Stream(Axi4R(config))      else null

  def writeCmd  = aw
  def writeData = w
  def writeRsp  = b
  def readCmd   = ar
  def readRsp   = r

  def >> (that : Axi4) : Unit = {
    if(that.config.mode.write){
      this.writeCmd  >> that.writeCmd
      this.writeData >> that.writeData
      this.writeRsp  << that.writeRsp
    }

    if(that.config.mode.read) {
      this.readCmd >> that.readCmd
      this.readRsp << that.readRsp
      assert(this.config.idWidth <= that.config.idWidth,s"$this idWidth > $that idWidth")

      that.readCmd.id.removeAssignements()
      that.readCmd.id := this.readCmd.id.resized

      this.readRsp.id.removeAssignements()
      this.readRsp.id := that.readRsp.id.resized
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

object  Axi4SpecRenamer{
  def apply(that : Axi4): Unit ={
    def doIt = {
      that.flatten.foreach((bt) => {
        bt.setName(bt.getName().replace("_payload_",""))
        bt.setName(bt.getName().replace("_valid","valid"))
        bt.setName(bt.getName().replace("_ready","ready"))
        if(bt.getName().startsWith("io_")) bt.setName(bt.getName().replaceFirst("io_",""))
      })
    }
    if(Component.current == that.component)
      that.component.addPrePopTask(() => {doIt})
    else
      doIt
  }
}

