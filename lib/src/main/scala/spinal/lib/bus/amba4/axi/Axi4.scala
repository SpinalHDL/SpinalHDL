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

  //Return the increment of a address depending the burst configuration (INCR,WRAP,FIXED)
  def incr(address : UInt,burst : Bits,len : UInt,size : UInt,bytePerWord : Int) : UInt = {
    val result = UInt(address.getWidth bits)
    val highCat = if(address.getWidth > 12) address(address.high downto 12) else U""
    val sizeValue = (0 to bytePerWord).map(idx => idx === size).asBits.asUInt
    val base = address(Math.min(12,address.getWidth) - 1 downto 0).resize(12)
    val baseIncr = base + sizeValue
    val wrapCaseMax = 3 + log2Up(bytePerWord)
    val wrapCaseWidth = log2Up(wrapCaseMax+1)
    val wrapCase = size.resize(wrapCaseWidth) + len.mux(
      M"----1---" -> U"11",
      M"-----1--" -> U"10",
      M"------1-" -> U"01",
      default     -> U"00"
    )
    val baseWrap = sizeValue
    switch(burst){
      is(Axi4.burst.FIXED){
        result := address
      }
      is(Axi4.burst.WRAP){
        val cases = Vec((0 until wrapCaseMax).map(i => base(11 downto i + 1) @@ baseIncr(i downto 0)))
        result := highCat @@ cases(wrapCase)
      }
      default{
        result := highCat @@ baseIncr
      }
    }
    result
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
                      useUser      : Boolean = false,
                      useStrb      : Boolean = true,
                      userWidth    : Int = -1,
                      mode         : Axi4Mode = READ_WRITE ) {

  def addressType = UInt(addressWidth bits)
  def dataType = Bits(dataWidth bits)
  def idType = UInt(idWidth bits)
  def lenType = UInt(8 bits)
  def bytePerWord = dataWidth/8
  def symboleRange = log2Up(bytePerWord)-1 downto 0
  def wordRange    = addressWidth-1 downto log2Up(bytePerWord)

  def isWriteOnly = mode == WRITE_ONLY
  def isReadOnly = mode == READ_ONLY


  def asReadOnly() = copy(mode = READ_ONLY)
  def asWriteOnly() = copy(mode = WRITE_ONLY)

  def canRead = mode == READ_ONLY || mode == READ_WRITE
  def canWrite = mode == WRITE_ONLY || mode == READ_WRITE
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
  val size   = if(config.useSize)   UInt(3 bits)                else null
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

  def setSize(sizeBurst :UInt) : Unit = if(config.useBurst) size := sizeBurst
  def setLock(lockType :Bits) : Unit = if(config.useLock) lock := lockType
  def setCache(cacheType : Bits) : Unit = if (config.useCache ) cache := cacheType

}

case class Axi4Ar(config: Axi4Config) extends Axi4Ax(config)
case class Axi4Aw(config: Axi4Config) extends Axi4Ax(config)

object Axi4Ar{
  implicit class StreamPimper(stream : Stream[Axi4Ar]){
    case class Axi4ArUnburstified(axiConfig : Axi4Config) extends Bundle {
      val addr   = UInt(axiConfig.addressWidth bits)
      val id     = if(axiConfig.useId)     UInt(axiConfig.idWidth bits)   else null
      val region = if(axiConfig.useRegion) Bits(4 bits)                else null
      val size   = if(axiConfig.useSize)   UInt(3 bits)                else null
      val burst  = if(axiConfig.useBurst)  Bits(2 bits)                else null
      val lock   = if(axiConfig.useLock)   Bits(1 bits)                else null
      val cache  = if(axiConfig.useCache)  Bits(4 bits)                else null
      val qos    = if(axiConfig.useQos)    Bits(4 bits)                else null
      val user   = if(axiConfig.useUser)   Bits(axiConfig.userWidth bits) else null
      val prot   = Bits(3 bits)
    }
    
    def unburstify : Stream[Fragment[Axi4ArUnburstified]] = {
      case class State() extends Bundle{
        val busy = Bool
        val len = UInt(8 bits)
        val beat = UInt(8 bits)
        val transaction = Axi4ArUnburstified(stream.config)
      }
      val result = Stream Fragment(Axi4ArUnburstified(stream.config))
      val stateNext = State()
      val state = RegNext(stateNext)
      val doResult = Bool

      stateNext := state
      doResult := state.busy

      val addrIncrRange = (Math.min(11,stream.config.addressWidth-1) downto 0)
      stateNext.transaction.addr(addrIncrRange) := Axi4.incr(
        address = state.transaction.addr(addrIncrRange),
        burst = state.transaction.burst,
        len = state.len,
        size = state.transaction.size,
        bytePerWord = stream.config.bytePerWord
      )

      when(result.ready){
        stateNext.beat := state.beat - 1
      }

      when(stream.fire){
        stateNext.busy := True
        stateNext.beat := stream.len
        stateNext.len  := stream.len
        doResult := True
        stateNext.transaction.assignSomeByName(stream.payload)
      }

      when(stateNext.beat === 0){
        stateNext.busy := False
      }

      stream.ready := !state.busy & result.ready

      result.valid := doResult
      result.last := stateNext.beat === 0
      result.fragment := stateNext.transaction
      result
    }
  }
}

/**
  * Definition of the Write data channel
  * @param config Axi4 configuration class
  */
case class Axi4W(config: Axi4Config) extends Bundle {
  val data = Bits(config.dataWidth bits)
  val strb = if(config.useStrb) Bits(config.bytePerWord bits) else null
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
    if(that.config.mode.write && this.config.mode.write){
      this.writeCmd  >> that.writeCmd
      this.writeData >> that.writeData
      this.writeRsp  << that.writeRsp

      that.writeCmd.id.removeAssignements()
      that.writeCmd.id := this.writeCmd.id.resized

      this.writeRsp.id.removeAssignements()
      this.writeRsp.id := that.writeRsp.id.resized
    }

    if(that.config.mode.read && this.config.mode.read) {
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

  def toReadOnly(): Axi4 ={
    assert(config.mode != WRITE_ONLY)
    val ret = Axi4(config.asReadOnly())
    ret << this
    ret
  }
  def toWriteOnly(): Axi4 ={
    assert(config.mode != READ_ONLY)
    val ret = Axi4(config.asWriteOnly())
    ret << this
    ret
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

