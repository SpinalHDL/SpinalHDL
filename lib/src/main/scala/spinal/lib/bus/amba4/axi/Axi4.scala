/******************************************************************************
  *  This file describes the Axi4 interface
  *
  *   _________________________________________________________________________
  *  | Global | Write Data | Write Addr | Write Resp | Read Addr  | Read Data  |
  *  |   -    |    w       |    aw      |      b     |     ar     |     r      |
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
 * Configuration class for the Axi4 bus
 */
case class Axi4Config(addressWidth : Int,
                      dataWidth    : Int,
                      idWidth      : Int = -1,
                      useId        : Boolean = true,
                      useRegion    : Boolean = true,
                      useBurst     : Boolean = true,
                      useLock      : Boolean = true,
                      useCache     : Boolean = true,
                      useSize      : Boolean = true,
                      useQos       : Boolean = true,
                      useLen       : Boolean = true,
                      useLast      : Boolean = true,
                      useResp      : Boolean = true,
                      useProt      : Boolean = true,
                      useStrb      : Boolean = true,
                      useAllStrb   : Boolean = false,
                      arUserWidth  : Int = -1,
                      awUserWidth  : Int = -1,
                      rUserWidth   : Int = -1,
                      wUserWidth   : Int = -1,
                      bUserWidth   : Int = -1,

                      readIssuingCapability     : Int = -1,
                      writeIssuingCapability    : Int = -1,
                      combinedIssuingCapability : Int = -1,
                      readDataReorderingDepth   : Int = -1) {


  def useArUser = arUserWidth >= 0
  def useAwUser = awUserWidth >= 0
  def useRUser = rUserWidth >= 0
  def useWUser = wUserWidth >= 0
  def useBUser = bUserWidth >= 0
  def useArwUser = arwUserWidth >= 0 //Shared AR/AW channel
  def arwUserWidth = Math.max(arUserWidth, awUserWidth)

  if(useId)
    require(idWidth >= 0,"You need to set idWidth")

  require(combinedIssuingCapability >= scala.math.max(readIssuingCapability, writeIssuingCapability),
    "Inconsistent combined issuing capability")
  require(readDataReorderingDepth <= readIssuingCapability,
    "Inconsistent read data reordering depth")

  require(List(8, 16, 32, 64, 128, 256, 512, 1024) contains dataWidth,
    "Valid data width: 8, 16, 32, 64, 128, 256, 512 or 1024 bit")

  def addressType = UInt(addressWidth bits)
  def dataType = Bits(dataWidth bits)
  def idType = UInt(idWidth bits)
  def lenType = UInt(8 bits)
  def bytePerWord = dataWidth/8
  def symbolRange = log2Up(bytePerWord)-1 downto 0
  def wordRange    = addressWidth-1 downto log2Up(bytePerWord)
  def toFullConfig(defaultIdWidth : Int = 1) : Axi4Config = {
    this.copy(
      idWidth = if(this.useId) this.idWidth else defaultIdWidth,
      useId = true,
      useRegion = true,
      useBurst = true,
      useLock = true,
      useCache = true,
      useSize = true,
      useQos = true,
      useLen = true,
      useLast = true,
      useResp = true,
      useProt = true,
      useStrb = true
    )
  }
}


trait Axi4Bus

/**
 * Axi4 interface definition
 * @param config Axi4 configuration class
 */
case class Axi4(config: Axi4Config) extends Bundle with IMasterSlave with Axi4Bus{

  val aw = Stream(Axi4Aw(config))
  val w  = Stream(Axi4W(config))
  val b  = Stream(Axi4B(config))
  val ar = Stream(Axi4Ar(config))
  val r  = Stream(Axi4R(config))

  def writeCmd  = aw
  def writeData = w
  def writeRsp  = b
  def readCmd   = ar
  def readRsp   = r

  def <<(that : Axi4) : Unit = that >> this
  def >> (that : Axi4) : Unit = {
    this.readCmd drive that.readCmd
    this.writeCmd drive that.writeCmd
    this.writeData drive that.writeData
    that.readRsp drive this.readRsp
    that.writeRsp drive this.writeRsp
  }

  def <<(that : Axi4WriteOnly) : Unit = that >> this
  def >> (that : Axi4WriteOnly) : Unit = {
    this.writeCmd drive that.writeCmd
    this.writeData drive that.writeData
    that.writeRsp drive this.writeRsp
  }


  def <<(that : Axi4ReadOnly) : Unit = that >> this
  def >> (that : Axi4ReadOnly) : Unit = {
    this.readCmd drive that.readCmd
    that.readRsp drive this.readRsp
  }

  def axValidPipe() : Axi4 = {
    val sink = Axi4(config)
    sink.ar << this.ar.validPipe()
    sink.aw << this.aw.validPipe()
    sink.w  << this.w
    sink.r  >> this.r
    sink.b  >> this.b
    sink
  }

  override def asMaster(): Unit = {
    master(ar,aw,w)
    slave(r,b)
  }

  def toReadOnly(idleOthers: Boolean = false): Axi4ReadOnly ={
    val ret = Axi4ReadOnly(config)
    ret << this
    if(idleOthers){
      this.writeCmd.setBlocked()
      this.writeData.setBlocked()
      this.writeRsp.setIdle()
    }
    ret
  }

  def toWriteOnly(idleOthers: Boolean = false): Axi4WriteOnly ={
    val ret = Axi4WriteOnly(config)
    ret << this
    if(idleOthers){
      this.readCmd.setBlocked()
      this.readRsp.setIdle()
    }
    ret
  }

  def setIdle(): this.type = {
    this.writeCmd.setIdle()
    this.writeData.setIdle()
    this.writeRsp.setBlocked()
    this.readCmd.setIdle()
    this.readRsp.setBlocked()
    this
  }

  def setBlocked(): this.type = {
    this.writeCmd.setBlocked()
    this.writeData.setBlocked()
    this.writeRsp.setIdle()
    this.readCmd.setBlocked()
    this.readRsp.setIdle()
    this
  }

  def toShared() : Axi4Shared = {
    Axi4ToAxi4Shared(this)
  }

  def toFullConfig(): Axi4= {
    val ret = Axi4(config.toFullConfig())
    ret << this
    ret
  }

  def pipelined(
    aw: StreamPipe = StreamPipe.NONE,
    w: StreamPipe = StreamPipe.NONE,
    b: StreamPipe = StreamPipe.NONE,
    ar: StreamPipe = StreamPipe.NONE,
    r: StreamPipe = StreamPipe.NONE
  ): Axi4 = {
    val ret = cloneOf(this)
    ret.aw << this.aw.pipelined(aw)
    ret.w << this.w.pipelined(w)
    ret.b.pipelined(b) >> this.b
    ret.ar << this.ar.pipelined(ar)
    ret.r.pipelined(r) >> this.r
    ret
  }
}




/**
  * Definition of the constants used by the Axi4 bus
  */
object Axi4{
  val boundaryWidth = 12

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

  object prot{
    def apply() = Bits(3 bits)
    def PRIVILEGED   = B"001"
    def NON_SECURE   = B"010"
    def INSTRUCTION  = B"100"
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
    val area = new Area {
      val maxSize = log2Up(bytePerWord)
      val validSizeWidth = log2Up(maxSize + 1)
      val validSize = size(0, validSizeWidth bits)
      val result = UInt(address.getWidth bits)
      val highCat = if (address.getWidth > 12) address(address.high downto 12) else U""
      val sizeValue = (0 to maxSize).map(idx => idx === validSize).asBits.asUInt
      val alignMask = (0 to maxSize - 1).map(idx => idx < validSize).asBits.asUInt.resize(12)
      val base = address(Math.min(12, address.getWidth) - 1 downto 0).resize(12) & ~alignMask
      val baseIncr = base + sizeValue
      val wrapCaseMax = maxSize + 3 // 3 is the maximum result of the len.mux() below
      val wrapCaseWidth = log2Up(wrapCaseMax + 1)
      val wrapCase = validSize.resize(wrapCaseWidth) + len.mux(
        M"----1---" -> U"11",
        M"----01--" -> U"10",
        M"----001-" -> U"01",
        default -> U"00"
      )
      switch(burst) {
        is(Axi4.burst.FIXED) {
          result := address
        }
        is(Axi4.burst.WRAP) {
          val cases = Vec((0 to wrapCaseMax).map(i => base(11 downto i + 1) @@ baseIncr(i downto 0)))
          result := (highCat @@ cases(wrapCase)).resized
        }
        default {
          result := (highCat @@ baseIncr).resized
        }
      }
    }.setWeakName("Axi4Incr")
    area.result
  }
}
