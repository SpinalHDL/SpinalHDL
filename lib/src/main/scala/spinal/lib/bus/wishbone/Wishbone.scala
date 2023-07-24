package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib._

/** This class is used for configuring the Wishbone class
  * @param addressWidth size in bits of the address line
  * @param dataWidth size in bits of the data line
  * @param selWidth size in bits of the selection line, deafult to 0 (disabled)
  * @param useSTALL activate the stall line, default to false (disabled)
  * @param useLOCK activate the lock line, default to false (disabled)
  * @param useERR activate the error line, default to false (disabled)
  * @param useRTY activate the retry line, default to false (disabled)
  * @param useCTI activate the CTI line, deafult to 0 (disabled)
  * @param tgaWidth size in bits of the tag address linie, deafult to 0 (disabled)
  * @param tgcWidth size in bits of the tag cycle line, deafult to 0 (disabled)
  * @param tgdWidth size in bits of the tag data line, deafult to 0 (disabled)
  * @param useBTE activate the Burst Type Extension, default to false (disabled)
  * @example {{{
  * val wishboneBusConf = new WishboneConfig(32,8).withCycleTag(8).withDataTag(8)
  * val wishboneBus = new Wishbone(wishboneBusConf)
  * }}}
  */
case class WishboneConfig(
  val addressWidth : Int,
  val dataWidth : Int,
  val selWidth : Int = 0,
  val useSTALL : Boolean = false,
  val useLOCK : Boolean = false,
  val useERR : Boolean = false,
  val useRTY : Boolean = false,
  val useCTI : Boolean = false,
  val tgaWidth : Int = 0,
  val tgcWidth : Int = 0,
  val tgdWidth : Int = 0,
  val useBTE : Boolean = false
){
  def useTGA = tgaWidth > 0
  def useTGC = tgcWidth > 0
  def useTGD = tgdWidth > 0
  def useSEL = selWidth > 0

  def isPipelined = useSTALL

  def pipelined : WishboneConfig = this.copy(useSTALL = true)

  def withDataTag(size : Int)    : WishboneConfig = this.copy(tgdWidth = size)
  def withAddressTag(size : Int) : WishboneConfig = this.copy(tgaWidth = size)
  def withCycleTag(size : Int)   : WishboneConfig = this.copy(tgdWidth = size)
  def withCycleTypeIdentifier    : WishboneConfig = this.copy(useCTI = true)
  def withBurstType              : WishboneConfig = this.copy(useCTI = true, useBTE = true)
}

/** This class rappresent a Wishbone bus
  * @param config an istance of WishboneConfig, it will be used to configurate the Wishbone Bus
  */
case class Wishbone(config: WishboneConfig) extends Bundle with IMasterSlave {
  /////////////////////
  // MINIMAL SIGNALS //
  /////////////////////
  val CYC       = Bool()
  val STB       = Bool()
  val ACK       = Bool()
  val WE        = Bool()
  val ADR       = UInt(config.addressWidth bits)
  val DAT_MISO  = Bits(config.dataWidth bits)
  val DAT_MOSI  = Bits(config.dataWidth bits)

  ///////////////////////////
  // OPTIONAL FLOW CONTROS //
  ///////////////////////////
  val SEL       = if(config.useSEL)   Bits(config.selWidth bits) else null
  val STALL     = if(config.useSTALL) Bool()                     else null
  val ERR       = if(config.useERR)   Bool()                     else null
  val LOCK      = if(config.useLOCK)  Bool()                     else null
  val RTY       = if(config.useRTY)   Bool()                     else null
  val CTI       = if(config.useCTI)   Bits(3 bits)               else null

  //////////
  // TAGS //
  //////////
  val TGD_MISO  = if(config.useTGD)   Bits(config.tgdWidth bits) else null
  val TGD_MOSI  = if(config.useTGD)   Bits(config.tgdWidth bits) else null
  val TGA       = if(config.useTGA)   Bits(config.tgaWidth bits) else null
  val TGC       = if(config.useTGC)   Bits(config.tgcWidth bits) else null
  val BTE       = if(config.useBTE)   Bits(2 bits)               else null


  override def asMaster(): Unit = {
    outWithNull(DAT_MOSI, TGD_MOSI, ADR, CYC, LOCK, SEL, STB, TGA, TGC, WE, CTI, BTE)
    inWithNull(DAT_MISO, TGD_MISO, ACK, STALL, ERR, RTY)
  }


  /** Clear all the relevant signals in the wishbone bus
    * @example{{{
    * val wishbone1 = master(Wishbone(WishboneConfig(8,8)))
    * val wishbone2 = slave(Wishbone(WishboneConfig(8,8)))
    * val wishbone2 = slave(Wishbone(WishboneConfig(8,8).withDataTag(8)))
    *
    * // this will clear only the following signals: CYC,ADR,DAT_MOSI,STB,WE
    * wishbone1.clearAll()
    * // this will clear only the following signals: DAT_MISO,ACK
    * wishbone2.clearAll()
    * // this will clear only the following signals: DAT_MISO,ACK,TGD_MISO
    * wishbone3.clearAll()
    * }}}
    */
  override def clearAll() : this.type = {
    /////////////////////
    // MINIMAl SIGLALS //
    /////////////////////
    if( isMasterInterface) this.CYC.clear()
    if( isMasterInterface) this.ADR.clearAll()
    if( isMasterInterface) this.DAT_MOSI.clearAll()
    if(!isMasterInterface) this.DAT_MISO.clearAll()
    if( isMasterInterface) this.STB.clear()
    if( isMasterInterface) this.WE.clear()
    if(!isMasterInterface) this.ACK.clear()

    ///////////////////////////
    // OPTIONAL FLOW CONTROS //
    ///////////////////////////
    if(this.config.useSTALL && !isMasterInterface) this.STALL.clear()
    if(this.config.useERR   && !isMasterInterface) this.ERR.clear()
    if(this.config.useLOCK  &&  isMasterInterface) this.LOCK.clear()
    if(this.config.useRTY   && !isMasterInterface) this.RTY.clear()
    if(this.config.useSEL   &&  isMasterInterface) this.SEL.clearAll()
    if(this.config.useCTI   &&  isMasterInterface) this.CTI.clearAll()

    //////////
    // TAGS //
    //////////
    if(this.config.useTGA &&  isMasterInterface) this.TGA.clearAll()
    if(this.config.useTGC &&  isMasterInterface) this.TGC.clearAll()
    if(this.config.useBTE &&  isMasterInterface) this.BTE.clearAll()
    if(this.config.useTGD && !isMasterInterface) this.TGD_MISO.clearAll()
    if(this.config.useTGD &&  isMasterInterface) this.TGD_MOSI.clearAll()

    this
  }

  /** Connect common Wishbone signals
    * this fuction will auto resize the slave address line
    * only if slave.addressWidth <= master.addressWidth
    * @example{{{wishboneMaster >> wishboneSlave}}}
    */
  def >> (that : Wishbone) : Unit = {
    assert(that.config.addressWidth <= this.config.addressWidth)
    assert(that.config.dataWidth == this.config.dataWidth)
    /////////////////////
    // MINIMAL SIGNALS //
    /////////////////////
    that.CYC      := this.CYC
    that.ADR      := this.ADR.resized
    that.DAT_MOSI := this.DAT_MOSI
    this.DAT_MISO := that.DAT_MISO
    that.STB      := this.STB
    that.WE       := this.WE
    this.ACK      := that.ACK

    ///////////////////////////
    // OPTIONAL FLOW CONTROS //
    ///////////////////////////
    Wishbone.driveWeak(that.STALL,this.STALL,null,false,true)
    Wishbone.driveWeak(that.ERR,this.ERR,null,false,true)
    Wishbone.driveWeak(this.LOCK,that.LOCK,null,false,true)
    Wishbone.driveWeak(that.RTY,this.RTY,null,false,true)
    Wishbone.driveWeak(this.SEL,that.SEL,null,false,true)
    Wishbone.driveWeak(this.CTI,that.CTI,null,false,true)


    //////////
    // TAGS //
    //////////
    Wishbone.driveWeak(this.TGA,that.TGA,null,false,true)
    Wishbone.driveWeak(this.TGC,that.TGC,null,false,true)
    Wishbone.driveWeak(this.BTE,that.BTE,null,false,true)
    Wishbone.driveWeak(that.TGD_MISO,this.TGD_MISO,null,false,true)
    Wishbone.driveWeak(this.TGD_MOSI,that.TGD_MOSI,null,false,true)
  }

  /** Connect common Wishbone signals
    * @example{{{wishboneSlave << wishboneMaster }}}
    */
  def << (that : Wishbone) : Unit = that >> this

  /** Connect to a wishbone bus with optional resize.
    * This will drop all the signals that are not in common
    * @param that the wishbone bus that i want to connect, must be a wishbone slave
    * @param allowDataResize allow the resize of the data lines, deafult to false
    * @param allowAddressResize allow the resize of the address line, deafult to false
    * @param allowTagResize allow the resize of the tag lines, deafult to false
    */
  def connectTo(that : Wishbone, allowDataResize : Boolean = false, allowAddressResize : Boolean = false, allowTagResize : Boolean = false) : Unit = {
    /////////////////////
    // MINIMAL SIGNALS //
    /////////////////////
    that.CYC      := this.CYC
    that.STB      := this.STB
    that.WE       := this.WE
    this.ACK      := that.ACK

    Wishbone.driveWeak(this.ADR,that.ADR,null,allowAddressResize,false)
    Wishbone.driveWeak(this.DAT_MOSI,that.DAT_MOSI,null,allowDataResize,false)
    Wishbone.driveWeak(that.DAT_MISO,this.DAT_MISO,null,allowDataResize,false)

    ///////////////////////////
    // OPTIONAL FLOW CONTROS //
    ///////////////////////////
    Wishbone.driveWeak(that.STALL,this.STALL,null,false,true)
    Wishbone.driveWeak(that.ERR,this.ERR,null,false,true)
    Wishbone.driveWeak(this.LOCK,that.LOCK,null,false,true)
    Wishbone.driveWeak(that.RTY,this.RTY,null,false,true)
    Wishbone.driveWeak(this.SEL,that.SEL,null,false,true)
    Wishbone.driveWeak(this.CTI,that.CTI,null,false,true)

    //////////
    // TAGS //
    //////////
    Wishbone.driveWeak(this.TGA,that.TGA,null,allowTagResize,true)
    Wishbone.driveWeak(this.TGC,that.TGC,null,allowTagResize,true)
    Wishbone.driveWeak(this.BTE,that.BTE,null,allowTagResize,true)
    Wishbone.driveWeak(that.TGD_MISO,this.TGD_MISO,null,allowTagResize,true)
    Wishbone.driveWeak(this.TGD_MOSI,that.TGD_MOSI,null,allowTagResize,true)
  }

  def isCycle = CYC
  def isStall : Bool    = if(config.isPipelined)  isCycle && STALL
                          else                    False
  def isAck : Bool      = if(config.isPipelined)  isCycle && ACK && !STALL
                          else                    isCycle && ACK && STB
  def isTransfer : Bool = if(config.isPipelined)  isCycle && STB && !STALL
                          else                    isCycle && STB
  def isWrite : Bool    = isTransfer &&  WE
  def isRead : Bool     = isTransfer && !WE
  def doSend  : Bool    = isTransfer && isAck
  def doWrite : Bool    = doSend &&  WE
  def doRead  : Bool    = doSend && !WE

}

object Wishbone{
  def apply(addressWidth : Int, dataWidth : Int) : Wishbone = Wishbone(WishboneConfig(addressWidth, dataWidth))

  /** Connect to signal with some check
    * This will ceck if the two signal are null, and if one of them are, connect with some condition
    * @param from must be an input
    * @param to must be an output
    * @param defaultValue if "from" is null, drive "to" with this value
    * @param allowResize allow resize allow the resize of the "from" signal
    * @param allowDrop allow to not connect if one of the two siglar are null
    */
  def driveWeak[T <: Data](from: T, to: T, defaultValue: () => T, allowResize : Boolean, allowDrop: Boolean){
    (from != null, to != null) match{
      case (false, false) =>
      case (false, true)  => if(defaultValue != null) to := defaultValue()
      case (true , false) => if(!allowDrop) LocatedPendingError(s"$from can't drive $to because this last one doesn't has the corresponding pin")
      case (true , true)  => to := (if(allowResize) from.resized else from)
    }
  }

  /** Values for each type of cycle
    * This is a commodity object that define the values as specified in the wishbone specification
    */
  object CycleType{
    val classic               = 0
    val constantAddressBurst  = 1
    val incrementingBurst     = 2
    val endOfBurst            = 7
  }

  /** Values for each type burst
    * This is a commodity object that define the values as specified in the wishbone specification
    */
  object BurstType{
    val linearBurst     = 0
    val fourBeatWrap    = 1
    val eightBeatWrap   = 2
    val sixteenBeatWrap = 3
  }
}
