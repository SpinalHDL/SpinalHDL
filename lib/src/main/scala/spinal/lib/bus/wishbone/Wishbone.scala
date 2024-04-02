package spinal.lib.bus.wishbone

import spinal.core._
import spinal.idslplugin.Location
import spinal.lib._

import scala.language.postfixOps

object AddressGranularity extends Enumeration {
  type AddressGranularity = Value
  @scala.deprecated("This option will be REMOVED in future versions, and the default will be Word granular. It exists " +
    "to prevent changing library behavior for existing code.")
  val UNSPECIFIED = Value

  /**
   * Word granular addressing increments by one per word. This is the standard defined behavior of the bus.
   */
  val WORD = Value

  /**
   * Byte granular addressing increments by one per byte. This is not in adherence with the wishbone standard but is
   * a commonly encountered configuration with external IP.
   */
  val BYTE = Value
}


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
  * @param addressGranularity This specifies the address granularity for the bus.
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
  val useBTE : Boolean = false,
  private val addressGranularity : AddressGranularity.AddressGranularity = AddressGranularity.UNSPECIFIED
){
  def useTGA = tgaWidth > 0
  def useTGC = tgcWidth > 0
  def useTGD = tgdWidth > 0
  def useSEL = selWidth > 0

  def isPipelined = useSTALL

  def pipelined : WishboneConfig = this.copy(useSTALL = true)

  def wordAddressInc(addressGranularityIfUnspecified : AddressGranularity.AddressGranularity = AddressGranularity.UNSPECIFIED): Int = {
    val effectiveAddressGranularity = if (addressGranularity == AddressGranularity.UNSPECIFIED) addressGranularityIfUnspecified else addressGranularity
    require(effectiveAddressGranularity != AddressGranularity.UNSPECIFIED, "If the bus has not been configured with an address granularity, one must be provided to wordAddressInc")
    effectiveAddressGranularity match {
      case AddressGranularity.BYTE => dataWidth / 8
      case _ => 1
    }
  }

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
    out.ports(DAT_MOSI, TGD_MOSI, ADR, CYC, LOCK, SEL, STB, TGA, TGC, WE, CTI, BTE)
    in.ports(DAT_MISO, TGD_MISO, ACK, STALL, ERR, RTY)
  }

  def adaptSTB(stb : Bool, stall : Bool): Wishbone = new Composite(this) {
    if(STALL != null && stall == null) {
      /* Little state machine from chapter 5.1 of the wishbone B4 specification
         States:
         - idle     [wait4ack = False]
         - wait4ack [wait4ack = True]
         Logic:
         if (io.wbs.STB) change state to wait4ack [io.wbs.STB = False]
         if (io.wbs.ACK) change state to idle     [io.wbs.STB = io.wbm.STB]
      */
      val wait4ack = Reg(Bool()) init(False)
      wait4ack.setName("wait4ack")
      // The spec doesn't specify to do this; but if io.wbs.STALL is asserted, it doesn't make sense to wait4ack. In
      // some places it is implied that a master should never assert CYC/STB when stall is high, but that is never a
      // actually stated as a rule anywhere
      val unstalledSTB = STB && !STALL
      when(!wait4ack && unstalledSTB){
        wait4ack := True
      }.elsewhen(wait4ack && ACK){
        wait4ack := False
      }
      STB := !wait4ack && stb
    } else {
      STB := stb
    }

    this
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

    connectTo(that, allowAddressResize = true)
  }

  override def autoConnect(that: Data)(implicit loc: Location): Unit = {
    that match {
      case that: Wishbone => autoConnectImpl(that)
      case _               => SpinalError(s"Function autoConnect is not implemented between $this and $that")
    }
  }

  private def autoConnectImpl (that : Wishbone) : Unit = {
    def dirSolve(that: Data): IODirection = {
      if(that.component == Component.current)
        that.getDirection
      else
        that.getDirection match {
          case `in`    => out
          case `out`   => in
          case `inout` => inout
          case null    => null
        }
    }

    val thisDir = dirSolve(this.CYC)
    val thatDir = dirSolve(that.CYC)
    val thisTrue = this.CYC
    val thatTrue = that.CYC

    (thisDir, thatDir) match {
      case (`out`, `in`) => this << that
      case (`out`, null) => this << that
      case (`in`, `out`) => that << this
      case (`in`, null) => that << this
      case (null, `in`) => this << that
      case (null, `out`) => that << this
      // errors
      case (null, null) => LocatedPendingError(s"AUTOCONNECT FAILED, directionless signals can't be autoconnected ${this} <> ${that}")
      case _ if thisDir != thisTrue.getDirection ^ thatDir != thatTrue.getDirection => LocatedPendingError(s"AUTOCONNECT FAILED, mismatched directions for connections between parent and child component ${this} <> ${that}")
      case _ => LocatedPendingError(s"AUTOCONNECT FAILED, mismatched directions ${this} <> ${that}")
    }
  }

  /** Connect common Wishbone signals
    * @example{{{wishboneSlave << wishboneMaster }}}
    */
  def << (that : Wishbone) : Unit = that >> this

  /** Connect to a wishbone bus with optional resize.
    * This will drop all the signals that are not in common
    * @param that the wishbone bus that i want to connect, must be a wishbone slave
    * @param allowDataResize allow the resize of the data lines, default to false
    * @param allowAddressResize allow the resize of the address line, default to false
    * @param allowTagResize allow the resize of the tag lines, default to false
    */
  def connectTo(that : Wishbone, allowDataResize : Boolean = false, allowAddressResize : Boolean = false, allowTagResize : Boolean = false) : Unit = {
    /////////////////////
    // MINIMAL SIGNALS //
    /////////////////////
    that.CYC      := this.CYC

    // This sets stall and STB
    that.adaptSTB(this.STB, this.STALL)
    that.WE       := this.WE
    this.ACK      := that.ACK

    that.assignByteAddress(this.byteAddress(AddressGranularity.WORD), allowAddressResize = allowAddressResize)
    Wishbone.driveWeak(this.DAT_MOSI,that.DAT_MOSI,allowResize = allowDataResize)
    Wishbone.driveWeak(that.DAT_MISO,this.DAT_MISO,allowResize = allowDataResize)

    ///////////////////////////
    // OPTIONAL FLOW CONTROS //
    ///////////////////////////
    Wishbone.driveWeak(that.STALL,this.STALL, defaultValue = () => !this.ACK && this.CYC, allowDrop = true)
    Wishbone.driveWeak(that.ERR,this.ERR, allowDrop = true)
    Wishbone.driveWeak(this.LOCK,that.LOCK, allowDrop = true)
    Wishbone.driveWeak(that.RTY,this.RTY, allowDrop = true)
    Wishbone.driveWeak(this.SEL,that.SEL, allowDrop = true)
    Wishbone.driveWeak(this.CTI,that.CTI, defaultValue = () => B(0), allowDrop = true)

    //////////
    // TAGS //
    //////////
    Wishbone.driveWeak(this.TGA,that.TGA, allowResize = allowTagResize,allowDrop = true)
    Wishbone.driveWeak(this.TGC,that.TGC, allowResize = allowTagResize,allowDrop = true)
    Wishbone.driveWeak(this.BTE,that.BTE, allowResize = allowTagResize,allowDrop = true)
    Wishbone.driveWeak(that.TGD_MISO,this.TGD_MISO, allowResize = allowTagResize,allowDrop = true)
    Wishbone.driveWeak(this.TGD_MOSI,that.TGD_MOSI, allowResize = allowTagResize,allowDrop = true)
  }

  def isCycle = CYC

  def masterHasRequest = isCycle && STB
  private def slaveRequestAck = if(config.isPipelined) !STALL else ACK
  def isAcceptingRequests = if(config.isPipelined) !STALL else True

  @deprecated("This status check doesn't map pipelined modes correctly, prefer isRequestStalled")
  def isStall : Bool    = if(config.isPipelined)  isCycle && STALL
                          else                    False

  def isRequestStalled  = masterHasRequest && !slaveRequestAck

  @deprecated("This status check doesn't map pipelined modes correctly, prefer isRequestAck")
  def isAck : Bool      = if(config.isPipelined)  isCycle && ACK && !STALL
                          else                    isCycle && ACK && STB
  def isRequestAck      = slaveRequestAck && masterHasRequest

  @deprecated("This status check doesn't map pipelined modes correctly, prefer masterHasRequest or isRequestAck " +
    "depending on whether you want to check if a request exists or if one was acknowledged")
  def isTransfer : Bool = if(config.isPipelined)  isCycle && STB && !STALL
                          else                    isCycle && STB
  def isWrite : Bool    = masterHasRequest &&  WE
  def isRead : Bool     = masterHasRequest && !WE
  def doSend  : Bool    = masterHasRequest && isRequestAck
  def doWrite : Bool    = doSend &&  WE
  def doRead  : Bool    = doSend && !WE

  def byteAddress(addressGranularityIfUnspecified : AddressGranularity.AddressGranularity = AddressGranularity.UNSPECIFIED) : UInt = {
    ADR << log2Up((config.dataWidth / 8) / config.wordAddressInc(addressGranularityIfUnspecified))
  }

  def assignByteAddress(byteAddress : UInt, addressGranularityIfUnspecified : AddressGranularity.AddressGranularity = AddressGranularity.WORD, allowAddressResize : Boolean = false): Unit = {
    val wordAddressSize = config.addressWidth + log2Up(config.wordAddressInc(addressGranularityIfUnspecified))
    val busGranularAddress = byteAddress >> log2Up((config.dataWidth / 8) / config.wordAddressInc(addressGranularityIfUnspecified))
    assert(!allowAddressResize || (wordAddressSize == busGranularAddress.getWidth) || (config.addressWidth == byteAddress.getWidth),
      s"allowAddressResize must be true to assign from an unlike address space for ${this} and ${byteAddress}")
    ADR := busGranularAddress.resized
  }

  def setDefaultStall(): this.type = {
    // A master with [STALL_I] asserted will not initiate new transactions until the [STALL_I]
    // condition is negated. This can easily be achieved with a [STALL_I] as a function of
    // [ACK_O] and [CYC_I].
    if(config.isPipelined) {
      STALL := CYC && !ACK
    }
    this
  }
}

object Wishbone{
  def apply(addressWidth : Int, dataWidth : Int) : Wishbone = Wishbone(WishboneConfig(addressWidth, dataWidth))

  /** Connect to signal with some check
    * This will check if the two signal are null, and if one of them are, connect with some condition
    * @param from must be an input
    * @param to must be an output
    * @param defaultValue if "from" is null, drive "to" with this value
    * @param allowResize allow resize allow the resize of the "from" signal
    * @param allowDrop allow to not connect if one of the two signals are null
    */
  def driveWeak[T <: Data](from: T, to: T, defaultValue: () => T = null, allowResize : Boolean = false, allowDrop: Boolean = false): Unit = {
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
