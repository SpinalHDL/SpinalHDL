package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.AddressMapping
import spinal.lib.bus.wishbone.Wishbone


object WeakConnector{
  def apply[T <: Data](source : Bundle,sink : Bundle, by : T,to : T,defaultValue : () => T,allowUpSize: Boolean, allowDownSize: Boolean,allowDrop : Boolean) : Unit = {
    (to != null,by != null) match {
      case (false,false) =>
      case (true,false) => if(defaultValue != null) to := defaultValue() else LocatedPendingError(s"$source can't drive $to because this first doesn't has the corresponding pin")
      case (false,true) => if(!allowDrop) LocatedPendingError(s"$by can't drive $sink because this last one doesn't has the corresponding pin")
      case (true,true) => {
        val toWidth = widthOf(to)
        val byWidth = widthOf(by)
        if(toWidth == byWidth) to := by
        else if(toWidth < byWidth) (if(allowDownSize) to := by.resized else  LocatedPendingError(s"$by can't drive $to because the width do not match"))
        else (if(allowUpSize) to := by.resized else  LocatedPendingError(s"$by can't drive $to because the width do not match"))
      }
    }
  }
}

object Bmb{
  val boundaryWidth = 12
  val boundarySize = 1 << boundaryWidth
  object Cmd {
    object Opcode {
      val READ = 0
      val WRITE = 1

      def isWrite(opcode : Bits) = opcode === WRITE
    }
  }
  object Rsp {
    object Opcode {
      val SUCCESS = 0
      val ERROR = 1
    }
  }

  object Inv {
    object Opcode {
      val EXCEPTED_SOURCE = 0
      val ALL = 1
    }
  }

  def incr(address : UInt, p : BmbParameter) : UInt = {
    val result = UInt(address.getWidth bits)
    val highCat = if (address.getWidth > boundaryWidth) address(address.high downto boundaryWidth) else U""
    val base = address(Math.min(boundaryWidth, address.getWidth) - 1 downto 0).resize(boundaryWidth)
    result := (highCat @@ ((base + p.byteCount) & ~U(p.byteCount-1, widthOf(base) bits))).resized
    result
  }
  def addToAddress(address : UInt, value : UInt, p : BmbParameter) : UInt = {
    val result = UInt(address.getWidth bits)
    val highCat = if (address.getWidth > boundaryWidth) address(address.high downto boundaryWidth) else U""
    val base = address(Math.min(boundaryWidth, address.getWidth) - 1 downto 0).resize(boundaryWidth)
    result := (highCat @@ (base + value)).resized
    result
  }

  def apply(access : BmbAccessParameter, invalidation: BmbInvalidationParameter) : Bmb = Bmb(BmbParameter(access,invalidation))
  def apply(access : BmbAccessParameter) : Bmb = Bmb(BmbParameter(access, BmbInvalidationParameter()))
}

case class BmbMasterParameterIdMapping(range : AddressMapping, maximumPendingTransactionPerId : Int)
case class BmbMasterParameter(idMapping : Seq[BmbMasterParameterIdMapping])
case class BmbSlaveParameter(maximumPendingTransactionPerId : Int)

object BmbParameter{
  object BurstAlignement {
    trait Kind{
      def allowByte : Boolean = false
      def allowWord : Boolean = false
    }
    object BYTE extends Kind{
      override def allowByte = true
      override def allowWord = true
    }
    object WORD extends Kind{
      override def allowWord = true
    }
    object LENGTH extends Kind
  }

  def apply(mr : BmbAccessParameter, sr : BmbInvalidationParameter) : BmbParameter = BmbParameter(
    addressWidth                   = mr.addressWidth,
    dataWidth                      = mr.dataWidth,
    lengthWidth                    = mr.lengthWidth,
    sourceWidth                    = mr.sourceWidth,
    contextWidth                   = mr.contextWidth,
    alignment                      = mr.alignment,
    alignmentMin                   = mr.alignmentMin,
    canRead                        = mr.canRead,
    canWrite                       = mr.canWrite,
    canExclusive                   = mr.canExclusive,
    canInvalidate                  = sr.canInvalidate,
    canSync                        = sr.canSync,
    invalidateLength               = sr.invalidateLength,
    invalidateAlignment            = sr.invalidateAlignment
  )
}

case class BmbParameter(addressWidth : Int,
                        dataWidth : Int,
                        lengthWidth : Int,
                        sourceWidth : Int,
                        contextWidth : Int,
                        alignment : BmbParameter.BurstAlignement.Kind = BmbParameter.BurstAlignement.WORD,
                        alignmentMin : Int = 0,
                        accessLatencyMin : Int = 1,
                        canRead : Boolean = true,
                        canWrite : Boolean = true,
                        canExclusive : Boolean = false,
                        canInvalidate : Boolean = false,
                        canSync : Boolean = false,
                        invalidateLength : Int = 0,
                        invalidateAlignment : BmbParameter.BurstAlignement.Kind = BmbParameter.BurstAlignement.WORD,
                        maximumPendingTransactionPerId : Int = Int.MaxValue){
  assert(dataWidth % 8 == 0)
  assert(isPow2(byteCount))
  def byteCount = dataWidth/8
  def wordMask = byteCount-1
  def wordRange = wordRangeLength -1 downto 0
  def maskWidth = byteCount
  def allowBurst = lengthWidth > wordRangeLength
  def beatCounterWidth = lengthWidth - wordRangeLength + (if(alignment.allowByte) 1 else 0)
  def wordRangeLength = log2Up(byteCount)
  def transferBeatCount = (1 << lengthWidth) / byteCount + (if(alignment.allowByte) 1 else 0)

  def toAccessParameter = BmbAccessParameter(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
    lengthWidth = lengthWidth,
    sourceWidth = sourceWidth,
    contextWidth = contextWidth,
    alignment = alignment,
    alignmentMin = alignmentMin,
    canRead = canRead,
    canWrite = canWrite,
    canExclusive = canExclusive
  )
}

case class BmbAccessParameter(addressWidth : Int,
                              dataWidth : Int,
                              lengthWidth : Int,
                              sourceWidth : Int,
                              contextWidth : Int,
                              alignment : BmbParameter.BurstAlignement.Kind = BmbParameter.BurstAlignement.WORD,
                              alignmentMin : Int = 0,
                              canRead : Boolean = true,
                              canWrite : Boolean = true,
                              canExclusive : Boolean = false){
  def toBmbParameter() = BmbParameter(this, BmbInvalidationParameter())
}

case class BmbInvalidationParameter(canInvalidate : Boolean = false,
                                    canSync : Boolean = false,
                                    invalidateLength : Int = 0,
                                    invalidateAlignment : BmbParameter.BurstAlignement.Kind = BmbParameter.BurstAlignement.WORD)




case class BmbCmd(p : BmbParameter) extends Bundle{
  val source = UInt(p.sourceWidth bits)
  val opcode = Bits(1 bits)
  val exclusive = p.canExclusive generate Bool()
  val address = UInt(p.addressWidth bits)
  val length = UInt(p.lengthWidth bits)
  val data = p.canWrite generate Bits(p.dataWidth bits)
  val mask = p.canWrite generate Bits(p.maskWidth bits)
  val context = Bits(p.contextWidth bits)

  def isWrite = opcode === Bmb.Cmd.Opcode.WRITE
  def isRead = opcode === Bmb.Cmd.Opcode.READ

  def setWrite() = opcode := Bmb.Cmd.Opcode.WRITE
  def setRead() = opcode := Bmb.Cmd.Opcode.READ


  def weakAssignFrom(m : BmbCmd): Unit ={
    def s = this
    WeakConnector(m, s, m.source,  s.source,  defaultValue = null, allowUpSize = true , allowDownSize = false, allowDrop = false)
    WeakConnector(m, s, m.opcode,  s.opcode,  defaultValue = null, allowUpSize = false, allowDownSize = false, allowDrop = false)
    WeakConnector(m, s, m.address, s.address, defaultValue = null, allowUpSize = false, allowDownSize = true , allowDrop = false)
    WeakConnector(m, s, m.length,  s.length,  defaultValue = null, allowUpSize = true, allowDownSize = false , allowDrop = false)
    WeakConnector(m, s, m.data,    s.data,    defaultValue = () => Bits(m.p.dataWidth bits).assignDontCare() , allowUpSize = false, allowDownSize = false, allowDrop = true )
    WeakConnector(m, s, m.mask,    s.mask,    defaultValue = () => Bits(m.p.maskWidth bits).assignDontCare() , allowUpSize = false, allowDownSize = false, allowDrop = true)
    WeakConnector(m, s, m.context, s.context, defaultValue = null, allowUpSize = true,  allowDownSize = false, allowDrop = false)
    WeakConnector(m, s, m.exclusive, s.exclusive, defaultValue = null, allowUpSize = false,  allowDownSize = false, allowDrop = false)
  }

  def transferBeatCountMinusOne : UInt = {
    if(!p.alignment.allowByte){
      length(length.high downto log2Up(p.byteCount))
    } else {
      ((U"0" @@ length) + address(p.wordRange))(length.high + 1 downto log2Up(p.byteCount))
    }
  }
}

case class BmbRsp(p : BmbParameter) extends Bundle{
  val source = UInt(p.sourceWidth bits)
  val opcode = Bits(1 bits)
  val exclusive = p.canExclusive generate Bool()
  val data = p.canRead generate Bits(p.dataWidth bits)
  val context = Bits(p.contextWidth bits)

  def isSuccess = opcode === Bmb.Rsp.Opcode.SUCCESS
  def isError = opcode === Bmb.Rsp.Opcode.ERROR

  def setSuccess() = opcode := Bmb.Rsp.Opcode.SUCCESS
  def setError() = opcode := Bmb.Rsp.Opcode.ERROR

  def weakAssignFrom(m : BmbRsp): Unit ={
    def s = this
    WeakConnector(m, s, m.source,  s.source,  defaultValue = null, allowUpSize = false , allowDownSize = true, allowDrop = false)
    WeakConnector(m, s, m.opcode,  s.opcode,  defaultValue = null, allowUpSize = false, allowDownSize = false, allowDrop = false)
    WeakConnector(m, s, m.data,    s.data,    defaultValue = () => Bits(m.p.dataWidth bits).assignDontCare(), allowUpSize = false, allowDownSize = false, allowDrop = true )
    WeakConnector(m, s, m.context, s.context, defaultValue = null, allowUpSize = false,  allowDownSize = true, allowDrop = false)
    WeakConnector(m, s, m.exclusive, s.exclusive, defaultValue = null, allowUpSize = false,  allowDownSize = false, allowDrop = false)
  }
}

case class BmbInv(p: BmbParameter) extends Bundle{
  val all = Bool() //If cleared, should not invalidate the source
  val address = UInt(p.addressWidth bits)
  val length = UInt(p.invalidateLength bits)
  val source = UInt(p.sourceWidth bits)

  def weakAssignFrom(m : BmbInv): Unit ={
    def s = this
    WeakConnector(m, s, m.source , s.source , defaultValue = null, allowUpSize = false, allowDownSize = false, allowDrop = false)
    WeakConnector(m, s, m.address, s.address, defaultValue = null, allowUpSize = false, allowDownSize = false, allowDrop = false)
    WeakConnector(m, s, m.length , s.length , defaultValue = null, allowUpSize = false, allowDownSize = false, allowDrop = false)
    WeakConnector(m, s, m.all    , s.all    , defaultValue = null, allowUpSize = false, allowDownSize = false, allowDrop = false)
  }

  def transferBeatCountMinusOne(alignSize : Int) : UInt = {
    val wordRange = log2Up(alignSize)-1 downto 0
    if(!p.invalidateAlignment.allowByte){
      length(length.high downto log2Up(alignSize))
    } else {
      ((U"0" @@ length) + address(wordRange))(length.high + 1 downto log2Up(alignSize))
    }
  }
}

case class BmbAck(p: BmbParameter) extends Bundle{
  def weakAssignFrom(m : BmbAck): Unit ={

  }
}

case class BmbSync(p: BmbParameter) extends Bundle{
  val source = UInt(p.sourceWidth bits)

  def weakAssignFrom(m : BmbSync): Unit ={
    def s = this
    WeakConnector(m, s, m.source , s.source , defaultValue = null, allowUpSize = false, allowDownSize = false, allowDrop = false)
  }
}

case class Bmb(p : BmbParameter)  extends Bundle with IMasterSlave {
  val cmd = (p.canRead || p.canWrite) generate Stream(Fragment(BmbCmd(p)))
  val rsp = (p.canRead || p.canWrite) generate Stream(Fragment(BmbRsp(p))) //Out of order across source

  val inv = p.canInvalidate generate Stream(BmbInv(p))
  val ack = p.canInvalidate generate Stream(BmbAck(p)) //In order
  val sync = p.canSync generate Stream(BmbSync(p)) //In order

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
    if(p.canInvalidate) {
      slave(inv)
      master(ack)
    }
    if(p.canSync) {
      slave(sync)
    }
  }


  def <<(m : Bmb) : Unit = {
    val s = this
    s.cmd.arbitrationFrom(m.cmd)
    m.rsp.arbitrationFrom(s.rsp)

    s.cmd.last := m.cmd.last
    m.rsp.last := s.rsp.last

    s.cmd.weakAssignFrom(m.cmd)
    m.rsp.weakAssignFrom(s.rsp)

    if(p.canInvalidate){
      m.inv.arbitrationFrom(s.inv)
      s.ack.arbitrationFrom(m.ack)
      m.inv.weakAssignFrom(s.inv)
      s.ack.weakAssignFrom(m.ack)
    }
    if(p.canSync){
      m.sync.arbitrationFrom(s.sync)
      m.sync.weakAssignFrom(s.sync)
    }
  }
  def >>(s : Bmb) : Unit = s << this

  def cmdM2sPipe(): Bmb = {
    val ret = cloneOf(this)
    this.cmd.m2sPipe() >> ret.cmd
    this.rsp           << ret.rsp
    ret
  }

  def cmdS2mPipe(): Bmb = {
    val ret = cloneOf(this)
    this.cmd.s2mPipe() >> ret.cmd
    this.rsp << ret.rsp
    ret
  }

  def rspdS2mPipe(): Bmb = {
    val ret = cloneOf(this)
    this.cmd >> ret.cmd
    this.rsp << ret.rsp.m2sPipe()
    ret
  }
  def rspdM2sPipe(): Bmb = {
    val ret = cloneOf(this)
    this.cmd >> ret.cmd
    this.rsp << ret.rsp.s2mPipe()
    ret
  }

  def resize(dataWidth : Int): Bmb = this match {
    case _ if dataWidth == p.dataWidth => this
    case _ if dataWidth < p.dataWidth => {
      val bridge = BmbDownSizerBridge(
        inputParameter = p,
        outputParameter = BmbDownSizerBridge.outputParameterFrom(p, dataWidth)
      ).setCompositeName(this, "downSizer", true)
      bridge.io.input << this
      bridge.io.output
    }

    case _ if dataWidth > p.dataWidth => {
      val bridge = BmbUpSizerBridge(
        inputParameter = p,
        outputParameter = BmbUpSizerBridge.outputParameterFrom(p, dataWidth)
      ).setCompositeName(this, "upSizer", true)
      bridge.io.input << this
      bridge.io.output
    }
  }

  def unburstify() : Bmb = {
    val bridge = BmbUnburstify(p).setCompositeName(this, "unburstify", true)
    bridge.io.input << this
    bridge.io.output
  }

  def toWishbone() : Wishbone = {
    val bridge = BmbToWishbone(p).setCompositeName(this, "toWishbone", true)
    bridge.io.input << this
    bridge.io.output
  }

  def pipelined(cmdValid : Boolean = false,
                cmdReady : Boolean = false,
                cmdHalfRate : Boolean = false,
                rspValid : Boolean = false,
                rspReady : Boolean = false,
                rspHalfRate : Boolean = false,
                invValid : Boolean = false,
                invReady : Boolean = false,
                invHalfRate : Boolean = false,
                ackValid : Boolean = false,
                ackReady : Boolean = false,
                ackHalfRate : Boolean = false,
                syncValid : Boolean = false,
                syncReady : Boolean = false,
                syncHalfRate : Boolean = false
               ): Bmb = {
    val ret = Bmb(p)
    ret.cmd << cmd.pipelined(
      m2s = cmdValid,
      s2m = cmdReady,
      halfRate = cmdHalfRate
    )

    rsp << ret.rsp.pipelined(
      m2s = rspValid,
      s2m = rspReady,
      halfRate = rspHalfRate
    )

    if(p.canInvalidate){
      inv << ret.inv.pipelined(
        m2s = invValid,
        s2m = invReady,
        halfRate = invHalfRate
      )
      ret.ack << ack.pipelined(
        m2s = ackValid,
        s2m = ackReady,
        halfRate = ackHalfRate
      )
    }
    
    if(p.canSync){
      sync << ret.sync.pipelined(
        m2s = syncValid,
        s2m = syncReady,
        halfRate = syncHalfRate
      )
    }    
    ret
  }
}


