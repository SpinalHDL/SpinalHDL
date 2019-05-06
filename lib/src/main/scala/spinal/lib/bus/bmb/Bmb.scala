package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.AddressMapping


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
  object Cmd {
    object Opcode {
      val READ = 0
      val WRITE = 1
    }
  }
  object Rsp {
    object Opcode {
      val SUCCESS = 0
      val ERROR = 1
    }
  }

  def incr(address : UInt, p : BmbParameter) : UInt = {
    val result = UInt(address.getWidth bits)
    val highCat = if (address.getWidth > 12) address(address.high downto 12) else U""
    val base = address(Math.min(12, address.getWidth) - 1 downto 0).resize(12)
    val alignMask = p.byteCount-1
    result := (highCat @@ ((base + p.byteCount) & U(~alignMask).resized)).resized
    result
  }
}

case class BmbMasterParameterIdMapping(range : AddressMapping, maximumPendingTransactionPerId : Int)
case class BmbMasterParameter(idMapping : Seq[BmbMasterParameterIdMapping])
case class BmbSlaveParameter(maximumPendingTransactionPerId : Int)

case class BmbParameter(addressWidth : Int,
                        dataWidth : Int,
                        lengthWidth : Int,
                        sourceWidth : Int,
                        contextWidth : Int,
                        canRead : Boolean = true,
                        canWrite : Boolean = true,
                        allowUnalignedBurst : Boolean  = true,
                        maximumPendingTransactionPerId : Int = Int.MaxValue){
  assert(isPow2(dataWidth))
  def byteCount = dataWidth/8
  def maskWidth = byteCount
  def allowBurst = lengthWidth > log2Up(byteCount)
  def beatCounterWidth = lengthWidth - log2Up(byteCount)
}


case class BmbCmd(p : BmbParameter) extends Bundle{
  val source = UInt(p.sourceWidth bits)
  val opcode = Bits(1 bits)
  val address = UInt(p.addressWidth bits)
  val length = UInt(p.lengthWidth bits)
  val data = p.canWrite generate UInt(p.dataWidth bits)
  val mask = p.canWrite generate UInt(p.maskWidth bits)
  val context = Bits(p.contextWidth bits)

  def isWrite = opcode === Bmb.Cmd.Opcode.WRITE
  def isRead = opcode === Bmb.Cmd.Opcode.READ

  def setWrite = opcode := Bmb.Cmd.Opcode.WRITE
  def setRead = opcode := Bmb.Cmd.Opcode.READ


  def weakAssignFrom(m : BmbCmd): Unit ={
    def s = this
    WeakConnector(m, s, m.source,  s.source,  defaultValue = null, allowUpSize = true , allowDownSize = false, allowDrop = false)
    WeakConnector(m, s, m.opcode,  s.opcode,  defaultValue = null, allowUpSize = false, allowDownSize = false, allowDrop = false)
    WeakConnector(m, s, m.address, s.address, defaultValue = null, allowUpSize = false, allowDownSize = true , allowDrop = false)
    WeakConnector(m, s, m.length,  s.length,  defaultValue = null, allowUpSize = true, allowDownSize = false , allowDrop = false)
    WeakConnector(m, s, m.data,    s.data,    defaultValue = () => Bits(m.p.dataWidth bits).assignDontCare() , allowUpSize = false, allowDownSize = false, allowDrop = true )
    WeakConnector(m, s, m.mask,    s.mask,    defaultValue = () => Bits(m.p.maskWidth bits).assignDontCare() , allowUpSize = false, allowDownSize = false, allowDrop = true)
    WeakConnector(m, s, m.context, s.context, defaultValue = null, allowUpSize = true,  allowDownSize = false, allowDrop = false)
  }
}

case class BmbRsp(p : BmbParameter) extends Bundle{
  val source = UInt(p.sourceWidth bits)
  val opcode = Bits(1 bits)
  val data = p.canRead generate UInt(p.dataWidth bits)
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
  }
}


case class Bmb(p : BmbParameter)  extends Bundle with IMasterSlave {
  val cmd = Stream(Fragment(BmbCmd(p)))
  val rsp = Stream(Fragment(BmbRsp(p)))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }


  def <<(m : Bmb) : Unit = {
    val s = this
    s.cmd << m.cmd
    s.rsp >> m.rsp
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
}
