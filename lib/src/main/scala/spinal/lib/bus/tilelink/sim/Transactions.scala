package spinal.lib.bus.tilelink.sim

import spinal.core.sim._
import spinal.lib.bus.tilelink._
import spinal.sim.SimError

abstract class TransactionABCD {
  var address = BigInt(-1)
  var param = 0
  var source = -1
  var size = -1
  var mask: Array[Byte] = null
  var data: Array[Byte] = null
  var corrupt = false

  def withData : Boolean
  def withMask : Boolean

  def assertBeatOf(that: TransactionABCD, offset: Int): Unit = {
    assert(this.corrupt == that.corrupt)
    assert(this.address - offset == that.address)
    assert(this.param == that.param)
    assert(this.source == that.source)
    assert(this.size == that.size)
  }

  def copyNoData(dst: TransactionABCD, src: TransactionABCD): Unit = {
    dst.address = src.address
    dst.param = src.param
    dst.source = src.source
    dst.size = src.size
  }

  def copyNoData(): this.type
  def isRspOf(that : TransactionABCD) = {
    address == that.address && source == that.source && size == that.size
  }
  def assertRspOf(that : TransactionABCD) = {
    assert(isRspOf(that), s"Bad tilelink transaction\nCMD => $that\nRSP => $this")
  }

  override def toString = {
    val dataString = withData match {
      case false => ""
      case true => {
        val buf = new StringBuilder()
        buf ++= "\n-"
        for(i <- 0 until data.size){
          val enabled = if(withMask) ((mask(i/8) >> (i % 8)) & 1) != 0 else true
          buf ++= (enabled match {
            case false => " --"
            case true => f" ${data(i)}%02x"
          })
        }
        buf.toString()
      }

    }
    f"param=$param source=$source addr=0x$address%x bytes=${1<<size}$dataString"
  }
}

object TransactionA{
  def apply(p : ChannelA) = new TransactionA().setFrom(p)
}
class TransactionA extends TransactionABCD{
  var opcode  : Opcode.A.E = null
  var debugId = -1l

  def setFrom(p : ChannelA): this.type ={
    opcode = p.opcode.toEnum
    param = p.param.toInt
    source = p.source.toInt
    address = p.address.toBigInt
    size = p.size.toInt
    corrupt = p.corrupt.toBoolean
    if(p.withData) {
      mask = p.mask.toBytes
      data = p.data.toBytes
    }
    debugId = p.debugId.toLong
    this
  }

  override def withData = opcode match {
    case Opcode.A.PUT_FULL_DATA | Opcode.A.PUT_PARTIAL_DATA => true
    case _ => false
  }
  override def withMask = withData


  override def assertBeatOf(that: TransactionABCD, offset : Int) = {
    super.assertBeatOf(that, offset)
    assert(this.opcode == that.asInstanceOf[TransactionA].opcode)
    assert(this.debugId == that.asInstanceOf[TransactionA].debugId)
  }

  override def copyNoData() : this.type = {
    val ret = new TransactionA()
    ret.copyNoData(ret, this)
    ret.opcode = opcode
    ret.debugId = debugId
    ret.asInstanceOf[this.type]
  }

  override def toString = opcode + " " + super.toString

  override def isRspOf(that: TransactionABCD) = ???
}

object TransactionB{
  def apply(p : ChannelB) = new TransactionB().setFrom(p)
}
class TransactionB extends TransactionABCD{
  var opcode  : Opcode.B.E = null

  def setFrom(p : ChannelB): this.type ={
    opcode = p.opcode.toEnum
    param = p.param.toInt
    source = p.source.toInt
    address = p.address.toBigInt
    size = p.size.toInt
    corrupt = p.corrupt.toBoolean
    if(p.withData) {
      mask = p.mask.toBytes
      data = p.data.toBytes
    }
    this
  }

  override def withData = opcode match {
    case _ => false
  }
  override def withMask = withData

  override def assertBeatOf(that: TransactionABCD, offset : Int) = {
    super.assertBeatOf(that, offset)
    assert(this.opcode == that.asInstanceOf[TransactionB].opcode)
  }

  override def copyNoData() : this.type = {
    val ret = new TransactionB()
    ret.copyNoData(ret, this)
    ret.opcode = opcode
    ret.asInstanceOf[this.type]
  }

  override def toString = opcode + " " + super.toString
  override def isRspOf(that: TransactionABCD) = ???
}

object TransactionC{
  def apply(p : ChannelC) = new TransactionC().setFrom(p)
}
class TransactionC extends TransactionABCD{
  var opcode  : Opcode.C.E = null

  def setFrom(p : ChannelC): this.type ={
    opcode = p.opcode.toEnum
    param = p.param.toInt
    source = p.source.toInt
    address = p.address.toBigInt
    size = p.size.toInt
    corrupt = p.corrupt.toBoolean
    if(p.withData) {
      data = p.data.toBytes
    }
    this
  }

  override def withData = opcode match {
    case Opcode.C.PROBE_ACK_DATA | Opcode.C.RELEASE_DATA => true
    case _ => false
  }
  override def withMask = false

  override def assertBeatOf(that: TransactionABCD, offset : Int) = {
    super.assertBeatOf(that, offset)
    assert(this.opcode == that.asInstanceOf[TransactionC].opcode)
  }
  override def copyNoData() : this.type = {
    val ret = new TransactionC()
    ret.copyNoData(ret, this)
    ret.opcode = opcode
    ret.asInstanceOf[this.type]
  }

  override def toString = opcode + " " + super.toString
  override def isRspOf(that: TransactionABCD) = ??? ///TODO
}

object TransactionD{
  def apply(p : ChannelD, address : BigInt) = new TransactionD().setFrom(p, address)
}
class TransactionD extends TransactionABCD{
  var opcode  : Opcode.D.E = null
  var denied = false

  def setFrom(p : ChannelD, address : BigInt): this.type ={
    opcode = p.opcode.toEnum
    this.address = address
    param = p.param.toInt
    source = p.source.toInt
    size = p.size.toInt
    denied = p.denied.toBoolean
    corrupt = p.corrupt.toBoolean
    if(p.withData) {
      data = p.data.toBytes
    }
    this
  }

  override def withData = opcode match {
    case Opcode.D.ACCESS_ACK_DATA | Opcode.D.GRANT_DATA => true
    case _ => false
  }
  override def withMask = false

  override def assertBeatOf(that: TransactionABCD, offset : Int) = {
    super.assertBeatOf(that, offset)
    assert(this.opcode == that.asInstanceOf[TransactionD].opcode)
    assert(this.denied == that.asInstanceOf[TransactionD].denied)
  }

  override def copyNoData() : this.type = {
    val ret = new TransactionD()
    ret.copyNoData(ret, this)
    ret.opcode = opcode
    ret.denied = denied
    ret.asInstanceOf[this.type]
  }

  override def toString = opcode + " " + super.toString

  override def isRspOf(that: TransactionABCD) = {
    super.isRspOf(that) && (that match {
      case a: TransactionA => {
        a.opcode match {
          case Opcode.A.GET => {
            opcode == Opcode.D.ACCESS_ACK_DATA
          }
          case Opcode.A.PUT_FULL_DATA | Opcode.A.PUT_PARTIAL_DATA => {
            opcode == Opcode.D.ACCESS_ACK
          }
        }
      }
    })
  }
}


object TransactionE{
  def apply(p : ChannelE) = new TransactionE().setFrom(p)
}
class TransactionE {
  var sink = -1

  def setFrom(p : ChannelE): this.type ={
    sink = p.sink.toInt
    this
  }

  override def toString = sink.toString
}

class TransactionAggregator[T <: TransactionABCD](bytesPerBeat : Int)(callback : T => Unit){
  var access : T = null.asInstanceOf[T]
  var beat = 0
  def push(f : T): Unit ={
    val bytes = 1 << f.size
    val beats = if(f.withData) (bytes+bytesPerBeat-1)/bytesPerBeat else 1
    val bytesInBeat = bytesPerBeat min bytes
    beat match {
      case 0 => {
        access = f.copyNoData().asInstanceOf[T]
        if (access.withData) {
          access.data = Array.fill[Byte](bytes)(0)
          if(access.withMask)access.mask = Array.fill[Byte]((bytes + 7) / 8)(0)
        }
      }
      case _ => {
        f.assertBeatOf(access, beat * bytesPerBeat)
      }
    }

    if(access.withData) {
      val accessOffset = f.address.toInt & (bytes - 1)
      val beatOffset = f.address.toInt & (bytesPerBeat - 1)
      val withMask = access.withMask
      for (i <- 0 until bytesInBeat) {
        val from = beatOffset + i
        val to = accessOffset + i

        if(withMask) {
          val mv = ((f.mask(from / 8) >> (from % 8)) & 1)
          access.mask(to / 8) = (access.mask(to / 8) | (mv << (to % 8))).toByte
          access.data(to) = if(mv == 1) f.data(from) else -1
        } else {
          access.data(to) = f.data(from)
        }
      }
    }

    access.corrupt |= f.corrupt
    access match {
      case access : TransactionD => access.denied |= f.asInstanceOf[TransactionD].denied
      case _ =>
    }

    beat += 1
    if(beat == beats){
      callback(access)
      access = null.asInstanceOf[T]
      beat = 0
    }
  }
}