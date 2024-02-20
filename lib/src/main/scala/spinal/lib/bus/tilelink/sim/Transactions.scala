package spinal.lib.bus.tilelink.sim

import spinal.core.sim._
import spinal.lib.bus.tilelink._
import spinal.sim.SimError

import scala.reflect.ClassTag

abstract class TransactionABCD {
  type T <: TransactionABCD
  var address = BigInt(-1)
  var param = 0
  var source = -1
  var size = -1
  var mask: Array[Boolean] = null
  var data: Array[Byte] = null
  var corrupt = false

  def bytes = 1 << size
  def withData : Boolean
  def withMask : Boolean

  def setMask(b : spinal.core.Bits) = {
    val v = b.toBytes
  }
  def assertBeatOf(that: TransactionABCD, offset: Int): Unit = {
    def check(cond : Boolean) = assert(cond, s"Bad burst on :\n$that$this")
    check(this.corrupt == that.corrupt)
    check(this.address - offset == that.address)
    check(this.param == that.param)
    check(this.source == that.source)
    check(this.size == that.size)
  }

  def copyNoDataFrom(src: TransactionABCD): T = {
    address = src.address
    param = src.param
    source = src.source
    size = src.size
    this.asInstanceOf[T]
  }

  def copyNoData()(implicit evidence: ClassTag[T]): T
  def isRspOf(that : TransactionABCD) = {
    address == that.address && source == that.source && size == that.size
  }
  def assertRspOf(that : TransactionABCD) = {
    assert(isRspOf(that), s"Bad tilelink transaction\nCMD => $that\nRSP => $this")
  }

  def serialize(bytesPerBeat : Int)(implicit evidence: ClassTag[T]) : Array[T] = {
    val bytes = 1 << size
    val beats = if(withData) (bytes+bytesPerBeat-1)/bytesPerBeat else 1
    val bytesInBeat = bytesPerBeat min bytes
    val ret = Array.fill(beats)(copyNoData())
    for((beat, beatId) <- ret.zipWithIndex){
      beat.address += beatId*bytesInBeat
      val withMask = this.withMask
      if(withData){
        val address = beat.address.toInt
        var accessOffset = address & (bytes - 1)
        val beatOffset = address & (bytesPerBeat - 1)
        beat.data = new Array[Byte](bytesPerBeat)
        if(bytes < bytesPerBeat) simRandom.nextBytes(beat.data)
        if(withMask) {
          beat.mask = new Array(bytesPerBeat)
        }
        for (i <- 0 until bytesInBeat) {
          val to = beatOffset + i
          val from = accessOffset + i
          if(withMask) {
            beat.mask(to) = mask(from)
            if(mask(from)) {
              beat.data(to) = data(from)
            }
          } else {
            beat.data(to) = data(from)
          }
        }
      }
    }
    ret
  }

  override def toString = {
    val dataString = withData match {
      case false => ""
      case true => {
        val buf = new StringBuilder()
        buf ++= "\n-"
        for(i <- 0 until data.size){
          val enabled = if(withMask) mask(i) else true
          buf ++= (enabled match {
            case false => " --"
            case true => f" ${data(i)}%02x"
          })

        }
        buf.toString()
      }

    }
    f"param=$param source=$source addr=0x$address%x bytes=${1<<size}$dataString\n"
  }
}

object TransactionA{
  def apply(p : ChannelA) : TransactionA = apply().read(p)
  def apply() : TransactionA = new TransactionA()
}
class TransactionA extends TransactionABCD{
  override type T = TransactionA
  var opcode  : Opcode.A.E = null
  var debugId = -1l

  def read(p : ChannelA): this.type ={
    opcode = p.opcode.toEnum
    param = p.param.toInt
    source = p.source.toInt
    address = p.address.toBigInt
    size = p.size.toInt
    if(p.withData) {
      corrupt = p.corrupt.toBoolean
      if(withData) {
        mask = p.mask.toBooleans
        data = p.data.toBytes
      }
    }
    debugId = p.debugId.toLong
    this
  }

  def write(p : ChannelA): this.type ={
    p.opcode #= opcode
    p.param #= param
    p.source #= source
    p.address #= address
    p.size #= size
    if(p.withData) {
      p.corrupt #= corrupt
      if (withData) {
        p.mask #= mask
        p.data #= data
      }
    }
    p.debugId #= debugId
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

  override def copyNoData()(implicit evidence: ClassTag[T]) : T = {
    val ret = new TransactionA()
    ret.copyNoDataFrom(this)
    ret.opcode = opcode
    ret.debugId = debugId
    ret.asInstanceOf[this.type]
  }

  override def toString = opcode + " " + super.toString

  override def isRspOf(that: TransactionABCD) = ???
  def checkAligned(): Unit ={
    assert((address & (1 << size)-1).toInt == 0)
  }
}

object TransactionB{
  def apply() : TransactionB = new TransactionB()
  def apply(p : ChannelB) : TransactionB = apply().read(p)
}
class TransactionB extends TransactionABCD{
  override type T = TransactionB
  var opcode  : Opcode.B.E = null

  def read(p : ChannelB): this.type ={
    opcode = p.opcode.toEnum
    param = p.param.toInt
    source = p.source.toInt
    address = p.address.toBigInt
    size = p.size.toInt
    if(p.withData) {
      corrupt = p.corrupt.toBoolean
      if (withData) {
        mask = p.mask.toBooleans
        data = p.data.toBytes
      }
    }
    this
  }

  def write(p : ChannelB): this.type ={
    p.opcode #= opcode
    p.param #= param
    p.source #= source
    p.address #= address
    p.size #= size
    if(p.withData) {
      p.corrupt #= corrupt
      if (withData) {
        p.mask #= mask
        p.data #= data
      }
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

  override def copyNoData()(implicit evidence: ClassTag[T]) : T = {
    val ret = new TransactionB()
    ret.copyNoDataFrom( this)
    ret.opcode = opcode
    ret.asInstanceOf[this.type]
  }

  override def toString = opcode + " " + super.toString
  override def isRspOf(that: TransactionABCD) = ???
}

object TransactionC{
  def apply() : TransactionC = new TransactionC()
  def apply(p : ChannelC) : TransactionC = apply().read(p)
}
class TransactionC extends TransactionABCD{
  override type T = TransactionC
  var opcode  : Opcode.C.E = null

  def read(p : ChannelC): this.type ={
    opcode = p.opcode.toEnum
    param = p.param.toInt
    source = p.source.toInt
    address = p.address.toBigInt
    size = p.size.toInt
    if(p.withData) {
      corrupt = p.corrupt.toBoolean
      if (withData) {
        data = p.data.toBytes
      }
    }
    this
  }

  def write(p : ChannelC): this.type ={
    p.opcode #= opcode
    p.param #= param
    p.source #= source
    p.size #= size
    p.address #= address
    if(p.withData) {
      p.corrupt #= corrupt
      if(withData) {
        p.data #= data
      } else{
        p.data.randomize()
      }
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
  override def copyNoData()(implicit evidence: ClassTag[T]) : T = {
    val ret = new TransactionC()
    ret.copyNoDataFrom(this)
    ret.opcode = opcode
    ret.asInstanceOf[this.type]
  }

  override def toString = opcode + " " + super.toString
  override def isRspOf(that: TransactionABCD) = ??? ///TODO
}

object TransactionD{
  def apply(p : ChannelD, address : BigInt) = new TransactionD().read(p, address)
  def apply(abcd : TransactionABCD) = new TransactionD().copyNoDataFrom(abcd)
}
class TransactionD extends TransactionABCD{
  override type T = TransactionD
  var opcode  : Opcode.D.E = null
  var sink = BigInt(0)
  var denied = false

  def read(p : ChannelD, address : BigInt): this.type ={
    opcode = p.opcode.toEnum
    this.address = address
    param = p.param.toInt
    source = p.source.toInt
    size = p.size.toInt
    sink = p.sink.toBigInt
    denied = p.denied.toBoolean
    if(p.withData) {
      corrupt = p.corrupt.toBoolean
      if (withData) {
        data = p.data.toBytes
      }
    }
    this
  }
  
  def write(p : ChannelD): this.type ={
    p.opcode #= opcode
    p.param #= param
    p.source #= source
    p.size #= size
    p.denied #= denied
    p.sink #= sink
    if(p.withData) {
      p.corrupt #= corrupt
      if(withData) {
        p.data #= data
      } else{
        p.data.randomize()
      }
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
    assert(this.sink == that.asInstanceOf[TransactionD].sink)
  }

  override def copyNoData()(implicit evidence: ClassTag[T]) : T = {
    val ret = new TransactionD()
    ret.copyNoDataFrom(this)
    ret.opcode = opcode
    ret.denied = denied
    ret.sink = sink
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
          case Opcode.A.ACQUIRE_BLOCK => {
            opcode == Opcode.D.GRANT || opcode == Opcode.D.GRANT_DATA
          }
          case Opcode.A.ACQUIRE_PERM => {
            opcode == Opcode.D.GRANT
          }
        }
      }
    })
  }
}


object TransactionE{
  def apply() : TransactionE = new TransactionE()
  def apply(p : ChannelE) : TransactionE = apply().read(p)
  def apply(sink : BigInt) : TransactionE = {
    val e = new TransactionE()
    e.sink = sink
    e
  }
}
class TransactionE {
  var sink = BigInt(-1)

  def read(p : ChannelE): this.type ={
    sink = p.sink.toInt
    this
  }

  def write(p : ChannelE): this.type ={
    p.sink #= sink
    this
  }

  override def toString = sink.toString
}

class TransactionAggregator[T <: TransactionABCD](bytesPerBeat : Int)(callback : T => Unit)(implicit evidence: ClassTag[T]){
  var access : T = null.asInstanceOf[T]
  var beat = 0
  def push(f : T)(implicit evidence2: ClassTag[f.T]): Unit ={
    val bytes = 1 << f.size
    val beats = if(f.withData) (bytes+bytesPerBeat-1)/bytesPerBeat else 1
    val bytesInBeat = bytesPerBeat min bytes
    beat match {
      case 0 => {
        access = f.copyNoData().asInstanceOf[T]
        if (access.withData) {
          access.data = Array.fill[Byte](bytes)(0)
          if(access.withMask)access.mask = Array.fill[Boolean](bytes)(false)
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
          access.mask(to) = f.mask(from)
          access.data(to) = if(f.mask(from)) f.data(from) else -1
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