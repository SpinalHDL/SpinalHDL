package spinal.core

import scala.collection.mutable.ArrayBuffer

/**
  * Created by PIC32F_USER on 05/01/2016.
  */
object LInt{
  def apply(max : BigInt,min : BigInt): LInt = new LInt(max,min)
}

class LInt(val max : BigInt,val min : BigInt) extends MultiData with Num[LInt]{
  assert(max >= min)
  def  isSigned = min < 0
  def peak = max.abs.max(min.abs)
  def bitToRepresent(value : BigInt) = if(value > 0)
    log2Up(value)
  else
    log2Up(1-value) + 1
  def rawBitWidth = Math.max(bitToRepresent(max),bitToRepresent(min))
  val raw = Bits(rawBitWidth bit)

  override def elements: ArrayBuffer[(String, Data)] = ArrayBuffer(("" -> raw))

  override private[core] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = ???



  override def +(right: LInt): LInt = {
    val result = LInt(this.max + right.max,this.min+right.min)
    val resultWidth = result.raw.getWidth
    (this.isSigned,right.isSigned) match{
      case (false,false) => result.raw := (this.raw.toUInt.resize(resultWidth) + right.raw.toUInt.resize(resultWidth)).toBits
      case (_,_) => result.raw := (this.raw.toSInt.resize(resultWidth) + right.raw.toSInt.resize(resultWidth)).toBits
    }
    result
  }

  override def >(right: LInt): Bool = ???

  override def >>(shift: Int): LInt = ???

  override def <=(right: LInt): Bool = ???

  override def <(right: LInt): Bool = ???

  override def <<(shift: Int): LInt = ???

  override def -(right: LInt): LInt = ???

  override def >=(right: LInt): Bool = ???

  override def *(right: LInt): LInt = ???

}
