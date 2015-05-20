package spinal.core

import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC on 19.05.2015.
 */




object SFix{
  def apply(exp : Int,bitCount : Int) = new SFix(exp,bitCount)
}




abstract class XFix[T <: XFix[T,R],R <: Data with Num[R]](val exp : Int,val bitCount : Int) extends MultiData {
  val raw = rawFactory(exp,bitCount)

  override def elements: ArrayBuffer[(String, Data)] = ArrayBuffer(("raw" -> raw))

  def rawFactory(exp : Int,bitCount : Int) : R
  def difLsb(that : T) = (this.exp-this.bitCount) - (that.exp-that.bitCount)
  def alignLsb(that : T) : Tuple2[R,R] = {
    val lsbDif = difLsb(that)
    val left : R = if(lsbDif > 0) (this.raw << lsbDif) else this.raw
    val right : R = if(lsbDif < 0) (that.raw << -lsbDif) else that.raw
    (left,right)
  }
}



class SFix(exp : Int,bitCount : Int) extends XFix[SFix,SInt](exp,bitCount){


  override def rawFactory(exp: Int, bitCount: Int): SInt = SInt(bitCount bit)

  override private[spinal] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {
    that match{
      case that : SFix => {
        val difLsb = this.difLsb(that)
        if(difLsb > 0)
          this.raw := that.raw >> difLsb
        else if(difLsb < 0)
          this.raw := that.raw  << -difLsb
        else
          this.raw := that.raw
      }
      case _ => SpinalError("Undefined assignement")
    }
  }

  def +(that : SFix): SFix = {
    val (rawLeft,rawRight) = alignLsb(that)
    val ret = new SFix(Math.max(this.exp,that.exp),Math.max(rawLeft.getWidth,rawRight.getWidth))
    ret.raw := rawLeft + rawRight
    ret
  }

  def *(that : SFix): SFix = {
    val ret = new SFix(this.exp + that.exp,this.bitCount +that.bitCount)
    ret.raw := this.raw *that.raw
    ret
  }

  def <(that : SFix): Bool = {
    val (rawLeft, rawRight) = alignLsb(that)
    rawLeft < rawRight
  }
  override def clone(): this.type = new SFix(exp,bitCount).asInstanceOf[this.type]
}
