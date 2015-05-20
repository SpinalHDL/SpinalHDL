package spinal.core

import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC on 19.05.2015.
 */


class XX extends Data{
  override type SSelf = XX
  override def flatten: Seq[BaseType] = ???
  override def getBitsWidth: Int = ???
  override def assignFromBits(bits: Bits): Unit = ???
  override def toBits: Bits = ???
  override private[spinal] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = ???
}


object SFix{
  def apply(exp : Int,bitCount : Int) = new SFix(exp,bitCount)
}

@valClone
class SFix(val exp : Int,val bitCount : Int) extends MultiData{
  override type SSelf = SFix

  override def \(that: SSelf) = super.\(that)
  override def :=(that: SSelf): Unit = super.:=(that)
  override def <>(that: SSelf): Unit = super.<>(that)

  override def elements: ArrayBuffer[(String, Data)] = ArrayBuffer(("theRaw" -> raw))
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
  val raw = SInt(bitCount bit)


  def difLsb(that : SFix) = (this.exp-this.bitCount) - (that.exp-that.bitCount)
  def alignLsb(that : SFix) : Tuple2[SInt,SInt] = {
    val lsbDif = difLsb(that)
    val left = if(lsbDif > 0) (this.raw << lsbDif) else this.raw
    val right = if(lsbDif < 0) (that.raw << -lsbDif) else that.raw
    (left,right)
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
    val (rawLeft,rawRight) = alignLsb(that)
    rawLeft < rawRight
  }

//  override def :=(that: SSelf): Unit = {
//    val difLsb = this.difLsb(that)
//    if(difLsb > 0)
//      this.raw := that.raw >> difLsb
//    else if(difLsb < 0)
//      that.raw << -difLsb
//    else
//      this.raw := that.raw
//  }
}
