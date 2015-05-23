package spinal.core



import scala.collection.mutable.ArrayBuffer


/**
 * Created by PIC on 19.05.2015.
 */


object SFix{
  def apply(exp : Int,bitCount : Int) : SFix = new SFix(exp,bitCount)
  def apply(config: FixedPointFactory) : SFix = config.SFix
}


case class FixedPointFactory(exp : Int,bitCount : Int){
  def SFix = spinal.core.SFix.apply(exp,bitCount)
}


abstract class XFix[T <: XFix[T,R],R <: Data with Num[R]](val exp : Int,val bitCount : Int) extends MultiData {
  val raw = rawFactory(exp,bitCount)

  override def elements: ArrayBuffer[(String, Data)] = ArrayBuffer(("raw" -> raw))

  def rawFactory(exp : Int,bitCount : Int) : R
  def fixFactory(exp : Int,bitCount : Int) : T

  def difLsb(that : T) = (this.exp-this.bitCount) - (that.exp-that.bitCount)
  def alignLsb(that : T) : Tuple2[R,R] = {
    val lsbDif = difLsb(that)
    val left : R = if(lsbDif > 0) (this.raw << lsbDif) else this.raw
    val right : R = if(lsbDif < 0) (that.raw << -lsbDif) else that.raw
    (left,right)
  }


  def doAddSub(that : T,sub : Boolean) : T = {
    val (rawLeft,rawRight) = alignLsb(that)
    val ret = fixFactory(Math.max(this.exp,that.exp),Math.max(rawLeft.getBitsWidth,rawRight.getBitsWidth))
    ret.raw := (if(sub) rawLeft - rawRight else rawLeft + rawRight)
    ret
  }

  def doMul(that : T) : T = {
    val ret = fixFactory(this.exp + that.exp,this.bitCount +that.bitCount)
    ret.raw := this.raw *that.raw
    ret
  }

  def doSmaller(that : T) : Bool = {
    val (rawLeft, rawRight) = alignLsb(that)
    rawLeft < rawRight
  }

  def doSmallerEguals(that : T) : Bool = {
    val (rawLeft, rawRight) = alignLsb(that)
    rawLeft <= rawRight
  }
  override def autoConnect(that: Data): Unit = autoConnectBaseImpl(that)

  override private[spinal] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {

    that match{
      case that if this.getClass.isAssignableFrom(that.getClass) => {
        val t = that.asInstanceOf[T]
        val difLsb = this.difLsb(t)
        if(difLsb > 0)
          this.raw := t.raw >> difLsb
        else if(difLsb < 0)
          this.raw := t.raw  << -difLsb
        else
          this.raw := t.raw
      }
      case _ => SpinalError("Undefined assignement")
    }
  }
}


//Fix autoconnect
class SFix(exp : Int,bitCount : Int) extends XFix[SFix,SInt](exp,bitCount){
  override def rawFactory(exp: Int, bitCount: Int): SInt = SInt(bitCount bit)
  override def fixFactory(exp: Int, bitCount: Int): SFix = SFix(exp,bitCount)

  def +(that : SFix) : SFix = doAddSub(that,false)
  def -(that : SFix) : SFix = doAddSub(that,true)


  def *(that : SFix): SFix = doMul(that)

  def <(that : SFix): Bool = doSmaller(that)
  def >(that : SFix): Bool = that.doSmaller(this)
  def <=(that : SFix): Bool = doSmallerEguals(that)
  def >=(that : SFix): Bool = that.doSmallerEguals(this)

  override def clone(): this.type = new SFix(exp,bitCount).asInstanceOf[this.type]
}
