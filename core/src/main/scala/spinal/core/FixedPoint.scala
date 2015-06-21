package spinal.core



import scala.collection.mutable.ArrayBuffer
import spinal.core._

/**
 * Created by PIC on 19.05.2015.
 */


object SFix{
  def apply(exp : Int,bitCount : BitCount) : SFix = new SFix(exp,bitCount.value)
  def apply(config : (Int,Int)) : SFix = new SFix(config._1,config._2)
//  def apply(config: FixFactory) : SFix = config.SFix
}


//case class FixFactory(exp : Int,bitCount : Int){
//  def SFix = spinal.core.SFix.apply(exp,bitCount)
//}


abstract class XFix[T <: XFix[T,R],R <: Data with Num[R]](val exp : Int,val bitCount : Int) extends MultiData {
  val raw = rawFactory(exp,bitCount)

  override def elements: ArrayBuffer[(String, Data)] = ArrayBuffer(("" -> raw))

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

  def doShiftLeft(that : Int) : T = {
    val ret = fixFactory(exp + that,bitCount +that)
    ret.raw := this.raw << that
    ret
  }
  def doShiftRight(that : Int) : T = {
    val ret = fixFactory(exp - that,bitCount - that)
    ret.raw := this.raw >> that
    ret
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
  override def fixFactory(exp: Int, bitCount: Int): SFix = SFix(exp,bitCount bit)

  def +(that : SFix) : SFix = doAddSub(that,false)
  def -(that : SFix) : SFix = doAddSub(that,true)
  def *(that : SFix): SFix = doMul(that)

  def << (that : Int) : SFix = doShiftLeft(that)
  def >> (that : Int) : SFix = doShiftRight(that)

  def <(that : SFix): Bool = doSmaller(that)
  def >(that : SFix): Bool = that.doSmaller(this)
  def <=(that : SFix): Bool = doSmallerEguals(that)
  def >=(that : SFix): Bool = that.doSmallerEguals(this)

  //TODO assert if that is greater or smaller than max/max of this
  def >=(that : Double): Bool = {
    val other = cloneOf(this)
    other := that
    this >= other
  }

  def := (that : Double): Unit ={
    val value = BigDecimal.valueOf(that * math.pow(2.0,bitCount - exp)).toBigInt()
    this.raw := S(value)
  }


  def init(that : Double) : this.type = {
    val initValue = cloneOf(this)
    initValue := that
    this init(initValue)
    this
  }

  override def clone(): this.type = new SFix(exp,bitCount).asInstanceOf[this.type]
}

object SFix2D {
  def apply(exp: Int, bitCount: BitCount): SFix2D = new SFix2D(exp, bitCount.value)
  def apply(copy: SFix): SFix2D = SFix2D(copy.exp, copy.bitCount bit)

}

//@valClone
class SFix2D(val exp: Int, val bitCount: Int) extends Bundle {
  val x = SFix(exp, bitCount bit)
  val y = SFix(exp, bitCount bit)

  override def clone(): SFix2D.this.type = new SFix2D(exp,bitCount).asInstanceOf[this.type]
}
