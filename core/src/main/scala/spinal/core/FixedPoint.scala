package spinal.core



import scala.collection.mutable.ArrayBuffer
import spinal.core._

/**
 * Created by PIC on 19.05.2015.
 */


object SFix{
  def apply(expMax : ExpCount,bitCount : BitCount) : SFix = new SFix(expMax.value,bitCount.value)
  def apply(expMax : ExpCount,expMin : ExpCount) : SFix = new SFix(expMax.value,1 + expMax.value - expMin.value)
  //def apply(config : (Int,Int)) : SFix = new SFix(config._1,config._2)
//  def apply(config: FixFactory) : SFix = config.SFix
}

object UFix{
  def apply(expMax : ExpCount,bitCount : BitCount) : UFix = new UFix(expMax.value,bitCount.value)
  def apply(expMax : ExpCount,expMin : ExpCount) : UFix = new UFix(expMax.value, expMax.value - expMin.value)
  //def apply(config : (Int,Int)) : UFix = new UFix(config._1,config._2)
  //  def apply(config: FixFactory) : UFix = config.UFix
}


trait SFixCast{
  def toSFix(sint : SInt) = sint.toSFix
}

trait UFixCast{
  def toUFix(uint : UInt) : UFix = uint.toUFix
}

object SFix2D {
  def apply(maxExp: ExpCount, bitCount: BitCount): SFix2D = new SFix2D(maxExp.value, bitCount.value)
  def apply(copy: SFix): SFix2D = SFix2D(copy.maxExp exp, copy.bitCount bit)

}

object UFix2D {
  def apply(maxExp: ExpCount, bitCount: BitCount): UFix2D = new UFix2D(maxExp.value, bitCount.value)
  def apply(copy: UFix): UFix2D = UFix2D(copy.maxExp exp, copy.bitCount bit)
}




abstract class XFix[T <: XFix[T,R],R <: BitVector with Num[R]](val maxExp : Int,val bitCount : Int) extends MultiData {
  val raw = rawFactory(maxExp,bitCount)
  def expMin : Int

  override def elements: ArrayBuffer[(String, Data)] = ArrayBuffer(("" -> raw))

  def rawFactory(exp : Int,bitCount : Int) : R
  def fixFactory(exp : Int,bitCount : Int) : T

  def difLsb(that : T) = (this.maxExp-this.bitCount) - (that.maxExp-that.bitCount)
  def alignLsb(that : T) : Tuple2[R,R] = {
    val lsbDif = difLsb(that)
    val left : R = if(lsbDif > 0) (this.raw << lsbDif) else this.raw
    val right : R = if(lsbDif < 0) (that.raw << -lsbDif) else that.raw
    (left,right)
  }



  def doAddSub(that : T,sub : Boolean) : T = {
    val (rawLeft,rawRight) = alignLsb(that)
    val ret = fixFactory(Math.max(this.maxExp,that.maxExp),Math.max(rawLeft.getBitsWidth,rawRight.getBitsWidth))
    ret.raw := (if(sub) rawLeft - rawRight else rawLeft + rawRight)
    ret
  }

//  def doMul(that : T) : T = {
//    val ret = fixFactory(this.exp + that.exp + 1,this.bitCount +that.bitCount)
//    ret.raw := this.raw *that.raw
//    ret
//  }

  def doSmaller(that : T) : Bool = {
    val (rawLeft, rawRight) = alignLsb(that)
    rawLeft < rawRight
  }

  def doSmallerEguals(that : T) : Bool = {
    val (rawLeft, rawRight) = alignLsb(that)
    rawLeft <= rawRight
  }

  def doShiftLeft(that : Int) : T = {
    val ret = fixFactory(maxExp + that,bitCount +that)
    ret.raw := this.raw << that
    ret
  }
  def doShiftRight(that : Int) : T = {
    val ret = fixFactory(maxExp - that,bitCount - that)
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
          this.raw :~= t.raw >> difLsb
        else if(difLsb < 0)
          this.raw :~= t.raw  << -difLsb
        else
          this.raw :~= t.raw
      }
      case _ => SpinalError("Undefined assignement")
    }
  }
}


//Fix autoconnect
class SFix(maxExp : Int,bitCount : Int) extends XFix[SFix,SInt](maxExp,bitCount) {
  override def rawFactory(maxExp: Int, bitCount: Int): SInt = SInt(bitCount bit)
  override def fixFactory(maxExp: Int, bitCount: Int): SFix = SFix(maxExp exp,bitCount bit)
  override def expMin : Int = maxExp-bitCount+1


  def +(that : SFix) : SFix = doAddSub(that,false)
  def -(that : SFix) : SFix = doAddSub(that,true)
  def *(that : SFix): SFix = {
    val ret = fixFactory(this.maxExp + that.maxExp + 1,this.bitCount +that.bitCount)
    ret.raw := this.raw *that.raw
    ret
  }

  def << (that : Int) : SFix = doShiftLeft(that)
  def >> (that : Int) : SFix = doShiftRight(that)

  def <(that : SFix): Bool = doSmaller(that)
  def >(that : SFix): Bool = that.doSmaller(this)
  def <=(that : SFix): Bool = doSmallerEguals(that)
  def >=(that : SFix): Bool = that.doSmallerEguals(this)


  def >=(that : Double): Bool = {
    if(that > maxValue){
      SpinalWarning("Impossible comparaison at " + ScalaLocated.getScalaTrace)
      return False
    }
    val other = cloneOf(this)
    other := that
    this >= other
  }

  def := (that : Double): Unit ={
    val value = BigDecimal.valueOf(that * math.pow(2.0,bitCount - maxExp - 1)).toBigInt()
    this.raw := value
  }

  def := (that : BigInt): Unit ={
    val expMin = this.expMin
    if(expMin > 0)
      this.raw := that >> expMin
    else
      this.raw := that << -expMin
  }

  def init(that : Double) : this.type = {
    val initValue = cloneOf(this)
    initValue := that
    this init(initValue)
    this
  }



  //Rounded down
  def toSInt : SInt = {
    if(maxExp < 0)
      return S(0)
    else {
      if(expMin == 0)
        return raw
      else if(expMin > 0)
        return raw << expMin
      else
        return raw >> (-expMin)
    }
  }

  def maxValue : Double = {
    Math.pow(2.0,maxExp)*(1.0-1.0/(math.pow(2.0,bitCount-1)))
  }
  def minValue : Double = -Math.pow(2.0,maxExp)

  override def clone(): this.type = new SFix(maxExp,bitCount).asInstanceOf[this.type]
}




//@valClone
class SFix2D(val maxExp: Int, val bitCount: Int) extends Bundle {
  val x = SFix(maxExp exp, bitCount bit)
  val y = SFix(maxExp exp, bitCount bit)

  override def clone(): SFix2D.this.type = new SFix2D(maxExp,bitCount).asInstanceOf[this.type]
}




class UFix(maxExp : Int,bitCount : Int) extends XFix[UFix,UInt](maxExp,bitCount) {
  override def rawFactory(maxExp: Int, bitCount: Int): UInt = UInt(bitCount bit)
  override def fixFactory(maxExp: Int, bitCount: Int): UFix = UFix(maxExp exp,bitCount bit)
  override def expMin : Int = maxExp-bitCount

  def +(that : UFix) : UFix = doAddSub(that,false)
  def -(that : UFix) : UFix = doAddSub(that,true)
  def *(that : UFix): UFix = {
    val ret = fixFactory(this.maxExp + that.maxExp,this.bitCount +that.bitCount)
    ret.raw := this.raw *that.raw
    ret
  }

  def << (that : Int) : UFix = doShiftLeft(that)
  def >> (that : Int) : UFix = doShiftRight(that)

  def <(that : UFix): Bool = doSmaller(that)
  def >(that : UFix): Bool = that.doSmaller(this)
  def <=(that : UFix): Bool = doSmallerEguals(that)
  def >=(that : UFix): Bool = that.doSmallerEguals(this)


  def >=(that : Double): Bool = {
    if(that > maxValue){
      SpinalWarning("Impossible comparaison at " + ScalaLocated.getScalaTrace)
      return False
    }
    val other = cloneOf(this)
    other := that
    this >= other
  }

  def := (that : Double): Unit ={
    assert(that >= 0)
    val value = BigDecimal.valueOf(that * math.pow(2.0,bitCount - maxExp)).toBigInt()
    this.raw := value
  }
  def := (that : BigInt): Unit ={
    assert(that >= 0)
    val expMin = this.expMin
    if(expMin > 0)
      this.raw := that >> expMin
    else
      this.raw := that << -expMin
  }

  def init(that : Double) : this.type = {
    val initValue = cloneOf(this)
    initValue := that
    this init(initValue)
    this
  }

  //Rounded down
  def toUInt : UInt = {
    if(maxExp < 0)
      return U(0)
    else {
      if(expMin == 0)
        return raw
      else if(expMin > 0)
        return raw << expMin
      else
        return raw >> (-expMin)
    }
  }

  def fractionalPart : UFix = {
    assert(this.bitCount-this.maxExp > 0)
    val ret = UFix(0 exp,-expMin bit)
    ret := this.autoResize()
    ret
  }


  def maxValue : Double = {
    Math.pow(2.0,maxExp)*(1.0-1.0/(math.pow(2.0,bitCount-1)))
  }
  def minValue : Double = 0.0

  override def clone(): this.type = new UFix(maxExp,bitCount).asInstanceOf[this.type]
}



//@valClone
class UFix2D(val maxExp: Int, val bitCount: Int) extends Bundle {
  val x = UFix(maxExp exp, bitCount bit)
  val y = UFix(maxExp exp, bitCount bit)

  override def clone(): UFix2D.this.type = new UFix2D(maxExp,bitCount).asInstanceOf[this.type]
}
