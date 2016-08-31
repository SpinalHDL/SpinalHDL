package spinal.core

import scala.collection.mutable.ArrayBuffer
import spinal.core._

/**
  * Created by PIC on 19.05.2015.
  */

trait SFixFactory extends TypeFactory{
  def SFix(peak: ExpNumber, width: BitCount): SFix = postTypeFactory(new SFix(peak.value, width.value))
  def SFix(peak: ExpNumber, resolution: ExpNumber): SFix = postTypeFactory(new SFix(peak.value, 1 + peak.value - resolution.value))
}

trait UFixFactory extends TypeFactory{
  def UFix(peak: ExpNumber, width: BitCount): UFix = postTypeFactory(new UFix(peak.value, width.value))
  def UFix(peak: ExpNumber, resolution: ExpNumber): UFix = postTypeFactory(new UFix(peak.value, peak.value - resolution.value))
}


trait SFixCast {
  def toSFix(sint: SInt) = sint.toSFix
}

trait UFixCast {
  def toUFix(uint: UInt): UFix = uint.toUFix
}

object SFix2D {
  def apply(maxExp: ExpNumber, bitCount: BitCount): SFix2D = new SFix2D(maxExp.value, bitCount.value)

  def apply(copy: SFix): SFix2D = SFix2D(copy.maxExp exp, copy.bitCount bit)

}

object UFix2D {
  def apply(maxExp: ExpNumber, bitCount: BitCount): UFix2D = new UFix2D(maxExp.value, bitCount.value)

  def apply(copy: UFix): UFix2D = UFix2D(copy.maxExp exp, copy.bitCount bit)
}


abstract class XFix[T <: XFix[T, R], R <: BitVector with Num[R]](val maxExp: Int, val bitCount: Int) extends MultiData {
  val raw = rawFactory(maxExp, bitCount)

  def minExp: Int

  raw.setRefOwner(this)
  raw.setPartialName("",true)

  override def elements: ArrayBuffer[(String, Data)] = {
    ArrayBuffer(("" -> raw))
  }

  def rawFactory(exp: Int, bitCount: Int): R
  def fixFactory(exp: Int, bitCount: Int): T
  def difLsb(that: T) = (this.maxExp - this.bitCount) - (that.maxExp - that.bitCount)

  def resolution : BigDecimal

  def alignLsb(that: T): Tuple2[R, R] = {
    val lsbDif = difLsb(that)
    val left: R = if (lsbDif > 0) this.raw << lsbDif else this.raw
    val right: R = if (lsbDif < 0) that.raw << -lsbDif else that.raw
    (left, right)
  }

  def doAddSub(that: T, sub: Boolean): T = {
    val (rawLeft, rawRight) = alignLsb(that)
    val ret = fixFactory(Math.max(this.maxExp, that.maxExp), Math.max(rawLeft.getBitsWidth, rawRight.getBitsWidth))
    ret.raw := (if (sub) rawLeft - rawRight else rawLeft + rawRight)
    ret
  }

  //  def doMul(that : T) : T = {
  //    val ret = fixFactory(this.exp + that.exp + 1,this.bitCount +that.bitCount)
  //    ret.raw := this.raw *that.raw
  //    ret
  //  }

  def doSmaller(that: T): Bool = {
    val (rawLeft, rawRight) = alignLsb(that)
    rawLeft < rawRight
  }

  def doSmallerEguals(that: T): Bool = {
    val (rawLeft, rawRight) = alignLsb(that)
    rawLeft <= rawRight
  }

  def doShiftLeft(that: Int): T = {
    val ret = fixFactory(maxExp + that, bitCount)
    ret.raw := this.raw
    ret
  }

  def doShiftRight(that: Int): T = {
    val ret = fixFactory(maxExp - that, bitCount)
    ret.raw := this.raw
    ret
  }

  def doShiftLeftBorned(that: Int): T = {
    val ret = fixFactory(maxExp + that, bitCount + that)
    ret.raw := this.raw << that
    ret
  }

  def doShiftRightBorned(that: Int): T = {
    val ret = fixFactory(maxExp - that, bitCount - that)
    ret.raw := this.raw >> that
    ret
  }

  override def autoConnect(that: Data): Unit = autoConnectBaseImpl(that)

  def truncated : this.type = {
    val copy = cloneOf(this)
    copy.raw := this.raw
    copy.addTag(tagTruncated)
    copy.asInstanceOf[this.type]
  }
  override private[spinal] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = {
    that match {
      case that if this.getClass.isAssignableFrom(that.getClass) => {
        val t = that.asInstanceOf[T]
        if(this.maxExp < t.maxExp || this.minExp > t.minExp){
          if(!t.hasTag(tagTruncated)){
            val trace = ScalaLocated.long
            globalData.pendingErrors += (() => (s"$this can't be assigned by $t because of truncation. You can do x := y.truncated if that's fine.\n $trace"))
          }
        }
        val difLsb = this.difLsb(t)
        if (difLsb > 0)
          this.raw := (t.raw >> difLsb).resized
        else if (difLsb < 0)
          this.raw := (t.raw << -difLsb).resized
        else
          this.raw := (t.raw).resized
      }
      case _ => SpinalError("Undefined assignment")
    }
  }
}

// Fix autoconnect
class SFix(maxExp: Int, bitCount: Int) extends XFix[SFix, SInt](maxExp, bitCount) {
  override def rawFactory(maxExp: Int, bitCount: Int): SInt = SInt(bitCount bit)

  override def fixFactory(maxExp: Int, bitCount: Int): SFix = SFix(maxExp exp, bitCount bit)

  override def minExp: Int = maxExp - bitCount + 1

  def +(that: SFix): SFix = doAddSub(that, false)
  def -(that: SFix): SFix = doAddSub(that, true)
  def *(that: SFix): SFix = {
    val ret = fixFactory(this.maxExp + that.maxExp + 1, this.bitCount + that.bitCount)
    ret.raw := this.raw * that.raw
    ret
  }

  def <<(that: Int): SFix = doShiftLeft(that)
  def >>(that: Int): SFix = doShiftRight(that)
  def <<|(that: Int): SFix = doShiftLeftBorned(that)
  def >>|(that: Int): SFix = doShiftRightBorned(that)
  def <(that: SFix): Bool = doSmaller(that)
  def >(that: SFix): Bool = that.doSmaller(this)
  def <=(that: SFix): Bool = doSmallerEguals(that)
  def >=(that: SFix): Bool = that.doSmallerEguals(this)
  def >=(that: Double): Bool = {
    if (that > maxValue) {
      SpinalWarning("Impossible comparison at " + ScalaLocated.long)
      return False
    }
    val other = cloneOf(this)
    other := that
    this >= other
  }

  def :=(that: Double): Unit = {
    val value = BigDecimal.valueOf(that * math.pow(2.0, bitCount - maxExp - 1)).toBigInt()
    this.raw := value
  }

  def :=(that: BigInt): Unit = {
    val minExp = this.minExp
    if (minExp > 0)
      this.raw := that >> minExp
    else
      this.raw := that << -minExp
  }

  def init(that: Double): this.type = {
    val initValue = cloneOf(this)
    initValue := that
    this init (initValue)
    this
  }

  // Rounded down
  def toSInt: SInt = {
    if (maxExp < 0)
      return S(0)
    else {
      if (minExp == 0)
        return raw
      else if (minExp > 0)
        return raw << minExp
      else
        return raw >> (-minExp)
    }
  }

  //TODO Should update the calculation with BigDecimal
  def maxValue: BigDecimal = Math.pow(2.0, maxExp) * (1.0 - BigDecimal(1.0 / (math.pow(2.0, bitCount - 1))))
  def minValue: BigDecimal = -Math.pow(2.0, maxExp)
  override def resolution: BigDecimal = Math.pow(2.0, maxExp-bitCount+1)

  override def clone(): this.type = new SFix(maxExp, bitCount).asInstanceOf[this.type]

  override def toString(): String = s"${component.getPath() + "/" + this.getDisplayName()} : ${getClass.getSimpleName}[peak=2^${maxExp} resolution=2^${minExp}]"
}


//@valClone
class SFix2D(val maxExp: Int, val bitCount: Int) extends Bundle {
  val x = SFix(maxExp exp, bitCount bit)
  val y = SFix(maxExp exp, bitCount bit)

  def truncated : this.type = {
    val copy = clone()
    copy.x := this.x
    copy.y := this.y
    copy.x.addTag(tagTruncated)
    copy.y.addTag(tagTruncated)
    copy
  }

  override def clone(): this.type = new SFix2D(maxExp, bitCount).asInstanceOf[this.type]
}


class UFix(maxExp: Int, bitCount: Int) extends XFix[UFix, UInt](maxExp, bitCount) {
  override def rawFactory(maxExp: Int, bitCount: Int): UInt = UInt(bitCount bit)
  override def fixFactory(maxExp: Int, bitCount: Int): UFix = UFix(maxExp exp, bitCount bit)
  override def minExp: Int = maxExp - bitCount

  def +(that: UFix): UFix = doAddSub(that, false)
  def -(that: UFix): UFix = doAddSub(that, true)
  def *(that: UFix): UFix = {
    val ret = fixFactory(this.maxExp + that.maxExp, this.bitCount + that.bitCount)
    ret.raw := this.raw * that.raw
    ret
  }

  def <<(that: Int): UFix = doShiftLeft(that)
  def >>(that: Int): UFix = doShiftRight(that)
  def <<|(that: Int): UFix = doShiftLeftBorned(that)
  def >>|(that: Int): UFix = doShiftRightBorned(that)
  def <(that: UFix): Bool = doSmaller(that)
  def >(that: UFix): Bool = that.doSmaller(this)
  def <=(that: UFix): Bool = doSmallerEguals(that)
  def >=(that: UFix): Bool = that.doSmallerEguals(this)

  def >=(that: Double): Bool = {
    if (that > maxValue) {
      SpinalWarning("Impossible comparison at " + ScalaLocated.long)
      return False
    }
    val other = cloneOf(this)
    other := that
    this >= other
  }

  def :=(that: Double): Unit = {
    assert(that >= 0)
    val value = BigDecimal.valueOf(that * math.pow(2.0, bitCount - maxExp)).toBigInt()
    this.raw := value
  }

  def :=(that: BigInt): Unit = {
    assert(that >= 0)
    val minExp = this.minExp
    if (minExp > 0)
      this.raw := that >> minExp
    else
      this.raw := that << -minExp
  }

  def init(that: Double): this.type = {
    val initValue = cloneOf(this)
    initValue := that
    this init (initValue)
    this
  }

  //Rounded down
  def toUInt: UInt = {
    if (maxExp < 0)
      U(0)
    else {
      if (minExp == 0)
        raw
      else if (minExp > 0)
        raw << minExp
      else
        raw >> (-minExp)
    }
  }

  def fractionalPart: UFix = {
    assert(this.bitCount - this.maxExp > 0)
    val ret = UFix(0 exp, -minExp bit)
    ret := this.resized
    ret
  }


  def maxValue: BigDecimal = Math.pow(2.0, maxExp) * (1.0 - 1.0 / (math.pow(2.0, bitCount - 1)))
  def minValue: BigDecimal = 0.0
  override def resolution: BigDecimal = Math.pow(2.0, maxExp-bitCount)

  override def clone: this.type = new UFix(maxExp, bitCount).asInstanceOf[this.type]
}


//@valClone
class UFix2D(val maxExp: Int, val bitCount: Int) extends Bundle {
  val x = UFix(maxExp exp, bitCount bit)
  val y = UFix(maxExp exp, bitCount bit)

  def truncated : this.type = {
    val copy = clone()
    copy.x := this.x
    copy.y := this.y
    copy.x.addTag(tagTruncated)
    copy.y.addTag(tagTruncated)
    copy
  }

  override def clone(): UFix2D.this.type = new UFix2D(maxExp, bitCount).asInstanceOf[this.type]
}
