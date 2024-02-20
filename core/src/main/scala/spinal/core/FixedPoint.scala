/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core

import spinal.core.internals.ScopeStatement
import spinal.idslplugin.Location

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait RoundType

object RoundType{
  case object CEIL           extends RoundType ;// Wikipedia name: RoundUp
  case object FLOOR          extends RoundType ;// Wikipedia name: RoundDown
  case object FLOORTOZERO    extends RoundType ;// Wikipedia name: RoundToZero
  case object CEILTOINF      extends RoundType ;// Wikipedia name: RoundToInf
  case object ROUNDUP        extends RoundType ;// Wikipedia name: RoundHalfUp
  case object ROUNDDOWN      extends RoundType ;// Wikipedia name: RoundHalfDown
  case object ROUNDTOZERO    extends RoundType ;// Wikipedia name: RoundHalfToZero
  case object ROUNDTOINF     extends RoundType ;// Wikipedia name: RoundHalfToInf
  case object ROUNDTOEVEN    extends RoundType ;// Wikipedia name: RoundHalfToEven; Have not been implemented yet
  case object ROUNDTOODD     extends RoundType ;// Wikipedia name: RoundHalfToOdd ; Have not been implemented yet
  case object SCRAP           extends RoundType ;// If any bitsthrown is set, then LSB will be set
}

case class FixPointConfig(roundType: RoundType,
                          symmetric: Boolean){
  def on[B](body : => B) = {
    FixPointProperty(this) on {
      body
    }
  }
  def apply[B](body : => B): B = on(body)
  def setAsDefault() = FixPointProperty.set(this)
}



object FixPointProperty extends ScopeProperty[FixPointConfig]{
  override def default = DefaultFixPointConfig
}

object getFixRound{
  def apply(): RoundType = FixPointProperty.get.roundType
}

object getFixSym{
  def apply(): Boolean = FixPointProperty.get.symmetric
}


//case class FixPointConfig(roundType: RoundType,
//                          symmetric: Boolean) {
//
//  def flush(infos: String = s"${this} enabled!"): FixPointConfig = {
//    setFixRound(roundType)
//    setFixSym(symmetric)
//    SpinalInfo(infos)
//    this
//  }
//
//  def apply[T](block: => T): T = {
//    val outer = FixPointConfig(getFixRound(), getFixSym())
//    this.flush()
//    val ret: T = block
//    outer.flush(s"$outer recovered")
//    ret
//  }
//}
//

//
///*singleton Object*/
//private object GlobalRoundType{
//  private var roundType: RoundType = RoundType.ROUNDTOINF
//
//  def apply(): RoundType = roundType
//
//  def set(round: RoundType) = {
//    roundType = round
//    roundType
//  }
//}
//
//private object GlobalSymmetricType{
//  private var symmetric: Boolean = false
//
//  def apply(): Boolean = symmetric
//
//  def set(sym: Boolean) = {
//    symmetric = sym
//    symmetric
//  }
//}
//
//object getFixRound{
//  def apply(): RoundType = GlobalRoundType()
//}
//
//object setFixRound{
//  def apply(round: RoundType): RoundType = GlobalRoundType.set(round)
//}
//
//object getFixSym{
//  def apply(): Boolean = GlobalSymmetricType()
//}
//
//object setFixSym{
//  def apply(sym: Boolean): Boolean = GlobalSymmetricType.set(sym)
//}
//
//object ResetFixConfig{
//  def apply() = {
//    DefaultFixPointConfig().flush()
//  }
//}
//
//object ShowFixConfig{
//  def apply(pretag: String = "") = {
//    SpinalInfo(pretag + " " + FixPointConfig(getFixRound(),getFixSym()).toString())
//  }
//}



trait SFixFactory extends TypeFactory{
  def SFix(peak: ExpNumber, width: BitCount): SFix = postTypeFactory(new SFix(peak.value, width.value))
  def SFix(peak: ExpNumber, resolution: ExpNumber): SFix = postTypeFactory(new SFix(peak.value, 1 + peak.value - resolution.value))
}


trait UFixFactory extends TypeFactory{
  def UFix(peak: ExpNumber, width: BitCount): UFix = postTypeFactory(new UFix(peak.value, width.value))
  def UFix(peak: ExpNumber, resolution: ExpNumber): UFix = postTypeFactory(new UFix(peak.value, peak.value - resolution.value))
}


trait SFixCast {
  @deprecated("Use xxx.toSFix instead", "???")
  def toSFix(sint: SInt) = sint.toSFix
}


trait UFixCast {
  @deprecated("Use xxx.toUFix instead", "???")
  def toUFix(uint: UInt): UFix = uint.toUFix
}


/**
  * Base class for SFix and UFix
  */
abstract class XFix[T <: XFix[T, R], R <: BitVector with Num[R]](val maxExp: Int, val bitCount: Int) extends MultiData with MinMaxDecimalProvider {
  require(bitCount >= 0, s"Length of fixed point number must be > 0 (not $bitCount)")

  val raw = rawFactory(maxExp, bitCount)

  def minExp: Int

  raw.setRefOwner(this)
  raw.setPartialName("", weak = true)

  override def elements: ArrayBuffer[(String, Data)] = {
    ArrayBuffer("" -> raw)
  }

  def rawFactory(exp: Int, bitCount: Int): R
  def fixFactory(exp: Int, bitCount: Int): T
  def fixFactory(maxExp: ExpNumber, bitCount: BitCount): T
  def fixFactory(maxExp: ExpNumber, resolution: ExpNumber): T

  def difLsb(that: T) = (this.maxExp - this.bitCount) - (that.maxExp - that.bitCount)

  def resolution : BigDecimal

  def alignLsb(that: T): (R, R) = {
    val lsbDif   = difLsb(that)
    val left: R  = if (lsbDif > 0) this.raw << lsbDif else this.raw
    val right: R = if (lsbDif < 0) that.raw << -lsbDif else that.raw
    (left, right)
  }

  def doAddSub(that: T, sub: Boolean): T = {
    val (rawLeft, rawRight) = alignLsb(that)
    val ret = fixFactory(Math.max(this.maxExp, that.maxExp), Math.max(rawLeft.getBitsWidth, rawRight.getBitsWidth))
    ret.raw := (if (sub) rawLeft - rawRight else rawLeft + rawRight)
    ret
  }

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

  override def autoConnect(that: Data)(implicit loc: Location): Unit = autoConnectBaseImpl(that)

  def truncated: this.type = {
    val copy = cloneOf(this)
    copy.raw := this.raw
    copy.addTag(tagTruncated)
    copy.asInstanceOf[this.type]
  }

  def truncated(maxExp: ExpNumber, bitCount: BitCount): T = {
    val t = fixFactory(maxExp, bitCount)
    t := this.truncated.asInstanceOf[T]
    t
  }
  def truncated(maxExp: ExpNumber, resolution: ExpNumber): T = {
    val t = fixFactory(maxExp, resolution)
    t := this.truncated.asInstanceOf[T]
    t
  }

  override protected def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
    that match {
      case that if this.getClass.isAssignableFrom(that.getClass) =>
        val t = that.asInstanceOf[T]
        if(this.maxExp < t.maxExp || this.minExp > t.minExp){
          if(!t.hasTag(tagTruncated)){
            val trace = ScalaLocated.long
            globalData.pendingErrors += (() => s"$this can't be assigned by $t because of truncation. You can do x := y.truncated if that's fine.\n $trace")
          }
        }
        val difLsb = this.difLsb(t)
        if (difLsb > 0)
          this.raw compositAssignFrom ((t.raw >> difLsb).resized, this.raw, kind)
        else if (difLsb < 0)
          this.raw compositAssignFrom ( (t.raw << -difLsb).resized, this.raw, kind)
        else
          this.raw compositAssignFrom ( t.raw.resized, this.raw, kind)
      case _ => SpinalError("Undefined assignment")
    }
  }

  override def getMuxType[M <: Data](list: TraversableOnce[M]): HardType[M] = {
    val fixed = list.filter(u => !u.hasTag(tagTruncated)).toSeq
    assert(fixed.nonEmpty, "Can't generate mux for all-truncated fixed point numbers")
    val muxMaxExp = fixed.map {
      case x: XFix[T,R] => x.maxExp
    }.max
    val muxMinExp = fixed.map {
      case x: XFix[T,R] => x.minExp
    }.min
    HardType(fixFactory(muxMaxExp exp, muxMinExp exp).asInstanceOf[M])
  }

  override def toMuxInput[M <: Data](muxOutput: M): M = {
    muxOutput match {
      case m: XFix[T,R] if(m.maxExp ==maxExp && m.bitCount == bitCount) => this.asInstanceOf[M]
      case _ =>
        val ret = cloneOf(muxOutput)
        ret.assignFrom(this)
        ret
    }
  }
}

//TODO Fix autoconnect
/**
  * Signed fix point
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/types/Fix SFix Documentation]]
  */
class SFix(maxExp: Int, bitCount: Int) extends XFix[SFix, SInt](maxExp, bitCount) {
  override def rawFactory(maxExp: Int, bitCount: Int): SInt = SInt(bitCount bit)

  override def fixFactory(maxExp: Int, bitCount: Int): SFix = SFix(maxExp exp, bitCount bit)
  override def fixFactory(maxExp: ExpNumber, bitCount: BitCount): SFix = SFix(maxExp, bitCount)
  override def fixFactory(maxExp: ExpNumber, resolution: ExpNumber): SFix = SFix(maxExp, resolution)

  override def minExp: Int = maxExp - bitCount + 1

  def +(that: SFix): SFix = doAddSub(that, sub = false)
  def -(that: SFix): SFix = doAddSub(that, sub = true)
  def *(that: SFix): SFix = {
    val ret = fixFactory(this.maxExp + that.maxExp + 1, this.bitCount + that.bitCount)
    ret.raw := this.raw * that.raw
    ret
  }

  def << (that: Int): SFix  = doShiftLeft(that)
  def >> (that: Int): SFix  = doShiftRight(that)
  def <<|(that: Int): SFix  = doShiftLeftBorned(that)
  def >>|(that: Int): SFix  = doShiftRightBorned(that)
  def <  (that: SFix): Bool = doSmaller(that)
  def >  (that: SFix): Bool = that.doSmaller(this)
  def <= (that: SFix): Bool = doSmallerEguals(that)
  def >= (that: SFix): Bool = that.doSmallerEguals(this)

  def >= (that: BigDecimal): Bool = {
    if (that > maxValue) {
      SpinalWarning("Impossible comparison at " + ScalaLocated.long)
      return False
    }
    val other = cloneOf(this)
    other := that
    this >= other
  }

  def >(that: BigDecimal): Bool = {
    if (that > maxValue) {
      SpinalWarning("Impossible comparison at " + ScalaLocated.long)
      return False
    }
    val other = cloneOf(this)
    other := that
    this > other
  }

  def <(that: BigDecimal): Bool = {
    if (that < minValue) {
      SpinalWarning("Impossible comparison at " + ScalaLocated.long)
      return False
    }
    val other = cloneOf(this)
    other := that
    this < other
  }

  def <=(that: BigDecimal): Bool = {
    if (that < minValue) {
      SpinalWarning("Impossible comparison at " + ScalaLocated.long)
      return False
    }
    val other = cloneOf(this)
    other := that
    this <= other
  }

  def :=(that: Int): Unit   = this := BigInt(that)
  def :=(that: Long): Unit  = this := BigInt(that)
  def :=(that: Float): Unit = this := BigDecimal(that.toDouble)

  def :=(that: BigDecimal): Unit = {
    assert(that <= this.maxValue, s"Literal $that is too big to be assigned in $this")
    assert(that >= this.minValue, s"Literal $that is too negative to be assigned in this $this")

    val shift = bitCount - maxExp - 1
    val value = if(shift >= 0)
      (that * BigDecimal(BigInt(1) << shift)).toBigInt
    else
      (that / BigDecimal(BigInt(1) << -shift)).toBigInt
    this.raw := value
  }

  def :=(that: BigInt): Unit = {
    assert(BigDecimal(that) <= this.maxValue, s"Literal $that is too big to be assigned in $this")
    assert(BigDecimal(that)  >= this.minValue, s"Literal $that is too negative to be assigned in this $this")

    val minExp = this.minExp
    if (minExp > 0)
      this.raw := that >> minExp
    else
      this.raw := that << -minExp
  }

  def init(that: BigDecimal): this.type = {
    val initValue = cloneOf(this)
    initValue := that
    this init (initValue)
    this
  }

  // Rounded down
  def toSInt: SInt = {
    if (maxExp < 0)
      S(0)
    else {
      if (minExp == 0)
        raw
      else if (minExp > 0)
        raw << minExp
      else
        raw >> (-minExp)
    }
  }

  //TODO Should update the calculation with BigDecimal
  def maxValue: BigDecimal = Math.pow(2.0, maxExp) * (1.0 - BigDecimal(1.0 / math.pow(2.0, bitCount - 1)))
  def minValue: BigDecimal = -Math.pow(2.0, maxExp)
  override def resolution: BigDecimal = Math.pow(2.0, maxExp-bitCount+1)

  override def clone: this.type = new SFix(maxExp, bitCount).asInstanceOf[this.type]

  override def toString(): String = s"${component.getPath() + "/" + this.getDisplayName()} : ${getClass.getSimpleName}[peak=2^$maxExp resolution=2^$minExp]"
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
    copy.asInstanceOf[this.type]
  }

  override def clone: this.type = new SFix2D(maxExp, bitCount).asInstanceOf[this.type]
}

/**
  * Unsigned fix point
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/types/Fix UFix Documentation]]
  */
class UFix(maxExp: Int, bitCount: Int) extends XFix[UFix, UInt](maxExp, bitCount) {

  override def rawFactory(maxExp: Int, bitCount: Int): UInt = UInt(bitCount bit)
  override def fixFactory(maxExp: Int, bitCount: Int): UFix = UFix(maxExp exp, bitCount bit)
  override def fixFactory(maxExp: ExpNumber, bitCount: BitCount): UFix = UFix(maxExp, bitCount)
  override def fixFactory(maxExp: ExpNumber, resolution: ExpNumber): UFix = UFix(maxExp, resolution)

  override def minExp: Int = maxExp - bitCount

  def +(that: UFix): UFix = doAddSub(that, sub = false)
  def -(that: UFix): UFix = doAddSub(that, sub = true)
  def *(that: UFix): UFix = {
    val ret = fixFactory(this.maxExp + that.maxExp, this.bitCount + that.bitCount)
    ret.raw := this.raw * that.raw
    ret
  }

  def << (that: Int): UFix  = doShiftLeft(that)
  def >> (that: Int): UFix  = doShiftRight(that)
  def <<|(that: Int): UFix  = doShiftLeftBorned(that)
  def >>|(that: Int): UFix  = doShiftRightBorned(that)
  def <  (that: UFix): Bool = doSmaller(that)
  def >  (that: UFix): Bool = that.doSmaller(this)
  def <= (that: UFix): Bool = doSmallerEguals(that)
  def >= (that: UFix): Bool = that.doSmallerEguals(this)

  def >=(that: BigDecimal): Bool = {
    if (that > maxValue) {
      SpinalWarning("Impossible comparison at " + ScalaLocated.long)
      return False
    }
    val other = cloneOf(this)
    other := that
    this >= other
  }

  def :=(that: Int): Unit   = this := BigInt(that)
  def :=(that: Long): Unit  = this := BigInt(that)
  def :=(that: Float): Unit = this := BigDecimal(that.toDouble)

  def :=(that: BigDecimal): Unit = {
    assert(that >= 0)
    assert(that <= this.maxValue, s"Literal $that is too big to be assigned in this $this")

    val shift = bitCount - maxExp
    val value = if(shift >= 0)
      (that * BigDecimal(BigInt(1) << shift)).toBigInt
    else
      (that / BigDecimal(BigInt(1) << -shift)).toBigInt
    this.raw := value
  }

  def :=(that: BigInt): Unit = {
    assert(that >= 0)
    assert(that < (BigInt(1) << maxExp), s"Literal $that is too big to be assigned in this $this")

    val minExp = this.minExp
    if (minExp > 0)
      this.raw := that >> minExp
    else
      this.raw := that << -minExp
  }

  def init(that: BigDecimal): this.type = {
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

  def maxValue: BigDecimal = Math.pow(2.0, maxExp) * (1.0 - 1.0 / math.pow(2.0, bitCount))
  def minValue: BigDecimal = 0.0

  override def resolution: BigDecimal = Math.pow(2.0, maxExp - bitCount)

  override def clone: this.type = new UFix(maxExp, bitCount).asInstanceOf[this.type]
}


/**
  * Two-dimensional XFix
  */
class UFix2D(val maxExp: Int, val bitCount: Int) extends Bundle {
  val x = UFix(maxExp exp, bitCount bit)
  val y = UFix(maxExp exp, bitCount bit)

  def truncated : this.type = {
    val copy = clone()
    copy.x := this.x
    copy.y := this.y
    copy.x.addTag(tagTruncated)
    copy.y.addTag(tagTruncated)
    copy.asInstanceOf[this.type]
  }

  override def clone: UFix2D.this.type = new UFix2D(maxExp, bitCount).asInstanceOf[this.type]
}


/**
  * Two-dimensional SFix
  */
object SFix2D {
  def apply(maxExp: ExpNumber, bitCount: BitCount): SFix2D = new SFix2D(maxExp.value, bitCount.value)

  def apply(copy: SFix): SFix2D = SFix2D(copy.maxExp exp, copy.bitCount bit)
}


/**
  * Two-dimensional UFix
  */
object UFix2D {
  def apply(maxExp: ExpNumber, bitCount: BitCount): UFix2D = new UFix2D(maxExp.value, bitCount.value)

  def apply(copy: UFix): UFix2D = UFix2D(copy.maxExp exp, copy.bitCount bit)
}


object SF{
  def apply(value: BigDecimal, peak: ExpNumber, width: BitCount): SFix = {
    val tmp = SFix(peak, width)
    tmp := value
    tmp
  }

  def apply(value: BigDecimal, peak: ExpNumber, resolution: ExpNumber): SFix = {
    val tmp = SFix(peak, resolution)
    tmp := value
    tmp
  }
}


object UF{
  def apply(value: BigDecimal, peak: ExpNumber, width: BitCount): UFix = {
    val tmp = UFix(peak, width)
    tmp := value
    tmp
  }

  def apply(value: BigDecimal, peak: ExpNumber, resolution: ExpNumber): UFix = {
    val tmp = UFix(peak, resolution)
    tmp := value
    tmp
  }
}
