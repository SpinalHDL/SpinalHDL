package spinal.lib.logic

import spinal.core._
import spinal.core.internals.Literal

object Masked{
  def apply(ml : MaskedLiteral) : Masked = Masked(ml.value, ml.careAbout)
  def apply(lit : Any) : Masked = lit match {
    case e: SpinalEnumElement[_] => Masked(e.spinalEnum.defaultEncoding.getValue(e), (BigInt(1) << e.spinalEnum.defaultEncoding.getWidth(e.spinalEnum))-1)
    case bt: BaseType => bt.head.source match {
      case lit : Literal => Masked(lit.getValue(), (BigInt(1) << widthOf(bt))-1)
    }
  }
  def one = Masked(1,1)
  def zero = Masked(0,1)
}

case class Masked(value : BigInt,care : BigInt){
  assert((value & ~care) == 0)
  var isPrime = true

  def < (that: Masked) = value < that.value || value == that.value && ~care < ~that.care

  def intersects(x: Masked) = ((value ^ x.value) & care & x.care) == 0

  def covers(x: Masked) = ((value ^ x.value) & care | (~x.care) & care) == 0

  def setPrime(value : Boolean) = {
    isPrime = value
    this
  }

  def mergeOneBitDifSmaller(x: Masked) = {
    val bit = value - x.value
    val ret = new Masked(value &~ bit, care & ~bit)
    //    ret.isPrime = isPrime || x.isPrime
    isPrime = false
    x.isPrime = false
    ret
  }
  def isSimilarOneBitDifSmaller(x: Masked) = {
    val diff = value - x.value
    care == x.care && value > x.value && (diff & diff - 1) == 0
  }

  def fuse(that : Masked) = {
    assert((this.value & that.value) == 0)
    assert((this.care & that.care) == 0)
    Masked(this.value | that.value, this.care | that.care)
  }


  def === (hard : Bits) : Bool = (hard & care) === (value & care)

  def shiftedLeft(amount : Int) = Masked(value << amount, care << amount)

  def toString(bitCount : Int) = (0 until bitCount).map(i => if(care.testBit(i)) (if(value.testBit(i)) "1" else "0") else "-").reverseIterator.reduce(_+_)

  override def toString = f"${value.toString(16)} ${care.toString(16)}"
}
