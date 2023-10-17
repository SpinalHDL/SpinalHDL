//package spinal.core
//
//import scala.collection.mutable.ArrayBuffer
//
///**
//  * Created by PIC32F_USER on 05/01/2016.
//  */
//object RInt{
//  def apply(max : BigInt,min : BigInt): RInt = new RInt(max,min)
//}
//
//class RInt(val max : BigInt,val min : BigInt) extends MultiData with Num[RInt]{
//  assert(max >= min)
//  def  isSigned = min < 0
//  def peak = max.abs.max(min.abs)
//  def bitToRepresent(value : BigInt) = if(value > 0)
//    log2Up(value)
//  else
//    log2Up(1-value) + 1
//  def rawBitWidth = Math.max(bitToRepresent(max),bitToRepresent(min))
//  val raw = Bits(rawBitWidth bit)
//
//  raw.setRefOwner(this)
//  raw.setPartialName("",true)
//  override def elements: ArrayBuffer[(String, Data)] = {
//
//    ArrayBuffer(("" -> raw))
//  }
//
//  override protected def assignFromImpl(that: AnyRef, conservative: Boolean): Unit = ???
//
//
//
//  override def +(right: RInt): RInt = {
//    val result = RInt(this.max + right.max,this.min+right.min)
//    val resultWidth = result.raw.getWidth
//    (this.isSigned,right.isSigned) match{
//      case (false,false) => result.raw := (this.raw.asUInt.resize(resultWidth) + right.raw.asUInt.resize(resultWidth)).asBits
//      case (_,_) => result.raw := (this.raw.asSInt.resize(resultWidth) + right.raw.asSInt.resize(resultWidth)).asBits
//    }
//    result
//  }
//
//  override def >(right: RInt): Bool = ???
//
//  override def >>(shift: Int): RInt = ???
//
//  override def <=(right: RInt): Bool = ???
//
//  override def <(right: RInt): Bool = ???
//
//  override def <<(shift: Int): RInt = ???
//
//  override def -(right: RInt): RInt = ???
//
//  override def >=(right: RInt): Bool = ???
//
//  override def *(right: RInt): RInt = ???
//
//  override def %(right: RInt): RInt = ???
//
//  override def /(right: RInt): RInt = ???
//}
