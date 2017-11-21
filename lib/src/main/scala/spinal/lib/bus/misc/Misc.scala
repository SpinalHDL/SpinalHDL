package spinal.lib.bus.misc

import spinal.core._

/**
 * Created by PIC32F_USER on 07/08/2016.
 */

trait AddressMapping{
  def hit(address : UInt) : Bool
  def removeOffset(address : UInt) : UInt
  def lowerBound : BigInt
  def applyOffset(addressOffset : BigInt) : AddressMapping
}

case class SingleMapping(address : BigInt) extends AddressMapping{
  override def hit(address: UInt) = this.address === address
  override def removeOffset(address: UInt) = U(0)
  override def lowerBound = address
  override def applyOffset(addressOffset: BigInt): AddressMapping = SingleMapping(address + addressOffset)
}

case class MaskMapping(base : BigInt,mask : BigInt) extends AddressMapping{
  def hit(address : UInt) : Bool = (address & base) === mask
  override def removeOffset(address: UInt) = address & ~mask
  override def lowerBound = base
  override def applyOffset(addressOffset: BigInt): AddressMapping = ???
}

object SizeMapping{
  implicit def implicitTuple1(that : (Int,Int)) : SizeMapping = SizeMapping(that._1,that._2)
  implicit def implicitTuple2(that : (BigInt,BigInt)) : SizeMapping = SizeMapping(that._1,that._2)
  implicit def implicitTuple3(that : (Int,BigInt)) : SizeMapping = SizeMapping(that._1,that._2)
  implicit def implicitTuple5(that : (Long,BigInt)) : SizeMapping = SizeMapping(that._1,that._2)
  implicit def implicitTuple4(that : (BigInt,Int)) : SizeMapping = SizeMapping(that._1,that._2)
}

case class SizeMapping(base : BigInt,size : BigInt) extends AddressMapping {
  def hit(address: UInt): Bool = if (isPow2(size) && base % size == 0)
    (address & S(-size, address.getWidth bits).asUInt) === (base)
  else
    address >= base && address < base + size

  def removeOffset(address: UInt): UInt = {
    if (isPow2(size) && base % size == 0)
      address & (size - 1)
    else
      address - base
  }.resize(log2Up(size))

  override def lowerBound = base
  override def applyOffset(addressOffset: BigInt): AddressMapping = SizeMapping(base + addressOffset, size)
}