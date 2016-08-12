package spinal.lib.bus.misc

import spinal.core._

/**
 * Created by PIC32F_USER on 07/08/2016.
 */

trait AddressMapping{
  def hit(address : UInt) : Bool
}

case class MaskMapping(base : BigInt,mask : BigInt) extends AddressMapping{
  def hit(address : UInt) : Bool = (address & base) === mask
}

object SizeMapping{
  implicit def implicitTuple1(that : (Int,Int)) : SizeMapping = SizeMapping(that._1,that._2)
  implicit def implicitTuple2(that : (BigInt,BigInt)) : SizeMapping = SizeMapping(that._1,that._2)
  implicit def implicitTuple3(that : (Int,BigInt)) : SizeMapping = SizeMapping(that._1,that._2)
  implicit def implicitTuple5(that : (Long,BigInt)) : SizeMapping = SizeMapping(that._1,that._2)
  implicit def implicitTuple4(that : (BigInt,Int)) : SizeMapping = SizeMapping(that._1,that._2)
}

case class SizeMapping(base : BigInt,size : BigInt)extends AddressMapping{
  def hit(address : UInt) : Bool = if(isPow2(size) && base % size == 0)
    (address & S(-size,address.getWidth bits).asUInt) === (base)
  else
      address >= base && address < size
}