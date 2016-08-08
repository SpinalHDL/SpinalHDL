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
  implicit def implicitTuple(that : (Int,Int)) : SizeMapping = SizeMapping(that._1,that._2)
}

case class SizeMapping(base : BigInt,size : BigInt)extends AddressMapping{
  def hit(address : UInt) : Bool = if(isPow2(size) && base % size == 0)
    (address & S(-size,address.getWidth bits).asUInt) === (base)
  else
      address >= base && address < size
}