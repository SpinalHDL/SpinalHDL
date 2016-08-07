package spinal.lib.bus.misc

import spinal.core._

/**
 * Created by PIC32F_USER on 07/08/2016.
 */
case class BaseMask(base : BigInt,mask : BigInt){
  def hit(address : UInt) : Bool = (address & base) === mask
}

object BaseSize{
  implicit def tupl(that : (Int,Int)) : BaseSize = BaseSize(that._1,that._2)
}
case class BaseSize(base : BigInt,size : BigInt){
  def hit(address : UInt) : Bool = if(isPow2(size) && base % size == 0)
    (address & S(-size,address.getWidth bits).asUInt) === (base)
  else
      address >= base && address < size
}