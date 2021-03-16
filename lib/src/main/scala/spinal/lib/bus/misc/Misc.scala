/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Lib                          **
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
package spinal.lib.bus.misc

import spinal.core._


object AddressMapping{
  def verifyOverlapping(mapping: Seq[AddressMapping]): Boolean = {
    val sizeMapped = mapping.filter(_.isInstanceOf[SizeMapping]).map(_.asInstanceOf[SizeMapping])
    SizeMapping.verifyOverlapping(sizeMapped)
  }
}

trait AddressMapping{
  def hit(address: UInt): Bool
  def removeOffset(address: UInt): UInt
  def lowerBound : BigInt
  def applyOffset(addressOffset: BigInt): AddressMapping
}


case class SingleMapping(address : BigInt) extends AddressMapping{
  override def hit(address: UInt) = this.address === address
  override def removeOffset(address: UInt) = U(0)
  override def lowerBound = address
  override def applyOffset(addressOffset: BigInt): AddressMapping = SingleMapping(address + addressOffset)
  override def toString: String = s"Address 0x${address.toString(16)}"
}


case class MaskMapping(base : BigInt,mask : BigInt) extends AddressMapping{
  override def hit(address: UInt): Bool = (address & base) === mask
  override def removeOffset(address: UInt) = address & ~mask
  override def lowerBound = base
  override def applyOffset(addressOffset: BigInt): AddressMapping = ???
}


object SizeMapping{
  implicit def implicitTuple1(that: (Int, Int))      : SizeMapping = SizeMapping(that._1, that._2)
  implicit def implicitTuple2(that: (BigInt, BigInt)): SizeMapping = SizeMapping(that._1, that._2)
  implicit def implicitTuple3(that: (Int, BigInt))   : SizeMapping = SizeMapping(that._1, that._2)
  implicit def implicitTuple5(that: (Long, BigInt))  : SizeMapping = SizeMapping(that._1, that._2)
  implicit def implicitTuple4(that: (BigInt, Int))   : SizeMapping = SizeMapping(that._1, that._2)

  /**
    * Verify that the mapping has no overlapping
    *
    *  @return : true = overlapping found, false = no overlapping
    */
  def verifyOverlapping(mappings: Seq[SizeMapping]): Boolean = {
    for(m1 <- mappings.indices; m2 <- mappings.indices if m1 != m2){ // fix when some SizeMappings are completely overlap.
      if(mappings(m1).overlap(mappings(m2))) return true
    }
    return false
  }
}

object AllMapping extends AddressMapping{
  override def hit(address: UInt): Bool = True
  override def removeOffset(address: UInt): UInt = address
  override def lowerBound: BigInt = 0
  override def applyOffset(addressOffset: BigInt): AddressMapping = ???
}

object DefaultMapping extends AddressMapping{
  override def hit(address: UInt): Bool = ???
  override def removeOffset(address: UInt): UInt = ???
  override def lowerBound: BigInt = ???
  override def applyOffset(addressOffset: BigInt): AddressMapping = ???
}

case class SizeMapping(base: BigInt, size: BigInt) extends AddressMapping {

  val end = base + size - 1

  override def hit(address: UInt): Bool = {
    if (isPow2(size) && base % size == 0){
      (address & ~U(size - 1, address.getWidth bits)) === (base)
    }else {
      if(base == 0)
        address < base + size
      else
        address >= base && address < base + size
    }
  }

  override def removeOffset(address: UInt): UInt = {
    if (isPow2(size) && base % size == 0)
      address & (size - 1)
    else
      address - base
  }.resize(log2Up(size))

  override def lowerBound = base
  override def applyOffset(addressOffset: BigInt): AddressMapping = SizeMapping(base + addressOffset, size)
  def overlap(that : SizeMapping) = this.base < that.base + that.size && this.base + this.size > that.base

  override def toString: String = f"$base%x $size%x"
}
