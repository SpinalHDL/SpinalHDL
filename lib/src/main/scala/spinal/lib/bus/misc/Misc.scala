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

import spinal.lib._
import spinal.core._
import spinal.lib.logic.{Masked, Symplify}
import spinal.core.sim.simRandom

import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

object AddressMapping{
  def verifyOverlapping(mapping: Seq[AddressMapping]): Boolean = {
    val sizeMapped = mapping.filter(_.isInstanceOf[SizeMapping]).map(_.asInstanceOf[SizeMapping])
    SizeMapping.verifyOverlapping(sizeMapped)
  }

  def decode(address : Bits, trueMapping : AddressMapping) : Bool = {
    val trueTerms = terms(trueMapping, widthOf(address))
    Symplify(address.asBits, trueTerms)
  }


  def decode(address : Bits, trueMapping : AddressMapping, falseMapping : AddressMapping) : Bool = {
    val trueTerms = terms(trueMapping, widthOf(address))
    val falseTerms = terms(falseMapping, widthOf(address))
    Symplify(address.asBits, trueTerms, falseTerms)
  }

  def decode(address : Bits, trueMapping : Seq[AddressMapping], falseMapping : Seq[AddressMapping]) : Bool = {
    decode(address, OrMapping(trueMapping), OrMapping(falseMapping))
  }


  def terms(mapping : AddressMapping, width : Int) : ArrayBuffer[Masked] = {
    val ret = ArrayBuffer[Masked]()
    val widthMask = (BigInt(1) << width)-1
    def rec(e : AddressMapping, mask : Masked): Unit = e match {
      case OrMapping(conds) => conds.foreach(cond => rec(cond, mask))
      case InterleavedMapping(mapping, blockSize, ratio, sel) => rec(mapping, mask fuse Masked(sel << log2Up(blockSize), ratio-1 << log2Up(blockSize)))
      case m : SizeMapping => {
        var ptr = m.base
        var end = m.base + m.size
        while(ptr < end){
          val step = if(ptr == 0) m.size else BigInt(1) << ptr.lowestSetBit
          if(ptr + step > end){
            while(ptr < end){
              val step = if(end == 0) m.size else BigInt(1) << end.lowestSetBit
              if(ptr <= end - step){
                end -= step
                ret += mask fuse Masked(end, (~(step-1)) & widthMask)
              }
            }
          } else {
            ret += mask fuse Masked(ptr, (~(step-1)) & widthMask)
            ptr += step
          }
        }
      }
    }
    rec(mapping, Masked(0,0))
    ret
  }
}


/*
- Provide offset address for decoder substraction
- Provide a inverted mapping (default mapping)
- Get maximal sequencial size
- Calculate a random address from the mapping of a given sequencial size
-  //Shift things ?????
 */
trait AddressMapping{
  def hit(address: UInt): Bool = ???
  def hit(address: BigInt): Boolean = ???
  def removeOffset(address: UInt): UInt = ???
  def lowerBound : BigInt = ???
  def highestBound : BigInt = ???
  def randomPick() : BigInt = ???
  @deprecated("Use withOffset instead")
  def applyOffset(addressOffset: BigInt): AddressMapping = withOffset(addressOffset)
  def withOffset(addressOffset: BigInt): AddressMapping = ???
  def withOffset(t: AddressTransformer): AddressMapping = ???
  def withOffsetInvert(t: AddressTransformer): AddressMapping = ???
  def width = log2Up(highestBound+1)
  def foreach(body : BigInt => Unit) : Unit = ???
  def maxSequentialSize : BigInt = ???
  def randomPick(bytes : BigInt, aligned : Boolean) : BigInt = ???
  def intersectImpl(that : AddressMapping, path : List[AddressMapping] = Nil) : AddressMapping = ???
  def intersect(that : AddressMapping) : AddressMapping = {
    intersectImpl(that, Nil)
  }
}

object NeverMapping extends AddressMapping{

}

case class SingleMapping(address : BigInt) extends AddressMapping{
  override def hit(address: UInt) = this.address === address
  override def hit(address: BigInt): Boolean = this.address == address
  override def removeOffset(address: UInt) = U(0)
  override def lowerBound = address
  override def highestBound = address
  override def randomPick() : BigInt = address
  override def withOffset(addressOffset: BigInt): AddressMapping = SingleMapping(address + addressOffset)
  override def toString: String = s"Address 0x${address.toString(16)}"
  override def foreach(body: BigInt => Unit) = body(address)
}


/**
 * Creates an address mapping using a bit mask.
 *
 * MaskMapping(0x0000, 0x8000) => matches 0x0000-0x8000
 * MaskMapping(0x40, 0xF0) => matches 0x40 - 0x4F
 *
 * @param base Address offset to use. Must be inside the mask
 * @param mask Bit mask applied to the address before the check
 */
case class MaskMapping(base : BigInt,mask : BigInt) extends AddressMapping{
  override def hit(address: UInt): Bool = (address & U(mask, widthOf(address) bits)) === base
  override def hit(address: BigInt): Boolean = (address & mask) == base
  override def removeOffset(address: UInt) = address & ~U(mask, widthOf(address) bits)
  override def lowerBound = base
  override def highestBound = ???
  override def randomPick() : BigInt = ???
  override def withOffset(addressOffset: BigInt): AddressMapping = ???
}

case class UnmaskMapping(base : BigInt,mask : BigInt) extends AddressMapping{
  assert((base & mask) == 0)
  override def hit(address: UInt): Bool = (address | U(mask, widthOf(address) bits)) === (base | mask)
  override def hit(address: BigInt): Boolean = (address | mask) == (base | mask)
  override def removeOffset(address: UInt) = ??? //address & ~U(mask, widthOf(address) bits)
  override def lowerBound = base
  override def highestBound = ???
  override def randomPick() : BigInt = ???
  override def withOffset(addressOffset: BigInt): AddressMapping = ???
}

case class OrMapping(conds : Seq[AddressMapping]) extends AddressMapping{
  import spinal.core.sim._
  override def hit(address: UInt): Bool = conds.map(_.hit(address)).orR
  override def hit(address: BigInt): Boolean = conds.exists(_.hit(address))
  override def removeOffset(address: UInt) = ??? //address & ~U(mask, widthOf(address) bits)
  override def lowerBound = conds.map(_.lowerBound).min
  override def highestBound = conds.map(_.highestBound).max
  override def randomPick() : BigInt = conds.randomPick().randomPick()
  override def withOffset(addressOffset: BigInt): AddressMapping = OrMapping(conds.map(_.withOffset(addressOffset)))
  override def intersectImpl(that : AddressMapping, path : List[AddressMapping] = Nil) : AddressMapping = {
    val filtred = conds.map(_.intersectImpl(that, this :: path)).filter(_ != NeverMapping)
    filtred.size match {
      case 0 => NeverMapping
      case 1 => filtred.head
      case _ => OrMapping(filtred)
    }
  }
  override def maxSequentialSize : BigInt = conds.map(_.maxSequentialSize).min
  override def randomPick(bytes : BigInt, aligned : Boolean) : BigInt = conds.randomPick().randomPick(bytes, aligned)
  override def withOffset(t: AddressTransformer): AddressMapping = OrMapping(conds.map(_.withOffset(t)))
  override def withOffsetInvert(t: AddressTransformer): AddressMapping = OrMapping(conds.map(_.withOffsetInvert(t)))
}

case class InvertMapping(inv : AddressMapping) extends AddressMapping{
  import spinal.core.sim._
  override def hit(address: UInt): Bool = !inv.hit(address)
  override def hit(address: BigInt): Boolean = !inv.hit(address)
  override def removeOffset(address: UInt) = inv.removeOffset(address)
  override def lowerBound = ???
  override def highestBound = ???
  override def randomPick() : BigInt = ???
  override def withOffset(addressOffset: BigInt): AddressMapping = InvertMapping(inv.withOffset(addressOffset))
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
  override def hit(address: BigInt): Boolean = true
  override def removeOffset(address: UInt): UInt = address
  override def lowerBound: BigInt = 0
  override def highestBound = ???
  override def randomPick() : BigInt = ???
  override def withOffset(addressOffset: BigInt): AddressMapping = ???
}

object DefaultMapping extends AddressMapping{
  override def hit(address: UInt): Bool = True
  override def hit(address: BigInt): Boolean = true
  override def removeOffset(address: UInt): UInt = ???
  override def lowerBound: BigInt = ???
  override def highestBound = ???
  override def randomPick() : BigInt = ???
  override def withOffset(addressOffset: BigInt): AddressMapping = ???
}

case class SizeMapping(base: BigInt, size: BigInt) extends AddressMapping {

  val end = base + size - 1

  def isAligned = isPow2(size) && base % size == 0

  override def hit(address: UInt): Bool = {
    if (isAligned){
      (address & ~U(size - 1, address.getWidth bits)) === (base)
    }else {
      (base == 0, log2Up(base + size + 1) > widthOf(address)) match {
        case (false, false) => address >= base && address < base + size
        case (false, true) => address >= base
        case (true, false) => address < base + size
        case (true, true) => True
      }
    }
  }
  override def hit(address: BigInt): Boolean = address >= base && address < base + size

  override def removeOffset(address: UInt): UInt = {
    if (isPow2(size) && base % size == 0)
      address & (size - 1)
    else
      address - base
  }.resize(log2Up(size))

  override def lowerBound = base
  override def highestBound = base + size - 1
  override def randomPick() : BigInt = base + BigInt(log2Up(size), simRandom) % size
  override def withOffset(addressOffset: BigInt): AddressMapping = SizeMapping(base + addressOffset, size)
  override def withOffset(t: AddressTransformer): AddressMapping = t match {
    case OffsetTransformer(offset) => SizeMapping(base - offset, size)
    case InterleaverTransformer(blockSize, ratio, sel) => SizeMapping(base/ratio, size/ratio)
  }
  override def withOffsetInvert(t: AddressTransformer): AddressMapping = t match {
    case OffsetTransformer(offset) => SizeMapping(base + offset, size)
    case InterleaverTransformer(blockSize, ratio, sel) => SizeMapping(base*ratio, size*ratio)
  }
  def overlap(that : SizeMapping) = this.base < that.base + that.size && this.base + this.size > that.base
  override def foreach(body: BigInt => Unit) = for(i <- 0 until size.toInt) body(base + i)

  override def toString: String = f"SM(0x$base%x, 0x$size%x)"
  override def maxSequentialSize : BigInt = size
  override def randomPick(bytes : BigInt, aligned : Boolean) : BigInt = {
    var addr = base + BigInt(size.bitLength, simRandom) % (size-bytes)
    if(aligned) addr = addr/bytes*bytes
    addr
  }

//  override def asFilterOf(that : AddressMapping): AddressMapping = {
//    def swap(e : AddressMapping) : Seq[AddressMapping] = {
//      case sm : SizeMapping =>
//      case e => e.walkSwap(that)
//    }
//
//    that.walkSwap()
//
//  }

  override def intersectImpl(that: AddressMapping, path: List[AddressMapping]) = {
    val hits = ArrayBuffer[AddressMapping]()

    //Will go though the other hierarchy to look for SizeMapping hit
    def rec(other : AddressMapping, otherPath : List[AddressMapping]) : Unit = other match{
      case other : SizeMapping => {
        if(other.base < this.base + this.size && this.base < other.base + other.size){
          val hitBase = this.base max other.base
          val hitEnd = this.base + this.size min other.base + other.size
          hits += otherPath.foldLeft[AddressMapping](SizeMapping(this.base max other.base, hitEnd - hitBase))((s, p) => p match {
            case p : InterleavedMapping => p.copy(mapping = s)
          })
        }
      }
      case other : InterleavedMapping => rec(other.mapping, other :: otherPath)
      case other : OrMapping => other.conds.foreach(e => rec(e, otherPath))
    }


    rec(that, Nil)

    hits.size match {
      case 0 => NeverMapping
      case 1 => hits.head
      case _ => OrMapping(hits)
    }
  }

}


case class InterleavedMapping(mapping : AddressMapping, blockSize : Int, ratio : Int, sel : Int) extends AddressMapping{
  assert(isPow2(blockSize))
  assert(isPow2(ratio))
  assert(sel < ratio)

  override def highestBound = mapping.highestBound
  override def lowerBound = mapping.lowerBound

  override def hit(address: UInt) = {
    val l2 = mapping.hit(address)
    val l1 = address(log2Up(blockSize), log2Up(ratio) bits) === sel
    l2 && l1
  }

  override def hit(address: BigInt) = {
    val blockId = (address.toInt / blockSize) & (ratio-1)
    val l2 = mapping.hit(address)
    val l1 = blockId == sel
    l1 && l2
  }

  override def withOffset(addressOffset: BigInt) = {
    copy(mapping = mapping.withOffset(addressOffset))
  }


  override def maxSequentialSize : BigInt = BigInt(blockSize) min mapping.maxSequentialSize
  override def randomPick(bytes : BigInt, aligned : Boolean) : BigInt = {
    import spinal.core.sim._
    val l20 = mapping.randomPick(bytes, aligned)
    val l1 = sel
    val mask = (ratio-1)*blockSize
    val addr = (l20 & ~mask) | l1 * blockSize
    addr
  }

  override def intersectImpl(that : AddressMapping, path : List[AddressMapping] = Nil) : AddressMapping = mapping.intersectImpl(that, this :: path) match {
    case NeverMapping => NeverMapping
    case e => this.copy(e)
  }


  override def withOffset(t: AddressTransformer): AddressMapping = copy(mapping.withOffset(t))
  override def withOffsetInvert(t: AddressTransformer): AddressMapping = copy(mapping.withOffsetInvert(t))
}


case class SizeMappingInterleaved(base: BigInt, size: BigInt, blockSize : Int, pattern : Seq[Boolean]) extends AddressMapping {
  assert(isPow2(pattern.size))
  val end = base + size - 1
  val patternIds = pattern.zipWithIndex.filter(_._1).map(_._2)

  override def hit(address: UInt): Bool = {
    val l2 = if (isPow2(size) && base % size == 0){
      (address & ~U(size - 1, address.getWidth bits)) === (base)
    }else {
      (base == 0, log2Up(base + size + 1) > widthOf(address)) match {
        case (false, false) => address >= base && address < base + size
        case (false, true) => address >= base
        case (true, false) => address < base + size
        case (true, true) => True
      }
    }
    val l1 = pattern.map(Bool(_)).read(address(log2Up(blockSize), log2Up(pattern.size) bits))
    l2 && l1
  }
  override def hit(address: BigInt): Boolean = {
    val blockId = (address.toInt / blockSize) & (pattern.size-1)
    address >= base && address < base + size && pattern(blockId)
  }

  override def removeOffset(address: UInt): UInt = {
    if (isPow2(size) && base % size == 0)
      address & (size - 1)
    else
      address - base
  }.resize(log2Up(size))

  override def lowerBound = base
  override def highestBound = base + size - 1
  override def randomPick() : BigInt = ??? //base + BigInt(log2Up(size), simRandom)
  override def withOffset(addressOffset: BigInt): AddressMapping = SizeMappingInterleaved(base + addressOffset, size, blockSize, pattern)
  def overlap(that : SizeMapping) = ??? //this.base < that.base + that.size && this.base + this.size > that.base
  override def foreach(body: BigInt => Unit) = ??? //for(i <- 0 until size.toInt) body(base + i)

  override def toString: String = f"$base%x $size%x $blockSize $pattern"
  override def maxSequentialSize : BigInt = blockSize
  override def randomPick(bytes : BigInt, aligned : Boolean) : BigInt = {
    import spinal.core.sim._
    val blockCount = size / (blockSize*pattern.size)
    val l2 = BigInt(blockCount.bitLength, simRandom) % blockCount
    val l1 = patternIds.randomPick()
    val l0 = simRandom.nextInt(blockSize)
    var addr = l2 * blockSize * pattern.size | l1 * blockSize | l0
    if(aligned) addr = addr/bytes*bytes
    addr
  }
}


/**
 * Model a transformation on a address, typicaly, an address decoder may apply an offset to each of its outputs
 */
trait AddressTransformer{
  def apply(address : BigInt) : BigInt
  def apply(address : UInt) : UInt
  def invert(address : BigInt) : BigInt
  def invert(address : UInt) : UInt
}

/**
 * @param offset For instance, if a slave is mapped at address 0x100 of a master => offset = 0x100
 */
case class OffsetTransformer(offset : BigInt) extends AddressTransformer{
  override def apply(address: BigInt) = address - offset
  override def apply(address: UInt) = address - offset
  override def invert(address: BigInt) = address + offset
  override def invert(address: UInt) = address + offset
  override def toString = f"OT(0x$offset%x)"
}

case class InterleaverTransformer(blockSize : Int, ratio : Int, sel : Int) extends AddressTransformer{
  assert(isPow2(blockSize))
  assert(isPow2(ratio))
  val blockSizeWidth = log2Up(blockSize)
  val ratioWidth = log2Up(ratio)
  override def apply(address: BigInt) = ((address >> blockSizeWidth + ratioWidth) << blockSizeWidth) | (address & (blockSize-1))
  override def apply(address: UInt) = U(address.dropLow(blockSizeWidth + ratioWidth) ## address(0, blockSizeWidth bits) )
  override def invert(address: BigInt) = ((address >> blockSizeWidth) << (blockSizeWidth + ratio)) | sel << blockSizeWidth | (address & (blockSize-1))
  override def invert(address: UInt) = U(address.dropLow(blockSizeWidth) ## B(sel, ratioWidth bits) ## address(0, blockSizeWidth bits))
  override def toString = f"IT(0x$blockSize%x, $ratio, $sel)"
}


//trait LogicalOp[T]{
//  def mincover(rhs : T) : T
//}