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

import spinal.core.internals._


/**
  * Base class to create Bit Vector from literal
  */
abstract class BitVectorLiteralFactory[T <: BitVector] {

  def apply(): T

  def apply(value: Int): T = this(BigInt(value))
  def apply(value: Int, width: BitCount): T = this(BigInt(value),width)

  def apply(value: BigInt): T = getFactory(value, -1, this().setAsTypeNode())
  def apply(value: BigInt, width: BitCount): T = getFactory(value, width.value, this().setWidth(width.value).setAsTypeNode())
  def apply(value: String): T = bitVectorStringParser(this, value,  isSigned)

  def apply(bitCount: BitCount, rangesValue: (Any, Any), _rangesValues: (Any, Any)*): T = this.aggregate(bitCount, rangesValue +: _rangesValues)

  def apply(rangesValue: (Any, Any), _rangesValues: (Any ,Any)*): T = {
    val rangesValues = rangesValue +: _rangesValues
    var hig = -1

    rangesValues.foreach {
      case (pos: Int, _) =>
        hig = Math.max(hig, pos)
      case (range: Range, _) =>
        hig = Math.max(hig, range.high)
      case (`default`, _) =>
        GlobalData.get.pendingErrors += (() => s"You can't use default -> ??? in this case. \nBut you can use it in the following case : myBits := (???,default -> ???, ???)\n${ScalaLocated.long}")
      case _ =>
    }

    this.aggregate(hig + 1 bit, rangesValues)
  }

  def getFactory: (BigInt, Int, T) => T

  def isSigned: Boolean

  private[core] def newInstance(bitCount: BitCount) : T

  private def aggregate(bitCount: BitCount, rangesValues: Seq[(Any, Any)]): T = {
    val ret: T = this.newInstance(bitCount.value bit)
    applyTuples(ret, rangesValues)
    ret
  }

  def applyTuples(on: T, rangesValues: Seq[(Any, Any)]): Unit = {
    //Apply default
    rangesValues.foreach {
      case (`default`, value: Boolean) => on.setAllTo(value)
      case (`default`, value: Bool) => on.setAllTo(value)
      case _ =>
    }

    //Apply specific access in order
    for(e <- rangesValues) e match{
      case (pos: Int, value: Boolean)   => on(pos) := Bool(value)
      case (pos: Int, value: Bool)      => on(pos) := value //literal extraction
      case (range: Range, value: Bool) =>
        for(i <- range)
          on(i) := value
      case (range: Range, value: Boolean) => on(range) := apply(
        if(! value)
          BigInt(0)
        else {
          if(!on.isInstanceOf[SInt])
            (BigInt(1) << range.size) - 1
          else
            BigInt(-1)
        }
      )
      case (range: Range, value: String) => on(range) := apply(value)
      case (range: Range, value: BitVector) if value.getClass == on.getClass => on(range) := value.asInstanceOf[T]
      case (`default`, value: Boolean) =>
      case (`default`, value: Bool)    =>
    }
  }
}


/**
  * Used to create a new Bits or cast to Bits
  */
object B extends BitVectorLiteralFactory[Bits] {
  def apply(): Bits = new Bits()
  def apply(that: Data): Bits = that.asBits

  override private[core] def newInstance(bitCount: BitCount): Bits = Bits(bitCount)
  override def isSigned: Boolean = false

  override def getFactory: (BigInt, Int, Bits) => Bits = {
    BitsLiteral.apply[Bits]
  }
}


/**
  * Used to create a new UInt or cast to UInt
  */
object U extends BitVectorLiteralFactory[UInt] {
  def apply(): UInt = new UInt()
  def apply(that: Bool): UInt = that.asUInt
  def apply(that: Bits): UInt = that.asUInt
  def apply(that: SInt): UInt = that.asUInt
  def apply(that: UFix): UInt = that.toUInt

  override private[core] def newInstance(bitCount: BitCount): UInt = UInt(bitCount)
  override def isSigned: Boolean = false
  override def getFactory: (BigInt, Int, UInt) => UInt = UIntLiteral.apply[UInt]
}


/**
  * Used to create a new SInt or cast to SInt
  */
object S extends BitVectorLiteralFactory[SInt] {
  def apply(): SInt = new SInt()
  def apply(that : Bool): SInt = that.asSInt
  def apply(that : Bits): SInt = that.asSInt
  def apply(that : UInt): SInt = that.asSInt
  def apply(that : SFix): SInt = that.toSInt

  override private[core] def newInstance(bitCount: BitCount): SInt = SInt(bitCount)
  override def isSigned: Boolean = true
  override def getFactory: (BigInt, Int, SInt) => SInt = SIntLiteral.apply[SInt]
}

