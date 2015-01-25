/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal

/**
 * Created by PIC18F on 22.01.2015.
 */

class BitCount(val value: Int) {

}

abstract class BitVectorFactory[T <: BitVector] {
  def apply(): T
  def apply(value: BigInt): T = BitsLiteral(value, -1, this())
  def apply(width: BitCount): T = this().setWidth(width.value)
  def apply(value: BigInt, width: BitCount): T = BitsLiteral(value, width.value, this().setWidth(width.value))
}

abstract class BitVector extends BaseType {
  var fixedWidth = -1

  def isFixedWidth = fixedWidth != -1

  def setWidth(width: Int): this.type = {
    fixedWidth = width
    this
  }

  override def clone: this.type = {
    val res = super.clone
    res.fixedWidth = this.fixedWidth
    res
  }

  override def normalizeInputs: Unit ={
    InputNormalize.nodeWidth(this)
  }

  def resize(width: Int): this.type
  //def resize(width: Int): this.type

  override def calcWidth: Int = {
    if (isFixedWidth) return fixedWidth
    if (inputs.size == 0) return -1
    return inputs(0).getWidth
  }

  //TODO use Vec DSL, getWidth must be totaly solved
  def toBools: Vec[Bool] = {
    val vec = new Vec(new Bool())
    for (i <- 0 until getWidth) vec.vec += this(i)
    vec
  }
  //extract bit
  //TODO bitId Range check ?
  def apply(bitId: Int): Bool = {
    val extract = ExtractBool(this, bitId)
    val bool = new Bool()
    bool.setInput(extract)
    bool
  }
  //extract bit
  //TODO bitId Range check ?
  def apply(bitId: UInt): Bool = {
    val extract = ExtractBool(this, bitId)
    val bool = new Bool()
    bool.setInput(extract)
    bool
  }
  //extract bits
  //TODO bitId Range check ?
  def apply(highBit: Int, lowBit: Int): Bits = {
    val extract = ExtractBitsVector(this, highBit, lowBit)
    val bits = new Bits()
    bits.setInput(extract)
    bits
  }

  //extract bits
  //TODO bitId Range check ?
  def apply(highBit: UInt, lowBit: UInt): Bits = {
    ExtractBitsVector(this, highBit, lowBit)
  }

  override def addTypeNodeFrom(node: Node): this.type = {
    val typeNode = super.addTypeNodeFrom(node)
    typeNode.fixedWidth = -1
    typeNode
  }


}