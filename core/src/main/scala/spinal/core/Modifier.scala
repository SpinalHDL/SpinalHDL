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

package spinal.core



object EnumCast {
  def apply(enum: SpinalEnumCraft[_], opName: String, that: Node, widthImpl: (Node) => Int = WidthInfer.inputMaxWidth): Modifier = {
    val op = new EnumCast(enum, opName, widthImpl)
    op.inputs += that
    op
  }


}


object Cast {
  def apply(opName: String, that: Node, widthImpl: (Node) => Int = WidthInfer.inputMaxWidth): Modifier = {
    val op = new Cast(opName, widthImpl)
    op.inputs += that
    op
  }
}

object Resize {
  def apply(opName: String, args: List[Node], widthImpl: (Node) => Int = WidthInfer.inputMaxWidth,simplifyNodeImpl: (Node) => Unit): Modifier = {
    val op = new Function(opName, widthImpl,simplifyNodeImpl)
    op.inputs ++= args
    op.inferredWidth = widthImpl(op)
    op
  }

}

object Function {
  def apply(opName: String, args: List[Node], widthImpl: (Node) => Int = WidthInfer.inputMaxWidth,simplifyNodeImpl : (Node) => Unit): Modifier = {
    val op = new Function(opName, widthImpl,simplifyNodeImpl)
    op.inputs ++= args
    op
  }
}

object UnaryOperator {
  def apply(opName: String, right: Node, widthImpl: (Node) => Int, normalizeInputsImpl: (Node) => Unit,simplifyNodeImpl : (Node) => Unit): Modifier = {
    val op = new Operator(opName, widthImpl, normalizeInputsImpl,simplifyNodeImpl)
    op.inputs += right
    op
  }
}

object BinaryOperator {
  def apply(opName: String, left: Node, right: Node, widthImpl: (Node) => Int, normalizeInputsImpl: (Node) => Unit,simplifyNodeImpl : (Node) => Unit): Modifier = {
    val op = new Operator(opName, widthImpl, normalizeInputsImpl,simplifyNodeImpl)
    op.inputs += left
    op.inputs += right
    op
  }
}

class Operator(opName: String, widthImpl: (Node) => Int, val normalizeInputsImpl: (Node) => Unit,simplifyNodeImpl : (Node) => Unit) extends Modifier(opName, widthImpl) {
  override def normalizeInputs: Unit = {
    normalizeInputsImpl(this)
  }

  override def simplifyNode: Unit = {
    simplifyNodeImpl(this)
  }
}

class Modifier(val opName: String, widthImpl: (Node) => Int) extends Node {
  override def calcWidth(): Int = {
    widthImpl(this)
  }


  override def toString(): String = {
    s"($opName ${this.inputs.map(in => if (in == null) "null" else in.nonRecursiveToString()).reduceLeft(_ + " " + _)})"
  }

  override def nonRecursiveToString(): String = opName
}

class Function(opName: String, widthImpl: (Node) => Int,simplifyNodeImpl : (Node) => Unit) extends Modifier(opName, widthImpl) {
  override def simplifyNode: Unit = {
    simplifyNodeImpl(this)
  }
}

class Cast(opName: String, widthImpl: (Node) => Int = WidthInfer.inputMaxWidth) extends Modifier(opName, widthImpl) {

}

class EnumCast(val enum: SpinalEnumCraft[_], opName: String, widthImpl: (Node) => Int = WidthInfer.inputMaxWidth) extends Modifier(opName, widthImpl) {
  override def normalizeInputs: Unit = {
    Misc.normalizeResize(this, 0, this.getWidth)
  }
}


class Multiplexer(opName: String) extends Modifier(opName, WidthInfer.multiplexImpl) {
  def cond = inputs(0)

  def whenTrue = inputs(1)

  def whenFalse = inputs(2)

  override def normalizeInputs: Unit = {
    Misc.normalizeResize(this, 1, this.getWidth)
    Misc.normalizeResize(this, 2, this.getWidth)
  }

  override def simplifyNode: Unit = ZeroWidth.multiplexerImpl(this)
}

object Mux {
  def apply[T <: Data](sel: Bool, whenTrue: T, whenFalse: T): T = {
    Multiplex.complexData(sel, whenTrue, whenFalse)
  }
}


private[spinal] object Multiplex {
  def apply(opName: String, sel: Bool, one: Node, zero: Node): Multiplexer = {

    val op = new Multiplexer(opName)
    op.inputs += sel
    op.inputs += one
    op.inputs += zero
    op
  }

  def baseType[T <: BaseType](sel: Bool, whenTrue: T, whenFalse: T): Multiplexer = {
    whenTrue.newMultiplexer(sel, whenTrue, whenFalse)
  }


  def complexData[T <: Data](sel: Bool, whenTrue: T, whenFalse: T): T = {
    val outType = if (whenTrue.getClass.isAssignableFrom(whenFalse.getClass)) whenTrue
    else if (whenFalse.getClass.isAssignableFrom(whenTrue.getClass)) whenFalse
    else throw new Exception("can't mux that")


    val muxOut = outType.clone()
    muxOut.flatten.foreach(_ match{
      case bv : BitVector => bv.fixedWidth = -1
      case _ =>
    })
    val muxInTrue = muxOut.clone()
    val muxInFalse = muxOut.clone()


    muxInTrue := whenTrue
    muxInFalse := whenFalse


    for ((out, t,  f) <- (muxOut.flatten, muxInTrue.flatten, muxInFalse.flatten).zipped) {
      if (t == null) SpinalError("Create a mux with incompatible true input type")
      if (f == null) SpinalError("Create a mux with incompatible false input type")

      out.setInput(Multiplex.baseType(sel, t, f))
    }
    muxOut
  }
}
abstract class Extract(opName: String) extends Modifier(opName, null){
  def getScopeBits: AssignedBits
  def getBitVector: Node
  def getParameterNodes: List[Node]
}

class ExtractBoolFixed(opName: String, bitVector: BitVector, bitId: Int) extends Extract(opName) {
  inputs += bitVector

  def getBitVector = inputs(0)
  def getBitId = bitId

  override def calcWidth: Int = 1

  override def checkInferedWidth: String = {
    if (bitId < 0 || bitId >= getBitVector.getWidth) {
      return s"Static bool extraction (bit ${bitId}) is outside the range (${getBitVector.getWidth - 1} downto 0) of ${getBitVector} at\n${getScalaTraceString}"
    }
    return null
  }
  def getParameterNodes: List[Node] = Nil
  def getScopeBits: AssignedBits = AssignedBits(bitId)
}

class ExtractBoolFloating(opName: String, bitVector: BitVector, bitId: UInt) extends Extract(opName) {
  override def calcWidth: Int = 1
  inputs += bitVector
  inputs += bitId

  def getBitVector = inputs(0)
  def getBitId = inputs(1)

  def getParameterNodes: List[Node] = inputs(1) :: Nil
  def getScopeBits: AssignedBits = AssignedBits(Math.min(getBitVector.getWidth-1,(1 << Math.min(20,bitId.getWidth)) - 1), 0)
}

//object ExtractBitsVectorFixed {
//  def apply(bitVector: BitVector, bitIdHi: Int, bitIdLow: Int): ExtractBitsVectorFixed = {
//    val op = new ExtractBitsVectorFixed(bitIdHi, bitIdLow)
//    op.inputs += bitVector
//    op
//  }
//
//
//  def apply(bitVector: BitVector, bitIdHi: UInt, bitIdLow: UInt): Bits = {
//    val sr = bitVector.toBits >> bitIdLow
//    val mask = (UInt(1) << bitIdLow) - UInt(1)
//    val ret = sr & mask.toBits;
//    ret
//  }
//}

class ExtractBitsVectorFixed(opName: String, bitVector: BitVector, hi: Int, lo: Int) extends Extract(opName) {
  if (hi - lo < -1) SpinalError(s"Static bits extraction with a negative size ($hi downto $lo)")

  override def calcWidth: Int = hi - lo + 1

  inputs += bitVector
  def getBitVector = inputs(0)
  def getHi = hi
  def getLo = lo


  override def checkInferedWidth: String = {
    val width = getBitVector.getWidth
    if (hi >= width || lo < 0) {
      return s"Static bits extraction ($hi downto $lo) is outside the range (${width - 1} downto 0) of ${getBitVector} at\n${getScalaTraceString}"
    }
    return null
  }

  def getParameterNodes: List[Node] =  Nil
  def getScopeBits: AssignedBits = AssignedBits(hi,lo)
}


class ExtractBitsVectorFloating(opName: String, bitVector: BitVector, offset: UInt, bitCount: BitCount) extends Extract(opName) {
  override def calcWidth: Int = bitCount.value

  offset.dontSimplifyIt() //Because it can appear at multipe location (o+bc-1 downto o)

  inputs += bitVector
  inputs += offset
  inputs += IntLiteral(bitCount.value)

  def getBitVector = inputs(0)
  def getOffset = inputs(1)
  def getBitCount = bitCount

  def getParameterNodes: List[Node] = inputs(1) :: Nil
  def getScopeBits: AssignedBits = AssignedBits(Math.min(getBitVector.getWidth-1,(1 << Math.min(20,offset.getWidth))+ bitCount.value - 1), 0)
}

object AssignedBits {
  def apply() = new AssignedBits
  def apply(bitId: Int): AssignedBits = {
    val ab = new AssignedBits
    ab.add(new AssignedRange(bitId, bitId))
    ab
  }
  def apply(hi: Int, lo: Int): AssignedBits = {
    val ab = new AssignedBits
    ab.add(new AssignedRange(hi, lo))
    ab
  }

  def union(a: AssignedBits, b: AssignedBits): AssignedBits = {
    val ret = AssignedBits()

    ret.add(a)
    ret.add(b)

    ret
  }


  def intersect(a: AssignedBits, b: AssignedBits): AssignedBits = {
    val ret = AssignedBits()
    ret.value = a.value & b.value
    ret
  }


}

class AssignedRange(val hi: Int, val lo: Int) {
  def toBigInt = ((BigInt(1) << (hi + 1 - lo)) - 1) << lo

}

class AssignedBits {
  var value: BigInt = 0


  def add(range: AssignedRange): Unit = {
    value = value | range.toBigInt
  }
  def add(range: AssignedBits): Unit = {
    value = value | range.value
  }
  def remove(range: AssignedRange): Unit = {
    value = value &~ range.toBigInt
  }
  def remove(range: AssignedBits): Unit = {
    value = value &~ range.value
  }

  def isEmpty = value == 0
}


trait AssignementNode extends Node {
  def getAssignedBits: AssignedBits
  def getScopeBits: AssignedBits
  def getOutBaseType: BaseType
}


class BitAssignmentFixed(out: BitVector, in: Node, bitId: Int) extends AssignementNode {
  inputs += in

  def getInput = inputs(0)
  def getBitId = bitId
  override def calcWidth: Int = out.getWidth

  override def checkInferedWidth: String = {
    if (bitId < 0 || bitId >= out.getWidth) {
      return s"Static bool extraction (bit ${bitId}) is outside the range (${out.getWidth - 1} downto 0) of ${out} at\n${getScalaTraceString}"
    }
    return null
  }

  def getAssignedBits: AssignedBits = AssignedBits(bitId)
  def getScopeBits: AssignedBits = getAssignedBits
  def getOutBaseType: BaseType = out
}

class BitAssignmentFloating(out: BitVector, in: Node, bitId: UInt) extends AssignementNode {
  inputs += in
  inputs += bitId

  def getInput = inputs(0)
  def getBitId = inputs(1)

  override def calcWidth: Int = out.getWidth

  def getAssignedBits: AssignedBits = AssignedBits()
  def getScopeBits: AssignedBits = AssignedBits(Math.min(out.getWidth-1,(1 << Math.min(20,bitId.getWidth)) - 1), 0)
  def getOutBaseType: BaseType = out
}


class RangedAssignmentFixed(out: BitVector, in: Node, hi: Int, lo: Int) extends AssignementNode {
  inputs += in

  def getInput = inputs(0)
  def getHi = hi
  def getLo = lo

  override def calcWidth: Int = out.getWidth

  override def checkInferedWidth: String = {
    val width = out.getWidth
    if (hi >= width || lo < 0) {
      return s"Static bits assignement ($hi downto $lo) is outside the range (${width - 1} downto 0) of ${out} at\n${getScalaTraceString}"
    }
    return null
  }

  override def normalizeInputs: Unit = {
    Misc.normalizeResize(this, 0, hi + 1 - lo)
  }


  def getAssignedBits: AssignedBits = AssignedBits(hi, lo)
  def getScopeBits: AssignedBits = getAssignedBits
  def getOutBaseType: BaseType = out

}

class RangedAssignmentFloating(out: BitVector, in: Node, offset: UInt, bitCount: BitCount) extends AssignementNode {
  inputs += in
  inputs += offset

  def getInput = inputs(0)
  def getOffset = inputs(1)
  def getBitCount = bitCount

  override def calcWidth: Int = out.getWidth


  override def normalizeInputs: Unit = {
    Misc.normalizeResize(this, 0, bitCount.value)
  }

  def getAssignedBits: AssignedBits = AssignedBits()
  def getScopeBits: AssignedBits = AssignedBits(Math.min(out.getWidth-1,(1 << Math.min(20,offset.getWidth))+ bitCount.value - 1), 0)
  def getOutBaseType: BaseType = out
}


class MultipleAssignmentNode extends Node {
  override def calcWidth: Int = WidthInfer.inputMaxWidth(this)
  override def normalizeInputs: Unit = {
    for (i <- 0 until inputs.size)
      Misc.normalizeResize(this, i, this.getWidth)
  }
}