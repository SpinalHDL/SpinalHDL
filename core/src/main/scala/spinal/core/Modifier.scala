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
//    Misc.normalizeResize(this, 0, this.getWidth)
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

object Sel{
  def apply[T <: Data](default : T,mappings : (Bool,T)*) :T = seq(default,mappings)
  def seq[T <: Data](default : T,mappings : Seq[(Bool,T)]): T ={
    val result = default.clone
    result := default
    for((cond,value) <- mappings.reverseIterator){
      when(cond){
        result := value
      }
    }
    result
  }
}

object SpinalMap {
  def apply[K <: Data, T <: Data](addr: K, default: T, mappings: (Any, T)*): T = list(addr,default,mappings)
  def list[K <: Data, T <: Data](addr: K, default: T, mappings: Seq[(Any, T)]): T = {
    val result = default.clone
    result := default
    for ((cond, value) <- mappings) {
      when(addr.isEguals(cond)) {
        result := value
      }
    }
    result
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
  def getBitVector: Node
  def getParameterNodes: List[Node]
  def getInputData: Node


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

  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
    case 0 =>
      if(outHi >= 0 && outLo == 0)
        (bitId, bitId)
      else
        (-1,0)
  }
  def getParameterNodes: List[Node] = Nil
  def getInputData: Node = getBitVector
}

class ExtractBoolFloating(opName: String, bitVector: BitVector, bitId: UInt) extends Extract(opName) {
  override def calcWidth: Int = 1
  inputs += bitVector
  inputs += bitId

  def getBitVector = inputs(0)
  def getBitId = inputs(1)

  def getParameterNodes: List[Node] = inputs(1) :: Nil
  def getInputData: Node = getBitVector

  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
    case 0 =>
      if(outHi >= 0 && outLo == 0)
        (Math.min(getBitVector.getWidth-1,(1 << Math.min(20,bitId.getWidth)) - 1), 0)
      else
        (-1,0)
    case 1 =>
      if(outHi >= 0 && outLo == 0)
        (getBitId.getWidth-1,0)
      else
        (-1,0)
  }
}

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
  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
    case 0 => (lo+outHi, lo+outLo)
  }
  def getInputData: Node = getBitVector
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
  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
    case 0 =>
      if(outHi >= outLo) //Not exact
        (Math.min(getBitVector.getWidth-1,(1 << Math.min(20,offset.getWidth))+ bitCount.value - 1), 0)
      else
        (-1,0)
    case 1 =>
      if(outHi >= outLo) //Not exact
        super.getOutToInUsage(inputId,outHi,outLo)
      else
        (-1,0)
    case 2 => (-1,0)
  }
  def getInputData: Node = getBitVector
}


//object AssignedBits {
//  def apply() = new AssignedBits
//  def apply(bitId: Int): AssignedBits = {
//    val ab = new AssignedBits
//    ab.add(new AssignedRange(bitId, bitId))
//    ab
//  }
//  def apply(hi: Int, lo: Int): AssignedBits = {
//    val ab = new AssignedBits
//    ab.add(new AssignedRange(hi, lo))
//    ab
//  }
//
//  def union(a: AssignedBits, b: AssignedBits): AssignedBits = {
//    val ret = AssignedBits()
//
//    ret.add(a)
//    ret.add(b)
//
//    ret
//  }
//
//
//  def intersect(a: AssignedBits, b: AssignedBits): AssignedBits = {
//    val ret = AssignedBits()
//    ret.value = a.value & b.value
//    ret
//  }
//
//
//}

object AssignedBits {
  //  def apply(bitId: Int): AssignedBits = {
  //    val ab = new AssignedBits
  //    ab.add(new AssignedRange(bitId, bitId))
  //    ab
  //  }
  //  def apply(hi: Int, lo: Int): AssignedBits = {
  //    val ab = new AssignedBits
  //    ab.add(new AssignedRange(hi, lo))
  //    ab
  //  }
  def union(a: AssignedBits, b: AssignedBits): AssignedBits = {
    val ret = new AssignedBits(a.width)

    ret.add(a)
    ret.add(b)

    ret
  }


  def intersect(a: AssignedBits, b: AssignedBits): AssignedBits = {
    val ret = a.clone()
    ret.intersect(b)
    ret
  }


  def intersect(a: AssignedRange, b: AssignedBits): AssignedBits = {
    val ret = b.clone()
    ret.intersect(a)
    ret
  }
  def intersect(a: AssignedBits, b: AssignedRange): AssignedBits = intersect(b,a)



}

object AssignedRange{
  def apply(hi : Int,lo : Int) = new AssignedRange(hi,lo)
  def apply(bit : Int) = new AssignedRange(bit,bit)
  def apply() = new AssignedRange(-1,-1)
}

class AssignedRange(val hi: Int, val lo: Int) {
  def toBigInt = ((BigInt(1) << (hi + 1 - lo)) - 1) << lo
  def toAssignedBits = {
    val ret = new AssignedBits(hi + 1)
    ret.add(this)
    ret
  }
}
//
//class AssignedBits(val width : Int) {
//  var value: BigInt = 0
//  override def clone() : AssignedBits = {
//    val ret = new AssignedBits(width)
//    ret.value = value
//    ret
//  }
//  def intersect(range: AssignedRange): Unit = {
//    value = value & range.toBigInt
//  }
//  def intersect(range: AssignedBits): Unit = {
//    value = value & range.value
//  }
//  def add(range: AssignedRange): Unit = {
//    value = value | range.toBigInt
//  }
//  def add(range: AssignedBits): Unit = {
//    value = value | range.value
//  }
//  def remove(range: AssignedRange): Unit = {
//    value = value &~ range.toBigInt
//  }
//  def remove(range: AssignedBits): Unit = {
//    value = value &~ range.value
//  }
//
//  def isEmpty = value == 0
//  def toBinaryString : String = value.toString(2)
//}
//
class AssignedBits(val width : Int) {
  def bitPerIndex = 32
  var value = new Array[Int]((width+bitPerIndex-1)/bitPerIndex)

  override def clone() : AssignedBits = {
    val ret = new AssignedBits(width)
    var idx = value.length
    while(idx != 0){
      idx -= 1
      ret.value(idx) = this.value(idx)
    }
    ret
  }

  def isIntersecting(range: AssignedRange): Boolean = {
    if(range.hi >= width)
      assert(false)
    var idx = value.length
    while(idx != 0){
      idx -= 1
      val hi = Math.min(range.hi - idx*bitPerIndex,bitPerIndex-1)
      val lo = Math.max(range.lo - idx*bitPerIndex,0)
      if(hi >= lo) {
        if((this.value(idx) & (((1l << (hi + 1)) - 1) - ((1l << lo) - 1)).toInt) != 0) {
          return true
        }
      }
    }
    return false
  }



  def + (that : AssignedRange) : AssignedBits = {
    val ret = clone()
    ret.add(that)
    ret
  }

  def intersect(range: AssignedBits): Unit = {
    assert(range.width == this.width)
    var idx = Math.min(value.length,range.value.length)
    while(idx != 0){
      idx -= 1
      this.value(idx) &= range.value(idx)
    }
  }

  def add(range: AssignedBits): Unit = {
    assert(range.width == this.width)
    var idx = Math.min(value.length,range.value.length)
    while(idx != 0){
      idx -= 1
      this.value(idx) |= range.value(idx)
    }
  }
  def remove(range: AssignedBits): Unit = {
    assert(range.width == this.width)
    var idx = Math.min(value.length,range.value.length)
    while(idx != 0){
      idx -= 1
      this.value(idx) &= ~range.value(idx)
    }
  }
  def intersect(range: AssignedRange): Unit = {
    assert(range.hi < width)
    var idx = value.length
    while(idx != 0){
      idx -= 1
      val hi = Math.min(range.hi - idx*bitPerIndex,bitPerIndex-1)
      val lo = Math.max(range.lo - idx*bitPerIndex,0)
      if(hi >= lo)
        this.value(idx) &= (((1l << (hi+1))-1)-((1l << lo)-1)).toInt
    }
  }
  def add(range: AssignedRange): Unit = {
    assert(range.hi < width)
    var idx = value.length
    while(idx != 0){
      idx -= 1
      val hi = Math.min(range.hi - idx*bitPerIndex,bitPerIndex-1)
      val lo = Math.max(range.lo - idx*bitPerIndex,0)
      if(hi >= lo)
        this.value(idx) |= (((1l << (hi+1))-1)-((1l << lo)-1)).toInt
    }
  }
  def remove(range: AssignedRange): Unit = {
    assert(range.hi < width)
    var idx = value.length
    while(idx != 0){
      idx -= 1
      val hi = Math.min(range.hi - idx*bitPerIndex,bitPerIndex-1)
      val lo = Math.max(range.lo - idx*bitPerIndex,0)
      if(hi >= lo)
        this.value(idx) &= ~(((1l << (hi+1))-1)-((1l << lo)-1)).toInt
    }
  }

  def toBinaryString : String = {
    val strs = for((e,idx) <- value.zipWithIndex.reverseIterator) yield {
      val str = e.toBinaryString
      val eWidth = if(idx == value.length-1) width-idx*bitPerIndex else 32

      "0"*(eWidth-str.length) + str
    }
    strs.reduce(_ + "_" + _)
  }
  def isEmpty = value.foldLeft(true)((c,e) => c && (e == 0))
}
trait AssignementNode extends Node {
  def getAssignedBits: AssignedRange //Bit that are allwas assigned
  def getScopeBits: AssignedRange //Bit tht could be assigned
  def getOutBaseType: BaseType

  def clone(out : Node) : this.type
}


class BitAssignmentFixed(out: BitVector, in: Node, bitId: Int) extends AssignementNode {
  inputs += in

  def getInput = inputs(0)
  def getBitId = bitId
  override def calcWidth: Int = bitId + 1

  override def checkInferedWidth: String = {
    if (bitId < 0 || bitId >= out.getWidth) {
      return s"Static bool extraction (bit ${bitId}) is outside the range (${out.getWidth - 1} downto 0) of ${out} at\n${getScalaTraceString}"
    }
    return null
  }

  def getAssignedBits: AssignedRange = AssignedRange(bitId)
  def getScopeBits: AssignedRange = getAssignedBits
  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = {
    if(outHi >= bitId && bitId >= outLo)
      (0,0)
    else
      (-1,0)
  }
  def getOutBaseType: BaseType = out

  override def clone(out: Node): this.type = new BitAssignmentFixed(out.asInstanceOf[BitVector],in,bitId).asInstanceOf[this.type]
}




class RangedAssignmentFixed(out: BitVector, in: Node, hi: Int, lo: Int) extends AssignementNode {
  inputs += in

  def getInput = inputs(0)
  def getHi = hi
  def getLo = lo

  override def calcWidth: Int = hi + 1

  override def checkInferedWidth: String = {
    val input = getInput
    if (input.component != null && hi + 1 - lo != input.getWidth) {
      return s"Assignement bit count missmatch. ${this} := ${input}} at\n${getScalaTraceString}"
    }

    val width = out.getWidth
    if (hi >= width || lo < 0) {
      return s"Static bits assignement ($hi downto $lo) is outside the range (${width - 1} downto 0) of ${out} at\n${getScalaTraceString}"
    }
    return null
  }

  override def normalizeInputs: Unit = {
    InputNormalize.bitVectoreAssignement(this,0,hi + 1 - lo)
  }



  def getAssignedBits: AssignedRange = AssignedRange(hi, lo)
  def getScopeBits: AssignedRange = getAssignedBits
  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = {
    val relativeLo = outLo-lo
    val relativeHi = outHi-lo
    if(relativeHi >= 0 && hi-lo >= relativeLo)
      super.getOutToInUsage(inputId,Math.min(relativeHi,hi-lo),Math.max(relativeLo,0))
    else
      (-1,0)
  }
  def getOutBaseType: BaseType = out
  override def clone(out: Node): this.type = new RangedAssignmentFixed(out.asInstanceOf[BitVector],in,hi,lo).asInstanceOf[this.type]

}


class BitAssignmentFloating(out: BitVector, in: Node, bitId: UInt) extends AssignementNode {
  inputs += in
  inputs += bitId

  def getInput = inputs(0)
  def getBitId = inputs(1)

  override def calcWidth: Int = 1 << Math.min(20,getBitId.getWidth)

  def getAssignedBits: AssignedRange = AssignedRange()
  def getScopeBits: AssignedRange = AssignedRange(Math.min(out.getWidth-1,(1 << Math.min(20,getBitId.getWidth)) - 1), 0)

  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
    case 0 =>
      if(outHi >= 0 && ((1 << Math.min(20,getBitId.getWidth)) - 1) >= outLo)
        (0,0)
      else
        (-1,0)
    case 1 =>
      if(outHi >= 0 && ((1 << Math.min(20,getBitId.getWidth)) - 1) >= outLo)
        super.getOutToInUsage(inputId,outHi,outLo)
      else
        (-1,0)
  }
  def getOutBaseType: BaseType = out
  override def clone(out: Node): this.type = new BitAssignmentFloating(out.asInstanceOf[BitVector],in,bitId).asInstanceOf[this.type]
}

class RangedAssignmentFloating(out: BitVector, in: Node, offset: UInt, bitCount: BitCount) extends AssignementNode {
  inputs += in
  inputs += offset

  def getInput = inputs(0)
  def getOffset = inputs(1)
  def getBitCount = bitCount

  override def calcWidth: Int = 1 << Math.min(20,offset.getWidth) + bitCount.value



  override def checkInferedWidth: String = {
    val input = getInput
    if (input.component != null && bitCount.value != input.getWidth) {
      return s"Assignement bit count missmatch. ${this} := ${input}} at\n${getScalaTraceString}"
    }

    return null
  }

  override def normalizeInputs: Unit = {
    InputNormalize.bitVectoreAssignement(this,0,bitCount.value)
  }



  def getAssignedBits: AssignedRange = AssignedRange()
  def getScopeBits: AssignedRange = AssignedRange(Math.min(out.getWidth-1,(1 << Math.min(20,offset.getWidth))+ bitCount.value - 1), 0)
  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = super.getOutToInUsage(inputId,outHi,outLo) //TODO
  def getOutBaseType: BaseType = out
  override def clone(out: Node): this.type = new RangedAssignmentFloating(out.asInstanceOf[BitVector],in,offset,bitCount).asInstanceOf[this.type]
}


class MultipleAssignmentNode extends Node {
  override def calcWidth: Int = WidthInfer.inputMaxWidth(this)
  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = (outHi,outLo)

  override def normalizeInputs: Unit = {
    for (i <- 0 until inputs.size)
      InputNormalize.bitVectoreAssignement(this,i,this.getWidth)
  }

  override private[core] def checkInferedWidth: String = {
    for (i <- 0 until inputs.size){
      val input = this.inputs(i)
      if (input != null && input.component != null && this.getWidth !=input.getWidth) {
        return s"Assignement bit count missmatch. ${this} := ${input}} at\n${getScalaTraceString}"
      }
    }
    return null
  }
}


