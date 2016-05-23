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

import scala.collection.mutable.ArrayBuffer







abstract class Resize extends Modifier(null,null){
  var size : Int = -1
  var input : Node = null
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node}
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {assert(id == 0); input}

  override def calcWidth(): Int = size
}

class ResizeBits extends Resize{
  override def opName: String = "resize(b,i)"
  override def simplifyNode: Unit = SymplifyNode.resizeImpl2(B.apply,this)
}
class ResizeUInt extends Resize{
  override def opName: String = "resize(u,i)"
  override def simplifyNode: Unit = SymplifyNode.resizeImpl2(U.apply,this)
}
class ResizeSInt extends Resize{
  override def opName: String = "resize(s,i)"
  override def simplifyNode: Unit = SymplifyNode.resizeImpl2(S.apply,this)
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

class Operator(opName: String, widthImpl: (Node) => Int, val normalizeInputsImpl: (Node) => Unit,simplifyNodeImpl : (Node) => Unit) extends ModifierImpl(opName, widthImpl) {
  override def normalizeInputs: Unit = {
    normalizeInputsImpl(this)
  }

  override def simplifyNode: Unit = {
    simplifyNodeImpl(this)
  }
}

class UnaryOperator extends Operator(null,null,null,null){
  var input : Node = null
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node}
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {assert(id == 0); input}
}

class ConstantOperator extends Operator(null,null,null,null){
  var input : Node = null
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node}
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {assert(id == 0); input}
}






class BinaryOperator extends Operator(null,null,null,null){
  var left,right  : Node = null

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    doThat(left,0)
    doThat(right,1)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    doThat(left)
    doThat(right)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 0 => left = node
    case 1 => right = node
  }

  override def getInputsCount: Int = 2
  override def getInputs: Iterator[Node] = Iterator(left,right)
  override def getInput(id: Int): Node = id match{
    case 0 => left
    case 1 => right
  }
}


object Operator{
  object Bool{
    class And extends BinaryOperator{
      override def opName: String = "&&"
      override def calcWidth(): Int = 1
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {}
    }

    class Or extends BinaryOperator{
      override def opName: String = "||"
      override def calcWidth(): Int = 1
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {}
    }

    class Xor extends BinaryOperator{
      override def opName: String = "B^B"
      override def calcWidth(): Int = 1
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {}
    }

    class Not extends UnaryOperator{
      override def opName: String = "!"
      override def calcWidth(): Int = input.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {}
    }

    class Equal extends BinaryOperator{
      override def opName: String = "B==B"
      override def calcWidth(): Int = 1
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {}
    }

    class NotEqual extends BinaryOperator{
      override def opName: String = "B!=B"
      override def calcWidth(): Int = 1
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {}
    }
  }

  object BitVector{
    abstract class And extends BinaryOperator{
      override def calcWidth(): Int = Math.max(left.getWidth,right.getWidth)
      override def normalizeInputs: Unit = {InputNormalize.nodeWidth(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryInductZeroWithOtherWidth(getLiteralFactory)(this)}
      def getLiteralFactory : (BigInt, BitCount) => Node
    }

    abstract class Or extends BinaryOperator{
      override def calcWidth(): Int = Math.max(left.getWidth,right.getWidth)
      override def normalizeInputs: Unit = {InputNormalize.nodeWidth(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryTakeOther(this)}
    }

    abstract class Xor extends BinaryOperator{
      override def calcWidth(): Int = Math.max(left.getWidth,right.getWidth)
      override def normalizeInputs: Unit = {InputNormalize.nodeWidth(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryTakeOther(this)}
    }

    abstract class Add extends BinaryOperator{
      override def calcWidth(): Int = Math.max(left.getWidth,right.getWidth)
      override def normalizeInputs: Unit = {InputNormalize.nodeWidth(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryTakeOther(this)}
    }

    abstract class Sub extends BinaryOperator{
      override def calcWidth(): Int = Math.max(left.getWidth,right.getWidth)
      override def normalizeInputs: Unit = {InputNormalize.nodeWidth(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryMinus(getLiteralFactory)(this)}
      def getLiteralFactory : (BigInt, BitCount) => Node
    }

    abstract class Mul extends BinaryOperator{
      override def calcWidth(): Int = left.getWidth + right.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.binaryInductZeroWithOtherWidth(getLiteralFactory)(this)}
      def getLiteralFactory : (BigInt, BitCount) => Node
    }

    abstract class Div extends BinaryOperator{
      override def calcWidth(): Int = left.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.unsignedDivImpl(this)}
    }

    abstract class Mod extends BinaryOperator{
      override def calcWidth(): Int = left.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.unsignedModImpl(this)}
    }

    abstract class Equal extends BinaryOperator{
      override def calcWidth(): Int = 1
      override def normalizeInputs: Unit = {InputNormalize.inputWidthMax(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryThatIfBoth(True)(this)}
    }

    abstract class NotEqual extends BinaryOperator{
      override def calcWidth(): Int = 1
      override def normalizeInputs: Unit = {InputNormalize.inputWidthMax(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryThatIfBoth(False)(this)}
    }

    abstract class ShiftRightByInt(val shift : Int) extends ConstantOperator{
      assert(shift >= 0)
      override def calcWidth(): Int = Math.max(0, input.getWidth - shift)
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.shiftRightImpl(this)}
    }

    abstract class ShiftRightByUInt extends BinaryOperator{
      override def calcWidth(): Int = left.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.shiftRightImpl(this)}
    }

    abstract class ShiftLeftByInt(val shift : Int) extends ConstantOperator{
      assert(shift >= 0)
      override def calcWidth(): Int = input.getWidth + shift
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.shiftLeftImpl(getLiteralFactory,this)}
      def getLiteralFactory : (BigInt, BitCount) => Node
    }

    abstract class ShiftLeftByUInt extends BinaryOperator{
      override def calcWidth(): Int = left.getWidth + (1 << right.getWidth) - 1
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.shiftLeftImpl(getLiteralFactory,this)}
      def getLiteralFactory : (BigInt, BitCount) => Node
    }


    abstract class RotateLeftByUInt extends BinaryOperator{
      override def calcWidth(): Int = left.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.rotateImpl(getLiteralFactory,this)}
      def getLiteralFactory : (BigInt, BitCount) => Node
    }
  }

  object Bits{
    class Cat extends BinaryOperator{
      override def opName: String = "b##b"
      override def calcWidth(): Int = left.getWidth + right.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.binaryTakeOther(this)}
    }

    class Not extends UnaryOperator{
      override def opName: String = "~b"
      override def calcWidth(): Int = input.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.unaryZero(this)}
    }

    class And extends BitVector.And{
      override def opName: String = "b&b"
      override def getLiteralFactory: (BigInt, BitCount) => Node = B.apply
    }

    class Or extends BitVector.Or{
      override def opName: String = "b|b"
    }

    class Xor extends BitVector.Xor{
      override def opName: String = "b^b"
    }

    class Equal extends BitVector.Equal{
      override def opName: String = "b==b"
    }

    class NotEqual extends BitVector.NotEqual{
      override def opName: String = "b!=b"
    }

    class ShiftRightByInt(shift : Int) extends BitVector.ShiftRightByInt(shift){
      override def opName: String = "b>>i"
    }

    class ShiftRightByUInt extends BitVector.ShiftRightByUInt{
      override def opName: String = "b>>u"
    }

    class ShiftLeftByInt(shift : Int) extends BitVector.ShiftLeftByInt(shift){
      override def opName: String = "b<<i"
      override def getLiteralFactory: (BigInt, BitCount) => Node = B.apply
    }

    class ShiftLeftByUInt extends BitVector.ShiftLeftByUInt{
      override def opName: String = "b<<u"
      override def getLiteralFactory: (BigInt, BitCount) => Node = B.apply
    }

    class RotateLeftByUInt extends BitVector.RotateLeftByUInt{
      override def opName: String = "brotlu"
      def getLiteralFactory : (BigInt, BitCount) => Node = B.apply
    }

  }


  object UInt{
    class Not extends UnaryOperator{
      override def opName: String = "~u"
      override def calcWidth(): Int = input.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.unaryZero(this)}
    }

    class And extends BitVector.And{
      override def opName: String = "u&u"
      override def getLiteralFactory: (BigInt, BitCount) => Node = U.apply
    }

    class Or extends BitVector.Or{
      override def opName: String = "u|u"
    }

    class Xor extends BitVector.Xor{
      override def opName: String = "u^u"
    }

    class Add extends BitVector.Add{
      override def opName: String = "u+u"
    }

    class Sub extends BitVector.Sub{
      override def opName: String = "u-u"
      override def getLiteralFactory: (BigInt, BitCount) => Node = U.apply
    }

    class Mul extends BitVector.Mul{
      override def opName: String = "u*u"
      override def getLiteralFactory: (BigInt, BitCount) => Node = U.apply
    }

    class Div extends BitVector.Div{
      override def opName: String = "u/u"
    }

    class Mod extends BitVector.Mod{
      override def opName: String = "u%u"
    }

    class Smaller extends BinaryOperator{
      override def opName: String = "u<u"
      override def calcWidth(): Int = 1
      override def normalizeInputs: Unit = {InputNormalize.inputWidthMax(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryUIntSmaller(this)}
    }

    class SmallerOrEqual extends BinaryOperator{
      override def opName: String = "u<=u"
      override def calcWidth(): Int = 1
      override def normalizeInputs: Unit = {InputNormalize.inputWidthMax(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryUIntSmallerOrEgual(this)}
    }

    class Equal extends BitVector.Equal{
      override def opName: String = "u==u"
    }

    class NotEqual extends BitVector.NotEqual{
      override def opName: String = "u!=u"
    }

    class ShiftRightByInt(shift : Int) extends BitVector.ShiftRightByInt(shift){
      override def opName: String = "u>>i"
    }

    class ShiftRightByUInt extends BitVector.ShiftRightByUInt{
      override def opName: String = "u>>u"
    }

    class ShiftLeftByInt(shift : Int) extends BitVector.ShiftLeftByInt(shift){
      override def opName: String = "u<<i"
      override def getLiteralFactory: (BigInt, BitCount) => Node = U.apply
    }

    class ShiftLeftByUInt extends BitVector.ShiftLeftByUInt{
      override def opName: String = "u<<u"
      override def getLiteralFactory: (BigInt, BitCount) => Node = U.apply
    }
  }

  object SInt{
    class Not extends UnaryOperator{
      override def opName: String = "~s"
      override def calcWidth(): Int = input.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.unaryZero(this)}
    }

    class Minus extends UnaryOperator{
      override def opName: String = "-s"
      override def calcWidth(): Int = input.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.unaryZero(this)}
    }

    class And extends BitVector.And{
      override def opName: String = "s&s"
      override def getLiteralFactory: (BigInt, BitCount) => Node = S.apply
    }

    class Or extends BitVector.Or{
      override def opName: String = "s|s"
    }

    class Xor extends BitVector.Xor{
      override def opName: String = "s^s"
    }

    class Add extends BitVector.Add{
      override def opName: String = "s+s"
    }

    class Sub extends BitVector.Sub{
      override def opName: String = "s-s"
      override def getLiteralFactory: (BigInt, BitCount) => Node = U.apply
    }

    class Mul extends BitVector.Mul{
      override def opName: String = "s*s"
      override def getLiteralFactory: (BigInt, BitCount) => Node = U.apply
    }

    class Div extends BitVector.Div{
      override def opName: String = "s/s"
    }

    class Mod extends BitVector.Mod{
      override def opName: String = "s%s"
    }

    class Smaller extends BinaryOperator{
      override def opName: String = "s<s"
      override def calcWidth(): Int = 1
      override def normalizeInputs: Unit = {InputNormalize.inputWidthMax(this)}
      override def simplifyNode: Unit = {SymplifyNode.binarySIntSmaller(this)}
    }

    class SmallerOrEqual extends BinaryOperator{
      override def opName: String = "s<=s"
      override def calcWidth(): Int = 1
      override def normalizeInputs: Unit = {InputNormalize.inputWidthMax(this)}
      override def simplifyNode: Unit = {SymplifyNode.binarySIntSmallerOrEgual(this)}
    }

    class Equal extends BitVector.Equal{
      override def opName: String = "s==s"
    }

    class NotEqual extends BitVector.NotEqual{
      override def opName: String = "s!=s"
    }

    class ShiftRightByInt(shift : Int) extends BitVector.ShiftRightByInt(shift){
      override def opName: String = "s>>i"
    }

    class ShiftRightByUInt extends BitVector.ShiftRightByUInt{
      override def opName: String = "s>>u"
    }

    class ShiftLeftByInt(shift : Int) extends BitVector.ShiftLeftByInt(shift){
      override def opName: String = "s<<i"
      override def getLiteralFactory: (BigInt, BitCount) => Node = S.apply
    }

    class ShiftLeftByUInt extends BitVector.ShiftLeftByUInt{
      override def opName: String = "s<<u"
      override def getLiteralFactory: (BigInt, BitCount) => Node = S.apply
    }
  }
}

/*
def >>(that: Int): Bits = wrapBinaryOperator("b>>i", IntLiteral(that), WidthInfer.shiftRightWidth, InputNormalize.none, SymplifyNode.shiftRightImpl)
def <<(that: Int): Bits = wrapBinaryOperator("b<<i", IntLiteral(that), WidthInfer.shiftLeftWidth, InputNormalize.none, SymplifyNode.shiftLeftImpl(B.apply))
def >>(that: UInt): Bits = wrapBinaryOperator("b>>u", that, WidthInfer.shiftRightWidth, InputNormalize.none, SymplifyNode.shiftRightImpl)
def <<(that: UInt): Bits = wrapBinaryOperator("b<<u", that, WidthInfer.shiftLeftWidth, InputNormalize.none, SymplifyNode.shiftLeftImpl(B.apply))
def rotateLeft(that: UInt): Bits = wrapBinaryOperator("brotlu", that, WidthInfer.input0Width, InputNormalize.none, SymplifyNode.rotateImpl(B.apply))


  override def >>(that: Int): SInt = wrapBinaryOperator("s>>i", IntLiteral(that), WidthInfer.shiftRightWidth,InputNormalize.none,SymplifyNode.shiftRightImpl);
  override def <<(that: Int): SInt = wrapBinaryOperator("s<<i", IntLiteral(that), WidthInfer.shiftLeftWidth,InputNormalize.none,SymplifyNode.shiftLeftImpl(S.apply));
  def >>(that: UInt): SInt = wrapBinaryOperator("s>>u", that, WidthInfer.shiftRightWidth,InputNormalize.none,SymplifyNode.shiftRightImpl);
  def <<(that: UInt): SInt = wrapBinaryOperator("s<<u", that, WidthInfer.shiftLeftWidth,InputNormalize.none,SymplifyNode.shiftLeftImpl(S.apply));

  override def >>(that: Int): UInt = wrapBinaryOperator("u>>i", IntLiteral(that), WidthInfer.shiftRightWidth, InputNormalize.none,SymplifyNode.shiftRightImpl);
  override def <<(that: Int): UInt = wrapBinaryOperator("u<<i", IntLiteral(that), WidthInfer.shiftLeftWidth, InputNormalize.none,SymplifyNode.shiftLeftImpl(U.apply));
  def >>(that: UInt): UInt = wrapBinaryOperator("u>>u", that, WidthInfer.shiftRightWidth, InputNormalize.none,SymplifyNode.shiftRightImpl);
  def <<(that: UInt): UInt = wrapBinaryOperator("u<<u", that, WidthInfer.shiftLeftWidth, InputNormalize.none,SymplifyNode.shiftLeftImpl(U.apply));





def ##(right: Bits): Bits = newBinaryOperator("b##b", right, WidthInfer.cumulateInputWidth, InputNormalize.none, SymplifyNode.binaryTakeOther)
def |(that: Bits): Bits = newBinaryOperator("b|b", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth, SymplifyNode.binaryTakeOther)
def &(that: Bits): Bits = newBinaryOperator("b&b", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth, SymplifyNode.binaryInductZeroWithOtherWidth(B.apply))
def ^(that: Bits): Bits = newBinaryOperator("b^b", that, WidthInfer.inputMaxWidth, InputNormalize.nodeWidth, SymplifyNode.binaryTakeOther)
def unary_~(): Bits = newUnaryOperator(new OperatorBitsNot)
def >>(that: Int): Bits = newBinaryOperator("b>>i", IntLiteral(that), WidthInfer.shiftRightWidth, InputNormalize.none, SymplifyNode.shiftRightImpl)
def <<(that: Int): Bits = newBinaryOperator("b<<i", IntLiteral(that), WidthInfer.shiftLeftWidth, InputNormalize.none, SymplifyNode.shiftLeftImpl(B.apply))
def >>(that: UInt): Bits = newBinaryOperator("b>>u", that, WidthInfer.shiftRightWidth, InputNormalize.none, SymplifyNode.shiftRightImpl)
def <<(that: UInt): Bits = newBinaryOperator("b<<u", that, WidthInfer.shiftLeftWidth, InputNormalize.none, SymplifyNode.shiftLeftImpl(B.apply))
def rotateLeft(that: UInt): Bits = newBinaryOperator("brotlu", that, WidthInfer.input0Width, InputNormalize.none, SymplifyNode.rotateImpl(B.apply))

*/



//TODO remove me
abstract class ModifierImpl(opName: String, widthImpl: (Node) => Int) extends Modifier(opName,widthImpl) {
  val inputs = new ArrayBuffer[Node](3)

  override def getInputsCount = inputs.length
  override def getInput(id : Int) : Node = inputs(id)
  override def setInput(id : Int,node : Node) : Unit = inputs(id) = node

  override def getInputs : Iterator[Node] = inputs.iterator

  override def onEachInput(doThat : (Node,Int) => Unit) : Unit = {
    var idx = getInputsCount
    while(idx != 0){
      idx -= 1
      doThat(getInput(idx),idx)
    }
  }

  override def onEachInput(doThat : (Node) => Unit) : Unit = {
    var idx = getInputsCount
    while(idx != 0){
      idx -= 1
      doThat(getInput(idx))
    }
  }
}


abstract class Modifier(opName_ : String, widthImpl: (Node) => Int) extends Node {
  override def calcWidth(): Int = {
    widthImpl(this)
  }

  def opName = opName_

  override def toString(): String = {
    s"($opName ${this.getInputs.map(in => if (in == null) "null" else in.nonRecursiveToString()).reduceLeft(_ + " " + _)})"
  }

  override def nonRecursiveToString(): String = opName
}

abstract class Function(opName: String, widthImpl: (Node) => Int,simplifyNodeImpl : (Node) => Unit) extends Modifier(opName, widthImpl) {
  override def simplifyNode: Unit = {
    simplifyNodeImpl(this)
  }
}

abstract class Cast extends Modifier(null, null) {
  var input : Node = null
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node}
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {assert(id == 0); input}

  override def calcWidth(): Int = input.getWidth
}

class CastSIntToBits extends Cast{
  override def opName: String = "s->b"
}
class CastUIntToBits extends Cast{
  override def opName: String = "u->b"
}
class CastBitsToUInt extends Cast{
  override def opName: String = "b->u"
}
class CastSIntToUInt extends Cast{
  override def opName: String = "s->u"
}
class CastBitsToSInt extends Cast{
  override def opName: String = "b->s"
}
class CastUIntToSInt extends Cast{
  override def opName: String = "u->s"
}
class CastBoolToBits extends Cast{
  override def opName: String = "B->b"
}
class CastEnumToBits extends Cast{
  override def opName: String = "e->b"
}
class CastBitsToEnum(val enum: SpinalEnumCraft[_]) extends Cast {
  override def opName: String = "b->e"
}
class CastEnumToEnum(val enum: SpinalEnumCraft[_]) extends Cast {
  override def opName: String = "e->e"
}



class Multiplexer(opName: String) extends Modifier(opName, WidthInfer.multiplexImpl) {
  var cond      : Node = null
  var whenTrue  : Node = null
  var whenFalse : Node = null

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    doThat(cond,0)
    doThat(whenTrue,1)
    doThat(whenFalse,2)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    doThat(cond)
    doThat(whenTrue)
    doThat(whenFalse)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 0 => cond = node
    case 1 => whenTrue = node
    case 2 => whenFalse = node
  }

  override def getInputsCount: Int = 3
  override def getInputs: Iterator[Node] = Iterator(cond,whenTrue,whenFalse)
  override def getInput(id: Int): Node = id match{
    case 0 => cond
    case 1 => whenTrue
    case 2 => whenFalse
  }

  override def calcWidth: Int = Math.max(whenTrue.getWidth, whenFalse.getWidth)




  override def normalizeInputs: Unit = {
    Misc.normalizeResize(this, 1, this.getWidth)
    Misc.normalizeResize(this, 2, this.getWidth)
  }

  override def simplifyNode: Unit = SymplifyNode.multiplexerImpl(this)
}

object Mux {
  def apply[T <: Data](sel: Bool, whenTrue: T, whenFalse: T): T = {
    Multiplex.complexData(sel, whenTrue, whenFalse)
  }
  def apply[T <: SpinalEnum](sel: Bool, whenTrue: SpinalEnumElement[T], whenFalse: SpinalEnumElement[T]): SpinalEnumCraft[T] = {
    Multiplex.complexData(sel, whenTrue(), whenFalse())
  }
  def apply[T <: SpinalEnum](sel: Bool, whenTrue: SpinalEnumCraft[T], whenFalse: SpinalEnumElement[T]): SpinalEnumCraft[T] = {
    Multiplex.complexData(sel, whenTrue, whenFalse())
  }
  def apply[T <: SpinalEnum](sel: Bool, whenTrue: SpinalEnumElement[T], whenFalse: SpinalEnumCraft[T]): SpinalEnumCraft[T] = {
    Multiplex.complexData(sel, whenTrue(), whenFalse)
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
  def apply[K <: Data, T <: Data](addr: K, mappings: (Any, T)*): T = list(addr,mappings)

  def list[K <: Data, T <: Data](addr: K, defaultValue: T, mappings: Seq[(Any, T)]): T = {
    val result : T = defaultValue.clone

    switch(addr){
      for ((cond, value) <- mappings) {
        cond match {
          case product : Product => {
            //  for(cond <- product.productIterator){
            is.list(product.productIterator) {
              result := value
            }
            //   }
          }
          case _ => {
            is(cond) {
              result := value
            }
          }
        }
      }
      default{
        result := defaultValue
      }
    }
    result
  }

  def list[K <: Data, T <: Data](addr: K, mappings: Seq[(Any, T)]): T = {
    val defaultValue = mappings.find(_._1 == default)
    if(!defaultValue.isDefined) new Exception("No default element in SpinalMap (default -> xxx)")
    list(addr,defaultValue.get._2,mappings.filter(_._1 != default))
  }
}


private[spinal] object Multiplex {
  def apply(opName: String, sel: Bool, one: Node, zero: Node): Multiplexer = {

    val op = new Multiplexer(opName)
    op.cond = sel
    op.whenTrue = one
    op.whenFalse = zero
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

      out.setInputWrap(0) = Multiplex.baseType(sel, t, f)
    }
    muxOut
  }
}
abstract class Extract extends Modifier(null, null){
  def getBitVector: Node
  def getParameterNodes: List[Node]
  def getInputData: Node
}

abstract class ExtractBoolFixed extends Extract{
  var input : Node = null
  var bitId : Int = -1
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node}
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {assert(id == 0); input}

  def getBitVector = getInput(0)
  def getBitId = bitId

  override def calcWidth: Int = 1

  override def checkInferedWidth: String = {
    if (bitId < 0 || bitId >= getBitVector.getWidth) {
      return s"Static bool extraction (bit ${bitId}) is outside the range (${getBitVector.getWidth - 1} downto 0) of ${getBitVector} at\n${getScalaLocationLong}"
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

class ExtractBoolFixedFromBits extends ExtractBoolFixed{
  override def opName: String = "extract(b,i)"
}
class ExtractBoolFixedFromUInt extends ExtractBoolFixed{
  override def opName: String = "extract(u,i)"
}
class ExtractBoolFixedFromSInt extends ExtractBoolFixed{
  override def opName: String = "extract(s,i)"
}

class ExtractBoolFloating extends Extract {
  var input  : Node = null
  var bitId  : Node = null

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    doThat(input,0)
    doThat(bitId,1)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    doThat(input)
    doThat(bitId)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 0 => input = node
    case 1 => bitId = node
  }

  override def getInputsCount: Int = 2
  override def getInputs: Iterator[Node] = Iterator(input,bitId)
  override def getInput(id: Int): Node = id match{
    case 0 => input
    case 1 => bitId
  }

  def getBitVector = getInput(0)
  def getBitId = getInput(1)

  override def calcWidth: Int = 1

  def getParameterNodes: List[Node] = getInput(1) :: Nil
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

class ExtractBoolFloatingFromBits extends ExtractBoolFloating{
  override def opName: String = "extract(b,u)"
}
class ExtractBoolFloatingFromUInt extends ExtractBoolFloating{
  override def opName: String = "extract(u,u)"
}
class ExtractBoolFloatingFromSInt extends ExtractBoolFloating{
  override def opName: String = "extract(s,u)"
}


class ExtractBitsVectorFixed extends Extract {
  def checkHiLo : Unit = if (hi - lo < -1)
    SpinalError(s"Static bits extraction with a negative size ($hi downto $lo)")

  var hi,lo : Int = -1
  var input : Node = null
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node}
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {assert(id == 0); input}



  def getBitVector = getInput(0)
  def getHi = hi
  def getLo = lo

  override def calcWidth: Int = hi - lo + 1

  override def checkInferedWidth: String = {
    val width = getBitVector.getWidth
    if (hi >= width || lo < 0) {
      return s"Static bits extraction ($hi downto $lo) is outside the range (${width - 1} downto 0) of ${getBitVector} at\n${getScalaLocationLong}"
    }
    return null
  }

  def getParameterNodes: List[Node] =  Nil
  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
    case 0 => (lo+outHi, lo+outLo)
  }
  def getInputData: Node = getBitVector
}

class ExtractBitsVectorFixedFromBits extends ExtractBitsVectorFixed{
  override def opName: String = "extract(b,i,i)"
}
class ExtractBitsVectorFixedFromUInt extends ExtractBitsVectorFixed{
  override def opName: String = "extract(u,i,i)"
}
class ExtractBitsVectorFixedFromSInt extends ExtractBitsVectorFixed{
  override def opName: String = "extract(s,i,i)"
}

//WHen used offset.dontSimplifyIt() Because it can appear at multipe location (o+bc-1 downto o)
class ExtractBitsVectorFloating extends Extract {
  var size    : Int = -1
  var input   : Node = null
  var offset  : UInt = null

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    doThat(input,0)
    doThat(offset,1)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    doThat(input)
    doThat(offset)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 0 => input = node
    case 1 => offset = node.asInstanceOf[UInt]
  }

  override def getInputsCount: Int = 2
  override def getInputs: Iterator[Node] = Iterator(input,offset)
  override def getInput(id: Int): Node = id match{
    case 0 => input
    case 1 => offset
  }

  override def calcWidth: Int = size

  def getBitVector = getInput(0)
  def getOffset = getInput(1)
  def getBitCount = size

  def getParameterNodes: List[Node] = getInput(1) :: Nil
  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
    case 0 =>
      if(outHi >= outLo) //Not exact
        (Math.min(getBitVector.getWidth-1,(1 << Math.min(20,offset.getWidth))+ size - 1), 0)
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

class ExtractBitsVectorFloatingFromBits extends ExtractBitsVectorFloating{
  override def opName: String = "extract(b,u,w)"
}
class ExtractBitsVectorFloatingFromUInt extends ExtractBitsVectorFloating{
  override def opName: String = "extract(u,u,w)"
}
class ExtractBitsVectorFloatingFromSInt extends ExtractBitsVectorFloating{
  override def opName: String = "extract(s,u,w)"
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

abstract class AssignementNode extends Node {
  def getAssignedBits: AssignedRange //Bit that are allwas assigned
  def getScopeBits: AssignedRange //Bit tht could be assigned
  def getOutBaseType: BaseType

  def clone(out : Node) : this.type
}


class BitAssignmentFixed(out: BitVector, in: Node, bitId: Int) extends AssignementNode {
  var input : Node = in

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)

  override def setInput(id: Int, node: Node): Unit = {
    assert(id == 0)
    this.input = node
  }
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {
    assert(id == 0)
    input
  }


  def getInput : Node = getInput(0)
  def getBitId = bitId
  override def calcWidth: Int = bitId + 1

  override def checkInferedWidth: String = {
    if (bitId < 0 || bitId >= out.getWidth) {
      return s"Static bool extraction (bit ${bitId}) is outside the range (${out.getWidth - 1} downto 0) of ${out} at\n${getScalaLocationLong}"
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
  var input : Node = in

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)

  override def setInput(id: Int, node: Node): Unit = {
    assert(id == 0)
    this.input = node
  }
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {
    assert(id == 0)
    input
  }

  def getInput : Node = getInput(0)
  def getHi = hi
  def getLo = lo

  override def calcWidth: Int = hi + 1

  override def checkInferedWidth: String = {
    val input = getInput
    if (input.component != null && hi + 1 - lo != input.getWidth) {
      return s"Assignment bit count mismatch. ${this} := ${input}} at\n${getScalaLocationLong}"
    }

    val width = out.getWidth
    if (hi >= width || lo < 0) {
      return s"Static bits assignment ($hi downto $lo) is outside the range (${width - 1} downto 0) of ${out} at\n${getScalaLocationLong}"
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

  override def toString(): String = s"${out.toString()}[$hi downto $lo]"
}


class BitAssignmentFloating(out: BitVector, in_ : Node, bitId_ : Node) extends AssignementNode {
  var input  : Node = in_
  var bitId  : Node = bitId_

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    doThat(input,0)
    doThat(bitId,1)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    doThat(input)
    doThat(bitId)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 0 => input = node
    case 1 => bitId = node
  }

  override def getInputsCount: Int = 2
  override def getInputs: Iterator[Node] = Iterator(input,bitId)
  override def getInput(id: Int): Node = id match{
    case 0 => input
    case 1 => bitId
  }


  def getInput  : Node = getInput(0)
  def getBitId  : Node = getInput(1)

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
  override def clone(out: Node): this.type = new BitAssignmentFloating(out.asInstanceOf[BitVector],in_,bitId_).asInstanceOf[this.type]
}

class RangedAssignmentFloating(out: BitVector, in_ : Node, offset_ : Node, bitCount: BitCount) extends AssignementNode {
  var input  : Node = in_
  var offset  : Node = offset_

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    doThat(input,0)
    doThat(offset,1)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    doThat(input)
    doThat(offset)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 0 => input = node
    case 1 => offset = node
  }

  override def getInputsCount: Int = 2
  override def getInputs: Iterator[Node] = Iterator(input,offset)
  override def getInput(id: Int): Node = id match{
    case 0 => input
    case 1 => offset
  }


  def getInput : Node = getInput(0)
  def getOffset = getInput(1)
  def getBitCount = bitCount

  override def calcWidth: Int = 1 << Math.min(20,offset_.getWidth) + bitCount.value



  override def checkInferedWidth: String = {
    val input = getInput
    if (input.component != null && bitCount.value != input.getWidth) {
      return s"Assignement bit count missmatch. ${this} := ${input}} at\n${getScalaLocationLong}"
    }

    return null
  }

  override def normalizeInputs: Unit = {
    InputNormalize.bitVectoreAssignement(this,0,bitCount.value)
  }



  def getAssignedBits: AssignedRange = AssignedRange()
  def getScopeBits: AssignedRange = AssignedRange(Math.min(out.getWidth-1,(1 << Math.min(20,offset_.getWidth))+ bitCount.value - 1), 0) //TODO dirty offset_
  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = super.getOutToInUsage(inputId,outHi,outLo) //TODO
  def getOutBaseType: BaseType = out
  override def clone(out: Node): this.type = new RangedAssignmentFloating(out.asInstanceOf[BitVector],in_,offset_,bitCount).asInstanceOf[this.type]
}


class MultipleAssignmentNode extends NodeWithVariableInputsCount with AssignementTreePart{
  override def calcWidth: Int = WidthInfer.multipleAssignmentNodeWidth(this)
  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = (outHi,outLo)

  override def normalizeInputs: Unit = {
    for (i <- 0 until getInputsCount)
      InputNormalize.bitVectoreAssignement(this,i,this.getWidth)
  }

  override private[core] def checkInferedWidth: String = {
    for (i <- 0 until getInputsCount){
      val input = this.getInput(i)
      if (input != null && input.component != null && this.getWidth !=input.getWidth) {
        return s"Assignement bit count missmatch. ${this} := ${input}} at\n${ScalaLocated.long(getAssignementContext(i))}"
      }
    }
    return null
  }


  var inputsThrowable : Array[Throwable] = null
  override def getAssignementContext(id: Int): Throwable =
    ArrayManager.getElseNull(inputsThrowable,id)
  override def setAssignementContext(id: Int,that : Throwable = globalData.getThrowable()): Unit =
    inputsThrowable = ArrayManager.setAllocate(inputsThrowable,id,that)


}


