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







abstract class Resize extends Modifier with Widthable{
  var size : Int = -1
  var input : Node with WidthProvider = null
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node.asInstanceOf[Node with WidthProvider]}
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




abstract class Operator extends Modifier

abstract class UnaryOperator extends Operator{
  type T <: Node
  var input : T = null.asInstanceOf[T]
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node.asInstanceOf[T]}
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {assert(id == 0); input}
}

abstract class UnaryOperatorWidthableInputs extends UnaryOperator{
  override type T = Node with WidthProvider
}


abstract class ConstantOperator extends Operator{
  type T <: Node
  var input : T = null.asInstanceOf[T]
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node.asInstanceOf[T]}
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {assert(id == 0); input}
}



abstract class ConstantOperatorWidthableInputs extends ConstantOperator{
  override type T = Node with WidthProvider
}



abstract class BinaryOperator extends Operator{
  type T <: Node
  var left,right  : T = null.asInstanceOf[T]

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    doThat(left,0)
    doThat(right,1)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    doThat(left)
    doThat(right)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 0 => left = node.asInstanceOf[T]
    case 1 => right = node.asInstanceOf[T]
  }

  override def getInputsCount: Int = 2
  override def getInputs: Iterator[Node] = Iterator(left,right)
  override def getInput(id: Int): Node = id match{
    case 0 => left
    case 1 => right
  }
}


abstract class BinaryOperatorWidthableInputs extends BinaryOperator{
  override type T = Node with WidthProvider
}



object Operator{
  object Bool{
    class And extends BinaryOperator{
      override def opName: String = "&&"
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {}
    }

    class Or extends BinaryOperator{
      override def opName: String = "||"
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {}
    }

    class Xor extends BinaryOperator{
      override def opName: String = "B^B"
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {}
    }

    class Not extends UnaryOperator{
      override def opName: String = "!"
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {}
    }

    class Equal extends BinaryOperator{
      override def opName: String = "B==B"
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {}
    }

    class NotEqual extends BinaryOperator{
      override def opName: String = "B!=B"
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {}
    }
  }

  object BitVector{
    abstract class And extends BinaryOperatorWidthableInputs with Widthable with CheckWidth{
      override def calcWidth(): Int = Math.max(left.getWidth,right.getWidth)
      override def normalizeInputs: Unit = InputNormalize.resizedOrUnfixedLit(this)
      override private[core] def checkInferedWidth: Unit = CheckWidth.allSame(this)
      override def simplifyNode: Unit = SymplifyNode.binaryInductZeroWithOtherWidth(getLiteralFactory,true)(this)
      def getLiteralFactory : (BigInt, BitCount) => Node
    }

    abstract class Or extends BinaryOperatorWidthableInputs with Widthable with CheckWidth{
      override def calcWidth(): Int = Math.max(left.getWidth,right.getWidth)
      override def normalizeInputs: Unit = InputNormalize.resizedOrUnfixedLit(this)
      override private[core] def checkInferedWidth: Unit = CheckWidth.allSame(this)
      override def simplifyNode: Unit = {SymplifyNode.binaryTakeOther(this,true)}
    }

    abstract class Xor extends BinaryOperatorWidthableInputs with Widthable with CheckWidth{
      override def calcWidth(): Int = Math.max(left.getWidth,right.getWidth)
      override def normalizeInputs: Unit = InputNormalize.resizedOrUnfixedLit(this)
      override private[core] def checkInferedWidth: Unit = CheckWidth.allSame(this)
      override def simplifyNode: Unit = {SymplifyNode.binaryTakeOther(this,true)}
    }

    abstract class Add extends BinaryOperatorWidthableInputs with Widthable{
      override def calcWidth(): Int = Math.max(left.getWidth,right.getWidth)
      override def normalizeInputs: Unit = {InputNormalize.nodeWidth(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryTakeOther(this)}
    }

    abstract class Sub extends BinaryOperatorWidthableInputs with Widthable{
      override def calcWidth(): Int = Math.max(left.getWidth,right.getWidth)
      override def normalizeInputs: Unit = {InputNormalize.nodeWidth(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryMinus(getLiteralFactory)(this)}
      def getLiteralFactory : (BigInt, BitCount) => Node
    }

    abstract class Mul extends BinaryOperatorWidthableInputs with Widthable{
      override def calcWidth(): Int = left.getWidth + right.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.binaryInductZeroWithOtherWidth(getLiteralFactory)(this)}
      def getLiteralFactory : (BigInt, BitCount) => Node
    }

    abstract class Div extends BinaryOperatorWidthableInputs with Widthable{
      override def calcWidth(): Int = left.getWidth
      override def normalizeInputs: Unit = {}
    }

    abstract class Mod extends BinaryOperatorWidthableInputs with Widthable{
      override def calcWidth(): Int = left.getWidth
      override def normalizeInputs: Unit = {}
    }

    abstract class Equal extends BinaryOperatorWidthableInputs{
      override def normalizeInputs: Unit = {InputNormalize.inputWidthMax(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryThatIfBoth(True)(this)}
    }

    abstract class NotEqual extends BinaryOperatorWidthableInputs{
      override def normalizeInputs: Unit = {InputNormalize.inputWidthMax(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryThatIfBoth(False)(this)}
    }

    abstract class ShiftRightByInt(val shift : Int) extends ConstantOperatorWidthableInputs with Widthable{
      assert(shift >= 0)
      override def calcWidth(): Int = Math.max(0, input.getWidth - shift)
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.shiftRightImpl(this)}
    }

    abstract class ShiftRightByUInt extends BinaryOperatorWidthableInputs with Widthable{
      override def calcWidth(): Int = left.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.shiftRightImpl(this)}
    }

    abstract class ShiftLeftByInt(val shift : Int) extends ConstantOperatorWidthableInputs with Widthable{
      assert(shift >= 0)
      override def calcWidth(): Int = input.getWidth + shift
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.shiftLeftImpl(getLiteralFactory,this)}
      def getLiteralFactory : (BigInt, BitCount) => Node
    }

    abstract class ShiftLeftByUInt extends BinaryOperatorWidthableInputs with Widthable{
      override def calcWidth(): Int = left.getWidth + (1 << right.getWidth) - 1
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.shiftLeftImpl(getLiteralFactory,this)}
      def getLiteralFactory : (BigInt, BitCount) => Node
    }


    abstract class RotateLeftByUInt extends BinaryOperatorWidthableInputs with Widthable{
      override def calcWidth(): Int = left.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.rotateImpl(getLiteralFactory,this)}
      def getLiteralFactory : (BigInt, BitCount) => Node
    }

    abstract class AllByBool(val theConsumer : Node) extends UnaryOperator with Widthable {
      override def calcWidth: Int = theConsumer.asInstanceOf[WidthProvider].getWidth
    }

  }

  object Bits{
    class Cat extends BinaryOperatorWidthableInputs with Widthable{
      override def opName: String = "b##b"
      override def calcWidth(): Int = left.getWidth + right.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.binaryTakeOther(this)}
    }

    class Not extends UnaryOperatorWidthableInputs with Widthable{
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

    class Equal extends BitVector.Equal with CheckWidth{
      override def normalizeInputs: Unit = {
        if(this.left.getWidth < this.right.getWidth)
          InputNormalize.resizedOrUnfixedLit(this, 0, this.right.getWidth)
        if(this.left.getWidth > this.right.getWidth)
          InputNormalize.resizedOrUnfixedLit(this, 1, this.left.getWidth)
      }
      override private[core] def checkInferedWidth: Unit = {
        if(this.left.getWidth != this.right.getWidth)
          PendingError(s"${this} inputs doesn't have the same width\n${this.getScalaLocationLong}")
      }
      override def opName: String = "b==b"
    }

    class NotEqual extends BitVector.NotEqual with CheckWidth{
      override def normalizeInputs: Unit = {
        if(this.left.getWidth < this.right.getWidth)
          InputNormalize.resizedOrUnfixedLit(this, 0, this.right.getWidth)
        if(this.left.getWidth > this.right.getWidth)
          InputNormalize.resizedOrUnfixedLit(this, 1, this.left.getWidth)
      }
      override private[core] def checkInferedWidth: Unit = {
        if(this.left.getWidth != this.right.getWidth)
          PendingError(s"${this} inputs doesn't have the same width\n${this.getScalaLocationLong}")
      }
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

    class AllByBool(theConsumer : Node) extends BitVector.AllByBool(theConsumer) {
      override def opName: String = "bAllByB"
    }
  }


  object UInt{
    class Not extends UnaryOperatorWidthableInputs  with Widthable{
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

    class Smaller extends BinaryOperator with Widthable{
      override def opName: String = "u<u"
      override def calcWidth(): Int = 1
      override def normalizeInputs: Unit = {InputNormalize.inputWidthMax(this)}
      override def simplifyNode: Unit = {SymplifyNode.binaryUIntSmaller(this)}
    }

    class SmallerOrEqual extends BinaryOperator with Widthable{
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

    class AllByBool(theConsumer : Node) extends BitVector.AllByBool(theConsumer) {
      override def opName: String = "uAllByB"
    }
  }

  object SInt{
    class Not extends UnaryOperatorWidthableInputs with Widthable{
      override def opName: String = "~s"
      override def calcWidth(): Int = input.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Unit = {SymplifyNode.unaryZero(this)}
    }

    class Minus extends UnaryOperatorWidthableInputs with Widthable{
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

    class Smaller extends BinaryOperator with Widthable{
      override def opName: String = "s<s"
      override def calcWidth(): Int = 1
      override def normalizeInputs: Unit = {InputNormalize.inputWidthMax(this)}
      override def simplifyNode: Unit = {SymplifyNode.binarySIntSmaller(this)}
    }

    class SmallerOrEqual extends BinaryOperator with Widthable{
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

    class AllByBool(theConsumer : Node) extends BitVector.AllByBool(theConsumer) {
      override def opName: String = "sAllByB"
    }
  }

  object Enum{
    class Equal(enumDef : SpinalEnum) extends BinaryOperator with InferableEnumEncodingImpl{
      override def opName: String = "e==e"
      override def normalizeInputs: Unit = {InputNormalize.enumImpl(this)}
      override def simplifyNode: Unit = {}

      override type T = Node with EnumEncoded
      override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
      override def getDefinition: SpinalEnum = enumDef
    }

    class NotEqual(enumDef : SpinalEnum) extends BinaryOperator with InferableEnumEncodingImpl{
      override def opName: String = "e!=e"
      override def normalizeInputs: Unit = {InputNormalize.enumImpl(this)}
      override def simplifyNode: Unit = {}

      override type T = Node with EnumEncoded
      override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
      override def getDefinition: SpinalEnum = enumDef
    }

  }
}



abstract class Modifier extends Node {
  def opName : String

  override def toString(): String = {
    s"($opName ${this.getInputs.map(in => if (in == null) "null" else in.nonRecursiveToString()).reduceLeft(_ + " " + _)})"
  }

  override def nonRecursiveToString(): String = opName

  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)
}


abstract class Cast extends Modifier {
  type T <: Node
  var input : T = null.asInstanceOf[T]
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node.asInstanceOf[T]}
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {assert(id == 0); input}
}

abstract class CastBitVectorToBitVector extends Cast with Widthable{
  override type T <: Node with WidthProvider
  override private[core] def calcWidth: Int = input.getWidth
}

class CastSIntToBits extends CastBitVectorToBitVector{
  override def opName: String = "s->b"
}
class CastUIntToBits extends CastBitVectorToBitVector{
  override def opName: String = "u->b"
}
class CastBitsToUInt extends CastBitVectorToBitVector{
  override def opName: String = "b->u"
}
class CastSIntToUInt extends CastBitVectorToBitVector{
  override def opName: String = "s->u"
}
class CastBitsToSInt extends CastBitVectorToBitVector{
  override def opName: String = "b->s"
}
class CastUIntToSInt extends CastBitVectorToBitVector{
  override def opName: String = "u->s"
}
class CastBoolToBits extends Cast with Widthable{
  override def opName: String = "B->b"
  override private[core] def calcWidth: Int = 1
}

class CastEnumToBits extends Cast with Widthable{
  override type T <: Node with EnumEncoded
  override def opName: String = "e->b"
  override private[core] def calcWidth: Int = input.getEncoding.getWidth(input.getDefinition)
}
class CastBitsToEnum(val enumDef: SpinalEnum) extends Cast with InferableEnumEncodingImpl with CheckWidth{
  override type T <: Node with WidthProvider
  override def opName: String = "b->e"
  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
  override def getDefinition: SpinalEnum = enumDef
  override private[core] def checkInferedWidth: Unit = if(input.getWidth !=  getEncoding.getWidth(enumDef))
    PendingError(s"$input has ${input.getWidth} bits in place of ${getEncoding.getWidth(enumDef)} bits at ${input.getScalaLocationLong}")
}
class CastEnumToEnum(enumDef: SpinalEnum) extends Cast with  InferableEnumEncodingImpl{
  override type T <: Node with EnumEncoded
  override def opName: String = "e->e"

  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
  override def getDefinition: SpinalEnum = enumDef
}



abstract class Multiplexer extends Modifier {
  type T <: Node
  var cond      : Node = null
  var whenTrue  : T = null.asInstanceOf[T]
  var whenFalse : T = null.asInstanceOf[T]

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
    case 1 => whenTrue = node.asInstanceOf[T]
    case 2 => whenFalse = node.asInstanceOf[T]
  }

  override def getInputsCount: Int = 3
  override def getInputs: Iterator[Node] = Iterator(cond,whenTrue,whenFalse)
  override def getInput(id: Int): Node = id match{
    case 0 => cond
    case 1 => whenTrue
    case 2 => whenFalse
  }
}

abstract class MultiplexedWidthable extends Multiplexer with Widthable with CheckWidth{
  override type T = Node with Widthable
  override def calcWidth: Int = Math.max(whenTrue.getWidth, whenFalse.getWidth)
  override private[core] def checkInferedWidth: Unit = {
    if(whenTrue.getWidth != whenFalse.getWidth){
      PendingError(s"${this} inputs doesn't have the same width at \n${this.getScalaLocationLong}")
    }
  }
}

class MultiplexerBool extends Multiplexer{
  override def opName: String = "mux(B,B,B)"
}
class MultiplexerBits extends MultiplexedWidthable{
  override def opName: String = "mux(B,b,b)"
  override def normalizeInputs: Unit = {
    InputNormalize.resizedOrUnfixedLitDeepOne(this, 1, this.getWidth)
    InputNormalize.resizedOrUnfixedLitDeepOne(this, 2, this.getWidth)
  }
}
class MultiplexerUInt extends MultiplexedWidthable{
  override def opName: String = "mux(B,u,u)"
  override def normalizeInputs: Unit = {
    Misc.normalizeResize(this, 1, this.getWidth)
    Misc.normalizeResize(this, 2, this.getWidth)
  }
}
class MultiplexerSInt extends MultiplexedWidthable{
  override def opName: String = "mux(B,s,s)"
  override def normalizeInputs: Unit = {
    Misc.normalizeResize(this, 1, this.getWidth)
    Misc.normalizeResize(this, 2, this.getWidth)
  }
}
class MultiplexerEnum(enumDef : SpinalEnum) extends Multiplexer with InferableEnumEncodingImpl{
  override type T = Node with EnumEncoded
  override def opName: String = "mux(B,e,e)"
  override def getDefinition: SpinalEnum = enumDef
  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
  override private[core] def normalizeInputs: Unit = {
    InputNormalize.enumImpl(this)
  }
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
    val result = cloneOf(default)
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
    val result : T = cloneOf(defaultValue)

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

//TODO DOC
object Select{
  def apply[T <: Data](default: T, mappings: (Bool, T)*): T = list(default,mappings)
  def apply[T <: Data](mappings: (Any, T)*): T = list(mappings)

  def list[ T <: Data]( defaultValue: T, mappings: Seq[(Bool, T)]): T = {
    val result : T = cloneOf(defaultValue)

    var ptr : WhenContext = null

    mappings.foreach{case (cond,that) => {
      if(ptr == null){
        ptr = when(cond){
          result := that
        }
      }else{
        ptr = ptr.elsewhen(cond){
          result := that
        }
      }
    }}

    if(ptr == null){
      result := defaultValue
    }else{
      ptr.otherwise{
        result := defaultValue
      }
    }
    result
  }

  def list[T <: Data](mappings: Seq[(Any, T)]): T = {
    val defaultValue = mappings.find(_._1 == default)
    if(!defaultValue.isDefined) new Exception("No default element in SpinalMap (default -> xxx)")
    val filterd = mappings.filter(_._1 != default).map(t => (t._1.asInstanceOf[Bool] -> t._2))
    list(defaultValue.get._2,filterd)
  }
}

private[spinal] object Multiplex {

  def baseType[T <: BaseType](sel: Bool, whenTrue: T, whenFalse: T): Multiplexer = {
    whenTrue.newMultiplexer(sel, whenTrue, whenFalse)
  }


  def complexData[T <: Data](sel: Bool, whenTrue: T, whenFalse: T): T = {
    val outType = if (whenTrue.getClass.isAssignableFrom(whenFalse.getClass)) whenTrue
    else if (whenFalse.getClass.isAssignableFrom(whenTrue.getClass)) whenFalse
    else throw new Exception("can't mux that")


    val muxOut = cloneOf(outType)
    muxOut.flatten.foreach(_ match{
      case bv : BitVector => bv.fixedWidth = -1
      case _ =>
    })
    val muxInTrue = cloneOf(muxOut)
    val muxInFalse = cloneOf(muxOut)


    muxInTrue := whenTrue
    muxInFalse := whenFalse


    for ((out, t,  f) <- (muxOut.flatten, muxInTrue.flatten, muxInFalse.flatten).zipped) {
      if (t == null) SpinalError("Create a mux with incompatible true input type")
      if (f == null) SpinalError("Create a mux with incompatible false input type")

      out.input = Multiplex.baseType(sel, t, f)
    }
    muxOut
  }
}
abstract class Extract extends Modifier{
  def getBitVector: Node
  def getParameterNodes: List[Node]
  def getInputData: Node
}

abstract class ExtractBoolFixed extends Extract with CheckWidth{
  var input : Node with WidthProvider = null
  var bitId : Int = -1
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node.asInstanceOf[Node with WidthProvider]}
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {assert(id == 0); input}

  def getBitVector = input
  def getBitId = bitId

  override def checkInferedWidth: Unit = {
    if (bitId < 0 || bitId >= getBitVector.getWidth) {
      PendingError(s"Static bool extraction (bit ${bitId}) is outside the range (${getBitVector.getWidth - 1} downto 0) of ${getBitVector} at\n${getScalaLocationLong}")
    }
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

abstract class ExtractBoolFloating extends Extract {
  var input  : Node with WidthProvider = null.asInstanceOf[Node with WidthProvider]
  var bitId  : Node with WidthProvider = null.asInstanceOf[Node with WidthProvider]

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    doThat(input,0)
    doThat(bitId,1)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    doThat(input)
    doThat(bitId)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 0 => input = node.asInstanceOf[Node with WidthProvider]
    case 1 => bitId = node.asInstanceOf[Node with WidthProvider]
  }

  override def getInputsCount: Int = 2
  override def getInputs: Iterator[Node] = Iterator(input,bitId)
  override def getInput(id: Int): Node = id match{
    case 0 => input
    case 1 => bitId
  }

  def getBitVector = input
  def getBitId = bitId

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


abstract class ExtractBitsVectorFixed extends Extract with WidthProvider with CheckWidth{
  def checkHiLo : Unit = if (hi - lo < -1)
    SpinalError(s"Static bits extraction with a negative size ($hi downto $lo)")

  var hi,lo : Int = -1
  var input : Node with WidthProvider = null
  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node.asInstanceOf[Node with WidthProvider]}
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {assert(id == 0); input}



  def getBitVector = input
  def getHi = hi
  def getLo = lo

  override def getWidth: Int = hi - lo + 1

  override def checkInferedWidth: Unit = {
    val width = input.getWidth
    if (hi >= width || lo < 0) {
      PendingError(s"Static bits extraction ($hi downto $lo) is outside the range (${width - 1} downto 0) of ${getBitVector} at\n${getScalaLocationLong}")
    }
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
abstract class ExtractBitsVectorFloating extends Extract with WidthProvider {
  var size    : Int = -1
  var input   : Node with WidthProvider = null
  var offset  : Node with WidthProvider = null

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    doThat(input,0)
    doThat(offset,1)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    doThat(input)
    doThat(offset)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 0 => input = node.asInstanceOf[Node with WidthProvider]
    case 1 => offset = node.asInstanceOf[Node with WidthProvider]
  }

  override def getInputsCount: Int = 2
  override def getInputs: Iterator[Node] = Iterator(input,offset)
  override def getInput(id: Int): Node = id match{
    case 0 => input
    case 1 => offset
  }

  override def getWidth: Int = size

  def getBitVector = input
  def getOffset = offset
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
  def isFull : Boolean = {
    for(i <- 0 to value.length-2){
      if(value(i) != 0xFFFFFFFF) return false
    }
    if(value.last != (1 << (width.toLong % 32))-1) return false
    true
  }
}

abstract class AssignementNode extends Node {
  def getAssignedBits: AssignedRange //Bit that are allwas assigned
  def getScopeBits: AssignedRange //Bit tht could be assigned
  def getOutBaseType: BaseType

  def clone(out : Node) : this.type
}
abstract class AssignementNodeWidthable extends AssignementNode with Widthable{
  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)
}

class BitAssignmentFixed(out: BitVector, in: Node, bitId: Int) extends AssignementNodeWidthable with CheckWidth{
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


  def getInput : Node = input
  def getBitId = bitId
  override def calcWidth: Int = bitId + 1

  override def checkInferedWidth: Unit = {
    if (bitId < 0 || bitId >= out.getWidth) {
      PendingError(s"Static bool extraction (bit ${bitId}) is outside the range (${out.getWidth - 1} downto 0) of ${out} at\n${getScalaLocationLong}")
    }
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



//TODO remove BitVector
class RangedAssignmentFixed(out: BitVector, in: Node, hi: Int, lo: Int) extends AssignementNodeWidthable with CheckWidth {
  var input : Node with WidthProvider = in.asInstanceOf[Node with WidthProvider]

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)

  override def setInput(id: Int, node: Node): Unit = {
    assert(id == 0)
    this.input = node.asInstanceOf[Node with WidthProvider]
  }
  override def getInputsCount: Int = 1
  override def getInputs: Iterator[Node] = Iterator(input)
  override def getInput(id: Int): Node = {
    assert(id == 0)
    input
  }

  def getInput : Node = input
  def getHi = hi
  def getLo = lo

  override def calcWidth: Int = hi + 1

  override def checkInferedWidth: Unit = {
    if (input.component != null && hi + 1 - lo != input.getWidth) {
      PendingError(s"Assignment bit count mismatch. ${AssignementTree.getDrivedBaseType(this)}($hi downto $lo) := ${input}} at\n${getScalaLocationLong}")
      return
    }

    val width = out.getWidth
    if (hi >= width || lo < 0) {
      PendingError(s"Static bits assignment ($hi downto $lo) is outside the range (${width - 1} downto 0) of ${out} at\n${getScalaLocationLong}")
      return
    }
  }

  override def normalizeInputs: Unit = {
    InputNormalize.resizedOrUnfixedLit(this,0,hi + 1 - lo)
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


class BitAssignmentFloating(out: BitVector, in_ : Node, bitId_ : Node) extends AssignementNodeWidthable{
  var input  : Node with WidthProvider = in_.asInstanceOf[Node with WidthProvider]
  var bitId  : Node with WidthProvider = bitId_.asInstanceOf[Node with WidthProvider]

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    doThat(input,0)
    doThat(bitId,1)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    doThat(input)
    doThat(bitId)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 0 => input = node.asInstanceOf[Node with WidthProvider]
    case 1 => bitId = node.asInstanceOf[Node with WidthProvider]
  }

  override def getInputsCount: Int = 2
  override def getInputs: Iterator[Node] = Iterator(input,bitId)
  override def getInput(id: Int): Node = id match{
    case 0 => input
    case 1 => bitId
  }


  def getInput  : Node = input
  def getBitId  : Node = bitId

  override def calcWidth: Int = 1 << Math.min(20,bitId.getWidth)

  def getAssignedBits: AssignedRange = AssignedRange()
  def getScopeBits: AssignedRange = AssignedRange(Math.min(out.getWidth-1,(1 << Math.min(20,bitId.getWidth)) - 1), 0)

  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
    case 0 =>
      if(outHi >= 0 && ((1 << Math.min(20,bitId.getWidth)) - 1) >= outLo)
        (0,0)
      else
        (-1,0)
    case 1 =>
      if(outHi >= 0 && ((1 << Math.min(20,bitId.getWidth)) - 1) >= outLo)
        super.getOutToInUsage(inputId,outHi,outLo)
      else
        (-1,0)
  }
  def getOutBaseType: BaseType = out
  override def clone(out: Node): this.type = new BitAssignmentFloating(out.asInstanceOf[BitVector],in_,bitId_).asInstanceOf[this.type]
}

class RangedAssignmentFloating(out: BitVector, in_ : Node, offset_ : Node, bitCount: BitCount) extends AssignementNodeWidthable  with CheckWidth {
  var input  : Node with WidthProvider = in_.asInstanceOf[Node with WidthProvider]
  var offset  : Node with WidthProvider = offset_.asInstanceOf[Node with WidthProvider]

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    doThat(input,0)
    doThat(offset,1)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    doThat(input)
    doThat(offset)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 0 => input = node.asInstanceOf[Node with WidthProvider]
    case 1 => offset = node.asInstanceOf[Node with WidthProvider]
  }

  override def getInputsCount: Int = 2
  override def getInputs: Iterator[Node] = Iterator(input,offset)
  override def getInput(id: Int): Node = id match{
    case 0 => input
    case 1 => offset
  }


  def getInput : Node = input
  def getOffset = offset
  def getBitCount = bitCount

  //TODO should not use constructor node ref
  override def calcWidth: Int = 1 << Math.min(20,offset_.asInstanceOf[Node with WidthProvider].getWidth) + bitCount.value

  //TODO should not use constructor node ref
  override def checkInferedWidth: Unit = {
    val input = getInput
    if (input.component != null && bitCount.value != input.asInstanceOf[Node with WidthProvider].getWidth) {
      PendingError(s"Assignement bit count missmatch. ${AssignementTree.getDrivedBaseType((this))}(${bitCount.value} bits) := ${input}} at\n${getScalaLocationLong}")
    }
  }

  override def normalizeInputs: Unit = {
    InputNormalize.resizedOrUnfixedLit(this,0,bitCount.value)
  }



  def getAssignedBits: AssignedRange = AssignedRange()
  //TODO should not use constructor node ref
  def getScopeBits: AssignedRange = AssignedRange(Math.min(out.getWidth-1,(1 << Math.min(20,offset_.asInstanceOf[Node with WidthProvider].getWidth))+ bitCount.value - 1), 0) //TODO dirty offset_
  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = super.getOutToInUsage(inputId,outHi,outLo) //TODO
  def getOutBaseType: BaseType = out
  override def clone(out: Node): this.type = new RangedAssignmentFloating(out.asInstanceOf[BitVector],in_,offset_,bitCount).asInstanceOf[this.type]
}


object MultipleAssignmentNode{
  def newFor(that : BaseType) : MultipleAssignmentNode = that match{
    case that : BitVector => new MultipleAssignmentNodeWidthable
    case that : SpinalEnumCraft[_] => new MultipleAssignmentNodeEnum(that.blueprint)
    case _ => new MultipleAssignmentNode
  }
}

class MultipleAssignmentNode extends Node with AssignementTreePart{
  type T <: Node
  val inputs = new ArrayBuffer[T](4)

  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)

  override def getInputsCount = inputs.length
  override def getInput(id : Int) : Node = inputs(id)
  override def setInput(id : Int,node : Node) : Unit = inputs(id) = node.asInstanceOf[T]

  override def getInputs : Iterator[Node] = inputs.iterator

  override def onEachInput(doThat : (Node,Int) => Unit) : Unit = {
    var idx = inputs.length
    while(idx != 0){
      idx -= 1
      doThat(getInput(idx),idx)
    }
  }

  override def onEachInput(doThat : (Node) => Unit) : Unit = {
    var idx = inputs.length
    while(idx != 0){
      idx -= 1
      doThat(getInput(idx))
    }
  }

  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = (outHi,outLo)

  var inputsThrowable : Array[Throwable] = null
  override def getAssignementContext(id: Int): Throwable =
    ArrayManager.getElseNull(inputsThrowable,id)
  override def setAssignementContext(id: Int,that : Throwable = globalData.getThrowable()): Unit =
    inputsThrowable = ArrayManager.setAllocate(inputsThrowable,id,that)


  def cloneMultipleAssignmentNode : this.type = new MultipleAssignmentNode().asInstanceOf[this.type]
}


class MultipleAssignmentNodeWidthable extends MultipleAssignmentNode with Widthable with CheckWidth{
  override type T = Node with WidthProvider
  override def calcWidth: Int = WidthInfer.multipleAssignmentNodeWidth(this)
  override def normalizeInputs: Unit = {
    for (i <- 0 until inputs.length)
      InputNormalize.resizedOrUnfixedLit(this,i,this.getWidth)
  }

  override private[core] def checkInferedWidth: Unit = {
    for(i <- 0 until inputs.length){
      val input = inputs(i)
      if (input != null && input.component != null && this.getWidth !=input.getWidth) {
        PendingError(s"Assignement bit count missmatch. ${AssignementTree.getDrivedBaseType(this)} := ${input}} at\n${ScalaLocated.long(getAssignementContext(i))}")
      }
    }
  }

  override def cloneMultipleAssignmentNode : this.type = new MultipleAssignmentNodeWidthable().asInstanceOf[this.type]
}

class MultipleAssignmentNodeEnum(enumDef : SpinalEnum) extends MultipleAssignmentNode with InferableEnumEncodingImpl{
  override type T = Node with EnumEncoded
  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
  override def getDefinition: SpinalEnum = enumDef
  override private[core] def normalizeInputs: Unit = {
    InputNormalize.enumImpl(this)
  }
  override def cloneMultipleAssignmentNode : this.type = new MultipleAssignmentNodeEnum(enumDef).asInstanceOf[this.type]
}

object AssertNode{
  def apply(cond : Bool,message : String,severity: AssertNodeSeverity) : Unit = {
    val node = new AssertNode
    node.cond = cond || ! when.getWhensCond(node.component.initialAssignementCondition)
    node.message = message
    node.severity = severity
    node.component.additionalNodesRoot += node
  }
}

trait AssertNodeSeverity
object NOTE     extends AssertNodeSeverity
object WARNING  extends AssertNodeSeverity
object ERROR    extends AssertNodeSeverity
object FAILURE  extends AssertNodeSeverity

class AssertNode extends SyncNode(){
  var cond : Node = null
  var message : String = null
  var severity : AssertNodeSeverity = null


  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)


  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(cond,4)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(cond)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 4 => cond = node
    case _ => super.setInput(id,node)
  }

  override def getInputsCount: Int = 1 + super.getInputsCount
  override def getInputs: Iterator[Node] = super.getInputs ++ Iterator(cond)
  override def getInput(id: Int): Node = id match{
    case 4 => cond
    case _ => super.getInput(id)
  }

  override def isUsingResetSignal: Boolean = false
  override def isUsingSoftResetSignal: Boolean = false
}