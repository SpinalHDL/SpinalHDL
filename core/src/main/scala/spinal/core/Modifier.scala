
///*
// * SpinalHDL
// * Copyright (c) Dolu, All rights reserved.
// *
// * This library is free software; you can redistribute it and/or
// * modify it under the terms of the GNU Lesser General Public
// * License as published by the Free Software Foundation; either
// * version 3.0 of the License, or (at your option) any later version.
// *
// * This library is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// * Lesser General Public License for more details.
// *
// * You should have received a copy of the GNU Lesser General Public
// * License along with this library.
// */
//
package spinal.core


import scala.collection.mutable.ArrayBuffer







abstract class Resize extends Expression with WidthProvider{
  var size : Int = -1
  var input : Expression with WidthProvider = null

  override def getWidth(): Int = size


  override def simplifyNode: Expression = {
    if(input.getWidth == 0){
      getLiteralFactory(0,size)
    } else {
      this
    }
  }
  def getLiteralFactory : (BigInt, Int) => Expression
  override def foreachExpression(func: (Expression) => Unit): Unit = func(input)
  override def remapExpressions(func: (Expression) => Expression): Unit = input = func(input).asInstanceOf[Expression with WidthProvider]
}

class ResizeBits extends Resize{
  override def getTypeObject = TypeBits
  override def opName: String = "resize(b,i)"
  override def getLiteralFactory: (BigInt, Int) => Expression = BitsLiteral.apply
//  override def simplifyNode: Unit = SymplifyNode.resizeImpl2(B.apply,this)
}
class ResizeUInt extends Resize{
  override def getTypeObject = TypeUInt
  override def opName: String = "resize(u,i)"
  override def getLiteralFactory: (BigInt, Int) => Expression = UIntLiteral.apply
}
class ResizeSInt extends Resize{
  override def getTypeObject = TypeSInt
  override def opName: String = "resize(s,i)"
  override def getLiteralFactory: (BigInt, Int) => Expression = SIntLiteral.apply
}
//
//
//
//
abstract class Operator extends Modifier
//
abstract class UnaryOperator extends Operator{
  type T <: Expression
  var source  : T = null.asInstanceOf[T]

  def foreachExpression(func : (Expression) => Unit) : Unit = {
    func(source)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    source = func(source).asInstanceOf[T]
  }

  //  type T <: Node
//  var input : T = null.asInstanceOf[T]
//  override def onEachInput(doThat: (Node, Int) => Unit): Unit = doThat(input,0)
//  override def onEachInput(doThat: (Node) => Unit): Unit = doThat(input)
//  override def setInput(id: Int, node: Node): Unit = {assert(id == 0); this.input = node.asInstanceOf[T]}
//  override def getInputsCount: Int = 1
//  override def getInputs: Iterator[Node] = Iterator(input)
//  override def getInput(id: Int): Node = {assert(id == 0); input}
}
//
abstract class UnaryOperatorWidthableInputs extends UnaryOperator with Widthable{
  override type T = Expression with WidthProvider
}


abstract class ConstantOperator extends Operator{
  type T <: Expression
  var source  : T = null.asInstanceOf[T]

  def foreachExpression(func : (Expression) => Unit) : Unit = {
    func(source)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    source = func(source).asInstanceOf[T]
  }
}



abstract class ConstantOperatorWidthableInputs extends ConstantOperator{
  override type T = Expression with WidthProvider
}



abstract class BinaryOperator extends Operator{
  type T <: Expression
  var left,right  : T = null.asInstanceOf[T]

  def foreachExpression(func : (Expression) => Unit) : Unit = {
    func(left)
    func(right)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    left = func(left).asInstanceOf[T]
    right = func(right).asInstanceOf[T]
  }

//  override def toString(): String = {
//    def inStr(that : T) = (if (that == null) "null" else that.nonRecursiveToString())
//    s"(${inStr(left)} $opName ${inStr(right)})"
//  }
}


abstract class BinaryOperatorWidthableInputs extends BinaryOperator{
  override type T = Expression with WidthProvider
}


object InferWidth{
  def canBeResized(that : Expression) = that match {
    case that : BitVectorLiteral => !that.hasSpecifiedBitCount
    case that : SpinalTagReady => that.hasTag(tagAutoResize)
    case _ => false
  }
  def notResizableElseMax(op : BinaryOperatorWidthableInputs) : Int = {
    val leftR = canBeResized(op.left)
    val rightR = canBeResized(op.right)
    if(leftR != rightR){
      if(leftR) op.right.getWidth else op.left.getWidth
    } else {
      Math.max(op.left.getWidth, op.right.getWidth)
    }
  }

  def notResizableElseMax(op : MultiplexedWidthable) : Int = {
    val leftR = canBeResized(op.whenTrue)
    val rightR = canBeResized(op.whenFalse)
    if(leftR != rightR){
      if(leftR) op.whenFalse.getWidth else op.whenTrue.getWidth
    } else {
      Math.max(op.whenTrue.getWidth, op.whenFalse.getWidth)
    }
  }
}
//
//
//
object Operator{
  object Bool{
    class And extends BinaryOperator{
      override def getTypeObject = TypeBool
      override def opName: String = "&&"
      override def normalizeInputs: Unit = {}
//      override def simplifyNode: Unit = {}
    }

    class Or extends BinaryOperator{
      override def getTypeObject = TypeBool
      override def opName: String = "||"
      override def normalizeInputs: Unit = {}
//      override def simplifyNode: Unit = {}
    }
//
    class Xor extends BinaryOperator{
  override def getTypeObject = TypeBool
      override def opName: String = "B^B"
      override def normalizeInputs: Unit = {}
//      override def simplifyNode: Unit = {}
    }

    class Not extends UnaryOperator{
      override def getTypeObject = TypeBool
      override def opName: String = "!"
      override def normalizeInputs: Unit = {}
//      override def simplifyNode: Unit = {}
    }

    class Equal extends BinaryOperator{
      override def getTypeObject = TypeBool
      override def opName: String = "B==B"
      override def normalizeInputs: Unit = {}
//      override def simplifyNode: Unit = {}
    }

    class NotEqual extends BinaryOperator{
      override def getTypeObject = TypeBool
      override def opName: String = "B!=B"
      override def normalizeInputs: Unit = {}
//      override def simplifyNode: Unit = {}
    }
  }
//
  object BitVector{
    abstract class And extends BinaryOperatorWidthableInputs with Widthable{
      def resizeFactory : Resize
//      def getLiteralFactory : (BigInt, Int) => Expression
      override def calcWidth(): Int = InferWidth.notResizableElseMax(this)
      override def normalizeInputs: Unit = {
        val targetWidth = getWidth
        left = InputNormalize.resizedOrUnfixedLit(left, targetWidth, resizeFactory, this, this)
        right = InputNormalize.resizedOrUnfixedLit(right, targetWidth, resizeFactory, this, this)
      }
      //override def simplifyNode: Expression = SymplifyNode.binaryInductZeroWithOtherWidth(getLiteralFactory,true)(this)
    }

    abstract class Or extends BinaryOperatorWidthableInputs with Widthable{
      def resizeFactory : Resize
//      def getLiteralFactory : (BigInt, Int) => Expression
      override def calcWidth(): Int = InferWidth.notResizableElseMax(this)
      override def normalizeInputs: Unit = {
        val targetWidth = getWidth
        left = InputNormalize.resizedOrUnfixedLit(left, targetWidth, resizeFactory, this, this)
        right = InputNormalize.resizedOrUnfixedLit(right, targetWidth, resizeFactory, this, this)
      }
      //override def simplifyNode: Expression = SymplifyNode.binaryInductZeroWithOtherWidth(getLiteralFactory,true)(this)
    }

    abstract class Xor extends BinaryOperatorWidthableInputs with Widthable {
      def resizeFactory : Resize
//      def getLiteralFactory : (BigInt, Int) => Expression
      override def calcWidth(): Int = InferWidth.notResizableElseMax(this)
      override def normalizeInputs: Unit = {
        val targetWidth = getWidth
        left = InputNormalize.resizedOrUnfixedLit(left, targetWidth, resizeFactory, this, this)
        right = InputNormalize.resizedOrUnfixedLit(right, targetWidth, resizeFactory, this, this)
      }
      //override def simplifyNode: Expression = SymplifyNode.binaryInductZeroWithOtherWidth(getLiteralFactory,true)(this)
    }

    abstract class Add extends BinaryOperatorWidthableInputs with Widthable{
      def resizeFactory : Resize
      override def calcWidth(): Int = InferWidth.notResizableElseMax(this)
      override def normalizeInputs: Unit = {
        val targetWidth = getWidth
        left = InputNormalize.resize(left, targetWidth, resizeFactory)
        right = InputNormalize.resize(right, targetWidth, resizeFactory)
      }
      //override def simplifyNode: Expression = SymplifyNode.binaryTakeOther(this)
    }

    abstract class Sub extends BinaryOperatorWidthableInputs with Widthable{
      def resizeFactory : Resize
      override def calcWidth(): Int = InferWidth.notResizableElseMax(this)
      override def normalizeInputs: Unit = {
        val targetWidth = getWidth
        left = InputNormalize.resize(left, targetWidth, resizeFactory)
        right = InputNormalize.resize(right, targetWidth, resizeFactory)
      }
//      override def simplifyNode: Unit = {SymplifyNode.binaryMinus(getLiteralFactory)(this)}
    }

    abstract class Mul extends BinaryOperatorWidthableInputs with Widthable{
      def getLiteralFactory : (BigInt, Int) => Expression
      override def calcWidth(): Int = left.getWidth + right.getWidth
      override def simplifyNode: Expression = {SymplifyNode.binaryInductZeroWithOtherWidth(getLiteralFactory)(this)}
    }

    abstract class Div extends BinaryOperatorWidthableInputs with Widthable{
      override def calcWidth(): Int = left.getWidth
    }

    abstract class Mod extends BinaryOperatorWidthableInputs with Widthable{
      override def calcWidth(): Int = left.getWidth
    }

    abstract class Equal extends BinaryOperatorWidthableInputs with ScalaLocated{
      override def getTypeObject = TypeBool
      override def normalizeInputs: Unit
      override def simplifyNode: Expression = {SymplifyNode.binaryThatIfBoth(new BoolLiteral(true))(this)}
    }

    abstract class NotEqual extends BinaryOperatorWidthableInputs with ScalaLocated{
      override def getTypeObject = TypeBool
      override def normalizeInputs: Unit
      override def simplifyNode: Expression = {SymplifyNode.binaryThatIfBoth(new BoolLiteral(false))(this)}
    }

    trait ShiftOperator
    abstract class ShiftRightByInt(val shift : Int) extends ConstantOperatorWidthableInputs with Widthable with ShiftOperator{
      assert(shift >= 0)
      override def calcWidth(): Int = Math.max(0, source.getWidth - shift)
    }

    abstract class ShiftRightByUInt extends BinaryOperatorWidthableInputs with Widthable with ShiftOperator{
      override def calcWidth(): Int = left.getWidth
      override def normalizeInputs: Unit = {}
      override def simplifyNode: Expression = if(right.getWidth == 0) left else this
    }

    abstract class ShiftLeftByInt(val shift : Int) extends ConstantOperatorWidthableInputs with Widthable with ShiftOperator{
      assert(shift >= 0)
      override def calcWidth(): Int = source.getWidth + shift
    }

    abstract class ShiftLeftByUInt extends BinaryOperatorWidthableInputs with Widthable with ShiftOperator{
      override def calcWidth(): Int = left.getWidth + (1 << right.getWidth) - 1
      override def simplifyNode: Expression = if(right.getWidth == 0) left else this
    }


    abstract class ShiftRightByIntFixedWidth(val shift : Int) extends ConstantOperatorWidthableInputs with Widthable with ShiftOperator{
      assert(shift >= 0)
      override def calcWidth(): Int = source.getWidth
    }

    abstract class ShiftLeftByIntFixedWidth(val shift : Int) extends ConstantOperatorWidthableInputs with Widthable with ShiftOperator{
      assert(shift >= 0)
      override def calcWidth(): Int = source.getWidth
      override def normalizeInputs: Unit = {}
    }

    abstract class ShiftLeftByUIntFixedWidth extends BinaryOperatorWidthableInputs with Widthable with ShiftOperator{
      override def calcWidth(): Int = left.getWidth
      override def simplifyNode: Expression = if(right.getWidth == 0) left else this
    }

//    abstract class AllByBool() extends UnaryOperator with Widthable {
//      val theConsumer : Expression = null
//      override def calcWidth: Int = theConsumer.asInstanceOf[WidthProvider].getWidth
//    }

  }

  object Bits{
    class Cat extends BinaryOperatorWidthableInputs with Widthable{
      override def getTypeObject = TypeBits
      override def opName: String = "b##b"
      override def calcWidth(): Int = left.getWidth + right.getWidth
      override def simplifyNode: Expression = {SymplifyNode.binaryTakeOther(this)}
    }

    class Not extends UnaryOperatorWidthableInputs{
      override def getTypeObject = TypeBits
      override def opName: String = "~b"
      override def calcWidth(): Int = source.getWidth
    }

    class And extends BitVector.And{
      override def getTypeObject = TypeBits
      override def opName: String = "b&b"
      def resizeFactory : Resize = new ResizeBits
//      override def getLiteralFactory: (BigInt, Int) => Expression = BitsLiteral.apply
    }

    class Or extends BitVector.Or{
      override def getTypeObject = TypeBits
      override def opName: String = "b|b"
      def resizeFactory : Resize = new ResizeBits
//      override def getLiteralFactory: (BigInt, Int) => Expression = BitsLiteral.apply
    }

    class Xor extends BitVector.Xor{
      override def getTypeObject = TypeBits
      override def opName: String = "b^b"
      def resizeFactory : Resize = new ResizeBits
//      override def getLiteralFactory: (BigInt, Int) => Expression = BitsLiteral.apply
    }

    class Equal extends BitVector.Equal{
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        left = InputNormalize.resizedOrUnfixedLit(left, targetWidth, new ResizeBits, right, this)
        right = InputNormalize.resizedOrUnfixedLit(right, targetWidth, new ResizeBits, left, this)
      }
      override def opName: String = "b==b"
    }

    class NotEqual extends BitVector.NotEqual{
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        left = InputNormalize.resizedOrUnfixedLit(left, targetWidth, new ResizeBits, right, this)
        right = InputNormalize.resizedOrUnfixedLit(right, targetWidth, new ResizeBits, left, this)
      }
      override def opName: String = "b!=b"
    }

    class ShiftRightByInt(shift : Int) extends BitVector.ShiftRightByInt(shift){
      override def getTypeObject = TypeBits
      override def opName: String = "b>>i"
    }

    class ShiftRightByUInt extends BitVector.ShiftRightByUInt{
      override def getTypeObject = TypeBits
      override def opName: String = "b>>u"
    }

    class ShiftLeftByInt(shift : Int) extends BitVector.ShiftLeftByInt(shift){
      override def getTypeObject = TypeBits
      override def opName: String = "b<<i"
    }

    class ShiftLeftByUInt extends BitVector.ShiftLeftByUInt{
      override def getTypeObject = TypeBits
      override def opName: String = "b<<u"
    }

    class ShiftRightByIntFixedWidth(shift : Int) extends BitVector.ShiftRightByIntFixedWidth(shift){
      override def getTypeObject = TypeBits
      override def opName: String = "b|>>i"
    }

    class ShiftLeftByIntFixedWidth(shift : Int) extends BitVector.ShiftLeftByIntFixedWidth(shift){
      override def getTypeObject = TypeBits
      override def opName: String = "b|<<i"
    }

    class ShiftLeftByUIntFixedWidth extends BitVector.ShiftLeftByUIntFixedWidth{
      override def getTypeObject = TypeBits
      override def opName: String = "b|<<u"
    }

////    class RotateLeftByUInt extends BitVector.RotateLeftByUInt{
////      override def opName: String = "brotlu"
////      def getLiteralFactory : (BigInt, BitCount) => Node = B.apply
////    }
//
//    class AllByBool(theConsumer : Node) extends BitVector.AllByBool(theConsumer) {
//      override def opName: String = "bAllByB"
//    }
  }
//
//
  object UInt{
    class Not extends UnaryOperatorWidthableInputs{
      override def getTypeObject = TypeUInt
      override def opName: String = "~u"
      override def calcWidth(): Int = source.getWidth
    }

    class And extends BitVector.And{
      override def getTypeObject = TypeUInt
      override def opName: String = "u&u"
      def resizeFactory : Resize = new ResizeUInt
    }

    class Or extends BitVector.Or{
      override def getTypeObject = TypeUInt
      override def opName: String = "u|u"
      def resizeFactory : Resize = new ResizeUInt
    }

    class Xor extends BitVector.Xor{
      override def getTypeObject = TypeUInt
      override def opName: String = "u^u"
      def resizeFactory : Resize = new ResizeUInt
    }

    class Add extends BitVector.Add{
      override def getTypeObject = TypeUInt
      override def opName: String = "u+u"
      def resizeFactory : Resize = new ResizeUInt
    }

    class Sub extends BitVector.Sub{
      override def getTypeObject = TypeUInt
      override def opName: String = "u-u"
      def resizeFactory : Resize = new ResizeUInt
    }

    class Mul extends BitVector.Mul{
      override def getTypeObject = TypeUInt
      override def opName: String = "u*u"
      override def getLiteralFactory: (BigInt, Int) => Expression = UIntLiteral.apply
    }

    class Div extends BitVector.Div{
      override def getTypeObject = TypeUInt
      override def opName: String = "u/u"
    }

    class Mod extends BitVector.Mod{
      override def getTypeObject = TypeUInt
      override def opName: String = "u%u"
    }

    class Smaller extends BinaryOperatorWidthableInputs{
      override def getTypeObject = TypeBool
      override def opName: String = "u<u"
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        left = InputNormalize.resize(left, targetWidth, new ResizeUInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeUInt)
      }
    }

    class SmallerOrEqual extends BinaryOperatorWidthableInputs{
      override def getTypeObject = TypeBool
      override def opName: String = "u<=u"
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        left = InputNormalize.resize(left, targetWidth, new ResizeUInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeUInt)
      }
    }

    class Equal extends BitVector.Equal{
      override def opName: String = "u==u"
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        left = InputNormalize.resize(left, targetWidth, new ResizeUInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeUInt)
      }
    }

    class NotEqual extends BitVector.NotEqual{
      override def opName: String = "u!=u"
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        left = InputNormalize.resize(left, targetWidth, new ResizeUInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeUInt)
      }
    }

    class ShiftRightByInt(shift : Int) extends BitVector.ShiftRightByInt(shift){
      override def getTypeObject = TypeUInt
      override def opName: String = "u>>i"
    }

    class ShiftRightByUInt extends BitVector.ShiftRightByUInt{
      override def getTypeObject = TypeUInt
      override def opName: String = "u>>u"
    }

    class ShiftLeftByInt(shift : Int) extends BitVector.ShiftLeftByInt(shift){
      override def getTypeObject = TypeUInt
      override def opName: String = "u<<i"
    }

    class ShiftLeftByUInt extends BitVector.ShiftLeftByUInt{
      override def getTypeObject = TypeUInt
      override def opName: String = "u<<u"
    }

    class ShiftRightByIntFixedWidth(shift : Int) extends BitVector.ShiftRightByIntFixedWidth(shift){
      override def getTypeObject = TypeUInt
      override def opName: String = "u|>>i"
    }

    class ShiftLeftByIntFixedWidth(shift : Int) extends BitVector.ShiftLeftByIntFixedWidth(shift){
      override def getTypeObject = TypeUInt
      override def opName: String = "u|<<i"
    }

    class ShiftLeftByUIntFixedWidth extends BitVector.ShiftLeftByUIntFixedWidth{
      override def getTypeObject = TypeUInt
      override def opName: String = "u|<<u"
    }

//    class AllByBool(theConsumer : Node) extends BitVector.AllByBool(theConsumer) {
//      override def opName: String = "uAllByB"
//    }
  }
//
  object SInt{
    class Not extends UnaryOperatorWidthableInputs with Widthable{
      override def getTypeObject = TypeSInt
      override def opName: String = "~s"
      override def calcWidth(): Int = source.getWidth
    }

    class Minus extends UnaryOperatorWidthableInputs with Widthable{
      override def getTypeObject = TypeSInt
      override def opName: String = "-s"
      override def calcWidth(): Int = source.getWidth
    }

    class And extends BitVector.And{
      override def getTypeObject = TypeSInt
      override def opName: String = "s&s"
      def resizeFactory : Resize = new ResizeSInt
    }

    class Or extends BitVector.Or{
      override def getTypeObject = TypeSInt
      override def opName: String = "s|s"
      def resizeFactory : Resize = new ResizeSInt
    }

    class Xor extends BitVector.Xor{
      override def getTypeObject = TypeSInt
      override def opName: String = "s^s"
      def resizeFactory : Resize = new ResizeSInt
    }

    class Add extends BitVector.Add{
      override def getTypeObject = TypeSInt
      override def opName: String = "s+s"
      def resizeFactory : Resize = new ResizeSInt
    }

    class Sub extends BitVector.Sub{
      override def getTypeObject = TypeSInt
      override def opName: String = "s-s"
      def resizeFactory : Resize = new ResizeSInt
    }

    class Mul extends BitVector.Mul{
      override def getTypeObject = TypeSInt
      override def opName: String = "s*s"
      override def getLiteralFactory: (BigInt, Int) => Expression = SIntLiteral.apply
    }

    class Div extends BitVector.Div{
      override def getTypeObject = TypeSInt
      override def opName: String = "s/s"
    }

    class Mod extends BitVector.Mod{
      override def getTypeObject = TypeSInt
      override def opName: String = "s%s"
    }

    class Smaller extends BinaryOperatorWidthableInputs{
      override def getTypeObject = TypeBool
      override def opName: String = "s<s"
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        left = InputNormalize.resize(left, targetWidth, new ResizeSInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeSInt)
      }
    }

    class SmallerOrEqual extends BinaryOperatorWidthableInputs{
      override def getTypeObject = TypeBool
      override def opName: String = "s<=s"
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        left = InputNormalize.resize(left, targetWidth, new ResizeSInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeSInt)
      }
    }

    class Equal extends BitVector.Equal{
      override def opName: String = "s==s"
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        left = InputNormalize.resize(left, targetWidth, new ResizeSInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeSInt)
      }
    }

    class NotEqual extends BitVector.NotEqual{
      override def opName: String = "s!=s"
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        left = InputNormalize.resize(left, targetWidth, new ResizeSInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeSInt)
      }
    }

    class ShiftRightByInt(shift : Int) extends BitVector.ShiftRightByInt(shift){
      override def getTypeObject = TypeSInt
      override def opName: String = "s>>i"
    }

    class ShiftRightByUInt extends BitVector.ShiftRightByUInt{
      override def getTypeObject = TypeSInt
      override def opName: String = "s>>u"
    }

    class ShiftLeftByInt(shift : Int) extends BitVector.ShiftLeftByInt(shift){
      override def getTypeObject = TypeSInt
      override def opName: String = "s<<i"
    }

    class ShiftLeftByUInt extends BitVector.ShiftLeftByUInt{
      override def getTypeObject = TypeSInt
      override def opName: String = "s<<u"
    }

    class ShiftRightByIntFixedWidth(shift : Int) extends BitVector.ShiftRightByIntFixedWidth(shift){
      override def getTypeObject = TypeSInt
      override def opName: String = "s|>>i"
    }

    class ShiftLeftByIntFixedWidth(shift : Int) extends BitVector.ShiftLeftByIntFixedWidth(shift){
      override def getTypeObject = TypeSInt
      override def opName: String = "s|<<i"
    }

    class ShiftLeftByUIntFixedWidth extends BitVector.ShiftLeftByUIntFixedWidth{
      override def getTypeObject = TypeSInt
      override def opName: String = "s|<<u"
    }
//
//    class AllByBool(theConsumer : Node) extends BitVector.AllByBool(theConsumer) {
//      override def opName: String = "sAllByB"
//    }
  }

  object Enum{
    class Equal(enumDef : SpinalEnum) extends BinaryOperator with InferableEnumEncodingImpl{
      override def getTypeObject: Any = TypeBool

      override def opName: String = "e==e"
      override def normalizeInputs: Unit = {InputNormalize.enumImpl(this)}

      override type T = Expression with EnumEncoded
      override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
      override def getDefinition: SpinalEnum = enumDef
    }

    class NotEqual(enumDef : SpinalEnum) extends BinaryOperator with InferableEnumEncodingImpl{
      override def getTypeObject: Any = TypeBool
      override def opName: String = "e!=e"
      override def normalizeInputs: Unit = {InputNormalize.enumImpl(this)}

      override type T = Expression with EnumEncoded
      override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
      override def getDefinition: SpinalEnum = enumDef
    }

  }
}
//
//
//
abstract class Modifier extends Expression {


//  override def toString(): String = {
//    s"($getClassIdentifier : $opName, defined in ${if(this.component != null)this.component.getPath() else "root"} with inputs : ${this.getInputs.map(in => if (in == null) "null" else in.nonRecursiveToString()).reduceLeft(_ + ", " + _)})"
//  }

//  override def nonRecursiveToString(): String = opName

//  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)
}
//
//
abstract class Cast extends Modifier {
  type T <: Expression
  var input : T = null.asInstanceOf[T]

  override def remapExpressions(func: (Expression) => Expression): Unit = input = func(input).asInstanceOf[T]
  override def foreachExpression(func: (Expression) => Unit): Unit = func(input)
}

abstract class CastBitVectorToBitVector extends Cast with Widthable{
  override type T <: Expression with WidthProvider
  override private[core] def calcWidth: Int = input.getWidth
}

class CastSIntToBits extends CastBitVectorToBitVector{
  override def getTypeObject = TypeBits
  override def opName: String = "s->b"
}
class CastUIntToBits extends CastBitVectorToBitVector{
  override def getTypeObject = TypeBits
  override def opName: String = "u->b"
}
class CastBitsToUInt extends CastBitVectorToBitVector{
  override def getTypeObject = TypeUInt
  override def opName: String = "b->u"
}
class CastSIntToUInt extends CastBitVectorToBitVector{
  override def getTypeObject = TypeUInt
  override def opName: String = "s->u"
}
class CastBitsToSInt extends CastBitVectorToBitVector{
  override def getTypeObject = TypeSInt
  override def opName: String = "b->s"
}
class CastUIntToSInt extends CastBitVectorToBitVector{
  override def getTypeObject = TypeSInt
  override def opName: String = "u->s"
}
class CastBoolToBits extends Cast with Widthable{
  override def getTypeObject = TypeBits
  override def opName: String = "B->b"
  override private[core] def calcWidth: Int = 1
}

class CastEnumToBits extends Cast with Widthable{
  override type T <: Expression with EnumEncoded
  override def opName: String = "e->b"
  override private[core] def calcWidth: Int = input.getEncoding.getWidth(input.getDefinition)
  override def getTypeObject: Any = TypeBits
}

class CastBitsToEnum(val enumDef: SpinalEnum) extends Cast with InferableEnumEncodingImpl{
  override type T <: Expression with WidthProvider
  override def opName: String = "b->e"
  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
  override def getDefinition: SpinalEnum = enumDef

  override def normalizeInputs: Unit = {
    input = InputNormalize.resizedOrUnfixedLit(input, getEncoding.getWidth(enumDef), new ResizeBits, this, this).asInstanceOf[T]
  }

  override def getTypeObject: Any = TypeEnum
}

class CastEnumToEnum(enumDef: SpinalEnum) extends Cast with  InferableEnumEncodingImpl{
  override type T <: Expression with EnumEncoded
  override def opName: String = "e->e"

  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
  override def getDefinition: SpinalEnum = enumDef
  override def getTypeObject: Any = TypeEnum
}



abstract class Multiplexer extends Modifier {
  type T <: Expression
  var cond      : Expression = null
  var whenTrue  : T = null.asInstanceOf[T]
  var whenFalse : T = null.asInstanceOf[T]

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    cond = func(cond)
    whenTrue = func(whenTrue).asInstanceOf[T]
    whenFalse = func(whenFalse).asInstanceOf[T]
  }
  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(cond)
    func(whenTrue)
    func(whenFalse)
  }
}

abstract class MultiplexedWidthable extends Multiplexer with Widthable{
  override type T = Expression with WidthProvider
  override def calcWidth: Int = InferWidth.notResizableElseMax(this)
}

class MultiplexerBool extends Multiplexer{
  override def getTypeObject: Any = TypeBool
  override def opName: String = "mux(B,B,B)"
}
class MultiplexerBits extends MultiplexedWidthable {
  override def getTypeObject: Any = TypeBits
  override def opName: String = "mux(B,b,b)"
  override def normalizeInputs: Unit = {
    val targetWidth = getWidth
    whenTrue = InputNormalize.resizedOrUnfixedLit(whenTrue, targetWidth, new ResizeBits, this, this)
    whenFalse = InputNormalize.resizedOrUnfixedLit(whenFalse, targetWidth, new ResizeBits, this, this)
  }
}
class MultiplexerUInt extends MultiplexedWidthable{
  override def getTypeObject: Any = TypeUInt
  override def opName: String = "mux(B,u,u)"
  override def normalizeInputs: Unit = {
    val targetWidth = getWidth
    whenTrue = InputNormalize.resize(whenTrue, targetWidth, new ResizeUInt)
    whenFalse = InputNormalize.resize(whenFalse, targetWidth, new ResizeUInt)
  }
}
class MultiplexerSInt extends MultiplexedWidthable{
  override def getTypeObject: Any = TypeSInt
  override def opName: String = "mux(B,s,s)"
  override def normalizeInputs: Unit = {
    val targetWidth = getWidth
    whenTrue = InputNormalize.resize(whenTrue, targetWidth, new ResizeSInt)
    whenFalse = InputNormalize.resize(whenFalse, targetWidth, new ResizeSInt)
  }
}
class MultiplexerEnum(enumDef : SpinalEnum) extends Multiplexer with InferableEnumEncodingImpl{
  override type T = Expression with EnumEncoded
  override def opName: String = "mux(B,e,e)"
  override def getDefinition: SpinalEnum = enumDef
  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
  override def normalizeInputs: Unit = {
    InputNormalize.enumImpl(this)
  }
  override def getTypeObject: Any = TypeEnum
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
  def apply[K <: BaseType, T <: Data](addr: K, mappings: (Any, T)*): T = list(addr,mappings)

  def list[K <: BaseType, T <: Data](addr: K, mappings: Seq[(Any, T)]): T = {
    val result : T = weakCloneOf(mappings.head._2)

    switch(addr){
      for ((cond, value) <- mappings) {
        cond match {
          case product: Product => {
            //  for(cond <- product.productIterator){
            is.list(product.productIterator) {
              result := value
            }
            //   }
          }
          case `default` => {
            default {
              result := value
            }
          }
          case _ => {
            is(cond) {
              result := value
            }
          }
        }
      }
    }
    result
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


    val muxOut = weakCloneOf(outType)
    val muxInTrue = cloneOf(muxOut)
    val muxInFalse = cloneOf(muxOut)


    muxInTrue := whenTrue
    muxInFalse := whenFalse


    for ((out, t,  f) <- (muxOut.flatten, muxInTrue.flatten, muxInFalse.flatten).zipped) {
      if (t == null) SpinalError("Create a mux with incompatible true input type")
      if (f == null) SpinalError("Create a mux with incompatible false input type")

      out.assignFrom(Multiplex.baseType(sel, t, f))
    }
    muxOut
  }
}


abstract class SubAccess extends Modifier{
//  def finalTarget : NameableExpression
  def getBitVector: Expression
}

abstract class BitVectorBitAccessFixed extends SubAccess with ScalaLocated {
  var source : Expression with WidthProvider = null
  var bitId : Int = -1

  override def getBitVector: Expression = source

  override def normalizeInputs: Unit = {
    if (bitId < 0 || bitId >= source.getWidth) {
      PendingError(s"Static bool extraction (bit ${bitId}) is outside the range (${source.getWidth - 1} downto 0) of ${source} at\n${getScalaLocationLong}")
    }
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    source = func(source).asInstanceOf[Expression with WidthProvider]
  }

  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(source)
  }

//  override def checkInferedWidth: Unit = {
//    if (bitId < 0 || bitId >= getBitVector.getWidth) {
//      PendingError(s"Static bool extraction (bit ${bitId}) is outside the range (${getBitVector.getWidth - 1} downto 0) of ${getBitVector} at\n${getScalaLocationLong}")
//    }
//  }
//
//  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
//    case 0 =>
//      if(outHi >= 0 && outLo == 0)
//        (bitId, bitId)
//      else
//        (-1,0)
//  }
//  def getParameterNodes: List[Node] = Nil
}

class BitsBitAccessFixed extends BitVectorBitAccessFixed{
  override def getTypeObject = TypeBool
  override def opName: String = "extract(b,i)"
}
class UIntBitAccessFixed extends BitVectorBitAccessFixed{
  override def getTypeObject = TypeBool
  override def opName: String = "extract(u,i)"
}
class SIntBitAccessFixed extends BitVectorBitAccessFixed{
  override def getTypeObject = TypeBool
  override def opName: String = "extract(s,i)"
}

abstract class BitVectorBitAccessFloating extends SubAccess with ScalaLocated {
  var source  : Expression with WidthProvider = null
  var bitId  : Expression with WidthProvider = null
  override def getBitVector: Expression = source

  override def normalizeInputs: Unit = {
    if(source.getWidth == 0){
      PendingError(s"Can't access ${source} bits, as it has none")
    }
  }

  def bitVectorBitAccessFixedFactory : BitVectorBitAccessFixed
  override def simplifyNode: Expression = {
    if(bitId.getWidth == 0){
      val access = bitVectorBitAccessFixedFactory
      access.bitId = 0
      access.source = source
      access
    }else{
      this
    }
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    source = func(source).asInstanceOf[Expression with WidthProvider]
    bitId = func(bitId).asInstanceOf[Expression with WidthProvider]
  }

  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(source)
    func(bitId)
  }

  //  def getParameterNodes: List[Node] = getInput(1) :: Nil
//  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
//    case 0 =>
//      if(outHi >= 0 && outLo == 0)
//        (Math.min(getBitVector.getWidth-1,(1 << Math.min(20,bitId.getWidth)) - 1), 0)
//      else
//        (-1,0)
//    case 1 =>
//      if(outHi >= 0 && outLo == 0)
//        (getBitId.getWidth-1,0)
//      else
//        (-1,0)
//  }
}

class BitsBitAccessFloating extends BitVectorBitAccessFloating{
  override def getTypeObject = TypeBool
  override def opName: String = "extract(b,u)"
  override def bitVectorBitAccessFixedFactory: BitVectorBitAccessFixed = new BitsBitAccessFixed
}
class UIntBitAccessFloating extends BitVectorBitAccessFloating{
  override def getTypeObject = TypeBool
  override def opName: String = "extract(u,u)"
  override def bitVectorBitAccessFixedFactory: BitVectorBitAccessFixed = new UIntBitAccessFixed
}
class SIntBitAccessFloating extends BitVectorBitAccessFloating{
  override def getTypeObject = TypeBool
  override def opName: String = "extract(s,u)"
  override def bitVectorBitAccessFixedFactory: BitVectorBitAccessFixed = new SIntBitAccessFixed
}


abstract class BitVectorRangedAccessFixed extends SubAccess with WidthProvider{
  var source : Expression with WidthProvider = null
  var hi, lo = -1
  override def getBitVector: Expression = source

  override def normalizeInputs: Unit = {
    if (hi >= source.getWidth || lo < 0) {
      PendingError(s"Static bits extraction ($hi downto $lo) is outside the range (${source.getWidth - 1} downto 0) of ${source} at\n${getScalaLocationLong}")
    }
  }

  def checkHiLo : Unit = if (hi - lo < -1)
    SpinalError(s"Static bits extraction with a negative size ($hi downto $lo)")

  override def getWidth: Int = hi - lo + 1

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    source = func(source).asInstanceOf[Expression with Widthable]
  }

  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(source)
  }


//  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
//    case 0 => (lo+outHi, lo+outLo)
//  }
}

class BitsRangedAccessFixed extends BitVectorRangedAccessFixed{
  override def getTypeObject = TypeBits
  override def opName: String = "extract(b,i,i)"
}
class UIntRangedAccessFixed extends BitVectorRangedAccessFixed{
  override def getTypeObject = TypeUInt
  override def opName: String = "extract(u,i,i)"
}
class SIntRangedAccessFixed extends BitVectorRangedAccessFixed{
  override def getTypeObject = TypeSInt
  override def opName: String = "extract(s,i,i)"
}
//
////WHen used offset.dontSimplifyIt() Because it can appear at multipe location (o+bc-1 downto o)
abstract class BitVectorRangedAccessFloating extends SubAccess with WidthProvider {
  var size    : Int = -1
  var source  : Expression with WidthProvider = null
  var offset  : Expression with WidthProvider = null
  override def getBitVector: Expression = source

  override def getWidth: Int = size

  override def normalizeInputs: Unit = {
    if(source.getWidth < size){
      PendingError(s"Can't access ${source} bits, as it has less than $size")
    }
  }

  def bitVectorRangedAccessFixedFactory : BitVectorRangedAccessFixed
  override def simplifyNode: Expression = {
    if(offset.getWidth == 0){
      val access = bitVectorRangedAccessFixedFactory
      access.lo = 0
      access.hi = size-1
      access.source = source
      access
    }else{
      this
    }
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    source = func(source).asInstanceOf[Expression with WidthProvider]
    offset = func(offset).asInstanceOf[Expression with WidthProvider]
  }

  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(source)
    func(offset)
  }

  //  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
//    case 0 =>
//      if(outHi >= outLo) //Not exact
//        (Math.min(getBitVector.getWidth-1,(1 << Math.min(20,offset.getWidth))+ size - 1), 0)
//      else
//        (-1,0)
//    case 1 =>
//      if(outHi >= outLo) //Not exact
//        super.getOutToInUsage(inputId,outHi,outLo)
//      else
//        (-1,0)
//    case 2 => (-1,0)
//  }
}

class BitsRangedAccessFloating extends BitVectorRangedAccessFloating{
  override def getTypeObject = TypeBits
  override def opName: String = "extract(b,u,w)"
  override def bitVectorRangedAccessFixedFactory: BitVectorRangedAccessFixed = new BitsRangedAccessFixed
}
class UIntRangedAccessFloating extends BitVectorRangedAccessFloating{
  override def getTypeObject = TypeUInt
  override def opName: String = "extract(u,u,w)"
  override def bitVectorRangedAccessFixedFactory: BitVectorRangedAccessFixed = new UIntRangedAccessFixed
}
class SIntRangedAccessFloating extends BitVectorRangedAccessFloating{
  override def getTypeObject = TypeSInt
  override def opName: String = "extract(s,u,w)"
  override def bitVectorRangedAccessFixedFactory: BitVectorRangedAccessFixed = new SIntRangedAccessFixed
}

////object AssignedBits {
////  def apply() = new AssignedBits
////  def apply(bitId: Int): AssignedBits = {
////    val ab = new AssignedBits
////    ab.add(new AssignedRange(bitId, bitId))
////    ab
////  }
////  def apply(hi: Int, lo: Int): AssignedBits = {
////    val ab = new AssignedBits
////    ab.add(new AssignedRange(hi, lo))
////    ab
////  }
////
////  def union(a: AssignedBits, b: AssignedBits): AssignedBits = {
////    val ret = AssignedBits()
////
////    ret.add(a)
////    ret.add(b)
////
////    ret
////  }
////
////
////  def intersect(a: AssignedBits, b: AssignedBits): AssignedBits = {
////    val ret = AssignedBits()
////    ret.value = a.value & b.value
////    ret
////  }
////
////
////}
//
//object AssignedBits {
//  //  def apply(bitId: Int): AssignedBits = {
//  //    val ab = new AssignedBits
//  //    ab.add(new AssignedRange(bitId, bitId))
//  //    ab
//  //  }
//  //  def apply(hi: Int, lo: Int): AssignedBits = {
//  //    val ab = new AssignedBits
//  //    ab.add(new AssignedRange(hi, lo))
//  //    ab
//  //  }
//  def union(a: AssignedBits, b: AssignedBits): AssignedBits = {
//    val ret = new AssignedBits(a.width)
//
//    ret.add(a)
//    ret.add(b)
//
//    ret
//  }
//
//
//  def intersect(a: AssignedBits, b: AssignedBits): AssignedBits = {
//    val ret = a.clone()
//    ret.intersect(b)
//    ret
//  }
//
//
//  def intersect(a: AssignedRange, b: AssignedBits): AssignedBits = {
//    val ret = b.clone()
//    ret.intersect(a)
//    ret
//  }
//  def intersect(a: AssignedBits, b: AssignedRange): AssignedBits = intersect(b,a)
//
//
//
//}
//
//object AssignedRange{
//  def apply(hi : Int,lo : Int) = new AssignedRange(hi,lo)
//  def apply(bit : Int) = new AssignedRange(bit,bit)
//  def apply() = new AssignedRange(-1,-1)
//}
//
//class AssignedRange(val hi: Int, val lo: Int) {
//  def toBigInt = ((BigInt(1) << (hi + 1 - lo)) - 1) << lo
//  def toAssignedBits = {
//    val ret = new AssignedBits(hi + 1)
//    ret.add(this)
//    ret
//  }
//}
////
////class AssignedBits(val width : Int) {
////  var value: BigInt = 0
////  override def clone() : AssignedBits = {
////    val ret = new AssignedBits(width)
////    ret.value = value
////    ret
////  }
////  def intersect(range: AssignedRange): Unit = {
////    value = value & range.toBigInt
////  }
////  def intersect(range: AssignedBits): Unit = {
////    value = value & range.value
////  }
////  def add(range: AssignedRange): Unit = {
////    value = value | range.toBigInt
////  }
////  def add(range: AssignedBits): Unit = {
////    value = value | range.value
////  }
////  def remove(range: AssignedRange): Unit = {
////    value = value &~ range.toBigInt
////  }
////  def remove(range: AssignedBits): Unit = {
////    value = value &~ range.value
////  }
////
////  def isEmpty = value == 0
////  def toBinaryString : String = value.toString(2)
////}
////
//class AssignedBits(val width : Int) {
//  def bitPerIndex = 32
//  var value = new Array[Int]((width+bitPerIndex-1)/bitPerIndex)
//
//  override def clone() : AssignedBits = {
//    val ret = new AssignedBits(width)
//    var idx = value.length
//    while(idx != 0){
//      idx -= 1
//      ret.value(idx) = this.value(idx)
//    }
//    ret
//  }
//
//  def isIntersecting(range: AssignedRange): Boolean = {
//    if(range.hi >= width)
//      assert(false)
//    var idx = value.length
//    while(idx != 0){
//      idx -= 1
//      val hi = Math.min(range.hi - idx*bitPerIndex,bitPerIndex-1)
//      val lo = Math.max(range.lo - idx*bitPerIndex,0)
//      if(hi >= lo) {
//        if((this.value(idx) & (((1l << (hi + 1)) - 1) - ((1l << lo) - 1)).toInt) != 0) {
//          return true
//        }
//      }
//    }
//    return false
//  }
//
//
//
//  def + (that : AssignedRange) : AssignedBits = {
//    val ret = clone()
//    ret.add(that)
//    ret
//  }
//
//  def intersect(range: AssignedBits): Unit = {
//    assert(range.width == this.width)
//    var idx = Math.min(value.length,range.value.length)
//    while(idx != 0){
//      idx -= 1
//      this.value(idx) &= range.value(idx)
//    }
//  }
//
//  def add(range: AssignedBits): Unit = {
//    assert(range.width == this.width)
//    var idx = Math.min(value.length,range.value.length)
//    while(idx != 0){
//      idx -= 1
//      this.value(idx) |= range.value(idx)
//    }
//  }
//  def remove(range: AssignedBits): Unit = {
//    assert(range.width == this.width)
//    var idx = Math.min(value.length,range.value.length)
//    while(idx != 0){
//      idx -= 1
//      this.value(idx) &= ~range.value(idx)
//    }
//  }
//  def intersect(range: AssignedRange): Unit = {
//    assert(range.hi < width)
//    var idx = value.length
//    while(idx != 0){
//      idx -= 1
//      val hi = Math.min(range.hi - idx*bitPerIndex,bitPerIndex-1)
//      val lo = Math.max(range.lo - idx*bitPerIndex,0)
//      if(hi >= lo)
//        this.value(idx) &= (((1l << (hi+1))-1)-((1l << lo)-1)).toInt
//    }
//  }
//  def add(range: AssignedRange): Unit = {
//    assert(range.hi < width)
//    var idx = value.length
//    while(idx != 0){
//      idx -= 1
//      val hi = Math.min(range.hi - idx*bitPerIndex,bitPerIndex-1)
//      val lo = Math.max(range.lo - idx*bitPerIndex,0)
//      if(hi >= lo)
//        this.value(idx) |= (((1l << (hi+1))-1)-((1l << lo)-1)).toInt
//    }
//  }
//  def remove(range: AssignedRange): Unit = {
//    assert(range.hi < width)
//    var idx = value.length
//    while(idx != 0){
//      idx -= 1
//      val hi = Math.min(range.hi - idx*bitPerIndex,bitPerIndex-1)
//      val lo = Math.max(range.lo - idx*bitPerIndex,0)
//      if(hi >= lo)
//        this.value(idx) &= ~(((1l << (hi+1))-1)-((1l << lo)-1)).toInt
//    }
//  }
//
//  def toBinaryString : String = {
//    val strs = for((e,idx) <- value.zipWithIndex.reverseIterator) yield {
//      val str = e.toBinaryString
//      val eWidth = if(idx == value.length-1) width-idx*bitPerIndex else 32
//
//      "0"*(eWidth-str.length) + str
//    }
//    strs.reduce(_ + "_" + _)
//  }
//  def isEmpty = value.foldLeft(true)((c,e) => c && (e == 0))
//  def isFull : Boolean = {
//    for(i <- 0 to value.length-2){
//      if(value(i) != 0xFFFFFFFF) return false
//    }
//    if(value.last != (1 << (width.toLong % 32))-1) return false
//    true
//  }
//}
//
abstract class AssignementExpression extends Expression {
  def finalTarget: BaseType
  override def foreachDrivingExpression(func : (Expression) => Unit) : Unit
  override def remapDrivingExpressions(func: (Expression) => Expression): Unit
//  def getAssignedBits: AssignedRange //Bit that are allwas assigned
//  def getScopeBits: AssignedRange //Bit tht could be assigned
//  def getOutBaseType: BaseType
//
//  def clone(out : Node) : this.type
}




abstract class BitVectorAssignementExpression extends AssignementExpression{
  override def getTypeObject = throw new Exception("Doesn't make sense")
  def minimalTargetWidth : Int
}

object BitAssignmentFixed{
  def apply(out: BitVector, bitId: Int ) = {
    val ret = new BitAssignmentFixed
    ret.out = out
    ret.bitId = bitId
    ret
  }
}
class BitAssignmentFixed() extends BitVectorAssignementExpression with ScalaLocated{
  var out: BitVector = null
  var bitId: Int = -1

  override def finalTarget: BaseType = out

  override def minimalTargetWidth: Int = bitId + 1

  override def normalizeInputs: Unit = {
    if (bitId < 0 || bitId >= out.getWidth) {
      PendingError(s"Static bool extraction (bit ${bitId}) is outside the range (${out.getWidth - 1} downto 0) of ${out} at\n${getScalaLocationLong}")
    }
  }

  override def foreachExpression(func: (Expression) => Unit): Unit = func(out)
  override def remapExpressions(func: (Expression) => Expression): Unit = {out = func(out).asInstanceOf[BitVector]}
  override def foreachDrivingExpression(func: (Expression) => Unit): Unit = {}
  override def remapDrivingExpressions(func: (Expression) => Expression): Unit = {}
  override def toString(): String = s"${out.toString()}[$bitId]"
  override def opName: String = "x(index) <="


  //  override def checkInferedWidth: Unit = {
  //    if (bitId < 0 || bitId >= out.getWidth) {
  //      PendingError(s"Static bool extraction (bit ${bitId}) is outside the range (${out.getWidth - 1} downto 0) of ${out} at\n${getScalaLocationLong}")
  //    }
  //  }
  //
  //  def getAssignedBits: AssignedRange = AssignedRange(bitId)
  //  def getScopeBits: AssignedRange = getAssignedBits
  //  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = {
  //    if(outHi >= bitId && bitId >= outLo)
  //      (0,0)
  //    else
  //      (-1,0)
  //  }
  //  def getOutBaseType: BaseType = out
  //
  //  override def clone(out: Node): this.type = new BitAssignmentFixed(out.asInstanceOf[BitVector],in,bitId).asInstanceOf[this.type]

}


object RangedAssignmentFixed{
  def apply(out: BitVector,hi: Int,lo: Int): RangedAssignmentFixed ={
    val assign = new RangedAssignmentFixed
    assign.out = out
    assign.hi = hi
    assign.lo = lo
    assign
  }
}

class RangedAssignmentFixed() extends BitVectorAssignementExpression with WidthProvider {
  var out: BitVector = null
  var hi = -1
  var lo = 0
  override def getWidth: Int = hi + 1 - lo
  override def finalTarget: BaseType = out
  override def minimalTargetWidth: Int = hi+1

  override def normalizeInputs: Unit = {
    if (hi >= out.getWidth || lo < 0) {
      PendingError(s"Static bits assignment ($hi downto $lo) is outside the range (${out.getWidth - 1} downto 0) of ${out} at\n${getScalaLocationLong}")
      return
    }
  }

  override def foreachExpression(func: (Expression) => Unit): Unit = func(out)
  override def remapExpressions(func: (Expression) => Expression): Unit = {out = func(out).asInstanceOf[BitVector]}
  override def foreachDrivingExpression(func : (Expression) => Unit) : Unit = {}
  override def remapDrivingExpressions(func: (Expression) => Expression): Unit = {}
  override def toString(): String = s"${out.toString()}[$hi downto $lo]"
  override def opName: String = "x(hi:lo) <="



  //  def getAssignedBits: AssignedRange = AssignedRange(hi, lo)
  //  def getScopeBits: AssignedRange = getAssignedBits
  //  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = {
  //    val relativeLo = outLo-lo
  //    val relativeHi = outHi-lo
  //    if(relativeHi >= 0 && hi-lo >= relativeLo)
  //      super.getOutToInUsage(inputId,Math.min(relativeHi,hi-lo),Math.max(relativeLo,0))
  //    else
  //      (-1,0)
  //  }
  //  def getOutBaseType: BaseType = out
  //  override def clone(out: Node): this.type = new RangedAssignmentFixed(out.asInstanceOf[BitVector],in,hi,lo).asInstanceOf[this.type]
}


object BitAssignmentFloating{
  def apply(out: BitVector,bitId: UInt): BitAssignmentFloating ={
    val assign = new BitAssignmentFloating
    assign.out = out
    assign.bitId = bitId
    assign
  }
}
class BitAssignmentFloating() extends BitVectorAssignementExpression{
  var out  : BitVector = null
  var bitId  : Expression with WidthProvider = null

  override def finalTarget: BaseType = out
  override def minimalTargetWidth: Int = 1 << Math.min(20,bitId.getWidth)
  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(out)
    func(bitId)
  }
  override def remapExpressions(func: (Expression) => Expression): Unit = {
    out = func(out).asInstanceOf[BitVector]
    bitId = func(bitId).asInstanceOf[Expression with WidthProvider]

  }
  override def foreachDrivingExpression(func: (Expression) => Unit): Unit = {
    func(bitId)
  }
  override def remapDrivingExpressions(func: (Expression) => Expression): Unit = {
    bitId = func(bitId).asInstanceOf[Expression with WidthProvider]
  }
  override def toString(): String = s"${out.toString()}[$bitId]"
  override def opName: String = "x(uIndex) <="

  override def simplifyNode: Expression = {
    if(bitId.getWidth == 0) {
      BitAssignmentFixed(out, 0)
    }else
      this
  }
//  def getAssignedBits: AssignedRange = AssignedRange()
//  def getScopeBits: AssignedRange = AssignedRange(Math.min(out.getWidth-1,(1 << Math.min(20,bitId.getWidth)) - 1), 0)
//
//  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
//    case 0 =>
//      if(outHi >= 0 && ((1 << Math.min(20,bitId.getWidth)) - 1) >= outLo)
//        (0,0)
//      else
//        (-1,0)
//    case 1 =>
//      if(outHi >= 0 && ((1 << Math.min(20,bitId.getWidth)) - 1) >= outLo)
//        super.getOutToInUsage(inputId,outHi,outLo)
//      else
//        (-1,0)
//  }
//  def getOutBaseType: BaseType = out
//  override def clone(out: Node): this.type = new BitAssignmentFloating(out.asInstanceOf[BitVector],in_,bitId_).asInstanceOf[this.type]
}

object RangedAssignmentFloating{
  def apply(out: BitVector,offset: UInt,bitCount: Int): RangedAssignmentFloating ={
    val assign = new RangedAssignmentFloating
    assign.out = out
    assign.offset = offset
    assign.bitCount = bitCount
    assign
  }
}

class RangedAssignmentFloating() extends BitVectorAssignementExpression with WidthProvider {
  var out  : BitVector = null
  var offset  : Expression with WidthProvider = null
  var bitCount : Int = -1

  override def normalizeInputs: Unit = {
    if (out.getWidth < bitCount) {
      PendingError(s"Dynamic bits assignment of $bitCount bits is outside the range (${out.getWidth - 1} downto 0) of ${out} at\n${getScalaLocationLong}")
      return
    }
  }

  override def simplifyNode: Expression = {
    if(offset.getWidth == 0) {
      RangedAssignmentFixed(out, bitCount-1, 0)
    }else
      this
  }

  override def getWidth: Int = bitCount
  override def finalTarget: BaseType = out
  override def minimalTargetWidth: Int = 1 << Math.min(20,offset.getWidth) + bitCount
  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(out)
    func(offset)
  }
  override def remapExpressions(func: (Expression) => Expression): Unit = {
    out = func(out).asInstanceOf[BitVector]
    offset = func(offset).asInstanceOf[Expression with WidthProvider]

  }
  override def foreachDrivingExpression(func: (Expression) => Unit): Unit = {
    func(offset)
  }
  override def remapDrivingExpressions(func: (Expression) => Expression): Unit = {
    offset = func(offset).asInstanceOf[Expression with WidthProvider]
  }

  override def opName: String = "x(hi:lo) <="

//  override def normalizeInputs: Unit = {
//    InputNormalize.resizedOrUnfixedLit(this,0,bitCount.value)
//  }
//
//
//
//  def getAssignedBits: AssignedRange = AssignedRange()
//  //TODO should not use constructor node ref
//  def getScopeBits: AssignedRange = AssignedRange(Math.min(out.getWidth-1,(1 << Math.min(20,offset_.asInstanceOf[Node with WidthProvider].getWidth))+ bitCount.value - 1), 0) //TODO dirty offset_
//  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = super.getOutToInUsage(inputId,outHi,outLo) //TODO
//  def getOutBaseType: BaseType = out
//  override def clone(out: Node): this.type = new RangedAssignmentFloating(out.asInstanceOf[BitVector],in_,offset_,bitCount).asInstanceOf[this.type]
}


//object MultipleAssignmentNode{
//  def newFor(that : BaseType) : MultipleAssignmentNode = that match{
//    case that : BitVector => new MultipleAssignmentNodeWidthable
//    case that : SpinalEnumCraft[_] => new MultipleAssignmentNodeEnum(that.spinalEnum)
//    case _ => new MultipleAssignmentNode
//  }
//}
//
//class MultipleAssignmentNode extends Node with AssignementTreePart{
//  type T <: Node
//  val inputs = new ArrayBuffer[T](4)
//
//  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)
//
//  override def getInputsCount = inputs.length
//  override def getInput(id : Int) : Node = inputs(id)
//  override def setInput(id : Int,node : Node) : Unit = inputs(id) = node.asInstanceOf[T]
//
//  override def getInputs : Iterator[Node] = inputs.iterator
//
//  override def onEachInput(doThat : (Node,Int) => Unit) : Unit = {
//    var idx = inputs.length
//    while(idx != 0){
//      idx -= 1
//      doThat(getInput(idx),idx)
//    }
//  }
//
//  override def onEachInput(doThat : (Node) => Unit) : Unit = {
//    var idx = inputs.length
//    while(idx != 0){
//      idx -= 1
//      doThat(getInput(idx))
//    }
//  }
//
//  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = (outHi,outLo)
//
//  var inputsThrowable : Array[Throwable] = null
//  override def getAssignementContext(id: Int): Throwable =
//    ArrayManager.getElseNull(inputsThrowable,id)
//  override def setAssignementContext(id: Int,that : Throwable = globalData.getThrowable()): Unit =
//    inputsThrowable = ArrayManager.setAllocate(inputsThrowable,id,that)
//
//
//  def cloneMultipleAssignmentNode : this.type = new MultipleAssignmentNode().asInstanceOf[this.type]
//}
//
//
//class MultipleAssignmentNodeWidthable extends MultipleAssignmentNode with Widthable with CheckWidth{
//  override type T = Node with WidthProvider
//  override def calcWidth: Int = WidthInfer.multipleAssignmentNodeWidth(this)
//  override def normalizeInputs: Unit = {
//    for (i <- 0 until inputs.length)
//      InputNormalize.resizedOrUnfixedLit(this,i,this.getWidth)
//  }
//
//  override private[core] def checkInferedWidth: Unit = {
//    for(i <- 0 until inputs.length){
//      val input = inputs(i)
//      if (input != null && input.component != null && this.getWidth !=input.getWidth) {
//        PendingError(s"Assignement bit count missmatch. ${AssignementTree.getDrivedBaseType(this)} := ${input}} at\n${ScalaLocated.long(getAssignementContext(i))}")
//      }
//    }
//  }
//
//  override def cloneMultipleAssignmentNode : this.type = new MultipleAssignmentNodeWidthable().asInstanceOf[this.type]
//}
//
//class MultipleAssignmentNodeEnum(enumDef : SpinalEnum) extends MultipleAssignmentNode with InferableEnumEncodingImpl{
//  override type T = Node with EnumEncoded
//  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
//  override def getDefinition: SpinalEnum = enumDef
//  override private[core] def normalizeInputs: Unit = {
//    InputNormalize.enumImpl(this)
//  }
//  override def cloneMultipleAssignmentNode : this.type = new MultipleAssignmentNodeEnum(enumDef).asInstanceOf[this.type]
//}
//
object AssertNode{
  def apply(cond : Bool,message : Seq[Any],severity: AssertNodeSeverity) : Unit = {
    val node = AssertStatement(cond, message,severity)
    Component.current.addStatement(node)
  }
  def apply(cond : Bool,message : String,severity: AssertNodeSeverity) : Unit = AssertNode(cond, List(message),severity)
}
//
trait AssertNodeSeverity
object NOTE     extends AssertNodeSeverity
object WARNING  extends AssertNodeSeverity
object ERROR    extends AssertNodeSeverity
object FAILURE  extends AssertNodeSeverity

case class AssertStatement(var cond : Expression, message : Seq[Any],severity : AssertNodeSeverity) extends LeafStatement with ContextUser {
  override def normalizeInputs: Unit = {}

  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(cond)
    message.foreach(_ match {
      case e : Expression => func(e)
      case _ =>
    })
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    cond = func(cond) //TODO message
  }
}