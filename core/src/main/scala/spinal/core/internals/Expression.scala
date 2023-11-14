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
package spinal.core.internals

import spinal.core._
import spinal.idslplugin.Location

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


trait Expression extends BaseNode with ExpressionContainer {

  def opName: String
  def simplifyNode: Expression = this
  def getTypeObject: Any

  private[core] def foreachDrivingExpression(outHi: Int, outLo: Int)(f: (Expression, Int, Int) => Unit): Unit = foreachDrivingExpression{
    case input: Expression with WidthProvider => f(input, input.getWidth-1, 0)
    case input                                => f(input, 0, 0)
  }

  override def toString = opName
  def toStringRec(level : Int = 1) : String = toString
}


trait ExpressionContainer {

  def normalizeInputs: Unit = {}

  //Used by remapExpressions to propagate the remap if changes append
  def stabilized(func: (Expression) => Expression, seed : Expression): Expression = {
    var e = seed
    while(true) {
      val old = e
      e = func(e)
      if(old == e) return e
    }
    return e
  }

  def remapExpressions(func: (Expression) => Expression): Unit
  def remapDrivingExpressions(func: (Expression) => Expression): Unit = remapExpressions(func)

  def foreachExpression(func: (Expression) => Unit): Unit
  def foreachDrivingExpression(func: (Expression) => Unit): Unit = foreachExpression(func)

  def walkExpression(func: (Expression) => Unit): Unit = {
    foreachExpression(e => {
      func(e)
      e.walkExpression(func)
    })
  }

  // Traverse the subtrees first before accessing the parent node
  def walkExpressionPostorder(func: (Expression) => Unit): Unit = {
    foreachExpression(e => {
      e.walkExpressionPostorder(func)
      func(e)
    })
  }

  def walkDrivingExpressions(func: (Expression) => Unit): Unit = {
    foreachDrivingExpression(e => {
      func(e)
      e.walkDrivingExpressions(func)
    })
  }

  def walkRemapExpressions(func: (Expression) => Expression): Unit = {
    remapExpressions(func)
    foreachExpression(e => {
      e.walkRemapExpressions(func)
    })
  }

  def walkRemapDrivingExpressions(func: (Expression) => Expression): Unit = {
    remapDrivingExpressions(func)
    foreachDrivingExpression(e => {
      e.walkRemapDrivingExpressions(func)
    })
  }
}


abstract class AnalogDriver extends Expression {
  type T <: Expression

  var data: T = null.asInstanceOf[T]
  var enable: Expression = null

  def foreachExpression(func: (Expression) => Unit): Unit = {
    func(data)
    func(enable)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    data   = stabilized(func, data).asInstanceOf[T]
    enable = stabilized(func, enable)
  }

  override def toStringMultiLine() = {
    s"""$this
       |- data  operand : $data
       |- enable operand : $enable
       |""".stripMargin
  }
}


abstract class Resize extends Expression with WidthProvider {
  var size: Int = -1
  var input: Expression with WidthProvider = null

  override def getWidth: Int = size

  override def simplifyNode: Expression = {
    input.getWidth match {
      case 0 => getLiteralFactory(0, size)
      case s if s == size => input //Useless resize
      case _ => this
    }
  }

  def getLiteralFactory: (BigInt, Int) => Expression

  override def foreachExpression(func: (Expression) => Unit): Unit = func(input)
  override def remapExpressions(func: (Expression) => Expression): Unit = input = stabilized(func, input).asInstanceOf[Expression with WidthProvider]
}


class ResizeBits extends Resize {
  override def getTypeObject = TypeBits
  override def opName: String = s"resize(Bits,$size bits)"
  override def getLiteralFactory: (BigInt, Int) => Expression = BitsLiteral.apply
}


class ResizeUInt extends Resize {
  override def getTypeObject = TypeUInt
  override def opName: String = s"resize(UInt,$size bits)"
  override def getLiteralFactory: (BigInt, Int) => Expression = UIntLiteral.apply
}


class ResizeSInt extends Resize {
  override def getTypeObject = TypeSInt
  override def opName: String = s"resize(SInt,$size bits)"
  override def getLiteralFactory: (BigInt, Int) => Expression = SIntLiteral.apply
}


abstract class Operator extends Modifier {}


abstract class UnaryOperator extends Operator {
  type T <: Expression

  var source: T = null.asInstanceOf[T]

  def foreachExpression(func: (Expression) => Unit) : Unit = {
    func(source)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    source = stabilized(func, source).asInstanceOf[T]
  }
}


abstract class UnaryOperatorWidthableInputs extends UnaryOperator with Widthable {
  override type T = Expression with WidthProvider
}


abstract class ConstantOperator extends Operator {
  type T <: Expression

  var source: T = null.asInstanceOf[T]

  def foreachExpression(func: (Expression) => Unit): Unit = {
    func(source)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    source = stabilized(func, source).asInstanceOf[T]
  }
}

abstract class ConstantOperatorWidthableInputs extends ConstantOperator{
  override type T = Expression with WidthProvider
}

abstract class BinaryOperator extends Operator {
  type T <: Expression

  var left, right: T = null.asInstanceOf[T]

  def foreachExpression(func: (Expression) => Unit): Unit = {
    func(left)
    func(right)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    left = stabilized(func, left).asInstanceOf[T]
    right = stabilized(func, right).asInstanceOf[T]
  }

  override def toStringMultiLine() = {
    s"""$this
       |- Left  operand : $left
       |- Right operand : $right
       |""".stripMargin
  }
}


abstract class BinaryOperatorWidthableInputs extends BinaryOperator {
  override type T = Expression with WidthProvider

  def checkLiteralRange(check : (BitVectorLiteral, Expression with WidthProvider) => Unit): Unit ={
    def rec(that : Expression): Expression =  that match {
      case bt : BaseType if bt.isTypeNode && bt.hasOnlyOneStatement => bt.head match {
        case DataAssignmentStatement(target, source) if target == bt => source match {
          case lit: Literal => lit
          case _ => that
        }
        case _ => that
      }
      case _ => that
    }

    (rec(left), rec(right)) match {
      case (_ : BitVectorLiteral, _ : BitVectorLiteral) =>
      case (lit : BitVectorLiteral, value : T) => check(lit, value)
      case (value : T, lit : BitVectorLiteral) => check(lit, value)
      case _ =>
    }
  }

  def checkLiteralRanges(signed  : Boolean) : Unit = {
    if(globalData.config.allowOutOfRangeLiterals) return
    this match {
      case tr : SpinalTagReady => if(tr.hasTag(allowOutOfRangeLiterals)) return
      case _ =>
    }
    checkLiteralRange { (lit, value) =>
      if(lit.poisonMask == null && lit.getWidth > value.getWidth) {
        PendingError(s"OUT OF RANGE CONSTANT. Operator ${this.toStringMultiLine} is checking a value against a out of range constant\n${this.getScalaLocationLong}")
      }
    }
  }
}


object InferWidth
{
  def canBeResized(that : Expression) = that match {
    case that: SpinalTagReady => that.hasTag(tagAutoResize)
    case _                    => false
  }

  def notResizableElseMax(op: BinaryOperatorWidthableInputs): Int = {
    val leftR  = canBeResized(op.left)
    val rightR = canBeResized(op.right)

    if(leftR != rightR){
      if(leftR) op.right.getWidth else op.left.getWidth
    } else {
      Math.max(op.left.getWidth, op.right.getWidth)
    }
  }

  def notResizableElseMax(op: MultiplexerWidthable): Int = {
    var resizableMax, notResizableMax = -1

    op.inputs.foreach{
      case e if canBeResized(e) => resizableMax = Math.max(resizableMax, e.getWidth)
      case e                    => notResizableMax = Math.max(notResizableMax, e.getWidth)
    }

    if(notResizableMax != -1) {
      notResizableMax
    }else {
      resizableMax
    }
  }


  def notResizableElseMax(op: BinaryMultiplexerWidthable): Int = {
    var resizableMax, notResizableMax = -1

    List(op.whenTrue, op.whenFalse).foreach {
      case e if canBeResized(e) => resizableMax = Math.max(resizableMax, e.getWidth)
      case e => notResizableMax = Math.max(notResizableMax, e.getWidth)
    }

    if (notResizableMax != -1) {
    notResizableMax
    }else {
      resizableMax
    }
  }
}




/**
  * Define all operator for each type
  */
object Operator {
  object Formal{

    class RandomExpKind
    val RANDOM_ANY_SEQ = new RandomExpKind()
    val RANDOM_ANY_CONST = new RandomExpKind()
    val RANDOM_ALL_SEQ = new RandomExpKind()
    val RANDOM_ALL_CONST = new RandomExpKind()
    abstract class RandomExp(val kind : RandomExpKind) extends Expression{
      override def remapExpressions(func: Expression => Expression) = {}
      override def foreachExpression(func: Expression => Unit) = {}
      override def opName: String = "$random()"
    }
    class RandomExpBool(kind : RandomExpKind) extends RandomExp(kind) {
      override def getTypeObject = TypeBool
    }
    abstract class RandomExpBitVector(kind : RandomExpKind, val width : Int) extends RandomExp(kind) with WidthProvider {
      override def getWidth = width
    }
    class RandomExpBits(kind : RandomExpKind, width : Int) extends RandomExpBitVector(kind, width) {
      override def getTypeObject = TypeBits
    }
    class RandomExpUInt(kind : RandomExpKind, width : Int) extends RandomExpBitVector(kind, width) {
      override def getTypeObject = TypeUInt
    }
    class RandomExpSInt(kind : RandomExpKind, width : Int) extends RandomExpBitVector(kind, width) {
      override def getTypeObject = TypeSInt
    }
    class RandomExpEnum(var enumDef: SpinalEnum, kind : RandomExpKind) extends RandomExp(kind) with InferableEnumEncodingImpl{
      override def getTypeObject = TypeEnum
      override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
      override def getDefinition: SpinalEnum = enumDef
      override def swapEnum(e: SpinalEnum) = enumDef = e
    }

    abstract class Past(val delay : Int) extends UnaryOperator

    class PastBool(delay : Int) extends Past(delay) {
      override def getTypeObject = TypeBool
      override def opName: String = "$past(Bool)"
    }

    abstract class PastBitvector(delay : Int) extends Past(delay) with Widthable {
      override type T = Expression with WidthProvider
      override private[core] def calcWidth = source.getWidth
    }

    class PastBits(delay : Int) extends PastBitvector(delay) {
      override def getTypeObject = TypeBits
      override def opName: String = "$past(Bits)"
    }

    class PastUInt(delay : Int) extends PastBitvector(delay) {
      override def getTypeObject = TypeUInt
      override def opName: String = "$past(UInt)"
    }

    class PastSInt(delay : Int) extends PastBitvector(delay) {
      override def getTypeObject = TypeSInt
      override def opName: String = "$past(SInt)"
    }

    class PastEnum(var enumDef: SpinalEnum, delay : Int) extends Past(delay)  with InferableEnumEncodingImpl{
      override def getTypeObject = TypeEnum
      override def opName: String = "$past(Enum)"

      override def normalizeInputs: Unit = {InputNormalize.enumImpl(this)}

      override type T = Expression with EnumEncoded
      override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
      override def getDefinition: SpinalEnum = enumDef
      override def swapEnum(e: SpinalEnum) = enumDef = e
    }


    class Rose extends UnaryOperator{
      override def opName: String = "$rose(Bool)"
      override def getTypeObject: Any = TypeBool
    }

    class Fell extends UnaryOperator{
      override def opName: String = "$fell(Bool)"
      override def getTypeObject: Any = TypeBool
    }



    class Changed extends UnaryOperator{
      override def getTypeObject = TypeBool
      override def opName: String = "!$stable(...)"
    }


    class Stable extends UnaryOperator{
      override def getTypeObject = TypeBool
      override def opName: String = "$stable(...)"
    }

    class InitState extends Expression{
      override def remapExpressions(func: Expression => Expression): Unit = {}
      override def foreachExpression(func: Expression => Unit): Unit = {}

      override def getTypeObject = TypeBool
      override def opName: String = "$initstate(...)"
    }
  }

  /**
    * Bool operator
    */
  object Bool {

    class And extends BinaryOperator {
      override def getTypeObject = TypeBool
      override def opName: String = "Bool && Bool"
    }

    class Or extends BinaryOperator {
      override def getTypeObject = TypeBool
      override def opName: String = "Bool || Bool"
    }

    class Xor extends BinaryOperator {
      override def getTypeObject = TypeBool
      override def opName: String = "Bool ^ Bool"
    }

    class Not extends UnaryOperator {
      override def getTypeObject = TypeBool
      override def opName: String = "! Bool"
    }

    class Equal extends BinaryOperator {
      override def getTypeObject = TypeBool
      override def opName: String = "Bool === Bool"
    }

    class NotEqual extends BinaryOperator {
      override def getTypeObject = TypeBool
      override def opName: String = "Bool =/= Bool"
    }
  }


  /**
    * BitVector operator
    */
  object BitVector {
    class orR extends UnaryOperator {
      override type T = Expression with WidthProvider
      override def getTypeObject = TypeBool
      override def opName: String = "| Bits"
      override def simplifyNode = if(source.getWidth == 0) new BoolLiteral(false) else this
    }

    class andR extends UnaryOperator {
      override type T = Expression with WidthProvider
      override def getTypeObject = TypeBool
      override def opName: String = "& Bits"
      override def simplifyNode = if(source.getWidth == 0) new BoolLiteral(true) else this
    }

    class xorR extends UnaryOperator {
      override type T = Expression with WidthProvider
      override def getTypeObject = TypeBool
      override def opName: String = "^ Bits"
      override def simplifyNode = if(source.getWidth == 0) new BoolLiteral(false) else this
    }

    abstract class And extends BinaryOperatorWidthableInputs with Widthable {
      def resizeFactory: Resize
      override def calcWidth: Int = InferWidth.notResizableElseMax(this)
      override def normalizeInputs: Unit = {
        val targetWidth = getWidth
        left  = InputNormalize.resizedOrUnfixedLit(left, targetWidth, resizeFactory, this, this)
        right = InputNormalize.resizedOrUnfixedLit(right, targetWidth, resizeFactory, this, this)
      }
      override def toString() = s"(${super.toString()})[$getWidth bits]"
    }

    abstract class Or extends BinaryOperatorWidthableInputs with Widthable {
      def resizeFactory: Resize
      override def calcWidth: Int = InferWidth.notResizableElseMax(this)
      override def normalizeInputs: Unit = {
        val targetWidth = getWidth
        left  = InputNormalize.resizedOrUnfixedLit(left, targetWidth, resizeFactory, this, this)
        right = InputNormalize.resizedOrUnfixedLit(right, targetWidth, resizeFactory, this, this)
      }
      override def toString() = s"(${super.toString()})[$getWidth bits]"
    }

    abstract class Xor extends BinaryOperatorWidthableInputs with Widthable {
      def resizeFactory: Resize
      override def calcWidth: Int = InferWidth.notResizableElseMax(this)
      override def normalizeInputs: Unit = {
        val targetWidth = getWidth
        left  = InputNormalize.resizedOrUnfixedLit(left, targetWidth, resizeFactory, this, this)
        right = InputNormalize.resizedOrUnfixedLit(right, targetWidth, resizeFactory, this, this)
      }
      override def toString() = s"(${super.toString()})[$getWidth bits]"
    }


    abstract class Add extends BinaryOperatorWidthableInputs with Widthable {
      def resizeFactory: Resize
      override def calcWidth: Int = InferWidth.notResizableElseMax(this)
      override def normalizeInputs: Unit = {
        val targetWidth = getWidth
        left  = InputNormalize.resize(left, targetWidth, resizeFactory)
        right = InputNormalize.resize(right, targetWidth, resizeFactory)
      }

      override def toString() = s"(${super.toString()})[$getWidth bits]"
    }

    abstract class Sub extends BinaryOperatorWidthableInputs with Widthable {
      def resizeFactory: Resize
      override def calcWidth: Int = InferWidth.notResizableElseMax(this)
      override def normalizeInputs: Unit = {
        val targetWidth = getWidth
        left  = InputNormalize.resize(left, targetWidth, resizeFactory)
        right = InputNormalize.resize(right, targetWidth, resizeFactory)
      }
      override def toString() = s"(${super.toString()})[$getWidth bits]"
    }

    abstract class Mul extends BinaryOperatorWidthableInputs with Widthable {
      def getLiteralFactory: (BigInt, Int) => Expression
      override def calcWidth: Int = left.getWidth + right.getWidth
      override def simplifyNode: Expression = {SymplifyNode.binaryInductZeroWithOtherWidth(getLiteralFactory)(this)}
      override def toString() = s"(${super.toString()})[$getWidth bits]"
    }

    abstract class Div extends BinaryOperatorWidthableInputs with Widthable {
      override def calcWidth: Int = left.getWidth
      override def toString() = s"(${super.toString()})[$getWidth bits]"
    }

    abstract class Mod extends BinaryOperatorWidthableInputs with Widthable {
      override def calcWidth: Int = left.getWidth min right.getWidth
      override def toString() = s"(${super.toString()})[$getWidth bits]"
    }

    abstract class Equal extends BinaryOperatorWidthableInputs with ScalaLocated with SpinalTagReady {
      override def getTypeObject = TypeBool
      override def normalizeInputs: Unit
      override def simplifyNode: Expression = {SymplifyNode.binaryThatIfBoth(new BoolLiteral(true))(this)}
    }

    abstract class NotEqual extends BinaryOperatorWidthableInputs with ScalaLocated with SpinalTagReady {
      override def getTypeObject = TypeBool
      override def normalizeInputs: Unit
      override def simplifyNode: Expression = {SymplifyNode.binaryThatIfBoth(new BoolLiteral(false))(this)}
    }

    trait ShiftOperator

    abstract class ShiftRightByInt(val shift: Int) extends ConstantOperatorWidthableInputs with Widthable with ShiftOperator {
      if(shift < 0) {
        LocatedPendingError(s"NEGATIVE SHIFT RIGHT of $shift on $source at")
      }
      override def calcWidth: Int = Math.max(0, source.getWidth - shift)
      override def toString() = s"(${super.toString()})[$getWidth bits]"
      override def simplifyNode: Expression = {
        if(shift == 0){
          source
        } else {
          this
        }
      }
    }

    abstract class ShiftRightByUInt extends BinaryOperatorWidthableInputs with Widthable with ShiftOperator {
      override def calcWidth: Int = left.getWidth
      override def simplifyNode: Expression = if(right.getWidth == 0) left else this
      override def toString() = s"(${super.toString()})[$getWidth bits]"
    }

    abstract class ShiftLeftByInt(val shift : Int) extends ConstantOperatorWidthableInputs with Widthable with ShiftOperator {
      if(shift < 0) {
        LocatedPendingError(s"NEGATIVE SHIFT LEFT of $shift on $source at")
      }

      override def calcWidth: Int = source.getWidth + shift
      def getLiteralFactory: (BigInt, Int) => BitVectorLiteral
      override def simplifyNode: Expression = {
        if(source.getWidth == 0){
          getLiteralFactory(0, this.getWidth)
        } else if(shift == 0){
          source
        } else {
          this
        }
      }

      override def toString() = s"(${super.toString()})[$getWidth bits]"
    }

    abstract class ShiftLeftByUInt extends BinaryOperatorWidthableInputs with Widthable with ShiftOperator {
      override def calcWidth: Int = left.getWidth + (1 << right.getWidth.min(30)) - 1
      def getLiteralFactory: (BigInt, Int) => BitVectorLiteral
      override def simplifyNode: Expression = {
        if(left.getWidth == 0){
          getLiteralFactory(0, this.getWidth)
        }else if(right.getWidth == 0){
          left
        }else{
          this
        }
      }
      override def toString() = s"(${super.toString()})[$getWidth bits]"
    }


    abstract class ShiftRightByIntFixedWidth(val shift: Int) extends ConstantOperatorWidthableInputs with Widthable with ShiftOperator {
      assert(shift >= 0)
      override def calcWidth: Int = source.getWidth
      override def toString() = s"(${super.toString()})[$getWidth bits]"
    }

    abstract class ShiftLeftByIntFixedWidth(val shift: Int) extends ConstantOperatorWidthableInputs with Widthable with ShiftOperator {
      assert(shift >= 0)
      override def calcWidth: Int = source.getWidth
      override def toString() = s"(${super.toString()})[$getWidth bits]"

    }

    abstract class ShiftLeftByUIntFixedWidth extends BinaryOperatorWidthableInputs with Widthable with ShiftOperator {
      override def calcWidth: Int = left.getWidth
      override def simplifyNode: Expression = if(right.getWidth == 0) left else this
      override def toString() = s"(${super.toString()})[$getWidth bits]"
    }
  }


  /**
    * Bits operator
    */
  object Bits {

    class Cat extends BinaryOperatorWidthableInputs with Widthable {
      override def getTypeObject = TypeBits
      override def opName: String = s"Bits ## Bits"
      override def calcWidth: Int = left.getWidth + right.getWidth
      override def simplifyNode: Expression = {SymplifyNode.binaryTakeOther(this)}
    }

    class Not extends UnaryOperatorWidthableInputs {
      override def getTypeObject = TypeBits
      override def opName: String = "~ Bits"
      override def calcWidth: Int = source.getWidth
    }

    class And extends BitVector.And {
      override def getTypeObject = TypeBits
      override def opName: String = "Bits & Bits"
      def resizeFactory: Resize = new ResizeBits
    }

    class Or extends BitVector.Or {
      override def getTypeObject = TypeBits
      override def opName: String = "Bits | Bits"
      def resizeFactory: Resize = new ResizeBits
    }

    class Xor extends BitVector.Xor {
      override def getTypeObject = TypeBits
      override def opName: String = "Bits ^ Bits"
      def resizeFactory: Resize = new ResizeBits
    }

    class Equal extends BitVector.Equal {
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        checkLiteralRanges(false)
        left = InputNormalize.resizedOrUnfixedLit(left, targetWidth, new ResizeBits, right, this)
        right = InputNormalize.resizedOrUnfixedLit(right, targetWidth, new ResizeBits, left, this)
      }
      override def opName: String = "Bits === Bits"
    }

    class NotEqual extends BitVector.NotEqual {
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        checkLiteralRanges(false)
        left = InputNormalize.resizedOrUnfixedLit(left, targetWidth, new ResizeBits, right, this)
        right = InputNormalize.resizedOrUnfixedLit(right, targetWidth, new ResizeBits, left, this)
      }
      override def opName: String = "Bits =/= Bits"
    }

    class ShiftRightByInt(shift: Int) extends BitVector.ShiftRightByInt(shift){
      override def getTypeObject = TypeBits
      override def opName: String = "Bits >> Int"
    }

    class ShiftRightByUInt extends BitVector.ShiftRightByUInt {
      override def getTypeObject = TypeBits
      override def opName: String = "Bits >> UInt"
    }

    class ShiftLeftByInt(shift: Int) extends BitVector.ShiftLeftByInt(shift) {
      override def getTypeObject = TypeBits
      override def opName: String = "Bits << Int"
      override def getLiteralFactory: (BigInt, Int) => BitVectorLiteral = BitsLiteral.apply
    }

    class ShiftLeftByUInt extends BitVector.ShiftLeftByUInt {
      override def getTypeObject = TypeBits
      override def opName: String = "Bits << UInt"
      override def getLiteralFactory: (BigInt, Int) => BitVectorLiteral = BitsLiteral.apply
    }

    class ShiftRightByIntFixedWidth(shift: Int) extends BitVector.ShiftRightByIntFixedWidth(shift) {
      override def getTypeObject = TypeBits
      override def opName: String = "Bits |>> Int"
    }

    class ShiftLeftByIntFixedWidth(shift : Int) extends BitVector.ShiftLeftByIntFixedWidth(shift) {
      override def getTypeObject = TypeBits
      override def opName: String = "Bits |<< Int"
    }

    class ShiftLeftByUIntFixedWidth extends BitVector.ShiftLeftByUIntFixedWidth {
      override def getTypeObject = TypeBits
      override def opName: String = "Bits |<< UInt"
    }
  }


  /**
    * UInt operator
    */
  object UInt {

    class Not extends UnaryOperatorWidthableInputs {
      override def getTypeObject    = TypeUInt
      override def opName: String   = "~ UInt"
      override def calcWidth: Int = source.getWidth
    }

    class And extends BitVector.And {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt & UInt"
      def resizeFactory: Resize   = new ResizeUInt
    }

    class Or extends BitVector.Or {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt | UInt"
      def resizeFactory: Resize   = new ResizeUInt
    }

    class Xor extends BitVector.Xor {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt ^ UInt"
      def resizeFactory: Resize   = new ResizeUInt
    }

    class Add extends BitVector.Add {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt + UInt"
      def resizeFactory: Resize   = new ResizeUInt
    }

    class Sub extends BitVector.Sub {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt - UInt"
      def resizeFactory: Resize   = new ResizeUInt
    }

    class Mul extends BitVector.Mul {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt * UInt"
      override def getLiteralFactory: (BigInt, Int) => Expression = UIntLiteral.apply
    }

    class Div extends BitVector.Div {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt / UInt"
    }

    class Mod extends BitVector.Mod {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt % UInt"
    }

    class Smaller extends BinaryOperatorWidthableInputs with SpinalTagReady {
      override def getTypeObject  = TypeBool
      override def opName: String = "UInt < UInt"
      override def simplifyNode: Expression = {SymplifyNode.binaryThatIfBoth(new BoolLiteral(false))(this)}
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        checkLiteralRanges(false)
        left  = InputNormalize.resize(left, targetWidth, new ResizeUInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeUInt)
      }
    }

    class SmallerOrEqual extends BinaryOperatorWidthableInputs with SpinalTagReady {
      override def getTypeObject  = TypeBool
      override def opName: String = "UInt <= UInt"
      override def simplifyNode: Expression = {SymplifyNode.binaryThatIfBoth(new BoolLiteral(true))(this)}
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        checkLiteralRanges(false)
        left  = InputNormalize.resize(left, targetWidth, new ResizeUInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeUInt)
      }
    }

    class Equal extends BitVector.Equal {
      override def opName: String = "UInt === UInt"
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        checkLiteralRanges(false)
        left  = InputNormalize.resize(left, targetWidth, new ResizeUInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeUInt)
      }
    }

    class NotEqual extends BitVector.NotEqual {
      override def opName: String = "UInt =/= UInt"
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        checkLiteralRanges(false)
        left  = InputNormalize.resize(left, targetWidth, new ResizeUInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeUInt)
      }
    }

    class ShiftRightByInt(shift: Int) extends BitVector.ShiftRightByInt(shift) {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt >> Int"
    }

    class ShiftRightByUInt extends BitVector.ShiftRightByUInt {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt >> UInt"
    }

    class ShiftLeftByInt(shift: Int) extends BitVector.ShiftLeftByInt(shift) {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt << Int"
      override def getLiteralFactory: (BigInt, Int) => BitVectorLiteral = UIntLiteral.apply
    }

    class ShiftLeftByUInt extends BitVector.ShiftLeftByUInt {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt << UInt"
      override def getLiteralFactory : (BigInt, Int) => BitVectorLiteral = UIntLiteral.apply
    }

    class ShiftRightByIntFixedWidth(shift: Int) extends BitVector.ShiftRightByIntFixedWidth(shift) {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt |>> Int"
    }

    class ShiftLeftByIntFixedWidth(shift: Int) extends BitVector.ShiftLeftByIntFixedWidth(shift) {
      override def getTypeObject  = TypeUInt
      override def opName: String = "UInt |<< Int"
    }

    class ShiftLeftByUIntFixedWidth extends BitVector.ShiftLeftByUIntFixedWidth {
      override def getTypeObject = TypeUInt
      override def opName: String = "UInt |<< UInt"
    }
  }


  /**
    * SInt operator
    */
  object SInt{

    class Not extends UnaryOperatorWidthableInputs with Widthable {
      override def getTypeObject    = TypeSInt
      override def opName: String   = "~ SInt"
      override def calcWidth: Int = source.getWidth
    }

    class Minus extends UnaryOperatorWidthableInputs with Widthable {
      override def getTypeObject    = TypeSInt
      override def opName: String   = "- SInt"
      override def calcWidth: Int = source.getWidth
    }

    class And extends BitVector.And {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt & SInt"
      def resizeFactory: Resize   = new ResizeSInt
    }

    class Or extends BitVector.Or {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt | SInt"
      def resizeFactory: Resize   = new ResizeSInt
    }

    class Xor extends BitVector.Xor {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt ^ SInt"
      def resizeFactory: Resize   = new ResizeSInt
    }

    class Add extends BitVector.Add {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt + SInt"
      def resizeFactory: Resize   = new ResizeSInt
    }

    class Sub extends BitVector.Sub {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt - SInt"
      def resizeFactory: Resize   = new ResizeSInt
    }

    class Mul extends BitVector.Mul {
      override def getTypeObject = TypeSInt
      override def opName: String = "SInt * SInt"
      override def getLiteralFactory: (BigInt, Int) => Expression = SIntLiteral.apply
    }

    class Div extends BitVector.Div {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt / SInt"
    }

    class Mod extends BitVector.Mod {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt % SInt"
    }

    class Smaller extends BinaryOperatorWidthableInputs with SpinalTagReady {
      override def getTypeObject = TypeBool
      override def opName: String = "SInt < SInt"
      override def simplifyNode: Expression = {SymplifyNode.binaryThatIfBoth(new BoolLiteral(false))(this)}
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        checkLiteralRanges(true)
        left  = InputNormalize.resize(left, targetWidth, new ResizeSInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeSInt)
      }
    }

    class SmallerOrEqual extends BinaryOperatorWidthableInputs with SpinalTagReady {
      override def getTypeObject = TypeBool
      override def opName: String = "SInt <= SInt"
      override def simplifyNode: Expression = {SymplifyNode.binaryThatIfBoth(new BoolLiteral(true))(this)}
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        checkLiteralRanges(true)
        left  = InputNormalize.resize(left, targetWidth, new ResizeSInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeSInt)
      }
    }

    class Equal extends BitVector.Equal {
      override def opName: String = "SInt === SInt"
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        checkLiteralRanges(true)
        left  = InputNormalize.resize(left, targetWidth, new ResizeSInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeSInt)
      }
    }

    class NotEqual extends BitVector.NotEqual {
      override def opName: String = "SInt =/= SInt"
      override def normalizeInputs: Unit = {
        val targetWidth = InferWidth.notResizableElseMax(this)
        checkLiteralRanges(true)
        left  = InputNormalize.resize(left, targetWidth, new ResizeSInt)
        right = InputNormalize.resize(right, targetWidth, new ResizeSInt)
      }
    }

    class ShiftRightByInt(shift: Int) extends BitVector.ShiftRightByInt(shift) {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt >> Int"
    }

    class ShiftRightByUInt extends BitVector.ShiftRightByUInt {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt >> UInt"
    }

    class ShiftLeftByInt(shift: Int) extends BitVector.ShiftLeftByInt(shift) {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt << Int"
      override def getLiteralFactory: (BigInt, Int) => BitVectorLiteral = SIntLiteral.apply
    }

    class ShiftLeftByUInt extends BitVector.ShiftLeftByUInt {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt << UInt"
      override def getLiteralFactory: (BigInt, Int) => BitVectorLiteral = SIntLiteral.apply
    }

    class ShiftRightByIntFixedWidth(shift: Int) extends BitVector.ShiftRightByIntFixedWidth(shift) {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt |>> Int"
    }

    class ShiftLeftByIntFixedWidth(shift: Int) extends BitVector.ShiftLeftByIntFixedWidth(shift) {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt |<< Int"
    }

    class ShiftLeftByUIntFixedWidth extends BitVector.ShiftLeftByUIntFixedWidth {
      override def getTypeObject  = TypeSInt
      override def opName: String = "SInt |<< UInt"
    }
  }


  /**
    * Enum operator
    */
  object Enum{

    class Equal(var enumDef: SpinalEnum) extends BinaryOperator with InferableEnumEncodingImpl {
      override def getTypeObject: Any = TypeBool

      override def opName: String = "Enum === Enum"
      override def normalizeInputs: Unit = {InputNormalize.enumImpl(this)}

      override type T = Expression with EnumEncoded
      override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
      override def getDefinition: SpinalEnum = enumDef
      override def swapEnum(e: SpinalEnum) = enumDef = e

      override def simplifyNode: Expression = {
        if (left.getDefinition.elements.size < 2)
          new BoolLiteral(true)
        else
          this
      }
    }

    class NotEqual(var enumDef: SpinalEnum) extends BinaryOperator with InferableEnumEncodingImpl {
      override def getTypeObject: Any = TypeBool
      override def opName: String = "Enum =/= Enum"
      override def normalizeInputs: Unit = {InputNormalize.enumImpl(this)}

      override type T = Expression with EnumEncoded
      override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
      override def getDefinition: SpinalEnum = enumDef
      override def swapEnum(e: SpinalEnum) = enumDef = e

      override def simplifyNode: Expression = {
        if (left.getDefinition.elements.size < 2)
          new BoolLiteral(false)
        else
          this
      }
    }
  }
}


/**
  * Modifier base class
  */
abstract class Modifier extends Expression {}


/**
  * Base class for Casting type
  */
abstract class Cast extends Modifier {
  type T <: Expression
  var input: T = null.asInstanceOf[T]

  override def remapExpressions (func: (Expression) => Expression): Unit = input = stabilized(func, input).asInstanceOf[T]
  override def foreachExpression(func: (Expression) => Unit): Unit = func(input)
}

/** BitVector -> BitVector */
abstract class CastBitVectorToBitVector extends Cast with Widthable {
  override type T <: Expression with WidthProvider
  override private[core] def calcWidth: Int = input.getWidth

  override def toString = s"($opName of $getWidth bits)"
}

/** SInt -> Bits */
class CastSIntToBits extends CastBitVectorToBitVector {
  override def getTypeObject =  TypeBits
  override def opName: String = "SInt -> Bits"
}

/** UInt -> Bits */
class CastUIntToBits extends CastBitVectorToBitVector {
  override def getTypeObject  = TypeBits
  override def opName: String = "UInt -> Bits"
}

/** Bits -> UInt */
class CastBitsToUInt extends CastBitVectorToBitVector {
  override def getTypeObject  = TypeUInt
  override def opName: String = "Bits -> UInt"
}

/** SInt -> UInt */
class CastSIntToUInt extends CastBitVectorToBitVector {
  override def getTypeObject  = TypeUInt
  override def opName: String = "SInt -> UInt"
}

/** Bits -> SInt */
class CastBitsToSInt extends CastBitVectorToBitVector {
  override def getTypeObject  = TypeSInt
  override def opName: String = "Bits -> SInt"
}

/** UInt -> SInt */
class CastUIntToSInt extends CastBitVectorToBitVector {
  override def getTypeObject  = TypeSInt
  override def opName: String = "UInt -> SInt"
}

/** Bool -> Bits */
class CastBoolToBits extends Cast with Widthable {
  override def getTypeObject  = TypeBits
  override def opName: String = "Bits -> Bits"
  override private[core] def calcWidth: Int = 1
}

/** Enum -> Bits */
class CastEnumToBits extends Cast with Widthable {
  override type T <: Expression with EnumEncoded
  override def opName: String = "Enum -> Bits"
  override private[core] def calcWidth: Int = input.getEncoding.getWidth(input.getDefinition)
  override def getTypeObject: Any = TypeBits
}

/** Bits -> Enum */
class CastBitsToEnum(var enumDef: SpinalEnum) extends Cast with InferableEnumEncodingImpl {
  override type T <: Expression with WidthProvider
  override def opName: String = "Bits -> Enum"
  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
  override def getDefinition: SpinalEnum = enumDef

  override def normalizeInputs: Unit = {
    input = InputNormalize.resizedOrUnfixedLit(input, getEncoding.getWidth(enumDef), new ResizeBits, this, this).asInstanceOf[T]
  }

  override def getTypeObject: Any = TypeEnum
  override def swapEnum(e: SpinalEnum) = enumDef = e
}

/** Enum -> Enum */
class CastEnumToEnum(var enumDef: SpinalEnum) extends Cast with  InferableEnumEncodingImpl {
  override type T <: Expression with EnumEncoded
  override def opName: String = "Enum -> Enum"

  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
  override def getDefinition: SpinalEnum = enumDef
  override def getTypeObject: Any = TypeEnum
  override def swapEnum(e: SpinalEnum) = enumDef = e
}


/**
  * Multiplexer base class
  */
abstract class Multiplexer extends Modifier {

  type T <: Expression
  var select: Expression  with WidthProvider = null
  var inputs: ArrayBuffer[T] = null.asInstanceOf[ArrayBuffer[T]]

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    select = stabilized(func, select).asInstanceOf[Expression  with WidthProvider]
    var idx = inputs.length
    while(idx != 0){
      idx -= 1
      val old = inputs(idx)
      val next = stabilized(func, old)
      if(old != next)
        inputs(idx) = next.asInstanceOf[T]
    }
  }

  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(select)
    inputs.foreach(func(_))
  }

  override def normalizeInputs: Unit = {}
}

/**
  * Widtable multiplexer
  */
abstract class MultiplexerWidthable extends Multiplexer with Widthable {
  override type T = Expression with WidthProvider
  override def calcWidth: Int = InferWidth.notResizableElseMax(this)

  override def toString = super.toString + s"[$getWidth bits]"
}

/** Bool multiplexer */
class MultiplexerBool extends Multiplexer {
  override def getTypeObject: Any = TypeBool
  override def opName: String     = "mux of Bool"
}

/** Bits multiplexer */
class MultiplexerBits extends MultiplexerWidthable {
  override def getTypeObject: Any = TypeBits
  override def opName: String     = s"mux of Bits"
  override def normalizeInputs: Unit = {
    super.normalizeInputs
    val targetWidth = getWidth
    var idx = inputs.length
    while(idx != 0){
      idx -= 1
      val old = inputs(idx)
      val next = InputNormalize.resizedOrUnfixedLit(old, targetWidth, new ResizeBits, this, this)
      if(old != next)
        inputs(idx) = next.asInstanceOf[T]
    }
  }
}

/** UInt multiplexer */
class MultiplexerUInt extends MultiplexerWidthable {
  override def getTypeObject: Any = TypeUInt
  override def opName: String     = s"mux of UInt"
  override def normalizeInputs: Unit = {
    super.normalizeInputs
    val targetWidth = getWidth
    var idx = inputs.length
    while(idx != 0){
      idx -= 1
      val old = inputs(idx)
      val next = InputNormalize.resize(old, targetWidth, new ResizeUInt)
      if(old != next)
        inputs(idx) = next.asInstanceOf[T]
    }
  }
}

/** SInt multiplexer */
class MultiplexerSInt extends MultiplexerWidthable {
  override def getTypeObject: Any = TypeSInt
  override def opName: String = s"mux of SInt"
  override def normalizeInputs: Unit = {
    super.normalizeInputs
    val targetWidth = getWidth
    var idx = inputs.length
    while(idx != 0){
      idx -= 1
      val old = inputs(idx)
      val next = InputNormalize.resize(old, targetWidth, new ResizeSInt)
      if(old != next)
        inputs(idx) = next.asInstanceOf[T]
    }
  }
}

/** Enum multiplexer */
class MultiplexerEnum(var enumDef: SpinalEnum) extends Multiplexer with InferableEnumEncodingImpl {
  override type T = Expression with EnumEncoded
  override def opName: String = s"mux of Enum"
  override def getDefinition: SpinalEnum = enumDef
  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
  override def normalizeInputs: Unit = {
    super.normalizeInputs
    for(i <- 0 until inputs.size){
      inputs(i) =  InputNormalize.enumImpl(this, inputs(i))
    }
  }
  override def getTypeObject: Any = TypeEnum
  override def swapEnum(e: SpinalEnum) = enumDef = e
}


/**
  * Binary multiplexer
  */
abstract class BinaryMultiplexer extends Modifier {
  type T <: Expression
  var cond      : Expression = null
  var whenTrue  : T = null.asInstanceOf[T]
  var whenFalse : T = null.asInstanceOf[T]

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    cond      = stabilized(func, cond)
    whenTrue  = stabilized(func, whenTrue).asInstanceOf[T]
    whenFalse = stabilized(func, whenFalse).asInstanceOf[T]
  }
  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(cond)
    func(whenTrue)
    func(whenFalse)
  }

  override def simplifyNode = {
    cond match {
      case lit : BoolLiteral if !lit.hasPoison() => if(lit.value) whenTrue else whenFalse
      case _ => this
    }
  }
}

/**
  * Widtable Binary multiplexer
  */
abstract class BinaryMultiplexerWidthable extends BinaryMultiplexer with Widthable {
  override type T = Expression with WidthProvider
  override def calcWidth: Int = InferWidth.notResizableElseMax(this)
  override def toString = s"(${super.toString })[$getWidth bits]"
}

/** Bool binary multiplexer */
class BinaryMultiplexerBool extends BinaryMultiplexer {
  override def getTypeObject: Any = TypeBool
  override def opName: String = "Bool ? Bool | Bool"
}

/** Bits binary multiplexer */
class BinaryMultiplexerBits extends BinaryMultiplexerWidthable {
  override def getTypeObject: Any = TypeBits
  override def opName: String =  "Bool ? Bits | Bits"
  override def normalizeInputs: Unit = {
    val targetWidth = getWidth
    whenTrue  = InputNormalize.resizedOrUnfixedLit(whenTrue, targetWidth, new ResizeBits, this, this)
    whenFalse = InputNormalize.resizedOrUnfixedLit(whenFalse, targetWidth, new ResizeBits, this, this)
  }
}

/** UInt binary multiplexer */
class BinaryMultiplexerUInt extends BinaryMultiplexerWidthable {
  override def getTypeObject: Any = TypeUInt
  override def opName: String = "Bool ? UInt | UInt"
  override def normalizeInputs: Unit = {
    val targetWidth = getWidth
    whenTrue  = InputNormalize.resize(whenTrue, targetWidth, new ResizeUInt)
    whenFalse = InputNormalize.resize(whenFalse, targetWidth, new ResizeUInt)
  }
}

/** SInt binary multiplexer */
class BinaryMultiplexerSInt extends BinaryMultiplexerWidthable {
  override def getTypeObject: Any = TypeSInt
  override def opName: String = "Bool ? Bits | Bits"
  override def normalizeInputs: Unit = {
    val targetWidth = getWidth
    whenTrue  = InputNormalize.resize(whenTrue, targetWidth, new ResizeSInt)
    whenFalse = InputNormalize.resize(whenFalse, targetWidth, new ResizeSInt)
  }
}

/** Enum binary multiplexer */
class BinaryMultiplexerEnum(var enumDef : SpinalEnum) extends BinaryMultiplexer with InferableEnumEncodingImpl {
  override type T = Expression with EnumEncoded
  override def opName: String = "Bool ? Bits | Bits"
  override def getDefinition: SpinalEnum = enumDef
  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
  override def normalizeInputs: Unit = {
    whenTrue = InputNormalize.enumImpl(this, whenTrue)
    whenFalse = InputNormalize.enumImpl(this, whenFalse)
  }
  override def getTypeObject: Any = TypeEnum
  override def swapEnum(e: SpinalEnum) = enumDef = e
}


/**
  * Multiplex
  */
private[spinal] object Multiplex {

  def baseType[T <: BaseType](sel: Bool, whenTrue: T, whenFalse: T): BinaryMultiplexer = {
    whenTrue.newMultiplexer(sel, whenTrue, whenFalse)
  }

  def complexData[T <: Data](sel: Bool, whenTrue: T, whenFalse: T): T = {
//    Vec(whenTrue, whenFalse).apply(U(sel))


    val outType = if (whenTrue.getClass.isAssignableFrom(whenFalse.getClass)) whenTrue
    else if (whenFalse.getClass.isAssignableFrom(whenTrue.getClass)) whenFalse
    else throw new Exception("can't mux that")

    val muxOut = outType.getMuxType(List(whenTrue, whenFalse))

    val ret = muxOut()
    def rec(ret : Data, elements : Seq[Data]): Unit ={
      ret match {
        case ret : MultiData =>{
          val iRet = ret.elements.iterator
          val iIn = elements.map(_.toMuxInput[Data](ret).asInstanceOf[MultiData].elements.iterator)
          val continue = true
          while(iRet.nonEmpty && continue){
            val dst = iRet.next()
            val srcs = iIn.map(_.next())
            assert(srcs.forall(_._1 == dst._1), "Doesn't match ???")
            rec(dst._2, srcs.map(_._2))
          }
        }
        case ret : BaseType => {
          val ab = ArrayBuffer[BaseType]()
          ab ++= elements.map(_.toMuxInput(ret))
          ret.assignFrom(Multiplex.baseType(sel, elements(0).toMuxInput(ret), elements(1).toMuxInput(ret)))
        }
      }
    }
    rec(ret, List(whenTrue, whenFalse))
    ret
  }
}

/**
  * Base class for a subAccess
  */
abstract class SubAccess extends Modifier {
  def getBitVector: Expression
}


/**
  * Base class fot accessing a bit in a bitvector with a fix index
  */
abstract class BitVectorBitAccessFixed extends SubAccess with ScalaLocated {
  var source: Expression with WidthProvider = null
  var bitId: Int = -1

  override def getBitVector: Expression = source

  override def normalizeInputs: Unit = {
    if (bitId < 0 || bitId >= source.getWidth) {
      PendingError(s"Static bool extraction (bit ${bitId}) is outside the range (${source.getWidth - 1} downto 0) of ${source} at\n${getScalaLocationLong}")
    }
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    source = stabilized(func, source).asInstanceOf[Expression with WidthProvider]
  }

  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(source)
  }


  override def toStringMultiLine() = {
    s"""$this
       |- vector : $source
       |- index  : $bitId
       |""".stripMargin
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

/** Bits access with a fix index */
class BitsBitAccessFixed extends BitVectorBitAccessFixed {
  override def getTypeObject  = TypeBool
  override def opName: String = "Bits(Int)"

  override def simplifyNode = source match{
    case source : BitVectorRangedAccessFixed => {
      bitId = bitId + source.lo
      this.source = source.source
      this
    }
    case _ => this
  }
}

/** UInt access with a fix index */
class UIntBitAccessFixed extends BitVectorBitAccessFixed {
  override def getTypeObject  = TypeBool
  override def opName: String = "UInt(Int)"
}

/** SInt access with a fix index */
class SIntBitAccessFixed extends BitVectorBitAccessFixed {
  override def getTypeObject  = TypeBool
  override def opName: String = "SInt(Int)"
}


/**
  * Base class fot accessing bit in a bitvector with a floating index
  */
abstract class BitVectorBitAccessFloating extends SubAccess with ScalaLocated {
  var source  : Expression with WidthProvider = null
  var bitId  : Expression with WidthProvider = null
  override def getBitVector: Expression = source

  override def normalizeInputs: Unit = {

    if(source.getWidth == 0){
      PendingError(s"Can't access ${source} bits, as it has none\n${getScalaLocationLong}")
    }
    if (bitId.getWidth > log2Up(source.getWidth)) {
      bitId = InputNormalize.resizedOrUnfixedLit(bitId, log2Up(source.getWidth), new ResizeUInt, this, this)
      //PendingError(s"Index ${bitId} used to access ${source} has too many bits\n${getScalaLocationLong}")
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
    source = stabilized(func, source).asInstanceOf[Expression with WidthProvider]
    bitId = stabilized(func, bitId).asInstanceOf[Expression with WidthProvider]
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

  override def toStringMultiLine() = {
    s"""$this
       |- vector : $source
       |- index  : $bitId
       |""".stripMargin
  }
}

/** Bits access with a floating index */
class BitsBitAccessFloating extends BitVectorBitAccessFloating {
  override def getTypeObject  = TypeBool
  override def opName: String = "Bits(UInt)"
  override def bitVectorBitAccessFixedFactory: BitVectorBitAccessFixed = new BitsBitAccessFixed
}

/** UInt access with a floating index */
class UIntBitAccessFloating extends BitVectorBitAccessFloating {
  override def getTypeObject  = TypeBool
  override def opName: String = "UInt(UInt)"
  override def bitVectorBitAccessFixedFactory: BitVectorBitAccessFixed = new UIntBitAccessFixed
}

/** SInt access with a floating index */
class SIntBitAccessFloating extends BitVectorBitAccessFloating {
  override def getTypeObject  = TypeBool
  override def opName: String = "SInt(UInt)"
  override def bitVectorBitAccessFixedFactory: BitVectorBitAccessFixed = new SIntBitAccessFixed
}


/**
  * Base class for accessing a range of bit in a bitvector with a fix range
  */
abstract class BitVectorRangedAccessFixed extends SubAccess with WidthProvider{
  var source: Expression with WidthProvider = null
  var hi, lo = -1

  override def getBitVector: Expression = source

  override def normalizeInputs: Unit = {
    if (hi >= source.getWidth || lo < 0) {
      PendingError(s"Static bits extraction ($hi downto $lo) is outside the range (${source.getWidth - 1} downto 0) of ${source} at\n${getScalaLocationLong}")
    }
  }

  def checkHiLo: Unit = {
    if (lo < 0) {
      SpinalError(s"Static bits extraction out of bound ($hi downto $lo)")
    }
    if (hi - lo < -1) {
      SpinalError(s"Static bits extraction with a negative size ($hi downto $lo)")
    }
  }

  override def getWidth: Int = hi - lo + 1

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    source = stabilized(func, source).asInstanceOf[Expression with Widthable]
  }

  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(source)
  }

  //  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
  //    case 0 => (lo+outHi, lo+outLo)
  //  }
}

/** Bits range access with a fix range */
class BitsRangedAccessFixed extends BitVectorRangedAccessFixed {
  override def getTypeObject  = TypeBits
  override def opName: String = "Bits(Int downto Int)"
}

/** UInt range access with a fix range */
class UIntRangedAccessFixed extends BitVectorRangedAccessFixed {
  override def getTypeObject  = TypeUInt
  override def opName: String = "UInt(Int downto Int)"
}

/** SInt range access with a fix range */
class SIntRangedAccessFixed extends BitVectorRangedAccessFixed {
  override def getTypeObject  = TypeSInt
  override def opName: String = "SInt(Int downto Int)"
}


/**
  * Base class for accessing a range of bits in a bitvector with a floating range
  *
  * When used offset.dontSimplifyIt() Because it can appear at multiple location (o+bc-1 downto o)
  */
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

  def bitVectorRangedAccessFixedFactory: BitVectorRangedAccessFixed
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
    source = stabilized(func, source).asInstanceOf[Expression with WidthProvider]
    offset = stabilized(func, offset).asInstanceOf[Expression with WidthProvider]
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

/** Bits range access with a floating range */
class BitsRangedAccessFloating extends BitVectorRangedAccessFloating {
  override def getTypeObject  = TypeBits
  override def opName: String = "Bits(UInt + Int downto UInt)"
  override def bitVectorRangedAccessFixedFactory: BitVectorRangedAccessFixed = new BitsRangedAccessFixed
}

/** UInt range access with a floating range */
class UIntRangedAccessFloating extends BitVectorRangedAccessFloating {
  override def getTypeObject  = TypeUInt
  override def opName: String = "UInt(UInt + Int downto UInt)"
  override def bitVectorRangedAccessFixedFactory: BitVectorRangedAccessFixed = new UIntRangedAccessFixed
}

/** SInt range access with a floating range */
class SIntRangedAccessFloating extends BitVectorRangedAccessFloating {
  override def getTypeObject  = TypeSInt
  override def opName: String = "SInt(UInt + Int downto UInt)"
  override def bitVectorRangedAccessFixedFactory: BitVectorRangedAccessFixed = new SIntRangedAccessFixed
}

/**
  * SuffixExpression
  */
class SuffixExpression extends Expression with ScalaLocated {
  var target: BaseType = null

  override def opName: String = "Prefix.Suffix"
  override def getTypeObject: Any = TypeStruct
  override def remapExpressions(func: Expression => Expression): Unit = {}
  override def foreachExpression(func: Expression => Unit): Unit = {}
}

object SuffixExpression {
  def apply(target: Expression): SuffixExpression = {
    if (!target.isInstanceOf[BaseType])
      LocatedPendingError(s"INVALID SUFFIX Cannot suffix non-BaseType expression ${target} at")

    val expr = new SuffixExpression

    expr.target = target.asInstanceOf[BaseType]

    expr
  }
}

/**
  * Assigned bits
  */
object AssignedBits {

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
  def intersect(a: AssignedBits, b: AssignedRange): AssignedBits = intersect(b, a)
}

/**
  * Range assignment
  */
object AssignedRange{
  def apply(hi: Int, lo: Int) = new AssignedRange(hi, lo)
  def apply(bit: Int) = new AssignedRange(bit, bit)
  def apply() = new AssignedRange(-1, -1)
}

class AssignedRange(val hi: Int, val lo: Int) {

  def toBigInt = ((BigInt(1) << (hi + 1 - lo)) - 1) << lo

  def toAssignedBits = {
    val ret = new AssignedBits(hi + 1)
    ret.add(this)
    ret
  }
}


/**
  * Bits assignment
  */
class AssignedBits(val width: Int) {

  def bitPerIndex = 32

  var value = new Array[Int]((width+bitPerIndex-1)/bitPerIndex)

  override def clone(): AssignedBits = {
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


  def clear(): Unit = {
    var idx = value.length
    while(idx != 0){
      idx -= 1
      value(idx) = 0
    }
  }

  def +(that: AssignedRange): AssignedBits = {
    val ret = clone()
    ret.add(that)
    ret
  }

  def intersect(range: AssignedBits): AssignedBits = {
    assert(range.width == this.width)
    var idx = Math.min(value.length,range.value.length)
    while(idx != 0){
      idx -= 1
      this.value(idx) &= range.value(idx)
    }
    this
  }

  def add(range: AssignedBits): Unit = {
    assert(range.width == this.width)
    var idx = Math.min(value.length,range.value.length)
    while(idx != 0){
      idx -= 1
      this.value(idx) |= range.value(idx)
    }
  }

  def addChangeReturn(range: AssignedBits): Boolean = {
    assert(range.width == this.width)
    var idx = Math.min(value.length,range.value.length)
    var changed = false
    while(idx != 0){
      idx -= 1
      val currentValue = this.value(idx)
      val ored = currentValue | range.value(idx)
      changed |= ored != currentValue
      this.value(idx) = ored
    }
    return changed
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

  def add(hi: Int, lo: Int): Unit = {
    assert(hi < width)
    var idx = value.length
    while(idx != 0){
      idx -= 1
      val hiA = Math.min(hi - idx*bitPerIndex,bitPerIndex-1)
      val loA = Math.max(lo - idx*bitPerIndex,0)
      if(hiA >= loA)
        this.value(idx) |= (((1l << (hiA+1))-1)-((1l << loA)-1)).toInt
    }
  }

  def add(range: AssignedRange): Unit = add(range.hi, range.lo)

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

  def isFull: Boolean = {
    if(width == 0) return true
    var remainingBits = width
    var i = 0
    while(remainingBits > 0){
      if(remainingBits > 31) {
        if (value(i) != 0xFFFFFFFF) return false
      }else{
        if (value(i) !=  (1 << remainingBits)-1) return false
      }
      i += 1
      remainingBits -= 32
    }
    true
  }
}


/**
  * Base class for expression assignment
  */
abstract class AssignmentExpression extends Expression {
  def finalTarget: BaseType
  override def foreachDrivingExpression(func: (Expression) => Unit): Unit
  override def remapDrivingExpressions(func: (Expression) => Expression): Unit
  def getMinAssignedBits: AssignedRange //Bit that are allwas assigned
  def getMaxAssignedBits: AssignedRange //Bit that are allwas assigned
  //  def getScopeBits: AssignedRange //Bit tht could be assigned
  //  def getOutBaseType: BaseType
  //  def clone(out : Node) : this.type
}


/**
  * Base class for BitVector assignment
  */
abstract class BitVectorAssignmentExpression extends AssignmentExpression {
  def minimalTargetWidth: Int
  def copyWithTarget(target : BitVector) : BitVectorAssignmentExpression
}


/**
  * Bit assignment with a fix index
  */
object BitAssignmentFixed {
  def apply(out: BitVector, bitId: Int ) = {
    val ret = new BitAssignmentFixed
    ret.out = out
    ret.bitId = bitId
    ret
  }
}

class BitAssignmentFixed() extends BitVectorAssignmentExpression with ScalaLocated {

  var out: BitVector = null
  var bitId: Int = -1

  override def copyWithTarget(target: BitVector) = BitAssignmentFixed(target, bitId)

  override def getTypeObject = TypeBool

  override def finalTarget: BaseType = out

  override def minimalTargetWidth: Int = bitId + 1

  override def normalizeInputs: Unit = {
    if (bitId < 0 || bitId >= out.getWidth) {
      PendingError(s"Static bool extraction (bit ${bitId}) is outside the range (${out.getWidth - 1} downto 0) of ${out} at\n${getScalaLocationLong}")
    }
  }

  override def foreachExpression(func: (Expression) => Unit): Unit = func(out)
  override def remapExpressions(func: (Expression) => Expression): Unit = {out = stabilized(func, out).asInstanceOf[BitVector]}
  override def foreachDrivingExpression(func: (Expression) => Unit): Unit = {}
  override def remapDrivingExpressions(func: (Expression) => Expression): Unit = {}
  override def toString(): String = s"${out.toString()}[$bitId]"
  override def opName: String = "x(index) <="


  //  override def checkInferedWidth: Unit = {
  //    if (bitId < 0 || bitId >= out.getWidth) {
  //      PendingError(s"Static bool extraction (bit ${bitId}) is outside the range (${out.getWidth - 1} downto 0) of ${out} at\n${getScalaLocationLong}")
  //    }
  //  }
  override def getMinAssignedBits: AssignedRange = AssignedRange(bitId)
  override def getMaxAssignedBits: AssignedRange = AssignedRange(bitId)
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


/**
  * Range assignment with fix range
  */
object RangedAssignmentFixed {
  def apply(out: BitVector, hi: Int, lo: Int): RangedAssignmentFixed = {
    val assign = new RangedAssignmentFixed
    assign.out = out
    assign.hi = hi
    assign.lo = lo
    assign
  }
}

class RangedAssignmentFixed() extends BitVectorAssignmentExpression with WidthProvider {
  var out: BitVector = null
  var hi = -1
  var lo = 0

  override def copyWithTarget(target: BitVector) = RangedAssignmentFixed(target, hi, lo)

  override def getWidth: Int = hi + 1 - lo
  override def finalTarget: BaseType = out
  override def minimalTargetWidth: Int = hi+1
  override def getTypeObject = out.getTypeObject

  override def normalizeInputs: Unit = {
    if (hi >= out.getWidth || lo < 0) {
      PendingError(s"Static bits assignment ($hi downto $lo) is outside the range (${out.getWidth - 1} downto 0) of ${out} at\n${getScalaLocationLong}")
      return
    }
  }

  override def foreachExpression(func: (Expression) => Unit): Unit = func(out)
  override def remapExpressions(func: (Expression) => Expression): Unit = {out = stabilized(func, out).asInstanceOf[BitVector]}
  override def foreachDrivingExpression(func : (Expression) => Unit): Unit = {}
  override def remapDrivingExpressions(func: (Expression) => Expression): Unit = {}
  override def toString(): String = s"${out.toString()}[$hi downto $lo]"
  override def opName: String = "x(hi:lo) <="

  override def getMinAssignedBits: AssignedRange = AssignedRange(hi, lo)
  override def getMaxAssignedBits: AssignedRange = AssignedRange(hi, lo)
}


/**
  * Bit assignment with floating index
  */
object BitAssignmentFloating {
  def apply(out: BitVector, bitId: Expression with WidthProvider): BitAssignmentFloating = {
    val assign = new BitAssignmentFloating
    assign.out   = out
    assign.bitId = bitId
    assign
  }
}

class BitAssignmentFloating() extends BitVectorAssignmentExpression with ScalaLocated {

  var out: BitVector = null
  var bitId: Expression with WidthProvider = null


  override def copyWithTarget(target: BitVector) = BitAssignmentFloating(target, bitId)

  override def getTypeObject = TypeBool
  override def finalTarget: BaseType = out
  override def minimalTargetWidth: Int = 1 << Math.min(20,bitId.getWidth)

  override def foreachExpression(func: (Expression) => Unit): Unit = {
    func(out)
    func(bitId)
  }

  override def remapExpressions(func: (Expression) => Expression): Unit = {
    out = stabilized(func, out).asInstanceOf[BitVector]
    bitId = stabilized(func, bitId).asInstanceOf[Expression with WidthProvider]

  }

  override def foreachDrivingExpression(func: (Expression) => Unit): Unit = {
    func(bitId)
  }

  override def remapDrivingExpressions(func: (Expression) => Expression): Unit = {
    bitId = stabilized(func, bitId).asInstanceOf[Expression with WidthProvider]
  }

  override def toString(): String = s"${out.toString()}[$bitId]"
  override def opName: String = "x(uIndex) <="

  override def simplifyNode: Expression = {
    if(bitId.getWidth == 0) {
      BitAssignmentFixed(out, 0)
    }else
      this
  }

  override def normalizeInputs: Unit = {
    if (bitId.getWidth > log2Up(out.getWidth)) {
      PendingError(s"Index ${bitId} used to access ${out} has too many bits\n${getScalaLocationLong}")
    }
  }

  override def getMinAssignedBits: AssignedRange = AssignedRange()
  override def getMaxAssignedBits: AssignedRange = AssignedRange(Math.min(out.getWidth-1,(1 << bitId.getWidth)-1), 0)
}


/**
  * Range assignment with a floating range
  */
object RangedAssignmentFloating{
  def apply(out: BitVector,offset: Expression with WidthProvider, bitCount: Int): RangedAssignmentFloating = {
    val assign = new RangedAssignmentFloating
    assign.out = out
    assign.offset = offset
    assign.bitCount = bitCount
    assign
  }
}

class RangedAssignmentFloating() extends BitVectorAssignmentExpression with WidthProvider {
  var out: BitVector = null
  var offset: Expression with WidthProvider = null
  var bitCount: Int = -1

  override def copyWithTarget(target: BitVector) = RangedAssignmentFloating(target, offset, bitCount)

  override def getTypeObject = out.getTypeObject

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
    out = stabilized(func, out).asInstanceOf[BitVector]
    offset = stabilized(func, offset).asInstanceOf[Expression with WidthProvider]

  }

  override def foreachDrivingExpression(func: (Expression) => Unit): Unit = {
    func(offset)
  }

  override def remapDrivingExpressions(func: (Expression) => Expression): Unit = {
    offset = stabilized(func, offset).asInstanceOf[Expression with WidthProvider]
  }

  override def toString(): String = s"${out.toString()}[$offset over $bitCount bits]"

  override def opName: String = "x(hi:lo) <="

  override def getMinAssignedBits: AssignedRange = AssignedRange()
  override def getMaxAssignedBits: AssignedRange = AssignedRange(Math.min(out.getWidth-1, (1 << offset.getWidth)-1 + bitCount - 1), 0)

  //  def getScopeBits: AssignedRange = AssignedRange(Math.min(out.getWidth-1,(1 << Math.min(20,offset_.asInstanceOf[Node with WidthProvider].getWidth))+ bitCount.value - 1), 0) //TODO dirty offset_
  //  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = super.getOutToInUsage(inputId,outHi,outLo) //TODO
  //  def getOutBaseType: BaseType = out
  //  override def clone(out: Node): this.type = new RangedAssignmentFloating(out.asInstanceOf[BitVector],in_,offset_,bitCount).asInstanceOf[this.type]
}




object SwitchStatementKeyBool{
  def apply(cond: Expression, key : MaskedLiteral=null): SwitchStatementKeyBool = {
    val ret  = new SwitchStatementKeyBool
    ret.cond = cond
    ret.key = key
    ret
  }
}

class SwitchStatementKeyBool extends Expression {
  var cond : Expression = null
  var key : MaskedLiteral = null

  override def opName: String = "is(b)"
  override def getTypeObject: Any = TypeBool
  override def remapExpressions(func: (Expression) => Expression): Unit = cond = stabilized(func, cond)
  override def foreachExpression(func: (Expression) => Unit): Unit = func(cond)
}


class SwitchStatementElement(var keys: ArrayBuffer[Expression], var scopeStatement: ScopeStatement) extends ScalaLocated{
}


/**
  * Literal trait
  */
trait Literal extends Expression {
  override def clone: this.type = ???
  final override def foreachExpression(func: (Expression) => Unit): Unit = {}
  final override def remapExpressions(func: (Expression) => Expression): Unit = {}
  private[core] def getBitsStringOn(bitCount : Int, poisonSymbol : Char) : String
  private[core] def getBitsStringOnNoPoison(bitCount : Int) : String = {
    require(!hasPoison)
    getBitsStringOn(bitCount,'?')
  }
  def getValue() : BigInt
  def hasPoison() : Boolean

  //  override def addAttribute(attribute: Attribute): Literal.this.type = addTag(attribute)
}


/**
  * Bits literal
  */
object BitsLiteral {

  def apply(value: BigInt, poisonMask: BigInt, specifiedBitCount: Int): BitsLiteral = {
    val valueBitCount  = value.bitLength
    val poisonBitCount = if(poisonMask != null) poisonMask.bitLength else 0
    val minimalWidth   = Math.max(poisonBitCount,valueBitCount)
    var bitCount       = specifiedBitCount

    if (value < 0) throw new Exception("literal value is negative and cannot be represented")

    if (bitCount != -1) {
      if (minimalWidth > bitCount) throw new Exception(s"literal 0x${value.toString(16)} can't fit in Bits($specifiedBitCount bits)")
    } else {
      bitCount = minimalWidth
    }

    BitsLiteral(value, poisonMask, bitCount,specifiedBitCount != -1)
  }

  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int, on: T) : T = {
    on.assignFrom(apply(value, null, specifiedBitCount))
    on
  }

  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int): BitsLiteral = apply(value, null, specifiedBitCount)

  def apply(value: BigInt, poisonMask: BigInt, bitCount: Int, hasSpecifiedBitCount: Boolean) = {
    val ret = new BitsLiteral
    ret.value = value
    ret.poisonMask = poisonMask
    ret.bitCount = bitCount
    ret.hasSpecifiedBitCount = hasSpecifiedBitCount
    ret
  }
}


/**
  * UInt literal
  */
object UIntLiteral {

  def apply(value: BigInt, poisonMask: BigInt, specifiedBitCount: Int): UIntLiteral = {
    val valueBitCount  = value.bitLength
    val poisonBitCount = if(poisonMask != null) poisonMask.bitLength else 0
    val minimalWidth   = Math.max(poisonBitCount, valueBitCount)
    var bitCount       = specifiedBitCount

    if (value < 0)
      throw new Exception("literal value is negative and cannot be represented")

    if (bitCount != -1) {
      if (minimalWidth > bitCount) throw new Exception(s"literal 0x${value.toString(16)} can't fit in UInt($specifiedBitCount bits)")
    } else {
      bitCount = minimalWidth
    }

    UIntLiteral(value, poisonMask, bitCount, specifiedBitCount != -1)
  }

  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int, on: T): T = {
    on.assignFrom(apply(value, null, specifiedBitCount))
    on
  }

  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int): UIntLiteral = apply(value, null, specifiedBitCount)

  def apply(value: BigInt, poisonMask: BigInt, bitCount: Int, hasSpecifiedBitCount: Boolean) = {
    val ret = new UIntLiteral
    ret.value = value
    ret.poisonMask = poisonMask
    ret.bitCount = bitCount
    ret.hasSpecifiedBitCount = hasSpecifiedBitCount
    ret
  }
}


/**
  * SInt literal
  */
object SIntLiteral {

  def apply(value: BigInt, poisonMask: BigInt, specifiedBitCount: Int): SIntLiteral = {
    val valueBitCount  = value.bitLength + (if (value != 0) 1 else 0)
    val poisonBitCount = if(poisonMask != null) poisonMask.bitLength else 0
    val minimalWidth   = Math.max(poisonBitCount,valueBitCount)
    var bitCount       = specifiedBitCount

    if (bitCount != -1) {
      if (minimalWidth > bitCount ) throw new Exception(s"literal 0x${value.toString(16)} can't fit in SInt($specifiedBitCount bits)")
    } else {
      bitCount = minimalWidth
    }

    SIntLiteral(value, poisonMask, bitCount, specifiedBitCount != -1)
  }

  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int, on: T): T = {
    on.assignFrom(apply(value,null,specifiedBitCount))
    on
  }

  def apply[T <: BitVector](value: BigInt, specifiedBitCount: Int): SIntLiteral = apply(value, null, specifiedBitCount)

  def apply(value: BigInt, poisonMask: BigInt, bitCount: Int, hasSpecifiedBitCount: Boolean) = {
    val ret = new SIntLiteral
    ret.value = value
    ret.poisonMask = poisonMask
    ret.bitCount = bitCount
    ret.hasSpecifiedBitCount = hasSpecifiedBitCount
    ret
  }
}

/**
  * Base class for BitVector literal
  */
abstract class BitVectorLiteral() extends Literal with WidthProvider {

  var value: BigInt = null
  var poisonMask : BigInt = null
  var bitCount: Int = -1
  var hasSpecifiedBitCount : Boolean = true

  override def getWidth: Int = bitCount
  override def getValue(): BigInt = if(hasPoison) throw new Exception("Poisoned value") else value

  override def hasPoison() = poisonMask != null && poisonMask != 0

  override def getBitsStringOn(bitCount: Int, poisonSymbol: Char): String = {

    def makeIt(fillWith: Boolean): String = {
      val str = new StringBuilder()
      val unsignedValue = if(value >= 0) value else ((BigInt(1) << bitCount) + value)
      val unsignedLength = unsignedValue.bitLength
      val poisonLength = if(poisonMask != null) poisonMask.bitLength else 0

      assert(bitCount >= unsignedLength)
      assert(bitCount >= poisonLength)

      val filling = if(fillWith) '1' else '0'

      for(i <- 0 until bitCount-unsignedLength) str += filling

      for(i <-  unsignedLength - 1 to 0 by - 1){
        str += (if(unsignedValue.testBit(i)) '1' else '0')
      }

      for(i <- poisonLength-1 to 0 by -1 if(poisonMask.testBit(i))){
        str(bitCount-i-1) = poisonSymbol
      }

      str.toString()
    }

    makeIt(isSignedKind && value < 0)
  }

  def hexString(bitCount: Int, aligin: Boolean = false):String = {
    val hexCount = scala.math.ceil(bitCount/4.0).toInt
    val alignCount = if (aligin) (hexCount * 4) else bitCount
    val unsignedValue = if(value >= 0) value else ((BigInt(1) << alignCount) + value)
    s"%${hexCount}s".format(unsignedValue.toString(16)).replace(' ','0')
  }


  def minimalValueBitWidth: Int = {
    val pureWidth = value.bitLength + (if(isSignedKind && value != 0) 1 else 0)
    if(hasPoison) Math.max(poisonMask.bitLength,pureWidth) else pureWidth
  }

  def isSignedKind: Boolean

  override def toString: String =  s"${'"'}${getBitsStringOn(bitCount, 'x')}${'"'} $bitCount bits)"
}

/**
  * Bit Literal
  */
class BitsLiteral extends BitVectorLiteral{
  override def getTypeObject = TypeBits
  override def isSignedKind: Boolean = false
  override def clone(): this.type = BitsLiteral(value, poisonMask, bitCount,hasSpecifiedBitCount).asInstanceOf[this.type]
  override def opName: String = "B\"xxx\""
  override def toString = "(B" + super.toString
}

/**
  * UInt literal
  */
class UIntLiteral extends BitVectorLiteral{
  override def getTypeObject = TypeUInt
  override def isSignedKind: Boolean = false
  override def clone(): this.type = UIntLiteral(value, poisonMask, bitCount,hasSpecifiedBitCount).asInstanceOf[this.type]
  override def opName: String = "U\"xxx\""
  override def toString = "(U" + super.toString
}

/**
  * SInt literal
  */
class SIntLiteral extends BitVectorLiteral{
  override def getTypeObject = TypeSInt
  override def isSignedKind: Boolean = true
  override def clone(): this.type = SIntLiteral(value, poisonMask, bitCount,hasSpecifiedBitCount).asInstanceOf[this.type]
  override def opName: String = "S\"xxx\""
  override def toString = "(S" + super.toString
}

/**
  * Bool literal
  */
object BoolLiteral {
  def apply(value: Boolean, on: Bool)(implicit loc: Location): Bool = {
    on.assignFrom(new BoolLiteral(value))
    on
  }
}

class BoolLiteral(val value: Boolean) extends Literal {

  override def getTypeObject = TypeBool
  override def opName: String = "Bool(x)"

  override def normalizeInputs: Unit = {}

  override def clone(): this.type = new BoolLiteral(value).asInstanceOf[this.type]

  override def getValue(): BigInt = if(value) 1 else 0
  override def getBitsStringOn(bitCount: Int, poisonSymbol: Char): String = {
    assert(bitCount == 1)
    (if(value) "1" else "0")
  }
  override def hasPoison() = false
}


/**
  * Poison boolean
  */
class BoolPoison() extends Literal {
  override def getValue(): BigInt = throw new Exception("Poison have no values")
  override def getTypeObject = TypeBool
  override def opName: String = "Bool(?)"
  override def hasPoison() = true
  override def normalizeInputs: Unit = {}

  override def clone(): this.type = new BoolPoison().asInstanceOf[this.type]

  override def getBitsStringOn(bitCount: Int, poisonSymbol: Char): String = {
    assert(bitCount == 1)
    poisonSymbol.toString()
  }
}
