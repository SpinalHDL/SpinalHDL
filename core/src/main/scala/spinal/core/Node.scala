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
//
//import spinal.core.Operator.BitVector._
//
import scala.collection.mutable
//import scala.collection.mutable.ArrayBuffer
//
//
object SymplifyNode {
//  def replaceNode(it: Node, by: Node): Unit = {
//    for (consumer <- it.consumers) {
//      consumer.onEachInput((input,i) => {
//        if (input == it) {
//          consumer.setInput(i,by)
//          by.consumers += consumer
//        }
//      })
//    }
//  }
//
//  def replaceNodeInput(it: Node,inId : Int,by : Node): Unit ={
//    it.getInput(inId).consumers -= it
//    it.setInput(inId,by)
//    by.consumers += it
//  }
//
//  def replaceNode(it: Node, by: Int): Unit = {
//    replaceNode(it, it.getInput(by))
//  }
//
//  def none(node: Node): Unit = {}



  def binaryTakeOther(node: BinaryOperatorWidthableInputs): Expression = {
    if (node.left.getWidth == 0) {
        node.right
    } else if (node.right.getWidth == 0) {
        node.left
    } else {
      node
    }
  }
//
//  def binaryUIntSmaller(node: Node): Unit = {
//    val w0 = node.getInput(0).asInstanceOf[WidthProvider].getWidth
//    val w1 = node.getInput(1).asInstanceOf[WidthProvider].getWidth
//    if (w0 == 0 && w1 == 0) {
//      Component.push(node.component)
//      replaceNode(node, False)
//      Component.pop(node.component)
//    } else if (w0 == 0) {
//      Component.push(node.component)
//      replaceNodeInput(node,0,U(0,w1 bit))
//      Component.pop(node.component)
//    } else if (w1 == 0) {
//      Component.push(node.component)
//      replaceNode(node, False)
//      Component.pop(node.component)
//    }
//  }
//
//  def binaryUIntSmallerOrEgual(node: Node): Unit = {
//    val w0 = node.getInput(0).asInstanceOf[WidthProvider].getWidth
//    val w1 = node.getInput(1).asInstanceOf[WidthProvider].getWidth
//    if (w0 == 0 && w1 == 0) {
//      Component.push(node.component)
//      replaceNode(node, True)
//      Component.pop(node.component)
//    } else if (w0 == 0) {
//      Component.push(node.component)
//      replaceNodeInput(node,0,U(0,w1 bit))
//      Component.pop(node.component)
//    } else if (w1 == 0) {
//      Component.push(node.component)
//      replaceNodeInput(node,1,U(0,w0 bit))
//      Component.pop(node.component)
//    }
//  }
//
//  def binarySIntSmaller(node: Node): Unit = {
//    val w0 = node.getInput(0).asInstanceOf[WidthProvider].getWidth
//    val w1 = node.getInput(1).asInstanceOf[WidthProvider].getWidth
//    if (w0 == 0 && w1 == 0) {
//      Component.push(node.component)
//      replaceNode(node, False)
//      Component.pop(node.component)
//    } else if (w0 == 0) {
//      Component.push(node.component)
//      replaceNodeInput(node,0,S(0,w1 bit))
//      Component.pop(node.component)
//    } else if (w1 == 0) {
//      Component.push(node.component)
//      replaceNodeInput(node,1,S(0,w0 bit))
//      Component.pop(node.component)
//    }
//  }
//
//  def binarySIntSmallerOrEgual(node: Node): Unit = {
//    val w0 = node.getInput(0).asInstanceOf[WidthProvider].getWidth
//    val w1 = node.getInput(1).asInstanceOf[WidthProvider].getWidth
//    if (w0 == 0 && w1 == 0) {
//      Component.push(node.component)
//      replaceNode(node, True)
//      Component.pop(node.component)
//    } else if (w0 == 0) {
//      Component.push(node.component)
//      replaceNodeInput(node,0,S(0,w1 bit))
//      Component.pop(node.component)
//    } else if (w1 == 0) {
//      Component.push(node.component)
//      replaceNodeInput(node,1,S(0,w0 bit))
//      Component.pop(node.component)
//    }
//  }
//
//
//
//  def binaryMinus(zeroFactory: (BigInt, BitCount) => Node)(node: Node): Unit = {
//    val w0 = node.getInput(0).asInstanceOf[WidthProvider].getWidth
//    val w1 = node.getInput(1).asInstanceOf[WidthProvider].getWidth
//    if(w1 == 0) {
//      replaceNode(node,0)
//    } else if (w0 == 0) {
//      Component.push(node.component)
//      replaceNodeInput(node,0,zeroFactory(0,w1 bit))
//      Component.pop(node.component)
//    }
//  }
//


  def binaryInductZeroWithOtherWidth(zeroFactory: (BigInt, Int) => Expression,strictResize : Boolean = false)(op : BinaryOperatorWidthableInputs): Expression = {
    def doIt(left : Expression with WidthProvider, right : Expression with WidthProvider) : Expression = {
      if(!strictResize || InputNormalize.isStrictlyResizable(left)) {
        zeroFactory(0, right.getWidth)
      } else {
        op
      }
    }

    if (op.left.getWidth == 0) {
      return doIt(op.left, op.right)
    }
    if (op.right.getWidth == 0) {
      return doIt(op.right,op.left)
    }
    op
  }
//
//
//
//
//  def resizeImpl2(zeroFactory: (BigInt, BitCount) => Node,node: Node): Unit = {
//    val w0 = node.getInput(0).asInstanceOf[WidthProvider].getWidth
//    if (w0 == 0) {
//      Component.push(node.component)
//      replaceNode(node, zeroFactory(0, node.asInstanceOf[WidthProvider].getWidth bit))
//      Component.pop(node.component)
//    }
//  }
//
//  def shiftRightImpl(node: ShiftRightByUInt): Unit = {
//    if (node.right.asInstanceOf[WidthProvider].getWidth == 0) {
//      Component.push(node.component)
//      replaceNode(node, 0)
//      Component.pop(node.component)
//    }
//  }
//
//  def shiftRightImpl(node: ShiftRightByInt): Unit = {
//    if (node.shift == 0) {
//      Component.push(node.component)
//      replaceNode(node, 0)
//      Component.pop(node.component)
//    }
//  }
//
//
//  def shiftRightFixedWidthImpl(node: ShiftRightByIntFixedWidth): Unit = {
//    if (node.shift == 0) {
//      Component.push(node.component)
//      replaceNode(node, 0)
//      Component.pop(node.component)
//    }
//  }
//
//  def shiftLeftImpl(zeroFactory: (BigInt, BitCount) => Node,node: ShiftLeftByUInt): Unit = {
//    if (node.left.asInstanceOf[WidthProvider].getWidth == 0) {
//      Component.push(node.component)
//      replaceNode(node, zeroFactory(0, node.asInstanceOf[WidthProvider].getWidth bit))
//      Component.pop(node.component)
//    } else if (node.right.asInstanceOf[WidthProvider].getWidth == 0) {
//      Component.push(node.component)
//      replaceNode(node, 0)
//      Component.pop(node.component)
//    }
//  }
//
//  def shiftLeftImpl(zeroFactory: (BigInt, BitCount) => Node,node: ShiftLeftByInt): Unit = {
//    if (node.input.asInstanceOf[WidthProvider].getWidth == 0) {
//      Component.push(node.component)
//      replaceNode(node, zeroFactory(0, node.asInstanceOf[WidthProvider].getWidth bit))
//      Component.pop(node.component)
//    } else if (node.shift == 0) {
//      Component.push(node.component)
//      replaceNode(node, 0)
//      Component.pop(node.component)
//    }
//  }
//
//  def shiftLeftFixedWidthImpl(zeroFactory: (BigInt, BitCount) => Node,node: ShiftLeftByUIntFixedWidth): Unit = {
//    if (node.left.asInstanceOf[WidthProvider].getWidth == 0) {
//      Component.push(node.component)
//      replaceNode(node, 0)
//      Component.pop(node.component)
//    } else if (node.right.asInstanceOf[WidthProvider].getWidth == 0) {
//      Component.push(node.component)
//      replaceNode(node, 0)
//      Component.pop(node.component)
//    }
//  }
//
//  def shiftLeftFixedWidthImpl(zeroFactory: (BigInt, BitCount) => Node,node: ShiftLeftByIntFixedWidth): Unit = {
//    if (node.input.asInstanceOf[WidthProvider].getWidth == 0) {
//      Component.push(node.component)
//      replaceNode(node, 0)
//      Component.pop(node.component)
//    } else if (node.shift == 0) {
//      Component.push(node.component)
//      replaceNode(node, 0)
//      Component.pop(node.component)
//    }
//  }
//
//  def rotateImpl(zeroFactory: (BigInt, BitCount) => Node,node: Node): Unit = {
//    val w0 = node.getInput(0).asInstanceOf[WidthProvider].getWidth
//    val w1 = node.getInput(1).asInstanceOf[WidthProvider].getWidth
//    if (w0 == 0) {
//      Component.push(node.component)
//      replaceNode(node, zeroFactory(0, node.asInstanceOf[WidthProvider].getWidth bit))
//      Component.pop(node.component)
//    } else if (w1 == 0) {
//      Component.push(node.component)
//      replaceNode(node, 0)
//      Component.pop(node.component)
//    }
//  }
//
//
//
//
  def binaryThatIfBoth(thatFactory: => Expression)(node: BinaryOperatorWidthableInputs): Expression = {
    if (node.left.getWidth == 0 && node.right.getWidth == 0)
      thatFactory
    else
      node
  }

//
//  def unaryShortCut(node: Node): Unit = {
//    if (node.getInput(0).asInstanceOf[WidthProvider].getWidth == 0) {
//      replaceNode(node, 0)
//    }
//  }
//  def unaryZero(node: Node): Unit = {
//    if (node.getInput(0).asInstanceOf[WidthProvider].getWidth == 0) {
//      Component.push(node.component)
//      replaceNode(node, U(0, 0 bit))
//      Component.pop(node.component)
//    }
//  }
}

object InputNormalize {
//  def none(node: Node): Unit = {
//
//  }
//
  def enumImpl(node : Expression with EnumEncoded) : Unit = {
    node.remapExpressions(input => input match {
      case input : Expression with EnumEncoded if node.getEncoding != input.getEncoding => {
        val cast = new CastEnumToEnum(node.getDefinition)
        cast.input = input.asInstanceOf[cast.T]
        cast.fixEncoding(node.getEncoding)
        cast
      }
      case _ => input
    })
  }

//  def resizedOrUnfixedLit(node : Expression): Unit ={
//    val targetWidth = node.asInstanceOf[WidthProvider].getWidth
//    node.remapExpressions(e => resizedOrUnfixedLit(e, targetWidth,))
//  }
//
  def isStrictlyResizable(that : Expression) : Boolean = {
    that match{
      case lit: BitVectorLiteral if (!lit.hasSpecifiedBitCount) =>
        true
      case bv : BitVector if (bv.hasTag(tagAutoResize)) =>
        true
      case _ =>
        false
    }
  }

  def resizedOrUnfixedLit(input : Expression with WidthProvider, targetWidth : Int, factory : => Resize,target : Expression,where : ScalaLocated): Expression with WidthProvider = {
    input match{
      case lit : BitVectorLiteral if (! lit.hasSpecifiedBitCount && lit.minimalValueBitWidth <= targetWidth) =>
        lit.bitCount = targetWidth
        lit
      case bt : BitVector if(bt.hasTag(tagAutoResize) && bt.getWidth != targetWidth) =>
        val ret = factory
        ret.input = input
        ret.size = targetWidth
        ret
      case _ =>
        if(input.getWidth != targetWidth){
          PendingError(s"${input} don't have the same width than $target at \n${where.getScalaLocationLong}")
        }
        input
    }
  }


  def resize(input : Expression with WidthProvider, targetWidth : Int, factory : => Resize): Expression with WidthProvider = {
    input match{
      case lit : BitVectorLiteral if (! lit.hasSpecifiedBitCount && lit.minimalValueBitWidth <= targetWidth) =>
        lit.bitCount = targetWidth
        lit
      case _ if input.getWidth != targetWidth =>
        val ret = factory
        ret.input = input
        ret.size = targetWidth
        ret
      case _ =>
        input
    }
  }
}
//
//object WidthInfer {
//  def multipleAssignmentNodeWidth(node: Node): Int = {
//    node.getInputs.foldLeft(-1)((best, n) => Math.max(best, if (n != null && !n.isInstanceOf[Reg]) n.asInstanceOf[WidthProvider].getWidth else -1))
//  }
//
//  def inputMaxWidth(node: Node): Int = {
//    node.getInputs.foldLeft(-1)((best, n) => Math.max(best, if (n != null) n.asInstanceOf[WidthProvider].getWidth else -1))
//  }
//
//  def multiplexImpl(node: Node): Int = {
//    Math.max(node.getInput(1).asInstanceOf[WidthProvider].getWidth, node.getInput(2).asInstanceOf[WidthProvider].getWidth)
//  }
//
//
//
//  def cumulateInputWidth(node: Node): Int = {
//    node.getInputs.foldLeft(0)((old, n) => old + Math.max(0, n.asInstanceOf[WidthProvider].getWidth))
//  }
//
//
//  def input0Width(node: Node): Int = {
//    node.getInput(0).asInstanceOf[WidthProvider].getWidth
//  }
//
//  def shiftLeftWidth(node: Node): Int = node.getInput(0).asInstanceOf[WidthProvider].getWidth + node.getInput(1).asInstanceOf[MinMaxProvider].maxValue.toInt
//  def shiftRightWidth(node: Node): Int = Math.max(0, node.getInput(0).asInstanceOf[WidthProvider].getWidth - node.getInput(1).asInstanceOf[MinMaxProvider].minValue.toInt)
//
//
//  def oneWidth(node: Node): Int = 1
//
//}
//
//object Node{
//
//  def walk(starts: Seq[Node],walker: (Node, (Node) => Unit) => Unit): Unit = {
//    val targetAlgoId = GlobalData.get.allocateAlgoId()
//    val pendingNodes = mutable.Stack[Node]()
//
//    def addNodeToStack(node: Node): Unit = {
//      if(node != null && node.component != null && node.algoId != targetAlgoId) {
//        pendingNodes.push(node)
//        node.algoId = targetAlgoId
//      }
//    }
//
//    starts.foreach(addNodeToStack(_))
//    while (!pendingNodes.isEmpty) {
//      walker(pendingNodes.pop, addNodeToStack)
//    }
//
//  }
//
//
//  def walk(starts: Seq[Node],walker: (Node) => Unit): Unit = {
//    walk(starts,(node,push) => {
//      walker(node)
//      node.onEachInput(push(_))
//    })
//  }
//
//  //Take an assignement tree and create an copy recursively where it's pointed
//  def cloneAssignementTree(finalOutput : Node,node : Node,into : Node,intoId : Int) : Unit = {
//    node match {
//      case node : MultipleAssignmentNode => {
//        val cpy = node.cloneMultipleAssignmentNode
//        for(i <- 0 until node.inputs.length) cpy.inputs += null.asInstanceOf[cpy.T]
//        node.onEachInput((input,inputId) => cloneAssignementTree(finalOutput,input,cpy,inputId))
//        into.setInput(intoId,cpy)
//      }
//      case node : WhenNode => {
//        val cpy = node.cloneWhenNode
//        node.onEachInput((input, inputId) => cloneAssignementTree(finalOutput,input, cpy, inputId))
//        into.setInput(intoId,cpy)
//      }
//      case node : AssignementNode => {
//        val cpy = node.clone(finalOutput)
//        node.onEachInput((input, inputId) => cloneAssignementTree(finalOutput,input, cpy, inputId))
//        into.setInput(intoId,cpy)
//      }
//      case node => into.setInput(intoId,node)
//    }
//  }
//
//  //Clone a Reg node
//  def cloneReg(outBaseType : BaseType,that : Reg) : Reg = {
//    val clone = that.cloneReg()
//    cloneAssignementTree(outBaseType,that.dataInput,clone,RegS.getDataInputId)
//    cloneAssignementTree(outBaseType,that.initialValue,clone,RegS.getInitialValueId)
//    clone.dataInput match {
//      case node : MultipleAssignmentNode =>{
//        if(node.inputs.head.isInstanceOf[Reg]) node.setInput(0,clone)
//      }
//      case _ =>
//    }
//    clone
//  }
//}
//
//
//abstract class NodeWithVariableInputsCount extends Node{
//  val inputs = new ArrayBuffer[Node](4)
//
//  override def getInputsCount = inputs.length
//  override def getInput(id : Int) : Node = inputs(id)
//  override def setInput(id : Int,node : Node) : Unit = inputs(id) = node
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
//}
//
//abstract class NodeWithoutInputs extends Node{
//  override def getInput(id: Int): Node = ???
//  override def getInputs: Iterator[Node] = Iterator()
//  override def getInputsCount: Int = 0
//  override def onEachInput(doThat: (Node) => Unit): Unit = {}
//  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {}
//  override def setInput(id: Int, node: Node): Unit = ???
//}
//
trait WidthProvider extends ScalaLocated  {
  def getWidth : Int
}
//
object CheckWidth{

//  def allSame(op : BinaryOperatorWidthableInputs)
//  def allSame(node : Node with Widthable): Unit ={
//    node.onEachInput((_input,id) => {
//      val input = _input.asInstanceOf[Node with Widthable]
//      if (input != null && input.component != null && node.getWidth != input.asInstanceOf[WidthProvider].getWidth) {
//        PendingError(s"${node} inputs doesn't have the same width (${input}) at \n${node.getScalaLocationLong}")
//      }
//    })
//  }
}
//
//trait CheckWidth{
//  private[core] def checkInferedWidth: Unit
//}
//
trait Widthable extends WidthProvider{
  private[core] var widthWhenNotInferred = -1
  private[core] var inferredWidth = -1

  private[core] def calcWidth: Int

  override def getWidth: Int = {
    if (globalData.nodeAreInferringWidth) {
      inferredWidth
    } else {
      val isFirst = globalData.nodeGetWidthWalkedSet.isEmpty
      if (globalData.nodeGetWidthWalkedSet.contains(this))
        SpinalError(s"Can't calculate width of $this when design is in construction phase")

      globalData.nodeGetWidthWalkedSet += this
      var temp: Int = 0;
      if (isFirst) {
        try {
          temp = calcWidth
        } catch {
          case e: Exception => {
            globalData.nodeGetWidthWalkedSet.clear()
            throw e
          }
        }
      } else {
        temp = calcWidth
      }

      if (temp == -1) {
        globalData.nodeGetWidthWalkedSet.clear()
        SpinalError(s"Can't infer width because of unspecified width on ${this.getScalaLocationLong}")
      }

      globalData.nodeGetWidthWalkedSet -= this

      if (isFirst) globalData.nodeGetWidthWalkedSet.clear()
      if (widthWhenNotInferred != -1 && widthWhenNotInferred != temp) SpinalError(s"getWidth result differ from last call $getScalaLocationLong")
      widthWhenNotInferred = temp
      temp
    }
  }



  private[core] def inferWidth: Boolean = {
    val newWidth: Int = calcWidth
    if (newWidth == -1) {
      return true
    } else if (newWidth != inferredWidth) {
      inferredWidth = newWidth
      return true;
    } else {
      return false
    }
  }
}

trait EnumEncoded{
  def getEncoding : SpinalEnumEncoding
  def propagateEncoding = false
  def getDefinition : SpinalEnum
  //Only used in the inferation phase
  def swapEncoding(encoding : SpinalEnumEncoding)
}

trait InferableEnumEncoding{
  private[core] def encodingProposal(e : SpinalEnumEncoding) : Boolean
  def bootInferration() : Unit
}



trait InferableEnumEncodingImplChoice
object InferableEnumEncodingImplChoiceUndone      extends InferableEnumEncodingImplChoice
object InferableEnumEncodingImplChoiceFixed       extends InferableEnumEncodingImplChoice
object InferableEnumEncodingImplChoiceAnticipated extends InferableEnumEncodingImplChoice
object InferableEnumEncodingImplChoiceInferred    extends InferableEnumEncodingImplChoice

trait InferableEnumEncodingImpl extends EnumEncoded  with InferableEnumEncoding with ContextUser with ScalaLocated{
  private[core] var encodingChoice : InferableEnumEncodingImplChoice = InferableEnumEncodingImplChoiceUndone
  private[core] var encoding  : SpinalEnumEncoding = null

  override def swapEncoding(encoding: SpinalEnumEncoding): Unit = this.encoding = encoding

  override def propagateEncoding = encodingChoice == InferableEnumEncodingImplChoiceFixed
  override def bootInferration(): Unit = {
    if(encodingChoice == InferableEnumEncodingImplChoiceUndone){
      encodingChoice = InferableEnumEncodingImplChoiceInferred
      encoding = getDefaultEncoding()
    }
  }

  private[core] def getDefaultEncoding() : SpinalEnumEncoding
  def fixEncoding(e : SpinalEnumEncoding) : Unit = {
    encoding = e
    encodingChoice = InferableEnumEncodingImplChoiceFixed
  }
  def copyEncodingConfig(that : InferableEnumEncodingImpl) : Unit = {
    this.encoding       = that.encoding
    this.encodingChoice = that.encodingChoice
  }

  private[core] override def encodingProposal(e : SpinalEnumEncoding) : Boolean = {
    def takeIt: Boolean ={
      if(encoding != e) {
        encoding = e
        encodingChoice = InferableEnumEncodingImplChoiceInferred
        true
      }else{
        false
      }
    }

    encodingChoice match {
      case `InferableEnumEncodingImplChoiceUndone`   => takeIt
      case `InferableEnumEncodingImplChoiceInferred` => takeIt
      case `InferableEnumEncodingImplChoiceAnticipated` => {
        if(encoding != e){
          globalData.pendingErrors += (() => (s"$this encoding has change between the elaboration phase and the compilation phase\n${this.getScalaLocationLong}"))
        }
        false
      }
      case `InferableEnumEncodingImplChoiceFixed` => false
    }
  }

  override def getEncoding: SpinalEnumEncoding = {
    if (globalData.nodeAreInferringEnumEncoding) {
      encoding
    } else {
      if(encodingChoice == InferableEnumEncodingImplChoiceUndone){
        encoding = getDefaultEncoding
        encodingChoice = InferableEnumEncodingImplChoiceAnticipated
      }
      encoding
    }
  }
}



//abstract class Node extends ContextUser with ScalaLocated with SpinalTagReady with GlobalDataUser {
//  globalData.netlistUpdate()
//  val consumers = new ArrayBuffer[Node](4)
//
//  def getInputsCount : Int = getInputs.size
//  def getInput(id : Int) : Node
//  def setInput(id : Int,node : Node) : Unit
//
//  def getInputs : Iterator[Node]
//  def onEachInput(doThat : (Node,Int) => Unit) : Unit
//  def onEachInput(doThat : (Node) => Unit) : Unit
//
//  private[core] var algoId = 0
//  private[core] def normalizeInputs: Unit = {}
//
//  def simplifyNode: Unit = {}
//
//
//  private[core] def getOutToInUsage(inputId : Int,outHi : Int, outLo : Int) : (Int,Int) = getInput(inputId) match{
//    case input : WidthProvider => (input.getWidth-1,0)
//    case _ => (0,0)
//  }
//
//  private[core] def getClassIdentifier: String = this.getClass.getName.split('.').last.replace("$","")
//
//  private[core] def isInBlackBoxTree = component.isInBlackBoxTree
//
//  private[core] def nonRecursiveToString(): String = {
//    toString()
//  }
//
//  private[core] def preInferationCheck() : Unit = {}
//
//  override def toString(): String = s"${super.toString()}"
//}
//
//
//
//class NoneNode extends NodeWithoutInputs {
//  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = (-1,0)
//
//  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)
//}
//
//
//
//class DontCareNode(target : BaseType) extends NodeWithoutInputs{
//  def getBaseType: BaseType = target
//  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)
//}
//
//class DontCareNodeEnum(val enum: SpinalEnum) extends DontCareNode(null) with InferableEnumEncodingImpl {
//  override def getDefinition: SpinalEnum = enum
//
//  private[core] override def getDefaultEncoding(): SpinalEnumEncoding = enum.defaultEncoding
//}
//
//
//
////TODO remove base type and add WidthProvider
//class DontCareNodeInfered(target : BaseType) extends DontCareNode(target) with Widthable{
//  override def calcWidth: Int = target.asInstanceOf[WidthProvider].getWidth
//}
//
//class DontCareNodeFixed(target : BaseType,fixedWidth : Int) extends DontCareNode(target)  with Widthable {
//  override def calcWidth : Int = fixedWidth
//}
//
//
//
//trait AssignementTreePart{
//  def setAssignementContext(id : Int,that : Throwable) : Unit
//  def getAssignementContext(id : Int) : Throwable
//}