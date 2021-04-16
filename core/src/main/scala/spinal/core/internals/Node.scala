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


object SymplifyNode {

  def binaryTakeOther(node: BinaryOperatorWidthableInputs): Expression = {
    if (node.left.getWidth == 0) {
        node.right
    } else if (node.right.getWidth == 0) {
        node.left
    } else {
      node
    }
  }

  def binaryInductZeroWithOtherWidth(zeroFactory: (BigInt, Int) => Expression, strictResize: Boolean = false)(op: BinaryOperatorWidthableInputs): Expression = {
    def doIt(left: Expression with WidthProvider, right: Expression with WidthProvider): Expression = {
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

  def binaryThatIfBoth(thatFactory: => Expression)(node: BinaryOperatorWidthableInputs): Expression = {
    if (node.left.getWidth == 0 && node.right.getWidth == 0)
      thatFactory
    else
      node
  }
}


object InputNormalize {

  def enumImpl(node: Expression with EnumEncoded): Unit = {
    node.remapExpressions {
      case input: Expression with EnumEncoded if node.getEncoding != input.getEncoding =>
        val cast = new CastEnumToEnum(node.getDefinition)
        cast.input = input.asInstanceOf[cast.T]
        cast.fixEncoding(node.getEncoding)
        cast
      case input => input
    }
  }

  def switchEnumImpl(node: SwitchStatement): Unit = {
    val value = node.value.asInstanceOf[Expression with EnumEncoded]

    node.remapExpressions {
      case input: Expression with EnumEncoded if value.getEncoding != input.getEncoding =>
        val cast = new CastEnumToEnum(value.getDefinition)
        cast.input = input.asInstanceOf[cast.T]
        cast.fixEncoding(value.getEncoding)
        cast
      case input => input
    }
  }

  def isStrictlyResizable(that: Expression): Boolean = {
    that match{
      case lit: BitVectorLiteral if !lit.hasSpecifiedBitCount =>  true
      case bv: BitVector if bv.hasTag(tagAutoResize)          =>  true
      case _                                                  => false
    }
  }

  def resizedOrUnfixedLit(input: Expression with WidthProvider, targetWidth: Int, factory: => Resize, target: Expression, where: ScalaLocated): Expression with WidthProvider = {
    input match{
      case lit: BitVectorLiteral if !lit.hasSpecifiedBitCount && lit.minimalValueBitWidth <= targetWidth =>
        lit.bitCount = targetWidth
        lit
      case bt: BitVector if bt.hasTag(tagAutoResize) && bt.getWidth != targetWidth =>
        val ret   = factory
        ret.input = input
        ret.size  = targetWidth
        ret
      case _ =>
        if(input.getWidth != targetWidth){
          PendingError(s"WIDTH MISMATCH (${targetWidth} bits <- ${input.getWidth} bits) on ${target.toStringMultiLine()}  at \n${where.getScalaLocationLong}")
        }
        input
    }
  }

  def assignementResizedOrUnfixedLit(assignement: AssignmentStatement): Expression = {
    val targetWidth = assignement.target.asInstanceOf[WidthProvider].getWidth
    val inputWidth  = assignement.source.asInstanceOf[WidthProvider].getWidth

    assignement.source match{
      case lit: BitVectorLiteral if !lit.hasSpecifiedBitCount && lit.minimalValueBitWidth <= targetWidth =>
        lit.bitCount = targetWidth
        lit
      case bt: BitVector if bt.hasTag(tagAutoResize) && bt.getWidth != targetWidth =>
        val ret = assignement.finalTarget.asInstanceOf[BitVector].resizeFactory
        ret.input = assignement.source.asInstanceOf[Expression with WidthProvider]
        ret.size = targetWidth
        ret
      case _ =>
        if(inputWidth != targetWidth){
          PendingError(s"WIDTH MISMATCH (${targetWidth} bits <- ${inputWidth} bits) on ${assignement.toStringMultiLine} at \n${assignement.getScalaLocationLong}")
        }
        assignement.source
    }
  }


  def resize(input: Expression with WidthProvider, targetWidth: Int, factory: => Resize): Expression with WidthProvider = {

    input match{
      case lit: BitVectorLiteral if (! lit.hasSpecifiedBitCount && lit.minimalValueBitWidth <= targetWidth) =>
        lit.bitCount = targetWidth
        lit
      case _ if input.getWidth != targetWidth =>
        val ret = factory
        ret.input = input
        ret.size  = targetWidth
        ret
      case _ =>
        input
    }
  }
}

trait Suffixable {
  def elements: Seq[(String, Data)]
}

trait WidthProvider extends ScalaLocated {
  def getWidth: Int
}


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
      var temp: Int = 0

      if (isFirst) {
        try {
          temp = calcWidth
        } catch {
          case e: Exception =>
            globalData.nodeGetWidthWalkedSet.clear()
            throw e
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
  def getEncoding: SpinalEnumEncoding
  def propagateEncoding = false
  def getDefinition: SpinalEnum
  //Only used in the inferation phase
  def swapEncoding(encoding : SpinalEnumEncoding)
}


trait InferableEnumEncoding{
  private[core] def encodingProposal(e: SpinalEnumEncoding): Boolean
  def bootInferration(): Unit
}


trait InferableEnumEncodingImplChoice
object InferableEnumEncodingImplChoiceUndone      extends InferableEnumEncodingImplChoice
object InferableEnumEncodingImplChoiceFixed       extends InferableEnumEncodingImplChoice
object InferableEnumEncodingImplChoiceAnticipated extends InferableEnumEncodingImplChoice
object InferableEnumEncodingImplChoiceInferred    extends InferableEnumEncodingImplChoice


trait InferableEnumEncodingImpl extends EnumEncoded  with InferableEnumEncoding with ContextUser with ScalaLocated{
  private[core] var encodingChoice: InferableEnumEncodingImplChoice = InferableEnumEncodingImplChoiceUndone
  private[core] var encoding : SpinalEnumEncoding = null

  override def swapEncoding(encoding: SpinalEnumEncoding): Unit = this.encoding = encoding

  override def propagateEncoding = encodingChoice == InferableEnumEncodingImplChoiceFixed

  override def bootInferration(): Unit = {
    if(encodingChoice == InferableEnumEncodingImplChoiceUndone){
      encodingChoice = InferableEnumEncodingImplChoiceInferred
      encoding       = getDefaultEncoding()
    }
  }

  private[core] def getDefaultEncoding(): SpinalEnumEncoding

  def fixEncoding(e: SpinalEnumEncoding): Unit = {
    encoding       = e
    encodingChoice = InferableEnumEncodingImplChoiceFixed
  }

  def copyEncodingConfig(that: InferableEnumEncodingImpl): Unit = {
    this.encoding       = that.encoding
    this.encodingChoice = that.encodingChoice
  }

  private[core] override def encodingProposal(e: SpinalEnumEncoding): Boolean = {
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
      case `InferableEnumEncodingImplChoiceUndone`      => takeIt
      case `InferableEnumEncodingImplChoiceInferred`    => takeIt
      case `InferableEnumEncodingImplChoiceAnticipated` =>
        if(encoding != e){
          globalData.pendingErrors += (() => s"$this encoding has change between the elaboration phase and the compilation phase\n${this.getScalaLocationLong}")
        }
        false
      case `InferableEnumEncodingImplChoiceFixed` => false
    }
  }

  override def getEncoding: SpinalEnumEncoding = {
    if (globalData.nodeAreInferringEnumEncoding) {
      encoding
    } else {
      if(encodingChoice == InferableEnumEncodingImplChoiceUndone){
        encoding = getDefaultEncoding()
        encodingChoice = InferableEnumEncodingImplChoiceAnticipated
      }
      encoding
    }
  }
}
