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


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC18F on 11.01.2015.
 */


trait ConditionalContext extends GlobalDataUser{

}


class WhenContext(whenStatement: WhenStatement) extends ConditionalContext with ScalaLocated {
  def otherwise(block: => Unit): Unit = {
    val dslContext = GlobalData.get.context
    dslContext.push(dslContext.head.copy(scope = whenStatement.whenFalse))
    block
    dslContext.pop()
  }

  def elsewhen(cond: Bool)(block: => Unit): WhenContext = {
    var newWhenContext : WhenContext = null
    otherwise {
      newWhenContext = when(cond)(block)
    }
    newWhenContext
  }
}

object ConditionalContext{
  def isTrue: Bool ={
    val globalData = GlobalData.get
    val originalContext = globalData.contextHead
    val componentContextStack = mutable.Stack[DslContext]()
    while(globalData.context.head.component == originalContext.component){
      componentContextStack.push(globalData.context.pop())
    }
    globalData.context.push(componentContextStack.pop())
    val cond = Bool()
    originalContext.component.dslBody.prepend(DataAssignementStatement(cond, new BoolLiteral(false)))

    while(componentContextStack.nonEmpty){
      globalData.context.push(componentContextStack.pop())
    }

    cond := True
    cond
  }
}

object when {
  def apply(cond: Bool)(block: => Unit): WhenContext = {
    val dslContext = cond.globalData.contextHead
    val whenStatement = new WhenStatement(cond)
    val whenContext = new WhenContext(whenStatement)
    dslContext.component.addStatement(whenStatement)
    cond.globalData.context.push(cond.globalData.contextHead.copy(scope = whenStatement.whenTrue))
    block
    cond.globalData.context.pop()
    whenContext
  }
}

//
//class SwitchStack(val value: Data) {
//  var lastWhen: WhenContext = null
//  var defaultBlock: () => Unit = null
//  val whenStackHead = GlobalData.get.conditionalAssignStack.head()
//}
//
//
//object WhenNode {
//
//  def apply(forThat : BaseType,w: WhenContext): WhenNode = {
//    apply(forThat,w, w.cond, null,null)
//  }
//
//  def apply(forThat : BaseType,w: WhenContext, cond: Bool, whenTrue: Node, whenFalse: Node): WhenNode = {
//    val ret = newFor(forThat,w)
//    ret.cond = cond
//    ret.whenTrue = whenTrue.asInstanceOf[ret.T]
//    ret.whenFalse = whenFalse.asInstanceOf[ret.T]
//    ret
//  }
//
//  def newFor(that : BaseType,w: WhenContext) : WhenNode = that match{
//    case that : BitVector => new WhenNodeWidthable(w)
//    case that : SpinalEnumCraft[_] => new WhenNodeEnum(w,that.spinalEnum)
//    case _ => new WhenNode(w)
//  }
//}
//
//class WhenNodeWidthable (w: WhenContext) extends WhenNode(w) with Widthable with CheckWidth{
//  override type T = Node with WidthProvider
//
//  override def calcWidth: Int = Math.max(if(whenTrue != null) whenTrue.getWidth else -1, if(whenFalse != null) whenFalse.getWidth else -1)
//
//  override def normalizeInputs: Unit = {
//    if(whenTrue != null)  InputNormalize.resizedOrUnfixedLit(this,1,this.getWidth)
//    if(whenFalse != null) InputNormalize.resizedOrUnfixedLit(this,2,this.getWidth)
//  }
//
//  override private[core] def checkInferedWidth: Unit = {
//    def doit(input : T,i : Int) : Unit = {
//      if (input != null && input.component != null && this.getWidth != input.getWidth) {
//        PendingError(s"Assignement bit count missmatch. ${AssignementTree.getDrivedBaseType(this)} := ${input}} at\n${ScalaLocated.long(getAssignementContext(i))}")
//      }
//    }
//
//    doit(whenTrue,1);
//    doit(whenFalse,2);
//  }
//
//  override def cloneWhenNode : this.type = new WhenNodeWidthable(w).asInstanceOf[this.type]
//}
//
//class WhenNodeEnum (w: WhenContext,enumDef : SpinalEnum) extends WhenNode(w) with InferableEnumEncodingImpl{
//  override type T = Node with EnumEncoded
//  override private[core] def getDefaultEncoding(): SpinalEnumEncoding = enumDef.defaultEncoding
//  override def getDefinition: SpinalEnum = enumDef
//  override private[core] def normalizeInputs: Unit = {
//    InputNormalize.enumImpl(this)
//  }
//  override def cloneWhenNode : this.type = new WhenNodeEnum(w,enumDef).asInstanceOf[this.type]
//}
//
//
//class SwitchContext(val value: Data) extends ConditionalContext{
//  var defaultBlockPresent = false
//  val conditionalAssignStackHead = GlobalData.get.conditionalAssignStack.head()
//  val defaultCond = True
//  var caseCount = 0
//  var defaultContext : CaseContext = null
//}
//
//class CaseContext(val switchContext: SwitchContext,val cond : Bool) extends ConditionalContext{
//  val id = switchContext.caseCount
//  switchContext.caseCount += 1
//}

//
//
//object switch {
//  def apply[T <: Data](value: T)(block: => Unit): Unit = {
//    ??? //TODO IR
////    //value.globalData.pushNetlistLock(() => {
//////      SpinalError(s"You should not use 'general statments' in the 'switch' scope, but only 'is' statments.\n${ScalaLocated.long}")
//////    })
////    val s = new SwitchStack(value)
////    value.globalData.switchStack.push(s)
////    block
////
////    //value.globalData.pushNetlistUnlock()
////    if (s.defaultBlock != null) {
////      if (s.lastWhen == null) {
////        block
////      } else {
////        s.lastWhen.otherwise(s.defaultBlock())
////      }
////    }
////
////    //value.globalData.popNetlistUnlock()
////    value.globalData.switchStack.pop(s)
////    //value.globalData.popNetlistLock
//  }
//}
//
//
//object is {
//  def apply(values:  Any*)(block: => Unit): Unit = list(values.iterator)(block)
//
//  def list(values: Iterator[Any])(block: => Unit): Unit = {
//    ??? //TODO IR
////    val globalData = GlobalData.get
////    if (globalData.switchStack.isEmpty) SpinalError("Use 'is' statement outside the 'switch'")
////    globalData.pushNetlistUnlock()
////
////    val value = globalData.switchStack.head()
////    if (value.whenStackHead != globalData.conditionalAssignStack.head())
////      SpinalError("'is' statement is not at the top level of the 'switch'")
////    val e = ArrayBuffer[Bool]()
////    val switchValue = value.value
////
////    def analyse(key: Any): Bool = {
////      key match {
////        case key: Data => switchValue.isEquals(key)
////        case key: Seq[_] => key.map(d => analyse(d)).reduce(_ || _)
////        case key: Int => {
////          switchValue match {
////            case switchValue: Bits => switchValue === B(key)
////            case switchValue: UInt => switchValue === U(key)
////            case switchValue: SInt => switchValue === S(key)
////            case _ => SpinalError("The switch is not a Bits, UInt or SInt")
////          }
////        }
////        case key: BigInt => {
////          switchValue match {
////            case switchValue: Bits => switchValue === B(key)
////            case switchValue: UInt => switchValue === U(key)
////            case switchValue: SInt => switchValue === S(key)
////            case _ => SpinalError("The switch is not a Bits, UInt or SInt")
////          }
////        }
////        case that : SpinalEnumElement[_] => switchValue.isEquals(that())
////        case key : MaskedLiteral => switchValue match {
////          case switchValue: Bits => switchValue === key
////          case switchValue: UInt => switchValue === key
////          case switchValue: SInt => switchValue === key
////          case _ => SpinalError("The switch is not a Bits, UInt or SInt")
////        }
////      }
////    }
////
////
////    val cond = values.map(analyse(_)).reduce(_ || _)
////    if (value.lastWhen == null) {
////      value.lastWhen = when(cond)(block)
////    } else {
////      value.lastWhen = value.lastWhen.elsewhen(cond)(block)
////    }
////    globalData.popNetlistUnlock()
//  }
//}
//
//object default {
//  def apply(block: => Unit): Unit = {
//    ???
////    val globalData = GlobalData.get
////    if (globalData.switchStack.isEmpty) SpinalError("Use 'default' statement outside the 'switch'")
////    globalData.pushNetlistUnlock()
////    val value = globalData.switchStack.head()
////
////    if (value.whenStackHead != globalData.conditionalAssignStack.head()) SpinalError("'default' statement is not at the top level of the 'switch'")
////    if (value.defaultBlock != null) SpinalError("'default' statement must appear only one time in the 'switch'")
////    value.defaultBlock = () => {
////      block
////    }
////    globalData.popNetlistUnlock()
//  }
//}
