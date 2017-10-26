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
    if(originalContext.scope == globalData.context.head.component.dslBody) return True
    val componentContextStack = mutable.Stack[DslContext]()
    while(globalData.context.head.component == originalContext.component){
      componentContextStack.push(globalData.context.pop())
    }

    globalData.context.push(componentContextStack.pop())
    val cond = Bool()
    originalContext.component.dslBody.prepend(DataAssignmentStatement(cond, new BoolLiteral(false)))

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
    dslContext.scope.append(whenStatement)
    cond.globalData.context.push(cond.globalData.contextHead.copy(scope = whenStatement.whenTrue))
    block
    cond.globalData.context.pop()
    whenContext
  }
}


class SwitchContext(val statement : SwitchStatement) {

}

object switch{
    def apply[T <: BaseType](value: T)(block: => Unit): Unit = {
      val globalData = value.globalData
      val dslContext = value.globalData.contextHead
      val switchStatement = new SwitchStatement(value)
      val switchContext = new SwitchContext(switchStatement)
      globalData.switchStack.push(switchContext)
//      globalData.context.push(globalData.contextHead.copy(scope = null))
      block
//      globalData.context.pop()
      globalData.switchStack.pop()
      dslContext.scope.append(switchStatement)

      //    //value.globalData.pushNetlistLock(() => {
  ////      SpinalError(s"You should not use 'general statments' in the 'switch' scope, but only 'is' statments.\n${ScalaLocated.long}")
  ////    })
  //    val s = new SwitchStack(value)
  //    value.globalData.switchStack.push(s)
  //    block
  //
  //    //value.globalData.pushNetlistUnlock()
  //    if (s.defaultBlock != null) {
  //      if (s.lastWhen == null) {
  //        block
  //      } else {
  //        s.lastWhen.otherwise(s.defaultBlock())
  //      }
  //    }
  //
  //    //value.globalData.popNetlistUnlock()
  //    value.globalData.switchStack.pop(s)
  //    //value.globalData.popNetlistLock
  }
}

object is{
  def apply(values:  Any*)(block: => Unit): Unit = list(values.iterator)(block)
  def list(values: Iterator[Any])(block: => Unit): Unit = {
    val globalData = GlobalData.get
    val switchContext = globalData.switchStack.head
    val switchElement = new SwitchStatementElement(ArrayBuffer[Expression](), new ScopeStatement(switchContext.statement))
    val switchValue = switchContext.statement.value

    def onBaseType(value : BaseType): Unit ={
      if(value.getClass == switchValue.getClass){
        switchElement.keys += value
      }else{
        SpinalError("is(xxx) doesn't match switch(yyy) type")
      }
    }
    values.foreach(value => value match {
      case value : BaseType => onBaseType(value)

      case key: Int => {
        switchValue match {
          case switchValue: Bits => onBaseType(B(key))
          case switchValue: UInt => onBaseType(U(key))
          case switchValue: SInt => onBaseType(S(key))
          case _ => SpinalError("The switch is not a Bits, UInt or SInt")
        }
      }
      case key: Long => {
        switchValue match {
          case switchValue: Bits => onBaseType(B(key))
          case switchValue: UInt => onBaseType(U(key))
          case switchValue: SInt => onBaseType(S(key))
          case _ => SpinalError("The switch is not a Bits, UInt or SInt")
        }
      }
      case key: BigInt => {
        switchValue match {
          case switchValue: Bits => onBaseType(B(key))
          case switchValue: UInt => onBaseType(U(key))
          case switchValue: SInt => onBaseType(S(key))
          case _ => SpinalError("The switch is not a Bits, UInt or SInt")
        }
      }
      case value : SpinalEnumElement[_] => onBaseType(value())
      case key : MaskedLiteral => switchValue match {
        case switchValue: Bits => switchElement.keys += SwitchStatementKeyBool(switchValue === key)
        case switchValue: UInt => switchElement.keys += SwitchStatementKeyBool(switchValue === key)
        case switchValue: SInt => switchElement.keys += SwitchStatementKeyBool(switchValue === key)
        case _ => SpinalError("The switch is not a Bits, UInt or SInt")
      }
//    }
//              case key: Data => switchValue.isEquals(key)
//              case key: Seq[_] => key.map(d => analyse(d)).reduce(_ || _)
    })


    switchContext.statement.elements += switchElement
    globalData.context.push(globalData.contextHead.copy(scope = switchElement.scopeStatement))
    block
    globalData.context.pop()
  }
}

object default {
  def apply(block: => Unit): Unit = {
    val globalData = GlobalData.get
    val switchContext = globalData.switchStack.head
    val defaultScope =  new ScopeStatement(switchContext.statement)
    switchContext.statement.defaultScope = defaultScope
    globalData.context.push(globalData.contextHead.copy(scope = defaultScope))
    block
    globalData.context.pop()
    //    val globalData = GlobalData.get
//    if (globalData.switchStack.isEmpty) SpinalError("Use 'default' statement outside the 'switch'")
//    globalData.pushNetlistUnlock()
//    val value = globalData.switchStack.head()
//
//    if (value.whenStackHead != globalData.conditionalAssignStack.head()) SpinalError("'default' statement is not at the top level of the 'switch'")
//    if (value.defaultBlock != null) SpinalError("'default' statement must appear only one time in the 'switch'")
//    value.defaultBlock = () => {
//      block
//    }
//    globalData.popNetlistUnlock()
  }
}

