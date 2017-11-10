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

import spinal.core.internals._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC18F on 11.01.2015.
 */


trait ConditionalContext extends GlobalDataUser{

}


class WhenContext(whenStatement: WhenStatement) extends ConditionalContext with ScalaLocated {
  def otherwise(block: => Unit): Unit = {
    whenStatement.whenFalse.push()
    block
    whenStatement.whenFalse.pop()
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
  def isTrue(rootScope : ScopeStatement) : Bool ={
    val globalData = GlobalData.get
    if(globalData.dslScope.head == rootScope) return True
    rootScope.push()
    val swap = rootScope.swap()
    val cond = False
    swap.appendBack()
    rootScope.pop()
    cond := True
    cond
  }
  def isTrue() : Bool = isTrue(GlobalData.get.dslScope.head.component.dslBody)
}

object when {
  def apply(cond: Bool)(block: => Unit): WhenContext = {
    val whenStatement = new WhenStatement(cond)
    val whenContext = new WhenContext(whenStatement)
    cond.globalData.dslScope.head.append(whenStatement)
    whenStatement.whenTrue.push()
    block
    whenStatement.whenTrue.pop()
    whenContext
  }
}


class SwitchContext(val statement : SwitchStatement) {

}

object switch{
    def apply[T <: BaseType](value: T)(block: => Unit): Unit = {
      val globalData = value.globalData
      val switchStatement = new SwitchStatement(value)
      val switchContext = new SwitchContext(switchStatement)
      globalData.switchStack.push(switchContext)
      block
      globalData.switchStack.pop()
      globalData.dslScope.head.append(switchStatement)
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
    switchElement.scopeStatement.push()
    block
    switchElement.scopeStatement.pop()
  }
}

object default {
  def apply(block: => Unit): Unit = {
    val globalData = GlobalData.get
    val switchContext = globalData.switchStack.head
    val defaultScope =  new ScopeStatement(switchContext.statement)
    switchContext.statement.defaultScope = defaultScope
    defaultScope.push()
    block
    defaultScope.pop()
  }
}

