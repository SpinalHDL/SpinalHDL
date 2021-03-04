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
package spinal.core

import spinal.core.internals._
import scala.collection.mutable.ArrayBuffer


trait ConditionalContext extends GlobalDataUser {
}


class SwitchContext(val statement: SwitchStatement) {
}


object ConditionalContext {

  def isTrue(rootScope: ScopeStatement): Bool ={
    val globalData = GlobalData.get

    if(DslScopeStack.get == rootScope) return True

    rootScope.push()

    val swap = rootScope.swap()
    val cond = False
    swap.appendBack()

    rootScope.pop()

    cond := True
    cond
  }

  def isTrue(): Bool = isTrue(DslScopeStack.get.component.dslBody)
}


/**
  * If statement
  *
  * @example {{{
  *     when(cond1){
  *       myCnt := 0
  *     }elsewhen(cond2){
  *       myCnt := myCnt + 1
  *     }otherwise{
  *       myCnt := myCnt - 1
  *     }
  * }}}
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/when_switch/ when Documentation]]
  */
object when {

  def apply(cond: Bool)(block: => Unit): WhenContext = {

    val whenStatement = new WhenStatement(cond)
    val whenContext   = new WhenContext(whenStatement)

    DslScopeStack.get.append(whenStatement)

    whenStatement.whenTrue.push()
    block
    whenStatement.whenTrue.pop()

    whenContext
  }
}

class ElseWhenClause(val cond : Bool, _block: => Unit){
  def unary_! : ElseWhenClause = new ElseWhenClause(!cond, _block)
  def block = _block
}


/**
  * else / else if  statement
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/when_switch/ when Documentation]]
  */
class WhenContext(whenStatement: WhenStatement) extends ConditionalContext with ScalaLocated {

  def otherwise(block: => Unit): Unit = {
    whenStatement.whenFalse.push()
    block
    whenStatement.whenFalse.pop()
  }

  def elsewhen(clause : ElseWhenClause) : WhenContext = protElsewhen(clause.cond)(clause.block)
//  @deprecated("Use `elsewhen` instead of `.elsewhen` (without the prefix `.`)", "1.1.2")
  def elsewhen(cond: Bool)(block: => Unit): WhenContext = protElsewhen(cond)(block)
  protected def protElsewhen(cond: Bool)(block: => Unit): WhenContext = {
    var newWhenContext: WhenContext = null
    otherwise {
      newWhenContext = when(cond)(block)
    }
    newWhenContext
  }
}




/**
  * case/switch statement
  *
  * @example {{{
  *     switch(x){
  *         is(value1){
  *             //execute when x === value1
  *         }
  *         is(value2){
  *             //execute when x === value2
  *         }
  *         default{
  *            //execute if none of precedent condition meet
  *         }
  *      }
  * }}}
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/when_switch/ switch Documentation]]
  */
object switch {

    def apply[T <: BaseType](value: T)(block: => Unit): Unit = {
      val globalData      = value.globalData
      val switchStatement = new SwitchStatement(value)
      val switchContext   = new SwitchContext(switchStatement)

      SwitchStack.push(switchContext)
      block
      SwitchStack.pop()

      DslScopeStack.get.append(switchStatement)
  }
}


/**
  * is statement of a switch case
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/when_switch/ switch Documentation]]
  */
object is {

  def apply(value: Any, values:  Any*)(block: => Unit): Unit = list((value +: values).iterator)(block)

  def list(values: Iterator[Any])(block: => Unit): Unit = {

    val globalData    = GlobalData.get
    val switchContext = SwitchStack.get
    val switchElement = new SwitchStatementElement(ArrayBuffer[Expression](), new ScopeStatement(switchContext.statement))
    val switchValue   = switchContext.statement.value

    def onBaseType(value : BaseType): Unit ={
      if(value.getClass == switchValue.getClass){
        switchElement.keys += value
      }else{
        SpinalError("is(xxx) doesn't match switch(yyy) type")
      }
    }



    values.foreach {
      case value: BaseType => onBaseType(value)
      case key: Int        =>
        switchValue match {
          case switchValue: Bits => onBaseType(B(key))
          case switchValue: UInt => onBaseType(U(key))
          case switchValue: SInt => onBaseType(S(key))
          case _                 => SpinalError("The switch is not a Bits, UInt or SInt")
        }
      case key: Long       =>
        switchValue match {
          case switchValue: Bits => onBaseType(B(key))
          case switchValue: UInt => onBaseType(U(key))
          case switchValue: SInt => onBaseType(S(key))
          case _                 => SpinalError("The switch is not a Bits, UInt or SInt")
        }
      case key: BigInt     =>
        switchValue match {
          case switchValue: Bits => onBaseType(B(key))
          case switchValue: UInt => onBaseType(U(key))
          case switchValue: SInt => onBaseType(S(key))
          case _                 => SpinalError("The switch is not a Bits, UInt or SInt")
        }
      case value: SpinalEnumElement[_] => onBaseType(value())
      case key: MaskedLiteral          => switchValue match {
        case switchValue: Bits => switchElement.keys += SwitchStatementKeyBool(switchValue === key)
        case switchValue: UInt => switchElement.keys += SwitchStatementKeyBool(switchValue === key)
        case switchValue: SInt => switchElement.keys += SwitchStatementKeyBool(switchValue === key)
        case _                 => SpinalError("The switch is not a Bits, UInt or SInt")
      }
      //    }
      //              case key: Data => switchValue.isEquals(key)
//      case key: Seq[Data] => switchElement.keys += SwitchStatementKeyBool(key.map(d => (switchValue.asInstanceOf[Data] === d)).reduce(_ || _))
    }


    switchContext.statement.elements += switchElement
    switchElement.scopeStatement.push()
    block
    switchElement.scopeStatement.pop()
  }
}


/**
  * default statement of a switch case
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/when_switch/ switch Documentation]]
  */
object default {

  def apply(block: => Unit): Unit = {

    val switchContext = SwitchStack.get
    val defaultScope  =  new ScopeStatement(switchContext.statement)

    switchContext.statement.defaultScope = defaultScope

    defaultScope.push()
    block
    defaultScope.pop()
  }
}
