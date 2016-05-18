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

  def push: Unit = {
    globalData.conditionalAssignStack.push(this)
  }

  def pop: Unit = {
    globalData.conditionalAssignStack.pop(this)
  }
}


object when {


  def doWhen(w: WhenContext, isTrue: Boolean)(block: => Unit): WhenContext = {
    w.isTrue = isTrue
    w.push
    block
    w.pop
    w
  }

  def apply(cond: Bool)(block: => Unit): WhenContext = {
    doWhen(new WhenContext(cond), true)(block)
  }

  //Allow the user to get a Bool that represent the  aggregation of all condition to the current context
  def getWhensCond(that: ContextUser): Bool = {
    val whenScope = if (that == null) null else that.conditionalAssignScope

    var ret: Bool = null
    for (conditionalAssign <- that.globalData.conditionalAssignStack.stack) conditionalAssign match {
      case w : WhenContext => {
        if (w == whenScope) return returnFunc
        val newCond = if (w.isTrue) w.cond else !w.cond
        if (ret == null) {
          ret = newCond
        } else {
          ret = ret && newCond
        }
      }
      case s : SwitchNode => ??? //TODO switch
    }


    def returnFunc = if (ret == null) True else ret

    returnFunc
  }
}

class WhenContext(val cond: Bool) extends Node with ConditionalContext {
  var isTrue: Boolean = true;
  var parentElseWhen: WhenContext = null
  var childElseWhen: WhenContext = null

  def otherwise(block: => Unit): Unit = {
    restackElseWhen
    when.doWhen(this, false)(block)
    destackElseWhen
  }

  def elsewhen(cond: Bool)(block: => Unit) : WhenContext = {
    var w : WhenContext = null
    otherwise({
      w = when(cond) {
        block
      }
      w.parentElseWhen = this
      this.childElseWhen = w
    })
    w
  }

  def restackElseWhen: Unit = {
    if (parentElseWhen == null) return
    parentElseWhen.restackElseWhen
    parentElseWhen.push
  }

  def destackElseWhen: Unit = {
    if (parentElseWhen == null) return
    parentElseWhen.pop
    parentElseWhen.destackElseWhen
  }

  override private[core] def calcWidth: Int = 1
}

class SwitchStack(val value: Data) {
  var lastWhen: WhenContext = null
  var defaultBlock: () => Unit = null
  val whenStackHead = GlobalData.get.conditionalAssignStack.head()
}


object WhenNode {

  def apply(w: WhenContext): WhenNode = {
    apply(w, w.cond, NoneNode(), NoneNode())
  }

  def apply(w: WhenContext, cond: Bool, whenTrue: Node, whenFalse: Node): WhenNode = {
    val ret = new WhenNode(w)
    ret.inputs += cond
    ret.inputs += whenTrue
    ret.inputs += whenFalse
    ret
  }
}

class WhenNode(val w: WhenContext) extends Node with AssignementTreePart {
  override def calcWidth: Int = Math.max(whenTrue.getWidth, whenFalse.getWidth)

  def cond = inputs(0)
  def whenTrue = inputs(1)
  def whenFalse = inputs(2)

  var whenTrueThrowable : Throwable = null
  var whenFalseThrowable : Throwable = null

  override def getAssignementContext(id: Int): Throwable = id match{
    case 1 => whenTrueThrowable
    case 2 => whenFalseThrowable
  }
  override def setAssignementContext(id: Int,that : Throwable = globalData.getThrowable()): Unit =  id match{
    case 1 => whenTrueThrowable  = that
    case 2 => whenFalseThrowable = that
  }

  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
    case 0 => (0, 0)
    case _ => (outHi,outLo)
  }

  override def normalizeInputs: Unit = {
    InputNormalize.bitVectoreAssignement(this,1,this.getWidth)
    InputNormalize.bitVectoreAssignement(this,2,this.getWidth)
  }

  override private[core] def checkInferedWidth: String = {
    for(i <- 1 to 2){
      val input = this.inputs(i)
      if (input != null && input.component != null && !input.isInstanceOf[NoneNode] && this.getWidth !=input.getWidth) {
        return s"Assignement bit count missmatch. ${this} := ${input}} at\n${ScalaLocated.long(getAssignementContext(i))}"
      }
    }
    return null
  }
}


class SwitchContext(val value: Data) extends ConditionalContext{
  var defaultBlockPresent = false
  val conditionalAssignStackHead = GlobalData.get.conditionalAssignStack.head()
  val defaultCond = True
  var caseCount = 0
  var defaultContext : CaseContext = null
}

class CaseContext(val switchContext: SwitchContext,val cond : Bool) extends ConditionalContext{
  val id = switchContext.caseCount
  switchContext.caseCount += 1
}



object switch {

  def apply[T <: Data](value: T)(block: => Unit): Unit = {
    val s = new SwitchStack(value)
    value.globalData.switchStack.push(s)
    block
    if (s.defaultBlock != null) {
      if (s.lastWhen == null) {
        block
      } else {
        s.lastWhen.otherwise(s.defaultBlock())
      }
    }
    value.globalData.switchStack.pop(s)
  }
}


object is {
  def apply(values: Any*)(block: => Unit): Unit = list(values.iterator)(block)

  def list(values: Iterator[Any])(block: => Unit): Unit = {
    val globalData = GlobalData.get
    if (globalData.switchStack.isEmpty) SpinalError("Use 'is' statement outside the 'switch'")

    val value = globalData.switchStack.head()
    if (value.whenStackHead != globalData.conditionalAssignStack.head())
      SpinalError("'is' statement is not at the top level of the 'switch'")
    val e = ArrayBuffer[Bool]()
    val switchValue = value.value

    def analyse(key: Any): Bool = {
      key match {
        case key: Data => switchValue.isEguals(key)
        case key: Seq[_] => key.map(d => analyse(d)).reduce(_ || _)
        case key: Int => {
          switchValue match {
            case switchValue: Bits => switchValue === B(key)
            case switchValue: UInt => switchValue === U(key)
            case switchValue: SInt => switchValue === S(key)
            case _ => SpinalError("The switch is not a Bits, UInt or SInt")
          }
        }
        case that : SpinalEnumElement[_] => switchValue.isEguals(that())
        case key : MaskedLiteral => switchValue match {
          case switchValue: Bits => switchValue === key
          case switchValue: UInt => switchValue === key
          case switchValue: SInt => switchValue === key
          case _ => SpinalError("The switch is not a Bits, UInt or SInt")
        }
      }
    }


    val cond = values.map(analyse(_)).reduce(_ || _)
    if (value.lastWhen == null) {
      value.lastWhen = when(cond)(block)
    } else {
      value.lastWhen = value.lastWhen.elsewhen(cond)(block)
    }
  }
}

object default {
  def apply(block: => Unit): Unit = {
    val globalData = GlobalData.get
    if (globalData.switchStack.isEmpty) SpinalError("Use 'default' statement outside the 'switch'")
    val value = globalData.switchStack.head()

    if (value.whenStackHead != globalData.conditionalAssignStack.head()) SpinalError("'default' statement is not at the top level of the 'switch'")
    if (value.defaultBlock != null) SpinalError("'default' statement must appear only one time in the 'switch'")
    value.defaultBlock = () => {
      block
    }
  }
}


object switch2 {

  def apply[T <: Data](value: T)(block: => Unit): Unit = {
    val s = new SwitchContext(value)
    value.globalData.conditionalAssignStack.push(s)
    block
    value.globalData.conditionalAssignStack.pop(s)
  }
}


object is2 {
  def apply(values: Any*)(block: => Unit): Unit = {
    val globalData = GlobalData.get
    if (globalData.conditionalAssignStack.isEmpty) SpinalError("Use 'is' statement outside the 'switch'")

    val switchContext : SwitchContext = globalData.conditionalAssignStack.head() match{
      case switchContext : SwitchContext => switchContext
      case _ => SpinalError("'is' statement is not at the top level of the 'switch'")
    }
    val e = ArrayBuffer[Bool]()
    val switchValue = switchContext.value

    def analyse(key: Any): Bool = {
      key match {
        case key: Data => switchValue.isEguals(key)
        case key: Seq[_] => key.map(d => analyse(d)).reduce(_ || _)
        case key: Int => {
          switchValue match {
            case switchValue: Bits => switchValue === B(key)
            case switchValue: UInt => switchValue === U(key)
            case switchValue: SInt => switchValue === S(key)
            case _ => SpinalError("The switch is not a Bits, UInt or SInt")
          }
        }
        case that : SpinalEnumElement[_] => switchValue.isEguals(that())
        case key : MaskedLiteral => switchValue match {
          case switchValue: Bits => switchValue === key
          case switchValue: UInt => switchValue === key
          case switchValue: SInt => switchValue === key
          case _ => SpinalError("The switch is not a Bits, UInt or SInt")
        }
      }
    }


    val cond = values.map(analyse(_)).reduce(_ || _)
    val caseContext = new CaseContext(switchContext,cond)
    caseContext.push
    switchContext.defaultCond := False
    block
    caseContext.pop
  }
}

//TODO switch make a proper implementation/check
object default2 {
  def apply(block: => Unit): Unit = {
    val globalData = GlobalData.get
    val switchContext : SwitchContext = globalData.conditionalAssignStack.head() match{
      case switchContext : SwitchContext => switchContext
      case _ => SpinalError("'is' statement is not at the top level of the 'switch'")
    }

    if (switchContext.defaultBlockPresent) SpinalError("'default' statement must appear only one time in the 'switch'")
    switchContext.defaultBlockPresent = true


    val cond = switchContext.defaultCond
    val caseContext = new CaseContext(switchContext,cond)
    switchContext.defaultContext = caseContext
    caseContext.push
    block
    caseContext.pop
  }
}

class CaseNode(val context: CaseContext) extends Node{
  inputs += context.cond
  inputs += null
  def cond = inputs(0)
  def assignement = inputs(1)

  override private[core] def calcWidth: Int = assignement.getWidth
}

class SwitchNode(val context: SwitchContext) extends Node{
  def cases = inputs

  override def normalizeInputs: Unit = {
    for((input,i)  <- inputs.zipWithIndex){
      Misc.normalizeResize(this, i, this.getWidth)
    }
  }

  override def calcWidth: Int = cases.foldLeft(-1)((a,b) => Math.max(a,b.getWidth))
}