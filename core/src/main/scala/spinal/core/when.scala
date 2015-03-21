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

/**
 * Created by PIC18F on 11.01.2015.
 */
object when {


  def doWhen(w: when, isTrue: Boolean)(block: => Unit): when = {
    w.isTrue = isTrue
    w.push
    block
    w.pop
    w
  }

  def apply(cond: Bool)(block: => Unit): when = {
    doWhen(new when(cond), true)(block)
  }



  def getWhensCond(that: ContextUser): Bool = {
    val whenScope = if (that == null) null else that.whenScope

    var ret: Bool = null
    for (w <- that.globalData.whenStack.stack) {
      if (w == whenScope) return returnFunc
      val newCond = (if (w.isTrue) w.cond else !w.cond)
      if (ret == null) {
        ret = newCond
      } else {
        ret = ret && newCond
      }
    }


    def returnFunc = if (ret == null) True else ret

    returnFunc
  }
}

class when(val cond: Bool) extends GlobalDataUser{
  var isTrue: Boolean = true;
  var parentElseWhen: when = null

  def otherwise(block: => Unit): Unit = {
    restackElseWhen
    when.doWhen(this, false)(block)
    destackElseWhen
  }

  def elsewhen(cond: Bool)(block: => Unit): when = {
    var w: when = null
    otherwise({
      w = when(cond) {
        block
      }
      w.parentElseWhen = this
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
    parentElseWhen.restackElseWhen
  }


  def push: Unit = {
   globalData.whenStack.push(this)
  }

  def pop: Unit = {
    globalData.whenStack.pop(this)
  }

}

class SwitchStack(val value: Data) {
  // var lastWhen : when = null
}

object switch {

  def apply[T <: Data](value: T)(block: => Unit): Unit = {
    val s = new SwitchStack(value)
    value.globalData.switchStack.push(s)
    block
    value.globalData.switchStack.pop(s)
  }
}


object is {

  def apply[T <: Data](value: T)(block: => Unit): Unit = is(value :: Nil)(block)
  def apply[T <: Data](value: T, values: T*)(block: => Unit): Unit = is(value :: values.toList)(block)

  def apply[T <: SpinalEnum](value: SpinalEnumElement[T])(block: => Unit): Unit = is(value() :: Nil)(block)
  def apply[T <: SpinalEnum](value: SpinalEnumElement[T],values: SpinalEnumElement[T]*)(block: => Unit): Unit = is(value() :: values.map(_()).toList)(block)


  def apply[T <: Data](keys: Iterable[T])(block: => Unit): Unit = {
    if (keys.isEmpty) SpinalError("There is no key in 'is' statement")
    val globalData = keys.head.globalData
    if (globalData.switchStack.isEmpty) SpinalError("Use 'is' statement outside the 'switch'")

    val value = globalData.switchStack.head()

    when(keys.map(key => (key === value.value)).reduceLeft(_ || _)) {
      block
    }
  }
}


object WhenNode{

  def apply(w : when) : WhenNode ={
    apply(w,w.cond,NoneNode(),NoneNode())
  }

  def apply(w : when,cond: Bool, whenTrue: Node, whenFalse: Node): WhenNode ={
    val ret = new WhenNode(w)
    ret.inputs += cond
    ret.inputs += whenTrue
    ret.inputs += whenFalse
    ret
  }
}

class WhenNode(val w : when) extends Node{
  override def calcWidth: Int = Math.max(whenTrue.getWidth,whenFalse.getWidth)

  def cond = inputs(0)
  def whenTrue = inputs(1)
  def whenFalse = inputs(2)

  override def normalizeInputs: Unit = {
    Misc.normalizeResize(this, 1, this.getWidth)
    Misc.normalizeResize(this, 2, this.getWidth)
  }
}