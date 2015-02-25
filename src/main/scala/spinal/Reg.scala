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

package spinal

import scala.collection.mutable.ArrayBuffer



object Reg {
  def apply[T <: Data](dataType: T, init: T = null.asInstanceOf[T],next : T = null.asInstanceOf[T]): T = {
    val regOut = dataType.clone()//.dontSimplifyIt
    val regInit = dataType.clone()
    if (init != null) regInit := init
    for (((eName, e), (y, initElement)) <- (regOut.flatten, regInit.flatten).zipped) {
      val reg = new Reg(e)
      reg.compositeTagReady = e
      e.inputs(0) = reg;
      if (init != null && initElement.inputs(0) != null && initElement.inputs(0).inputs(0) != null)
        reg.setInitialValue(initElement)
      e.compositeAssign = reg
    }

    if (init != null) regOut.setRegInit(init)

    if(next != null) regOut := next
    regOut
  }
}


object RegNext {
  def apply[T <: Data](next: T, init: T = null): T = Reg(next, init,next)
}

object RegInit {
  def apply[T <: Data](init: T): T = {
    Reg(init, init)
  }
}

object RegS {
  def getDataInputId: Int = 3
  def getInitialValueId: Int = 4
}

class Reg(outType: BaseType, clockDomain: ClockDomain = ClockDomain.current) extends SyncNode(clockDomain) with Assignable {
  inputs += this
  inputs += new NoneNode


  override def isUsingReset: Boolean = !getInitialValue.isInstanceOf[NoneNode]
  override def getSynchronousInputs: ArrayBuffer[Node] = super.getSynchronousInputs += getDataInput
  override def getResetStyleInputs: ArrayBuffer[Node] = super.getResetStyleInputs += getInitialValue

  def getDataInput: Node = inputs(RegS.getDataInputId)
  def getInitialValue: Node = inputs(RegS.getInitialValueId)

  def setDataInput(that: Node): Unit = inputs(RegS.getDataInputId) = that
  def setInitialValue(that: Node): Unit = {
    inputs(RegS.getInitialValueId) = that
    setUseReset
  }


  def calcWidth = WidthInfer.regImpl(this)

  override def normalizeInputs: Unit = {
    InputNormalize.regImpl(this)
  }

  def hasInitialValue = getInitialValue != null

  def getOutputByConsumers = consumers.find(_.isInstanceOf[BaseType]).get.asInstanceOf[BaseType]


  override def assignFrom(that: Data): Unit = {
    that match {
      case that: BaseType => {
        val (consumer,inputId) = BaseType.walkWhenNodes(outType, this, RegS.getDataInputId)
        consumer.inputs(inputId) = that
      }
      case _ => throw new Exception("Undefined assignement")
    }
  }
  override def toString: String = "Reg of " + outType.toString()
}
