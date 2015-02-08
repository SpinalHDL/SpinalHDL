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


/**
 * Created by PIC18F on 10.01.2015.
 */


object Reg {
  def apply[T <: Data](dataType: T, init: T = null): T = {
    val regOut = dataType.clone()
    val regInit = dataType.clone()
    if (init != null) regInit := init
    for (((eName, e), (y, initElement)) <- (regOut.flatten, regInit.flatten).zipped) {
      val useReset = initElement.inputs(0) != null && initElement.inputs(0).inputs(0) != null
      val reg = new Reg(e,useReset)
      e.dontSimplifyIt
      e.inputs(0) = reg;
      if(useReset)
        reg.setInitialValue(initElement)
      e.compositeAssign = reg
    }

    regOut
  }
}


object RegNext {
  def apply[T <: Data](next: T, init: T = null): T = {
    val reg = Reg(next, init)
    reg := next
    reg
  }
}

object RegInit {
  def apply[T <: Data](init: T): T = {
    Reg(init, init)
  }
}
object RegS{
  def getDataInputId: Int = 3
  def getInitialValueId: Int = 4
}

class Reg(val output: BaseType,useReset : Boolean, clockDomain: ClockDomain = ClockDomain.current) extends DelayNode(clockDomain,useReset) with Assignable {
  inputs += this
  inputs += this


  override def getSynchronousInputs: ArrayBuffer[Node] = super.getSynchronousInputs += getDataInput
  override def getResetStyleInputs: ArrayBuffer[Node] = super.getResetStyleInputs += getInitialValue

  def getDataInput: Node = inputs(RegS.getDataInputId)
  def getInitialValue: Node = inputs(RegS.getInitialValueId)

  def setDataInput(that : Node): Unit = inputs(RegS.getDataInputId) = that
  def setInitialValue(that : Node): Unit = inputs(RegS.getInitialValueId) = that


  def calcWidth = WidthInfer.regImpl(this)

  override def normalizeInputs: Unit = {
    InputNormalize.regImpl(this)
  }

  def hasInitialValue = getInitialValue != null


  override def assignFrom(that: Data): Unit = {
    that match {
      case that: BaseType => {
        BaseType.assignFrom(output, this,RegS.getDataInputId, that)
      }
      case _ => throw new Exception("Undefined assignement")
    }
  }
}
