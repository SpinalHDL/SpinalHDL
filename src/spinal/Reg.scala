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


/**
 * Created by PIC18F on 10.01.2015.
 */


object Reg {

  //TODO check betther init with null input
  def apply[T <: Data](dataType: T, init: T = null): T = {
    val regOut = dataType.clone()
    val regInit = dataType.clone()
    if (init != null) regInit := init
    for (((eName, e), (y, initElement)) <- (regOut.flatten, regInit.flatten).zipped) {
      val reg = new Reg(e)
      e.inputs(0) = reg;
      if (initElement.inputs(0) != null && initElement.inputs(0).inputs(0) != null)
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

class Reg(val output: BaseType, clockDomain: ClockDomain = ClockDomain.current) extends DelayNode(clockDomain) with Assignable {
  inputs += this
  inputs += this


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
