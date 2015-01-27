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
  def apply[T <: Data](dataType: T, init: T = null): T = {
    val regOut = dataType.clone()
    val regInit = dataType.clone()
    if(init != null) regInit assignFrom init
    for (((eName, e), (y, initElement)) <- (regOut.flatten, regInit.flatten).zipped) {
      val reg = new Reg(e)
      e.inputs(0) = reg;
      if(initElement.inputs(0) != null)
        reg.inputs(1) = initElement
      e.compositeAssign = reg
    }
/*
    if (init != null) {
      for (((x, regSignal), (y, initElement)) <- (regOut.flatten, init.flatten).zipped) {
        regSignal.inputs(0).inputs(1) = initElement
      }
    }*/

    regOut
  }
}


object RegNext{
  def apply[T <: Data](next : T,init : T = null): T ={
    val reg = Reg(next,init)
    reg assignFrom next
    reg
  }
}
object RegInit{
  def apply[T <: Data](init : T): T ={
    Reg(init,init)
  }
}

class Reg(output: BaseType, val clockDomain: ClockDomain = ClockDomain.current) extends Node with Assignable {
  inputs += this
  inputs += this
  inputs += clockDomain.clock
  inputs += clockDomain.clockEnable
  inputs += clockDomain.reset


  def calcWidth = WidthInfer.regImpl(this)

  def getDataInput: Node = inputs(0)
  def getInitialValue: Node = inputs(1)
  def getClock: Bool = inputs(2).asInstanceOf[Bool]
  def getClockEnable: Bool = inputs(3).asInstanceOf[Bool]
  def getReset: Bool = inputs(4).asInstanceOf[Bool]

  def hasInitialValue = getInitialValue != null


  override def assignFrom(that: Data): Unit = {
    that match {
      case that: BaseType => {
        BaseType.assignFrom(output, this, that)
      }
      case _ => throw new Exception("Undefined assignement")
    }

  }

  override def normalizeInputs: Unit ={
    InputNormalize.regImpl(this)
  }

}
