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

import scala.collection.mutable.ArrayBuffer


object Reg {
  //def apply[T <: SpinalEnum](enumKind: T, init: T = null.asInstanceOf[T],next : T = null.asInstanceOf[T]): SpinalEnumCraft[T] = Reg(enumKind.craft(),init,next).asInstanceOf[SpinalEnumCraft[T]]

  def apply[T <: Data](dataType: T, init: T = null.asInstanceOf[T],next : T = null.asInstanceOf[T]): T = {
    val regOut = dataType.clone()//.dontSimplifyIt
    for ( e <- regOut.flatten) {
      val reg = new Reg(e)
      reg.compositeTagReady = e
      e.inputs(0) = reg;
      e.compositeAssign = reg
    }

    if (init != null) regOut.init(init)
    if(next != null) regOut := next
    regOut
  }


 // def apply[T <: Data](dataType: T)(init: T = null.asInstanceOf[T],next : T = null.asInstanceOf[T]): T = Reg(dataType,init,next)
}


object RegNext {
  def apply[T <: Data](next: T, init: T = null.asInstanceOf[T]): T = Reg(next, init,next)
}
object RegNextWhen {
  def apply[T <: Data](next: T,cond : Bool, init: T = null.asInstanceOf[T]): T = {
    val reg = Reg(next,init)
    when(cond){
      reg := next
    }
    reg
  }
}


object RegInit {
  def apply[T <: Data](init: T): T = {
    Reg(init, init)
  }

  def apply[T <: SpinalEnum](init : SpinalEnumElement[T]) : SpinalEnumCraft[T] = apply(init())
}

/*
object BoolReg{
  def apply(set : Bool,reset : Bool) : Bool = {
    val r = Reg(Bool)
    when(set){
      r := True
    }
    when(reset){
      r := False
    }
    r
  }
}*/

object RegS {
  val getDataInputId: Int = 3
  val getInitialValueId: Int = 4
}

class Reg(outType: BaseType, clockDomain: ClockDomain = ClockDomain.current) extends SyncNode(clockDomain) with Assignable with AssignementTreePart {
  inputs += this
  inputs += new NoneNode

  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
    case RegS.getDataInputId => (outHi,outLo)
    case RegS.getInitialValueId => (outHi,outLo)
    case _ => super.getOutToInUsage(inputId,outHi,outLo)
  }

  object SyncNode {
    def getClockInputId: Int = 0
    def getClockEnableId: Int = 1
    def getClockResetId: Int = 2
  }

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

  override def normalizeInputs: Unit = InputNormalize.regImpl(this)
  override private[core] def checkInferedWidth: String = {
    val dataInput = this.inputs(RegS.getDataInputId)
    if (dataInput != null && dataInput.component != null && this.getWidth != dataInput.getWidth) {
      return s"Assignment bit count mismatch. ${this} := ${dataInput}} at \n${ScalaLocated.long(getAssignementContext(RegS.getDataInputId))}"
    }
    if (isUsingReset) {
      val resetDataInput = this.inputs(RegS.getInitialValueId)
      if (resetDataInput != null && resetDataInput.component != null && this.getWidth != resetDataInput.getWidth) {
        return s"Assignment bit count mismatch. ${this} := ${resetDataInput}} at \n${ScalaLocated.long(getAssignementContext(RegS.getDataInputId))}"
      }
    }
    return null
  }

  override def getAssignementContext(id: Int): Throwable = id match {
    case RegS.getDataInputId => outType.getAssignementContext(0)
    case _ => null
  }
  override def setAssignementContext(id: Int, that: Throwable): Unit = id match {
    case RegS.getDataInputId => outType.setAssignementContext(0,that)
    case _ => null
  }
  def hasInitialValue = getInitialValue != null

  def getOutputByConsumers = consumers.find(_.isInstanceOf[BaseType]).get.asInstanceOf[BaseType]


  override def assignFromImpl(that: AnyRef,conservative : Boolean): Unit = {
    that match {
      case that: BaseType => {
        val (consumer,inputId) = BaseType.walkWhenNodes(outType, this, RegS.getDataInputId,conservative)
        consumer.inputs(inputId) = that
      }
      case that : AssignementNode => {
        val (consumer,inputId) = BaseType.walkWhenNodes(outType, this, RegS.getDataInputId,conservative)
        consumer.inputs(inputId) = that
      }
      case _ => throw new Exception("Undefined assignement")
    }
  }

  override def toString: String = "Reg of " + outType.toString()
}
