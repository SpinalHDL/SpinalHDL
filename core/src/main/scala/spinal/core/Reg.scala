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
      val reg = newFor(e)
      reg.compositeTagReady = e
      e.input = reg;
      e.compositeAssign = reg
    }

    if (init != null) regOut.init(init)
    if(next != null) regOut := next
    regOut
  }
  def apply[T <: SpinalEnum](enumType: T): enumType.C = Reg(enumType())

 // def apply[T <: Data](dataType: T)(init: T = null.asInstanceOf[T],next : T = null.asInstanceOf[T]): T = Reg(dataType,init,next)

  private[core] def newFor(outType: BaseType, clockDomain: ClockDomain = ClockDomain.current) : Reg = outType match{
    case that : BitVector => new RegWidthable(outType,clockDomain)
    case _ => new Reg(outType,clockDomain)
  }
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


//TODO remove outType
class Reg (outType: BaseType, clockDomain: ClockDomain = ClockDomain.current) extends SyncNode(clockDomain) with Assignable with AssignementTreePart {
  type T <: Node

  var dataInput     : T = this.asInstanceOf[T]
  var initialValue  : T = null.asInstanceOf[T]

  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(dataInput,3)
    if(initialValue != null) doThat(initialValue,4)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    super.onEachInput(doThat)
    doThat(dataInput)
    if(initialValue != null) doThat(initialValue)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 3 => dataInput = node.asInstanceOf[T]
    case 4 if initialValue != null => initialValue = node.asInstanceOf[T]
    case _ => super.setInput(id,node)
  }

  override def getInputsCount: Int = super.getInputsCount + (if(initialValue != null) 2 else 1)
  override def getInputs: Iterator[Node] = super.getInputs ++ (if(initialValue != null) Iterator(dataInput,initialValue) else  Iterator(dataInput))
  override def getInput(id: Int): Node = id match{
    case 3 => dataInput
    case 4 if initialValue != null=> initialValue
    case _ => super.getInput(id)
  }




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

  override def isUsingResetSignal: Boolean = clockDomain.config.resetKind != BOOT && initialValue != null
  override def getSynchronousInputs: List[Node] = getDataInput :: super.getSynchronousInputs
  override def getResetStyleInputs: List[Node] = getInitialValue :: super.getResetStyleInputs

  def getDataInput: Node = dataInput
  def getInitialValue: Node = initialValue

  def setDataInput(that: Node): Unit = dataInput = that.asInstanceOf[T]
  def setInitialValue(that: Node): Unit = {
    initialValue = that.asInstanceOf[T]
    setUseReset
  }




  override def getAssignementContext(id: Int): Throwable = id match {
    case RegS.getDataInputId => outType.getAssignementContext(0)
    case _ => null
  }
  override def setAssignementContext(id: Int, that: Throwable): Unit = id match {
    case RegS.getDataInputId => outType.setAssignementContext(0,that)
    case _ =>
  }
  def hasInitialValue = getInitialValue != null

  def getOutputByConsumers = consumers.find(_.isInstanceOf[BaseType]).get.asInstanceOf[BaseType]


  override def assignFromImpl(that: AnyRef,conservative : Boolean): Unit = {
    that match {
      case that: BaseType => {
        BaseType.checkAssignability(outType,that.asInstanceOf[Node])
        val (consumer,inputId) = BaseType.walkWhenNodes(outType, this, RegS.getDataInputId,conservative)
        consumer.setInput(inputId, that)
      }
      case that : AssignementNode => {
        BaseType.checkAssignability(outType,that.asInstanceOf[Node])
        val (consumer,inputId) = BaseType.walkWhenNodes(outType, this, RegS.getDataInputId,conservative)
        consumer.setInput(inputId,that)
      }
      case _ => throw new Exception("Undefined assignement")
    }
  }

  override def toString: String = "Reg of " + outType.toString()
}


class RegWidthable(outType: BaseType, clockDomain: ClockDomain = ClockDomain.current)  extends Reg(outType,clockDomain) with Widthable with CheckWidth{
  override type T = Node with WidthProvider
  override def calcWidth = math.max(if (dataInput != this) dataInput.getWidth else -1, if (initialValue != null) initialValue.getWidth else -1)

  override def normalizeInputs: Unit = {
    val width = this.getWidth
    InputNormalize.bitVectoreAssignement(this, RegS.getDataInputId, width)
    if (this.initialValue != null) InputNormalize.bitVectoreAssignement(this, RegS.getInitialValueId, width)
  }

  override private[core] def checkInferedWidth: String = {
    if (dataInput != null && dataInput.component != null && this.getWidth != dataInput.getWidth) {
      return s"Assignment bit count mismatch. ${this} := ${dataInput}} at \n${ScalaLocated.long(getAssignementContext(RegS.getDataInputId))}"
    }
    if (initialValue != null && initialValue.component != null && this.getWidth != initialValue.getWidth) {
      return s"Assignment bit count mismatch. ${this} := ${initialValue}} at \n${ScalaLocated.long(getAssignementContext(RegS.getDataInputId))}"
    }
    return null
  }
}
