
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

import spinal.test.Try

import scala.collection.mutable.ArrayBuffer


trait IODirection {
  def applyIt[T <: Data](data: T): T
  def apply[T <: Data](data: T): T = applyIt(data)
  def apply(enum: SpinalEnum) = applyIt(enum.craft())


  object Bool extends BoolFactory {
    override def apply() = applyIt(super.apply())
  }

  object Bits extends BitsFactory {
    override def apply() = applyIt(super.apply())
  }

  object UInt extends UIntFactory {
    override def apply() = applyIt(super.apply())
  }

  object SInt extends SIntFactory {
    override def apply() = applyIt(super.apply())
  }


}

object in extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asInput
}

object out extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asOutput
}


//object IntBuilder {
//  implicit def IntToBuilder(value: Int) = new IntBuilder(value)
//}
//
//case class IntBuilder(i: Int) {
//  def bit = new BitCount(i)
//}


trait MinMaxProvider {
  def minValue: BigInt
  def maxValue: BigInt
}

trait ContextUser {
  var component = Component.current
  val whenScope = when.stack.head()
}

/*trait Delay {
  def getClockDomain: ClockDomain
}*/

object DelayNode{
  def getClockInputId: Int = 0
  def getClockEnableId: Int = 1
  def getClockResetId: Int = 2
}

abstract class DelayNode(clockDomain: ClockDomain = ClockDomain.current,useReset : Boolean) extends Node {
  inputs += clockDomain.clock
  inputs += clockDomain.clockEnable
  inputs += (if(useReset) clockDomain.reset else Bool(!clockDomain.resetActiveHigh))

  def getSynchronousInputs = ArrayBuffer[Node](getClock,getClockEnable) ++= (if(clockDomain.resetKind != ASYNC) getResetStyleInputs else Nil)
  def getAsynchronousInputs = ArrayBuffer[Node] () ++= (if(clockDomain.resetKind == ASYNC) getResetStyleInputs else Nil)

  def getResetStyleInputs = ArrayBuffer[Node](getReset)

  def isUsingReset : Boolean = useReset
  def getClockDomain: ClockDomain = clockDomain

  def getClock: Bool = inputs(DelayNode.getClockInputId).asInstanceOf[Bool]
  def getClockEnable: Bool = inputs(DelayNode.getClockInputId).asInstanceOf[Bool]
  def getReset: Bool = inputs(DelayNode.getClockResetId).asInstanceOf[Bool]
}

trait Assignable {
  def assignFrom(that: Data): Unit
}

trait CrossHierarchyInputs {
  def getCrossHierarchyInputsId: Seq[Int]
}

trait Nameable {
  private var name: String = ""
  var compositeName: Nameable = null
  def getName(): String = if (compositeName == null) name else compositeName.getName()
  def isUnnamed: Boolean = name == "" && compositeName == null
  def isNamed: Boolean = !isUnnamed
  var isWeak = true


  override def toString(): String = name

  def getNameElseThrow: String = name
  def setCompositeName(nameable: Nameable) = {
    compositeName = nameable
    name == ""
    isWeak = true
  }
  def setWeakName(name: String) = setName(name, true)
  def setName(nameable: Nameable) = {
    name = nameable.name
    isWeak = nameable.isWeak
    compositeName = null
  }

  def setName(name: String, weak: Boolean = false) = {
    compositeName = null
    if (!weak) {
      this.name = name;
      isWeak = false;
      nameChangeEvent(weak)
    }
    else if (isWeak) {
      this.name = name;
      nameChangeEvent(weak)
    }
  }

  protected def nameChangeEvent(weak: Boolean): Unit = {}
}