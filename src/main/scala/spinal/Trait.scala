
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


import scala.collection.mutable
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

  object Vec extends VecFactory {
    override def apply[T <: Data](elements: Iterable[T]): Vec[T] = applyIt(super.apply(elements))
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


trait ContextUser extends GlobalDataUser {
  var component = Component.current(globalData)
  val whenScope = globalData.whenStack.head()
  var instanceCounter = globalData.getInstanceCounter
}

trait GlobalDataUser{
  val globalData = GlobalData.get
}


object SyncNode {
  def getClockInputId: Int = 0
  def getClockEnableId: Int = 1
  def getClockResetId: Int = 2
}

abstract class SyncNode(clockDomain: ClockDomain = ClockDomain.current) extends Node {
  inputs += clockDomain.clock
  inputs += clockDomain.clockEnable
  inputs += Bool(!clockDomain.resetActiveHigh)

  def getLatency = 1

  def getSynchronousInputs = ArrayBuffer[Node](/*getClock, */ getClockEnable) ++= (if (clockDomain.resetKind != ASYNC) getResetStyleInputs else Nil)
  def getAsynchronousInputs = ArrayBuffer[Node]() ++= (if (clockDomain.resetKind == ASYNC) getResetStyleInputs else Nil)

  def getResetStyleInputs = ArrayBuffer[Node](getReset)

  def isUsingReset: Boolean
  def setUseReset = inputs(SyncNode.getClockResetId) = clockDomain.reset
  def getClockDomain: ClockDomain = clockDomain

  def getClock: Bool = inputs(SyncNode.getClockInputId).asInstanceOf[Bool]
  def getClockEnable: Bool = inputs(SyncNode.getClockInputId).asInstanceOf[Bool]
  def getReset: Bool = inputs(SyncNode.getClockResetId).asInstanceOf[Bool]
}

trait Assignable {
  def assignFrom(that: AnyRef,conservative : Boolean): Unit
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

  def setName(name: String, weak: Boolean = false): this.type = {
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
    this
  }

  protected def nameChangeEvent(weak: Boolean): Unit = {}
}

object ScalaLocated {
  var unfiltredFiles = mutable.Set("SpinalUtils.scala")
  var unfiltredPackages = mutable.Set("spinal.code.","spinal.bug.","spinal.scalaTest.")

  def getScalaTraceSmart : String = {
    val scalaTrace = new Throwable()
    val temp = scalaTrace.getStackTrace.filter(trace => {
      val className = trace.getClassName
      !((className.startsWith("scala.") || className.startsWith("spinal.")) && !ScalaLocated.unfiltredPackages.map(className.startsWith(_)).reduceLeft(_ || _)) || ScalaLocated.unfiltredFiles.contains(trace.getFileName)
    })
    temp.apply(0).toString
  }
}



trait ScalaLocated extends GlobalDataUser{
  val scalaTrace = if (!globalData.scalaLocatedEnable) null else new Throwable()

  def getScalaTrace = {
    scalaTrace
  }
  def getScalaTraceSmart = {
    val temp = scalaTrace.getStackTrace.filter(trace => {
      val className = trace.getClassName
      !((className.startsWith("scala.") || className.startsWith("spinal.")) && !ScalaLocated.unfiltredPackages.map(className.startsWith(_)).reduceLeft(_ || _)) || ScalaLocated.unfiltredFiles.contains(trace.getFileName)
    })
    temp
  }

  def getScalaTraceString(tab: String): String = {
    if (scalaTrace == null) return ""
    (getScalaTraceSmart.map(tab + _.toString) reduceLeft (_ + "\n" + _)) + "\n\n"
  }
  def getScalaTraceString: String = getScalaTraceString("    ")

  def getScalaLocationString: String = this.toString + " at\n" + getScalaTraceString
  def getScalaLocationStringShort: String = {
    if (scalaTrace == null) return ""
    this.toString + s" at ${getScalaTraceSmart.apply(0).toString}"
  }
}


trait SpinalTagReady {
  val spinalTags = mutable.Set[SpinalTag]()
  var compositeTagReady: SpinalTagReady = null

  def addTag(spinalTag: SpinalTag): this.type = {
    spinalTags += spinalTag
    this
  }

  def hasTag(spinalTag: SpinalTag): Boolean = {
    if (spinalTags.contains(spinalTag)) return true
    if (compositeTagReady != null && compositeTagReady.hasTag(spinalTag)) return true
    return false
  }
}

trait SpinalTag {

}

object crossClockDomain extends SpinalTag

trait ComponentPart {

}

object GlobalData {
  private val it = new ThreadLocal[GlobalData]
  def get = it.get()
  def reset = {
    it.set(new GlobalData)
    get
  }
}
//object GlobalData {
//  var it : GlobalData = null
//  def get = it
//  def reset = {
//    it = new GlobalData
//    get
//  }
//}
class GlobalData {
  var nodeAreInferringWidth = false
  val nodeGetWidthWalkedSet: mutable.Set[Node] = mutable.Set[Node]()
  val nodeWidthInferredCheck = ArrayBuffer[() => Unit]()
  val clockDomainStack = new SafeStack[ClockDomain]
  val componentStack = new SafeStack[Component]
  val switchStack = new SafeStack[SwitchStack]
  val whenStack = new SafeStack[when]
  var scalaLocatedEnable = false
  var instanceCounter = 0
  def getInstanceCounter: Int = {
    val temp = instanceCounter
    instanceCounter = instanceCounter + 1
    temp
  }
}