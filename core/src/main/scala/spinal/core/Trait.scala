
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

case class BitCount(val value: Int) {}
case class ExpCount(val value: Int) {}

case class LiteralInt(val value: BigInt) {}

trait IODirection extends BaseTypeFactory {
  def applyIt[T <: Data](data: T): T
  def apply[T <: Data](data: T): T = applyIt(data)
  def apply[T <: Data](datas: T*): Unit = datas.foreach(applyIt(_))
  def apply(enum: SpinalEnum) = applyIt(enum.craft())
  def cloneOf[T <: Data](that: T): T = applyIt(spinal.core.cloneOf(that))
  def apply(b : Int) = 10
  override def Bool = applyIt(super.Bool)
  override def Bits = applyIt(super.Bits)
  override def UInt = applyIt(super.UInt)
  override def SInt = applyIt(super.SInt)
  override def Vec[T <: Data](elements: TraversableOnce[T]): Vec[T] = applyIt(super.Vec(elements))


//  object Vec extends VecFactory {
//    override def apply[T <: Data](elements: TraversableOnce[T]): Vec[T] = applyIt(super.apply(elements))
//  }


}

object in extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asInput()
}

object out extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asOutput()
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
  var whenScope = globalData.whenStack.head()
  var instanceCounter = globalData.getInstanceCounter

  def isOlderThan(that : ContextUser) : Boolean = this.instanceCounter < that.instanceCounter
}

trait GlobalDataUser {
  val globalData = GlobalData.get
}

trait NameableByComponent extends Nameable with GlobalDataUser{
  override def getName(): String = {
    if(!globalData.nodeAreNamed) {
      if (isUnnamed) {
        val c = getComponent()
        if(c != null)c.nameElements()
      }
    }
    return super.getName()
  }

  def getComponent() : Component
}

object SyncNode {
  def getClockInputId: Int = 0
  def getClockEnableId: Int = 1
  def getClockResetId: Int = 2
}

abstract class SyncNode(clockDomain: ClockDomain = ClockDomain.current) extends Node {
  inputs += clockDomain.clock
  inputs += clockDomain.clockEnable
  inputs += Bool(!clockDomain.config.resetActiveHigh)

  final def getLatency = 1 //if not final => update latencyAnalyser

  def getSynchronousInputs = ArrayBuffer[Node](/*getClock, */ getClockEnable) ++= (if (clockDomain.config.resetKind != ASYNC) getResetStyleInputs else Nil)
  def getAsynchronousInputs = ArrayBuffer[Node]() ++= (if (clockDomain.config.resetKind == ASYNC) getResetStyleInputs else Nil)

  def getResetStyleInputs = ArrayBuffer[Node](getReset)

  def isUsingReset: Boolean
  def setUseReset = inputs(SyncNode.getClockResetId) = clockDomain.reset
  def getClockDomain: ClockDomain = clockDomain

  def getClock: Bool = inputs(SyncNode.getClockInputId).asInstanceOf[Bool]
  def getClockEnable: Bool = inputs(SyncNode.getClockEnableId).asInstanceOf[Bool]
  def getReset: Bool = inputs(SyncNode.getClockResetId).asInstanceOf[Bool]
}

trait Assignable {
  var compositeAssign: Assignable = null

  final def assignFrom(that: AnyRef, conservative: Boolean): Unit = {
    if (compositeAssign != null) {
      compositeAssign.assignFrom(that, conservative)
    } else {
      assignFromImpl(that, conservative)
    }
  }

  private[spinal] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit
}


trait Nameable {
  private var name: String = ""
  var compositeName: Nameable = null
  def getName(): String = if (compositeName == null) name else compositeName.getName()
  def isUnnamed: Boolean = name == "" && (compositeName == null || compositeName.isUnnamed)
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
    else if (isWeak && !isNamed) {
      this.name = name;
      nameChangeEvent(weak)
    }
    this
  }

  protected def nameChangeEvent(weak: Boolean): Unit = {}
}

object ScalaLocated {
  var unfiltredFiles = mutable.Set[String](/*"SpinalUtils.scala"*/)
  var filtredFiles = mutable.Set[String]()
  //var unfiltredPackages = mutable.Set("spinal.code.","spinal.bug.","spinal.scalaTest.")

  def getScalaTraceSmart: String = {
    val scalaTrace = new Throwable()
    val temp = scalaTrace.getStackTrace.filter(trace => {
      val className = trace.getClassName
      //  !((className.startsWith("scala.") || className.startsWith("spinal.")) && !ScalaLocated.unfiltredPackages.map(className.startsWith(_)).reduceLeft(_ || _)) || ScalaLocated.unfiltredFiles.contains(trace.getFileName)
      !(className.startsWith("scala.") || className.startsWith("spinal.core")) || ScalaLocated.unfiltredFiles.contains(trace.getFileName)
    })
    temp.apply(0).toString
  }

  def getScalaTrace: String = {
    val scalaTrace = new Throwable()
    val temp = scalaTrace.getStackTrace.filter(trace => {
      val className = trace.getClassName
      //  !((className.startsWith("scala.") || className.startsWith("spinal.")) && !ScalaLocated.unfiltredPackages.map(className.startsWith(_)).reduceLeft(_ || _)) || ScalaLocated.unfiltredFiles.contains(trace.getFileName)
      !(className.startsWith("scala.") || className.startsWith("spinal.core")) || ScalaLocated.unfiltredFiles.contains(trace.getFileName)
    })
    temp.map(_.toString).mkString("\n")
  }
}


trait ScalaLocated extends GlobalDataUser {
  val scalaTrace = if (!globalData.scalaLocatedEnable) null else new Throwable()

  def getScalaTrace = {
    scalaTrace
  }
  def getScalaTraceSmart = {
    val temp = scalaTrace.getStackTrace.filter(trace => {
      val className = trace.getClassName
      //    !((className.startsWith("scala.") || className.startsWith("spinal.")) && !ScalaLocated.unfiltredPackages.map(className.startsWith(_)).reduceLeft(_ || _)) || ScalaLocated.unfiltredFiles.contains(trace.getFileName)
      !(className.startsWith("scala.") || className.startsWith("spinal.core") || ScalaLocated.filtredFiles.contains(trace.getFileName)) || ScalaLocated.unfiltredFiles.contains(trace.getFileName)

    })
    temp
  }
  def getScalaTraceComplet = {
    val temp = scalaTrace.getStackTrace
    temp
  }
  def getScalaTraceCompletString(tab: String): String = {
    if (scalaTrace == null) return ""
    (getScalaTraceComplet.map(tab + _.toString) reduceLeft (_ + "\n" + _)) + "\n\n"
  }
  def getScalaTraceCompletString: String = getScalaTraceCompletString("    ")

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
  def isAssignedTo(that: SpinalTagReady) = that.hasTag(this)
}

object crossClockDomain extends SpinalTag

object crossClockBuffer extends SpinalTag

object randomBoot extends SpinalTag

object tagAutoResize extends SpinalTag

trait Area {

}

object ImplicitArea{
  implicit def toImplicit[T](area: ImplicitArea[T]): T = area.implicitValue
}
abstract class ImplicitArea[T] extends Area {
  def implicitValue: T
}

//object ImplicitArea2{
//  implicit def toImplicit[T](area: ImplicitArea2[T]): T = area.implicitValue
//}
//abstract class ImplicitArea2[T <: Data](dataType : T) extends Area{
//  protected val implicitValue = cloneOf(dataType)
//}

class ClockingArea(clockDomain: ClockDomain) extends Area with DelayedInit {
  clockDomain.push

  override def delayedInit(body: => Unit) = {
    body

    if ((body _).getClass.getDeclaringClass == this.getClass) {
      clockDomain.pop
    }
  }
}

class ClockEnableArea(clockEnable: Bool) extends Area with DelayedInit {
  val newClockEnable : Bool = if (ClockDomain.current.config.clockEnableActiveHigh)
    ClockDomain.current.readClockEnableWire & clockEnable
  else
    ClockDomain.current.readClockEnableWire | !clockEnable

  val clockDomain = ClockDomain.current.clone(clockEnable = newClockEnable)

  clockDomain.push

  override def delayedInit(body: => Unit) = {
    body

    if ((body _).getClass.getDeclaringClass == this.getClass) {
      clockDomain.pop
    }
  }
}


class ResetArea(reset: Bool, cumulative: Boolean) extends Area with DelayedInit {

  val newReset : Bool = if (ClockDomain.current.config.resetActiveHigh)
    (if (cumulative) (ClockDomain.current.readResetWire & reset) else reset)
  else
    (if (cumulative) (ClockDomain.current.readResetWire | !reset) else reset)
  val clockDomain = ClockDomain.current.clone(reset = newReset)
  clockDomain.push

  override def delayedInit(body: => Unit) = {
    body

    if ((body _).getClass.getDeclaringClass == this.getClass) {
      clockDomain.pop
    }
  }
}



object GlobalData {
  //Per thread implementation
  private val it = new ThreadLocal[GlobalData]
  def get = it.get()
  def reset = {
    it.set(new GlobalData)
    get
  }
}

//object GlobalData { //Per application implementation
//  var it : GlobalData = null
//  def get = it
//  def reset = {
//    it = new GlobalData
//    get
//  }
//}
class GlobalData {
  var commonClockConfig = ClockDomainConfig()

  var nodeAreNamed = false
  var nodeAreInferringWidth = false
  val nodeGetWidthWalkedSet: mutable.Set[Node] = mutable.Set[Node]()
  // val nodeWidthInferredCheck = ArrayBuffer[() => Unit]()
  val clockDomainStack = new SafeStack[ClockDomain]
  val componentStack = new SafeStack[Component]
  val switchStack = new SafeStack[SwitchStack]
  val whenStack = new SafeStack[when]
  val widthCheckers = new ArrayBuffer[WidthChecker]()

  var scalaLocatedEnable = false
  var instanceCounter = 0
  val postBackendTask = mutable.ArrayBuffer[() => Unit]()

  val jsonReports = ArrayBuffer[String]()

  def getInstanceCounter: Int = {
    val temp = instanceCounter
    instanceCounter = instanceCounter + 1
    temp
  }


  def addPostBackendTask(task :  => Unit) : Unit = postBackendTask += (() => task)
  def addJsonReport(report : String) : Unit = jsonReports += report
}


trait OverridedEqualsHashCode{
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def hashCode(): Int = super.hashCode()
}


abstract trait Num[T <: Data] {
  def +  (right: T): T
  def -  (right: T): T
  def *  (right: T): T

  def <  (right: T): Bool
  def <= (right: T): Bool
  def >  (right: T): Bool
  def >= (right: T): Bool

  def << (shift : Int) : T
  def >> (shift : Int) : T

  def min(right: T): T = Mux(this < right, this.asInstanceOf[T], right)
  def max(right: T): T = Mux(this < right, right, this.asInstanceOf[T])
}