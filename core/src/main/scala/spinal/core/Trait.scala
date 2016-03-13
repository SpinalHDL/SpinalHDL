
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

import java.util

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
  private[core] var conditionalAssignScope = globalData.conditionalAssignStack.head()
  private[core] var instanceCounter = globalData.getInstanceCounter

  private[core] def isOlderThan(that : ContextUser) : Boolean = this.instanceCounter < that.instanceCounter
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

  private[core] def getComponent() : Component
}

object SyncNode {
  val getClockInputId: Int = 0
  val getClockEnableId: Int = 1
  val getClockResetId: Int = 2
}

abstract class SyncNode(clockDomain: ClockDomain = ClockDomain.current) extends Node {
  inputs += clockDomain.clock
  inputs += clockDomain.clockEnable
  inputs += Bool(!clockDomain.config.resetActiveHigh)

  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
    case SyncNode.getClockInputId => (0,0)
    case SyncNode.getClockEnableId => (0,0)
    case SyncNode.getClockResetId => (0,0)
  }

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
  private[core] var compositeAssign: Assignable = null

  private[core] final def assignFrom(that: AnyRef, conservative: Boolean): Unit = {
    if (compositeAssign != null) {
      compositeAssign.assignFrom(that, conservative)
    } else {
      assignFromImpl(that, conservative)
    }
  }

  private[core] def assignFromImpl(that: AnyRef, conservative: Boolean): Unit
}


trait Nameable {
  private var name: String = ""
  private[core] var compositeName: Nameable = null
  def getName(): String = if (compositeName == null) name else compositeName.getName()
  def isUnnamed: Boolean = name == "" && (compositeName == null || compositeName.isUnnamed)
  def isNamed: Boolean = !isUnnamed
  private[core] var isWeak = true


  override def toString(): String = name

  private[core] def getNameElseThrow: String = name
  def setCompositeName(nameable: Nameable) = {
    compositeName = nameable
    name = ""
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
  private[core] val scalaTrace = if (!globalData.scalaLocatedEnable) null else new Throwable()

  private[core] def getScalaTrace = {
    scalaTrace
  }
  private[core] def getScalaTraceSmart = {
    val temp = scalaTrace.getStackTrace.filter(trace => {
      val className = trace.getClassName
      //    !((className.startsWith("scala.") || className.startsWith("spinal.")) && !ScalaLocated.unfiltredPackages.map(className.startsWith(_)).reduceLeft(_ || _)) || ScalaLocated.unfiltredFiles.contains(trace.getFileName)
      !(className.startsWith("scala.") || className.startsWith("spinal.core") || ScalaLocated.filtredFiles.contains(trace.getFileName)) || ScalaLocated.unfiltredFiles.contains(trace.getFileName)

    })
    temp
  }
  private[core] def getScalaTraceComplet = {
    val temp = scalaTrace.getStackTrace
    temp
  }
  private[core] def getScalaTraceCompletString(tab: String): String = {
    if (scalaTrace == null) return ""
    (getScalaTraceComplet.map(tab + _.toString) reduceLeft (_ + "\n" + _)) + "\n\n"
  }
  private[core] def getScalaTraceCompletString: String = getScalaTraceCompletString("    ")

  private[core] def getScalaTraceString(tab: String): String = {
    if (scalaTrace == null) return ""
    (getScalaTraceSmart.map(tab + _.toString) reduceLeft (_ + "\n" + _)) + "\n\n"
  }
  private[core] def getScalaTraceString: String = getScalaTraceString("    ")

  private[core] def getScalaLocationString: String = this.toString + " at\n" + getScalaTraceString
  private[core] def getScalaLocationStringShort: String = {
    if (scalaTrace == null) return ""
    this.toString + s" at ${getScalaTraceSmart.apply(0).toString}"
  }
}


trait SpinalTagReady {
  private[core] val spinalTags = mutable.Set[SpinalTag]()
  private[core] var compositeTagReady: SpinalTagReady = null

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

trait Area extends Nameable with ContextUser{
  override protected def nameChangeEvent(weak: Boolean): Unit = {
    Misc.reflect(this, (name, obj) => {
      obj match {
        case component: Component => {
          if (component.parent == this.component)
            component.setWeakName(this.getName() + "_" + name)
        }
        case namable: Nameable => {
          if (!namable.isInstanceOf[ContextUser])
            namable.setWeakName(this.getName() + "_" + name)
          else if (namable.asInstanceOf[ContextUser].component == component)
            namable.setWeakName(this.getName() + "_" + name)
          else {
            for (kind <- component.kinds) {
              //Allow to name a component by his io reference into the parent component
              if (kind.reflectIo == namable) {
                kind.setWeakName(this.getName() + "_" + name)
              }
            }
          }
        }
        case _ =>
      }
    })
  }
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
  var algoId = 1

  var commonClockConfig = ClockDomainConfig()
  var overridingAssignementWarnings = true
  var nodeAreNamed = false
  var nodeAreInferringWidth = false
  val nodeGetWidthWalkedSet: mutable.Set[Node] = mutable.Set[Node]()
  val clockSyncronous = mutable.HashMap[Bool,ArrayBuffer[Bool]]()
  // val nodeWidthInferredCheck = ArrayBuffer[() => Unit]()
  val clockDomainStack = new SafeStack[ClockDomain]
  val componentStack = new SafeStack[Component]{
//    override def pop(e: Component): Unit = {
//      for(task <- e.postCreationTask){
//        task()
//      }
//      e.postCreationTask.clear()
//      super.pop(e)
//    }
  }
  val switchStack = new SafeStack[SwitchStack] //TODO switch
  val conditionalAssignStack = new SafeStack[ConditionalContext]
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
  def /  (right: T): T
  def %  (right: T): T

  def <  (right: T): Bool
  def <= (right: T): Bool
  def >  (right: T): Bool
  def >= (right: T): Bool

  def << (shift : Int) : T
  def >> (shift : Int) : T

  def min(right: T): T = Mux(this < right, this.asInstanceOf[T], right)
  def max(right: T): T = Mux(this < right, right, this.asInstanceOf[T])
}