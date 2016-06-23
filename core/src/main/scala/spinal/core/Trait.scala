
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
case class ExpNumber(val value: Int) {}
case class PosCount(val value: Int) {}

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
  override def postTypeFactory[T <: Data](that: T): T = applyIt(that)
}

object in extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asInput()
}

object out extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asOutput()
}

object inWithNull extends IODirection {
  override def applyIt[T <: Data](data: T): T = if(data != null) data.asInput() else data
}

object outWithNull extends IODirection {
  override def applyIt[T <: Data](data: T): T = if(data != null) data.asOutput() else data
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
  val getClockSoftResetId: Int = 3
}

abstract class SyncNode(clockDomain: ClockDomain = ClockDomain.current) extends Node {
  var clock      : Bool = clockDomain.clock
  var enable     : Bool = clockDomain.clockEnable
  var reset      : Bool = clockDomain.reset
  var softReset  : Bool = clockDomain.softReset


  override def onEachInput(doThat: (Node, Int) => Unit): Unit = {
    doThat(clock,0)
    if(isUsingEnableSignal)doThat(enable,1)
    if(isUsingResetSignal) doThat(reset,2)
    if(isUsingSoftResetSignal) doThat(softReset,3)
  }
  override def onEachInput(doThat: (Node) => Unit): Unit = {
    doThat(clock)
    if(isUsingEnableSignal)doThat(enable)
    if(isUsingResetSignal) doThat(reset)
    if(isUsingSoftResetSignal) doThat(softReset)
  }

  override def setInput(id: Int, node: Node): Unit = id match{
    case 0 => clock = node.asInstanceOf[Bool]
    case 1 if(isUsingEnableSignal) => enable = node.asInstanceOf[Bool]
    case 2 if(isUsingResetSignal)  => reset = node.asInstanceOf[Bool]
    case 3 if(isUsingSoftResetSignal)  => softReset = node.asInstanceOf[Bool]
  }

  override def getInputsCount: Int = 1 + (if(isUsingEnableSignal) 1 else 0) + (if(isUsingResetSignal) 1 else 0) + (if(isUsingSoftResetSignal) 1 else 0)
  override def getInputs: Iterator[Node] = {
    val itr = (isUsingEnableSignal,isUsingResetSignal) match{
      case (false,false) => Iterator(clock             )
      case (false,true)  => Iterator(clock,       reset)
      case (true,false)  => Iterator(clock,enable      )
      case (true,true)   => Iterator(clock,enable,reset)
    }
    if(isUsingSoftResetSignal)
      itr ++ List(softReset)
    else
      itr
  }

  override def getInput(id: Int): Node = id match{
    case 0 => clock
    case 1 if(isUsingEnableSignal) => enable
    case 2 if(isUsingResetSignal)  => reset
    case 3 if(isUsingSoftResetSignal)  => softReset
  }


  override private[core] def getOutToInUsage(inputId: Int, outHi: Int, outLo: Int): (Int, Int) = inputId match{
    case SyncNode.getClockInputId =>  (0,0)
    case SyncNode.getClockEnableId => (0,0)
    case SyncNode.getClockResetId =>  (0,0)
    case 3 =>  (0,0)
  }

  final def getLatency = 1 //if not final => update latencyAnalyser

  def getSynchronousInputs = {
    var ret : List[Node] = Nil
    if(clockDomain.config.resetKind == SYNC  && isUsingResetSignal) ret = getResetStyleInputs ++ ret
    if(isUsingEnableSignal) ret = getClockEnable :: ret
    if(isUsingSoftResetSignal) ret = getSoftReset :: ret
    ret
  }

  def getAsynchronousInputs : List[Node] = (if (clockDomain.config.resetKind == ASYNC && isUsingResetSignal) getResetStyleInputs else Nil)

  def getResetStyleInputs = List[Node](getReset)

  def isUsingSoftResetSignal : Boolean
  def isUsingResetSignal: Boolean
  def isUsingEnableSignal: Boolean = enable != null
  def setUseReset = {
    reset = clockDomain.reset
  }
  def getClockDomain: ClockDomain = clockDomain

  def getClock: Bool = clock
  def getClockEnable: Bool = enable
  def getReset: Bool = reset
  def getSoftReset: Bool = softReset
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
  //def getDisplayName() : String = if(isNamed) getName() else "???"
  def getDisplayName() : String = {
    val name = getName()
    if(name.length == 0)
      "???"
    else
      name
  }
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

  def forEachNameables(doThat : (Any) => Unit) : Unit = {
    Misc.reflect(this, (name, obj) => {
      doThat(obj)
    })
  }
}

object ScalaLocated {
  var unfiltredFiles = mutable.Set[String](/*"SpinalUtils.scala"*/)
  var filtredFiles = mutable.Set[String]()
  //var unfiltredPackages = mutable.Set("spinal.code.","spinal.bug.","spinal.scalaTest.")

  def filterStackTrace(that : Array[StackTraceElement]) = that.filter(trace => {
    val className = trace.getClassName
    !(className.startsWith("scala.") || className.startsWith("spinal.core")) || ScalaLocated.unfiltredFiles.contains(trace.getFileName)
  })
  def short(scalaTrace : Throwable) : String = {
    if(scalaTrace == null) return "???"
    filterStackTrace(scalaTrace.getStackTrace)(0).toString
  }

  def long(scalaTrace : Throwable,tab : String = "    "): String = {
    if(scalaTrace == null) return "???"
    filterStackTrace(scalaTrace.getStackTrace).map(tab + _.toString).mkString("\n") + "\n\n"
  }

  def short: String = short(new Throwable())
  def long: String = long(new Throwable())
}


//class SpinalMessage{
//  private val nodes = ArrayBuffer[Node]()
//  private val components = ArrayBuffer[Component]()
//  private var message : String = null
//}

trait ScalaLocated extends GlobalDataUser {
  private[core] var scalaTrace = if (!globalData.scalaLocatedEnable) null else new Throwable()

  def getScalaLocationLong: String = ScalaLocated.long(scalaTrace)
  def getScalaLocationShort: String = ScalaLocated.short(scalaTrace)
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

  //Feed it with classOf[?] to avoid intermodule problems
  def getTag[T <: SpinalTag](clazz : Class[T]) : Option[T] = {
    val tag = spinalTags.find(_.getClass == clazz)
    if(tag.isDefined) return Option(tag.get.asInstanceOf[T])
    if (compositeTagReady != null) compositeTagReady.getTag[T](clazz)
    None
  }
}

trait SpinalTag {
  def isAssignedTo(that: SpinalTagReady) = that.hasTag(this)
}

object unusedTag extends SpinalTag
object crossClockDomain extends SpinalTag
object crossClockBuffer extends SpinalTag
object randomBoot extends SpinalTag
object tagAutoResize extends SpinalTag
object tagTruncated extends SpinalTag

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
            for (kind <- component.children) {
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
  val newClockEnable : Bool = if (ClockDomain.current.config.clockEnableActiveLevel == HIGH)
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

  val newReset : Bool = if (ClockDomain.current.config.resetActiveLevel == HIGH)
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
  def allocateAlgoId() : Int = {algoId += 1; return algoId-1}
  var commonClockConfig = ClockDomainConfig()
  var overridingAssignementWarnings = true
  var nodeAreNamed = false
  var nodeAreInferringWidth = false
  val nodeGetWidthWalkedSet = mutable.Set[Widthable]()
  val clockSyncronous = mutable.HashMap[Bool,ArrayBuffer[Bool]]()
  // val nodeWidthInferredCheck = ArrayBuffer[() => Unit]()
  val clockDomainStack = new SafeStack[ClockDomain]
  val componentStack = new SafeStackWithStackable[Component]{
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
 // val widthCheckers = new ArrayBuffer[WidthChecker]()

  var scalaLocatedEnable = false
  var instanceCounter = 0
  val pendingErrors = mutable.ArrayBuffer[() => String]()
  val postBackendTask = mutable.ArrayBuffer[() => Unit]()

  val jsonReports = ArrayBuffer[String]()

  def getInstanceCounter: Int = {
    val temp = instanceCounter
    instanceCounter = instanceCounter + 1
    temp
  }

  def getThrowable() = if(scalaLocatedEnable) new Throwable else null

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