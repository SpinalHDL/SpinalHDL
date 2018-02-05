/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Stack}
import spinal.core.internals._


/**
  * Trait used to set the direction of a data
  */
trait IODirection extends BaseTypeFactory {

  def applyIt[T <: Data](data: T): T
  def apply[T <: Data](data: T): T = applyIt(data)
  def apply[T <: Data](datas: T*): Unit = datas.foreach(applyIt(_))
  def apply(enum: SpinalEnum) = applyIt(enum.craft())
  def cloneOf[T <: Data](that: T): T = applyIt(spinal.core.cloneOf(that))

  override def Bool() = applyIt(super.Bool())
  override def Bits() = applyIt(super.Bits())
  override def UInt() = applyIt(super.UInt())
  override def SInt() = applyIt(super.SInt())
  override def Vec[T <: Data](elements: TraversableOnce[T]): Vec[T] = applyIt(super.Vec(elements))

  override def postTypeFactory[T <: Data](that: T): T = applyIt(that)
}

/** Set a data to input */
object in extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asInput()
}

/** Set a data to output */
object out extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asOutput()
}

/** Set a data to inout */
object inout extends IODirection {
  override def applyIt[T <: Data](data: T): T = data.asInOut()
}

/** Set a data to in if the data is not null */
object inWithNull extends IODirection {
  override def applyIt[T <: Data](data: T): T = if(data != null) data.asInput() else data
}

/** Set a data to out if the data is not null */
object outWithNull extends IODirection {
  override def applyIt[T <: Data](data: T): T = if(data != null) data.asOutput() else data
}


trait AssertNodeSeverity
object NOTE     extends AssertNodeSeverity
object WARNING  extends AssertNodeSeverity
object ERROR    extends AssertNodeSeverity
object FAILURE  extends AssertNodeSeverity


/** Min max base function */
trait MinMaxProvider {
  def minValue: BigInt
  def maxValue: BigInt
}

object GlobalData {

  /** Provide a thread local variable (Create a GlobalData for each thread) */
  private val it = new ThreadLocal[GlobalData]

  /** Return the GlobalData of the current thread */
  def get = it.get()

  /** Reset the GlobalData of the current thread */
  def reset = {
    it.set(new GlobalData)
    get
  }
}

/**
  * Global data
  */
class GlobalData {

  val dslScope       = new Stack[ScopeStatement]()
  val dslClockDomain = new Stack[ClockDomain]()

  def currentComponent = dslScope.headOption match {
    case None        => null
    case Some(scope) => scope.component
  }

  def currentScope = dslScope.headOption match {
    case None        => null
    case Some(scope) => scope
  }

  def currentClockDomain = dslClockDomain.headOption match {
    case None     => null
    case Some(cd) => cd
  }

  private var algoIncrementale = 1

  def allocateAlgoIncrementale(): Int = {
    assert(algoIncrementale != Integer.MAX_VALUE)
    algoIncrementale += 1
    return algoIncrementale - 1
  }

  var anonymSignalPrefix: String = null
  var commonClockConfig = ClockDomainConfig()

  var nodeAreNamed                 = false
  var nodeAreInferringWidth        = false
  var nodeAreInferringEnumEncoding = false

  val nodeGetWidthWalkedSet = mutable.Set[Widthable]()
  val clockSyncronous       = mutable.HashMap[Bool, ArrayBuffer[Bool]]()
  val switchStack           = Stack[SwitchContext]()

  var scalaLocatedEnable = false
  val scalaLocatedInterrests = mutable.HashSet[Class[_]]()
  var instanceCounter    = 0
  val pendingErrors      = mutable.ArrayBuffer[() => String]()
  val postBackendTask    = mutable.ArrayBuffer[() => Unit]()
  var netlistLockError   = new Stack[() => Unit]()

  netlistLockError.push(null)

  def pushNetlistLock(error: () => Unit): Unit = netlistLockError.push(error)

  def popNetlistLock(): Unit = netlistLockError.pop

  def pushNetlistUnlock(): Unit = netlistLockError.push(null)

  def popNetlistUnlock(): Unit = netlistLockError.pop

  def netlistUpdate(): Unit = {
    if(netlistLockError.head != null){
      netlistLockError.head()
    }
  }

  val jsonReports = ArrayBuffer[String]()

  def getInstanceCounter: Int = {
    val temp = instanceCounter
    instanceCounter = instanceCounter + 1
    temp
  }

  def getThrowable() = if(scalaLocatedEnable) new Throwable else null

  def addPostBackendTask(task: => Unit): Unit = postBackendTask += (() => task)
  def addJsonReport(report: String): Unit = jsonReports += report
}


/** Get a link to the globalData */
trait GlobalDataUser {
  val globalData = GlobalData.get
}


trait ContextUser extends GlobalDataUser with ScalaLocated{
  var parentScope = if(globalData != null) globalData.currentScope else null


  override def getScalaTrace() = {
    if(!globalData.scalaLocatedEnable && component != null) {
      globalData.scalaLocatedInterrests += component.getClass
    }
    super.getScalaTrace()
  }

  def component: Component = if(parentScope != null) parentScope.component else null

  private[core] var instanceCounter = if(globalData != null) globalData.getInstanceCounter else -1

  def getInstanceCounter = instanceCounter

  private[core] def isOlderThan(that: ContextUser): Boolean = this.instanceCounter < that.instanceCounter
}


trait NameableByComponent extends Nameable with GlobalDataUser {

  override def getName(): String = {
    if(!globalData.nodeAreNamed) {
      if (isUnnamed) {
        val c = getComponent()
        if(c != null)c.nameElements()
      }
    }
    super.getName()
  }

  private[core] def getComponent(): Component
}


/** Assignable trait */
trait Assignable {
  /* private[core] */var compositeAssign: Assignable = null

  /*private[core] */final def compositAssignFrom(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = {
    if (compositeAssign != null) {
      compositeAssign.compositAssignFrom(that, target, kind)
    } else {
      assignFromImpl(that, target, kind)
    }
  }

  private[core] def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit

  def getRealSourceNoRec: Any

  def getRealSource: Any = compositeAssign match {
    case null => this.getRealSourceNoRec
    case that => that.getRealSource
  }
}


object OwnableRef {

  def set(ownable: Any, owner: Any) = {
    if(ownable.isInstanceOf[OwnableRef])
      ownable.asInstanceOf[OwnableRef].setRefOwner(owner)
  }

  def proposal(ownable: Any, owner: Any) = {
    if(ownable.isInstanceOf[OwnableRef]) {
      val ownableTmp = ownable.asInstanceOf[OwnableRef]
      if(ownableTmp.refOwner == null)
        ownableTmp.asInstanceOf[OwnableRef].setRefOwner(owner)
    }
  }
}


trait OwnableRef {

  type RefOwnerType

  @dontName var refOwner: RefOwnerType = null.asInstanceOf[RefOwnerType]

  def setRefOwner(that: Any): Unit = {
    refOwner = that.asInstanceOf[RefOwnerType]
  }

  def getRefOwnersChain(): List[Any] = {
    refOwner match {
      case null              => Nil
      case owner: OwnableRef => owner.getRefOwnersChain() :+ owner
      case _                 => refOwner :: Nil
    }
  }
}


object Nameable{
  val UNNAMED               : Byte = 0
  val ABSOLUTE              : Byte = 1
  val NAMEABLE_REF          : Byte = 2
  val OWNER_PREFIXED        : Byte = 3
  val NAMEABLE_REF_PREFIXED : Byte = 4
}


trait Nameable extends OwnableRef with ContextUser{
  import Nameable._

  private var name: String = null
  private var nameableRef: Nameable = null

  private var mode: Byte = UNNAMED
  private var weak: Byte = 1

  private def getMode = mode

  private[core] def isWeak = weak != 0
  private[core] def setMode(mode: Byte)    = this.mode = mode
  private[core] def setWeak(weak: Boolean) = this.weak = if (weak) 1 else 0

  def isUnnamed: Boolean = getMode match{
    case UNNAMED               => true
    case ABSOLUTE              => name == null
    case NAMEABLE_REF          => nameableRef == null || nameableRef.isUnnamed
    case NAMEABLE_REF_PREFIXED => nameableRef == null || nameableRef.isUnnamed || name == null
    case OWNER_PREFIXED        => refOwner == null || refOwner.asInstanceOf[Nameable].isUnnamed
  }

  def isNamed: Boolean = !isUnnamed

  def getName(): String = getName("")
  def getName(default: String): String = getMode match{
    case UNNAMED               => default
    case ABSOLUTE              => name
    case NAMEABLE_REF          => if(nameableRef != null && nameableRef.isNamed) nameableRef.getName() else default
    case NAMEABLE_REF_PREFIXED => if(nameableRef != null && nameableRef.isNamed) nameableRef.getName() + "_" + name else default
    case OWNER_PREFIXED        =>
      if(refOwner != null) {
        val ref = refOwner.asInstanceOf[Nameable]
        if (ref.isNamed) {
          val ownerName = ref.getName()
          if(ownerName != "" && name != "")
            ownerName + "_" + name
          else
            ownerName + name
        } else {
          default
        }
      } else default
  }

  def getDisplayName(): String = {
    val name = getName()
    if(name.length == 0)
      "???"
    else
      name
  }

  override def toString: String = name

  private[core] def getNameElseThrow: String = {
    getName(null) match {
      case null =>  throw new Exception("Internal error")
      case name =>  name
    }
  }

  def setCompositeName(nameable: Nameable): this.type  = setCompositeName(nameable, weak = false)
  def setCompositeName(nameable: Nameable, weak: Boolean): this.type = {
    if (!weak || (mode == UNNAMED)) {
      nameableRef = nameable
      name = null
      setMode(NAMEABLE_REF)
      setWeak(weak)
    }
    this
  }

  def setCompositeName(nameable: Nameable, postfix: String): this.type = setCompositeName(nameable, postfix, weak = false)
  def setCompositeName(nameable: Nameable, postfix: String, weak: Boolean): this.type = {
    if (!weak || (mode == UNNAMED)) {
      nameableRef = nameable
      name = postfix
      setMode(NAMEABLE_REF_PREFIXED)
      setWeak(weak)
    }
    this
  }

  def setPartialName(owner: Nameable, name: String): this.type = setPartialName(owner, name, weak = false)
  def setPartialName(name: String): this.type = setPartialName(name, weak = false)

  def setPartialName(owner: Nameable, name: String, weak: Boolean): this.type = {
    if (!weak || (mode == UNNAMED)) {
      setRefOwner(owner)
      this.name = name
      setMode(OWNER_PREFIXED)
      setWeak(weak)
    }
    this
  }

  def setPartialName(name: String, weak: Boolean): this.type = {
    if (!weak || (mode == UNNAMED)) {
      this.name = name
      setMode(OWNER_PREFIXED)
      setWeak(weak)
    }
    this
  }

  def unsetName(): this.type = {
    setMode(Nameable.UNNAMED)
    this
  }

  def setName(name: String, weak: Boolean = false): this.type = {
    if (!weak || (mode == UNNAMED)) {
      this.name = name
      setMode(ABSOLUTE)
      setWeak(weak)
    }
    this
  }

  def setWeakName(name: String) : this.type = setName(name, weak = true)

  def foreachReflectableNameables(doThat: (Any) => Unit): Unit = {
    Misc.reflect(this, (name, obj) => {
      doThat(obj)
    })
  }
}


object ScalaLocated {

  var unfiltredFiles = mutable.Set[String](/*"SpinalUtils.scala"*/)
  var filtredFiles   = mutable.Set[String]()

  def filterStackTrace(that: Array[StackTraceElement]) = that.filter(trace => {
    val className = trace.getClassName
    !(className.startsWith("scala.") || className.startsWith("spinal.core") || !filter(trace.toString)) || ScalaLocated.unfiltredFiles.contains(trace.getFileName)
  })

  def short(scalaTrace: Throwable): String = {
    if(scalaTrace == null) return "???"
    filterStackTrace(scalaTrace.getStackTrace)(0).toString
  }

  def filter(that: String): Boolean = {
    if(that.startsWith("sun.reflect"))       return false
    if(that.startsWith("java.lang.reflect")) return false
    if(that.startsWith("java.lang.Class"))   return false
    if(that.startsWith("com.intellij"))      return false
    if(that.startsWith("org.scalatest"))     return false
    return true
  }

  def long(scalaTrace: Throwable, tab: String = "    "): String = {
    if(scalaTrace == null) return "???"

    filterStackTrace(scalaTrace.getStackTrace).map(_.toString).filter(filter).map(tab + _ ).mkString("\n") + "\n\n"
  }

  def short: String = short(new Throwable())
  def long: String  = long(new Throwable())
}


trait ScalaLocated extends GlobalDataUser {

  private var scalaTrace = if(globalData == null || !globalData.scalaLocatedEnable || (globalData.currentScope != null && !globalData.scalaLocatedInterrests.contains(globalData.currentScope.component.getClass))) {
    null
  } else {
    new Throwable()
  }

  def setScalaLocated(source: ScalaLocated): this.type = {
    scalaTrace = source.scalaTrace
    this
  }

  def getScalaTrace(): Throwable = scalaTrace


  def getScalaLocationLong: String = ScalaLocated.long(getScalaTrace())
  def getScalaLocationShort: String = ScalaLocated.short(getScalaTrace())
}


trait SpinalTagReady {

  var _spinalTags: mutable.Set[SpinalTag] =  null

  def spinalTags: mutable.Set[SpinalTag] = {
    if(_spinalTags == null)
      _spinalTags = new mutable.HashSet[SpinalTag]{
        override def initialSize: Int = 4
      }
    _spinalTags
  }

  def addTag(spinalTag: SpinalTag): this.type = {
    spinalTags += spinalTag
    this
  }

  def addTags(tags: Iterable[SpinalTag]): this.type = {
    spinalTags ++= tags
    this
  }

  def removeTag(spinalTag: SpinalTag): this.type = {
    if(_spinalTags != null)
      _spinalTags -= spinalTag
    this
  }

  def removeTags(tags: Iterable[SpinalTag]): this.type = {
    if(_spinalTags != null)
      _spinalTags --= tags
    this
  }

  def hasTag(spinalTag: SpinalTag): Boolean = {
    if (_spinalTags == null)             return false
    if (_spinalTags.contains(spinalTag)) return true
    return false
  }

  //Feed it with classOf[?] to avoid intermodule problems
  def getTag[T <: SpinalTag](clazz: Class[T]): Option[T] = {
    if(_spinalTags == null) return None
    val tag = _spinalTags.find(_.getClass == clazz)
    if(tag.isDefined) return Option(tag.get.asInstanceOf[T])
    None
  }

  def findTag(cond: (SpinalTag) => Boolean): Option[SpinalTag] = {
    if(_spinalTags == null) return None
    _spinalTags.find(cond)
  }

  def existsTag(cond: (SpinalTag) => Boolean): Boolean = {
    if(_spinalTags == null) return false
    _spinalTags.exists(cond)
  }

  def isEmptyOfTag: Boolean = {
    if(_spinalTags == null) return true
    _spinalTags.isEmpty
  }

  def filterTag(cond: (SpinalTag) => Boolean): Iterable[SpinalTag] = {
    if(_spinalTags == null) return Nil
    _spinalTags.filter(cond)
  }

  def addAttribute(attribute: Attribute): this.type
  def addAttribute(name: String): this.type = addAttribute(new AttributeFlag(name))
  def addAttribute(name: String, value: String): this.type = addAttribute(new AttributeString(name, value))

  def onEachAttributes(doIt: (Attribute) => Unit): Unit = {
    if(_spinalTags == null) return

    _spinalTags.foreach {
      case attribute: Attribute => doIt(attribute)
      case _                    =>
    }
  }

  def instanceAttributes: Iterable[Attribute] = {
    if(_spinalTags == null) return Nil
    val array = ArrayBuffer[Attribute]()
    _spinalTags.foreach(e => if(e.isInstanceOf[Attribute])array += e.asInstanceOf[Attribute])
    array
  }

  def instanceAttributes(language: Language): Iterable[Attribute] = {
    if(_spinalTags == null) return Nil
    val array = ArrayBuffer[Attribute]()
    _spinalTags.foreach(e => if(e.isInstanceOf[Attribute] && e.asInstanceOf[Attribute].isLanguageReady(language))array += e.asInstanceOf[Attribute])
    array
  }
}


object SpinalTagReady{
  def splitNewSink(source: SpinalTagReady, sink: SpinalTagReady): Unit = {
    source.instanceAttributes.foreach(e => {
      if(e.duplicative){
        sink.addTag(e)
      }
    })
  }
}


trait SpinalTag {
  def isAssignedTo(that: SpinalTagReady) = that.hasTag(this)
  def moveToSyncNode        = false //When true, Spinal will automaticaly move the tag to the driving syncNode
  def duplicative           = false
  def driverShouldNotChange = false
  def canSymplifyHost       = false
}

class DefaultTag(val that: BaseType) extends SpinalTag
object allowDirectionLessIoTag       extends SpinalTag
object unsetRegIfNoAssignementTag    extends SpinalTag
object allowAssignmentOverride       extends SpinalTag
object unusedTag                     extends SpinalTag
object noCombinatorialLoopCheck      extends SpinalTag
object noBackendCombMerge            extends SpinalTag
object crossClockDomain              extends SpinalTag{ override def moveToSyncNode = true }
object crossClockBuffer              extends SpinalTag{ override def moveToSyncNode = true }
object randomBoot                    extends SpinalTag{ override def moveToSyncNode = true }
object tagAutoResize                 extends SpinalTag{ override def duplicative = true }
object tagTruncated                  extends SpinalTag{
  override def duplicative = true
  override def canSymplifyHost: Boolean = true
}


object Driver {
  val startTime = System.currentTimeMillis()
  def executionTime: Double = (System.currentTimeMillis - startTime) / 1000.0
}


trait OverridedEqualsHashCode{
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def hashCode(): Int = super.hashCode()
}


/**
  * Base operations for numbers
  * @tparam T the type which is associated with the base operation
  */
trait Num[T <: Data] {

  /** Addition */
  def + (right: T): T
  /** Substraction */
  def - (right: T): T
  /** Multiplication */
  def * (right: T): T
  /** Division */
  def / (right: T): T
  /** Modulo */
  def % (right: T): T

  /** Is less than right */
  def <  (right: T): Bool
  /** Is equal or less than right */
  def <= (right: T): Bool
  /** Is greater than right */
  def >  (right: T): Bool
  /** Is equal or greater than right */
  def >= (right: T): Bool

  /** Logical left shift (w(T) = w(this) + shift)*/
  def << (shift: Int): T
  /** Logical right shift (w(T) = w(this) - shift)*/
  def >> (shift: Int): T

  /** Return the minimum value between this and right  */
  def min(right: T): T = Mux(this < right, this.asInstanceOf[T], right)
  /** Return the maximum value between this and right  */
  def max(right: T): T = Mux(this < right, right, this.asInstanceOf[T])
}


/**
  * Bitwise Operation
  * @tparam T the type which is associated with the bitwise operation
  */
trait BitwiseOp[T <: Data]{

  /** Logical AND operator */
  def &(right: T): T

  /** Logical OR operator */
  def |(right: T): T

  /** Logical XOR operator */
  def ^(right: T): T

  /** Inverse bitwise operator */
  def unary_~ : T
}
