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

import spinal.core.DslScopeStack.storeAsMutable
import spinal.core.Nameable._
import spinal.core.fiber.{Fiber, Handle}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Stack}
import spinal.core.internals._
import spinal.idslplugin.Location

trait DummyTrait
object DummyObject extends DummyTrait


trait AssertNodeSeverity
object NOTE     extends AssertNodeSeverity
object WARNING  extends AssertNodeSeverity
object ERROR    extends AssertNodeSeverity
object FAILURE  extends AssertNodeSeverity

object REPORT_TIME

/** Min max base function */
trait MinMaxProvider {
  def minValue: BigInt
  def maxValue: BigInt
}

trait MinMaxDecimalProvider {
  def minValue: BigDecimal
  def maxValue: BigDecimal
}

object GlobalData {

  /** Provide a thread local variable (Create a GlobalData for each thread) */
  private [core] val it = new ThreadLocal[GlobalData]

  /** Return the GlobalData of the current thread */
  def get = it.get()
  def set(gb : GlobalData) = {
    it.set(gb)
  }
  /** Reset the GlobalData of the current thread */
  def reset(config: SpinalConfig) = {
    it.set(new GlobalData(config))
    get
  }


}



object DslScopeStack extends ScopeProperty[ScopeStatement]{
  storeAsMutable = true
  override def default = null
}

object ClockDomainStack extends ScopeProperty[Handle[ClockDomain]]{
  storeAsMutable = true
  override def default = null
}

object SwitchStack extends ScopeProperty[SwitchContext]{
  storeAsMutable = true
  override def default = null
}


/**
  * Global data
  */
class GlobalData(val config : SpinalConfig) {

  private var algoIncrementale = 1
  var toplevel : Component = null

  def allocateAlgoIncrementale(): Int = {
    assert(algoIncrementale != Integer.MAX_VALUE)
    algoIncrementale += 1
    return algoIncrementale - 1
  }

  var anonymSignalPrefix: String = null
  var commonClockConfig = ClockDomainConfig()
  var phaseContext : PhaseContext = null

  var nodeAreNamed                 = false
  var nodeAreInferringWidth        = false
  var nodeAreInferringEnumEncoding = false

  val nodeGetWidthWalkedSet = mutable.Set[Widthable]()
  val clockSynchronous      = mutable.HashMap[Bool, ArrayBuffer[Bool]]()
  val zeroWidths          = mutable.LinkedHashSet[(Component, Widthable)]()

  var scalaLocatedEnable = false
  val scalaLocatedComponents = mutable.HashSet[Class[_]]()
  val scalaLocateds = mutable.HashSet[ScalaLocated]()
  val elab = new Fiber()
  elab.setName("global_elab")
  elab.inflightLock.globalData = this
  var onAreaInit = Option.empty[Area => Unit]

  def applyScalaLocated(): Unit ={
    try {
      val pc = GlobalData.get.phaseContext
      pc.walkComponents(c => {
        c.dslBody.walkStatements(s => {
          s match {
            case s : SwitchStatement => if(s.elements.exists(scalaLocateds.contains(_))) scalaLocatedComponents += c.getClass
            case _ =>
          }
          s.walkExpression(e => {
            if (scalaLocateds.contains(e)) {
              scalaLocatedComponents += c.getClass
            }
          })
        })
      })
      for(e <- pc.globalData.scalaLocateds) e match {
        case ctx : ContextUser => {
          val c = ctx.component
          if (c != null) {
            scalaLocatedComponents += c.getClass
          }
        }
        case _ =>
      }
    } catch {
      case e: Throwable =>
    }
  }

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


  val userDatabase      = mutable.LinkedHashMap[Any, Any]()
}


/** Get a link to the globalData */
trait GlobalDataUser {
  var globalData = GlobalData.get
}


trait ContextUser extends GlobalDataUser with ScalaLocated{
  var parentScope : ScopeStatement = if(globalData != null) DslScopeStack.get else null

  def component: Component = if(parentScope != null) parentScope.component else null

  private[core] var instanceCounter = if(globalData != null) globalData.getInstanceCounter else -1

  def getInstanceCounter = instanceCounter

  private[core] def isOlderThan(that: ContextUser): Boolean = this.instanceCounter < that.instanceCounter
}


trait NameableByComponent extends Nameable with GlobalDataUser {
  override def getName() : String = super.getName()
  def getPath(from : Component, to : Component): Seq[Component] ={
    var down = from.parents(from, List(from))
    var up = to.parents(to, List(to))
    var common : Component = null
    while(down.nonEmpty && up.nonEmpty && down.head == up.head){
      common = down.head
      down = down.tail
      up = up.tail
    }
    if(common != null)
      (down.reverse :+ common) ++ up
    else
      down.reverse ++ up
  }

  override def getName(default: String): String = {

    (getMode, nameableRef) match{
      case (NAMEABLE_REF_PREFIXED, other : NameableByComponent) if other.component != null &&  this.component != other.component =>
        val path = getPath(this.component, other.component) :+ nameableRef
        if(path.forall(_.isNamed))
          path.map(_.getName()).mkString("_") + "_" + name
        else
          default
      case (NAMEABLE_REF, other : NameableByComponent) if other.component != null &&  this.component != other.component =>
        val path = getPath(this.component, other.component) :+ nameableRef
        if(path.forall(_.isNamed))
          path.map(_.getName()).mkString("_")
        else
          default
      case _ => super.getName(default)
    }
  }


  override def isNamed: Boolean = {
    (getMode, nameableRef) match{
      case (NAMEABLE_REF_PREFIXED, other : NameableByComponent) if other.component != null &&  this.component != other.component =>
        nameableRef.isNamed && getPath(this.component, other.component).forall(_.isNamed)
      case (NAMEABLE_REF, other : NameableByComponent) if other.component != null && this.component != other.component =>
        nameableRef.isNamed && getPath(this.component, other.component).forall(_.isNamed)
      case _ => super.isNamed
    }
  }

  private[core] def getComponent(): Component
}


/** Assignable trait */
trait Assignable {
  /* private[core] */var compositeAssign: Assignable = null

  /*private[core] */final def compositAssignFrom(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
    if (compositeAssign != null) {
      compositeAssign.compositAssignFrom(that, target, kind)
    } else {
      assignFromImpl(that, target, kind)
    }
  }

  protected def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit

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

  def proposal(ownable: Any, owner: Any) : Boolean = {
    if(ownable.isInstanceOf[OwnableRef]) {
      val ownableTmp = ownable.asInstanceOf[OwnableRef]
      if(ownableTmp.refOwner == null) {
        ownableTmp.asInstanceOf[OwnableRef].setRefOwner(owner)
        return true
      }
    }
    return false
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


  val DATAMODEL_STRONG : Byte = 15
  val USER_SET : Byte = 10
  val DATAMODEL_WEAK : Byte = 5
  val USER_WEAK : Byte = 0
  val REMOVABLE : Byte = -5

  def getNameWithoutPrefix(prefix : Nameable, from : Nameable): String ={
    val stageSlices = prefix.getName.split('_')
    val postfixSlices = from.getName.split('_')
    var i = 0
    val iEnd = stageSlices.length min postfixSlices.length
    while(i != iEnd && stageSlices(i) == postfixSlices(i)) i += 1
    postfixSlices.drop(i).mkString("_")
  }
}


trait Nameable extends OwnableRef with ContextUser{
  import Nameable._

  var name: String = null
  @dontName protected var nameableRef: Nameable = null

  private var mode: Byte = UNNAMED
  private[core] var namePriority: Byte = -100

  protected def getMode = mode

  private[core] def isWeak = namePriority < USER_SET
//  private[core] def setMode(mode: Byte)    = this.mode = mode
//  private[core] def setWeak(weak: Boolean) = this.weak = if (weak) 1 else 0

  def isCompletelyUnnamed: Boolean = getMode == UNNAMED
  def isUnnamed: Boolean = getMode match{
    case UNNAMED               => true
    case ABSOLUTE              => name == null
    case NAMEABLE_REF          => nameableRef == null || nameableRef.isUnnamed
    case NAMEABLE_REF_PREFIXED => nameableRef == null || nameableRef.isUnnamed || name == null
    case OWNER_PREFIXED        => refOwner == null || refOwner.asInstanceOf[Nameable].isUnnamed
  }

  def isNamed: Boolean = !isUnnamed

  def getName(): String = getName("")
  def getPartialName() : String = name
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
          if(ownerName != "" && name != "") {
            if (refOwner.isInstanceOf[Suffixable])
              ownerName + "." + name
            else
              ownerName + "_" + name
          } else
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

  def setLambdaName(isNameBody : => Boolean)(nameGen : => String): this.type ={
    val p = this
    setCompositeName(new Nameable {
      override def isUnnamed = !isNameBody
      override def getName(default: String) = isNamed match {
        case true  => nameGen
        case false => default
      }
    })
    this
  }

  override def toString: String = name

  private[core] def getNameElseThrow: String = {
    getName(null) match {
      case null =>  throw new Exception("Internal error")
      case name =>  name
    }
  }

  def setNameAsWeak(): this.type ={
    namePriority = 0
    this
  }

  def isPriorityApplicable(namePriority: Byte): Boolean = namePriority match{
    case USER_WEAK => namePriority >= this.namePriority
    case USER_SET => namePriority >= this.namePriority
    case DATAMODEL_STRONG => namePriority > this.namePriority
    case DATAMODEL_WEAK => namePriority > this.namePriority
    case REMOVABLE => namePriority > this.namePriority
  }

  def overrideLocalName(name : String): this.type ={
    this.name = name
    this
  }

  def setCompositeName(nameable: Nameable): this.type  = setCompositeName(nameable, weak = false)
  def setCompositeName(nameable: Nameable, weak: Boolean): this.type = setCompositeName(nameable, if(weak) USER_WEAK else USER_SET)
  def setCompositeName(nameable: Nameable, namePriority: Byte): this.type = {
    if (isPriorityApplicable(namePriority)) {
      nameableRef = nameable
      name = null
      mode = NAMEABLE_REF
      this.namePriority = namePriority
    }
    this
  }

  def setCompositeName(nameable: Nameable, postfix: String): this.type = setCompositeName(nameable, postfix, weak = false)
  def setCompositeName(nameable: Nameable, postfix: String, weak: Boolean): this.type = setCompositeName(nameable, postfix,  if(weak) USER_WEAK else USER_SET)
  def setCompositeName(nameable: Nameable, postfix: String, namePriority: Byte): this.type = {
    if (isPriorityApplicable(namePriority)) {
      nameableRef = nameable
      name = postfix
      mode = NAMEABLE_REF_PREFIXED
      this.namePriority = namePriority
    }
    this
  }

  def setPartialName(owner: Nameable): this.type = setPartialName(owner, "", weak = false)
  def setPartialName(owner: Nameable, name: String): this.type = setPartialName(owner, name, weak = false)
  def setPartialName(name: String): this.type = setPartialName(name, weak = false)
  def setPartialName(owner: Nameable, name: String, weak: Boolean): this.type = setPartialName(owner,name, if(weak) USER_WEAK else USER_SET)
  def setPartialName(owner: Nameable, name: String, namePriority: Byte): this.type = {
    if (isPriorityApplicable(namePriority)) {
      setRefOwner(owner)
      this.name = name
      mode = OWNER_PREFIXED
      this.namePriority = namePriority
    }
    this
  }

  def setPartialName(name: String, weak: Boolean): this.type = setPartialName(name, if(weak) USER_WEAK else USER_SET)
  def setPartialName(name: String, namePriority: Byte): this.type = {
    if (isPriorityApplicable(namePriority)) {
      this.name = name
      mode = OWNER_PREFIXED
      this.namePriority = namePriority
    }
    this
  }
  def setPartialName(name: String, namePriority: Byte, owner : Any): this.type = {
    if (isPriorityApplicable(namePriority)) {
      this.name = name
      mode = OWNER_PREFIXED
      this.namePriority = namePriority
      OwnableRef.set(this, owner)
    }
    this
  }

  def unsetName(): this.type = {
    mode = Nameable.UNNAMED
    namePriority = -100
    name = null
    this
  }

  def setName(name : String) : this.type = setName(name, false)
  def setName(name: String, weak: Boolean): this.type = setName(name, if(weak) USER_WEAK else USER_SET)
  def setName(name: String, namePriority: Byte): this.type = {
    if (isPriorityApplicable(namePriority)) {
      this.name = name
      mode = ABSOLUTE
      this.namePriority = namePriority
    }
    this
  }

  def setWeakName(name: String) : this.type = setName(name, weak = true)

  def foreachReflectableNameables(doThat: (Any) => Unit): Unit = {
    Misc.reflect(this, (name, obj) => {
      doThat(obj)
    })
  }

  def reflectNames(): Unit = {
    Misc.reflect(this, (name, obj) => {
      obj match {
        case component: Component =>
          if (component.parent == this.component) {
            component.setPartialName(name, DATAMODEL_WEAK)
            OwnableRef.proposal(component, this)
          }
        case namable: Nameable =>
          if (!namable.isInstanceOf[ContextUser]) {
            namable.setPartialName(name, DATAMODEL_WEAK)
            OwnableRef.proposal(namable, this)
          } else if (namable.asInstanceOf[ContextUser].component == component){
            namable.setPartialName(name, DATAMODEL_WEAK)
            OwnableRef.proposal(namable, this)
          } else {
            if(component != null) for (kind <- component.children) {
              //Allow to name a component by his io reference into the parent component
              if (kind.reflectIo == namable) {
                kind.setPartialName(name, DATAMODEL_WEAK)
                OwnableRef.proposal(kind, this)
              }
            }
          }
        case _ =>
      }
    })
  }

}


trait ScalaLocated extends GlobalDataUser {

  var scalaTrace = if(globalData == null || !globalData.scalaLocatedEnable || (DslScopeStack.get != null && !globalData.scalaLocatedComponents.contains(DslScopeStack.get.component.getClass))) {
    null
  } else {
    new Throwable()
  }

  def setScalaLocated(source: ScalaLocated): this.type = {
    scalaTrace = source.scalaTrace
    this
  }

  def getScalaTrace(): Throwable = {
    globalData.scalaLocateds += this
    scalaTrace
  }


  def getScalaLocationLong: String = ScalaLocated.long(getScalaTrace())
  def getScalaLocationShort: String = ScalaLocated.short(getScalaTrace())
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

  def long2(trace: Array[StackTraceElement], tab: String = "    "): String = {
    if(trace == null) return "???"

    filterStackTrace(trace).map(_.toString).filter(filter).map(tab + _ ).mkString("\n") + "\n\n"
  }

  def short: String = short(new Throwable())
  def long: String  = long(new Throwable())
}


trait SpinalTagReady {

  var _spinalTags: mutable.LinkedHashSet[SpinalTag] =  null

  def spinalTags: mutable.LinkedHashSet[SpinalTag] = {
    if(_spinalTags == null)
      _spinalTags = new mutable.LinkedHashSet[SpinalTag]{
//        override def initialSize: Int = 4
      }
    _spinalTags
  }

  def addTag[T <: SpinalTag](spinalTag: T): this.type = {
    if (!spinalTag.allowMultipleInstance && hasTag(spinalTag.getClass)) {
      val existingTag = getTag(spinalTag.getClass).get
      SpinalError(s"Conflicting tags added to the same item! ${existingTag} ; ${spinalTag}")
    }
    else spinalTags += spinalTag
    this
  }

  def addTags[T <: SpinalTag](tags: Iterable[T]): this.type = {
    for (tag <- tags) addTag(tag)
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
  def hasTag[T <: SpinalTag](clazz: Class[T]): Boolean = {
    if (_spinalTags == null)             return false
    val tag = _spinalTags.find(_.getClass == clazz)
    if (tag.isDefined) return true
    return false
  }

  //Feed it with classOf[?] to avoid intermodule problems
  def getTag[T <: SpinalTag](clazz: Class[T]): Option[T] = {
    if(_spinalTags == null) return None
    val tag = _spinalTags.find(_.getClass == clazz)
    if(tag.isDefined) return Option(tag.get.asInstanceOf[T])
    None
  }

  def getTags() : mutable.LinkedHashSet[SpinalTag] = {
    if(_spinalTags == null)
      mutable.LinkedHashSet[SpinalTag]()
    else
      _spinalTags
  }
  def foreachTag(body : SpinalTag => Unit) : Unit = {
    if(_spinalTags == null) return
    _spinalTags.foreach(body)
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

  def addAttribute(attribute: Attribute): this.type = addTag(attribute)
  def addAttribute(name: String): this.type = addAttribute(new AttributeFlag(name))
  def addAttribute(name: String, value: String): this.type = addAttribute(new AttributeString(name, value))
  def addAttribute(name: String, value: Int): this.type = addAttribute(new AttributeInteger(name, value))

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
  def allowMultipleInstance = true // Allow multiple instances of the tag on the same object
  def ioTag                 = false // Propagate tag to IO

  def apply[T <: SpinalTagReady](that : T) : T = {
    that.addTag(this)
    that
  }
  def apply(that : SpinalTagReady, others : SpinalTagReady*) : Unit = {
    apply(that)
    others.foreach(apply)
  }
}


trait SpinalTagGetter[T] extends SpinalTag{
  def get() : T
}

class DefaultTag(val that: BaseType) extends SpinalTag
object allowDirectionLessIoTag       extends SpinalTag
object unsetRegIfNoAssignementTag    extends SpinalTag
object allowAssignmentOverride       extends SpinalTag
object allowOutOfRangeLiterals               extends SpinalTag{
  def apply(that : Bool) = doIt(that)
  def doIt(that : Bool) = {
    assert(that.dlcHasOnlyOne)
    that.dlcHead match {
      case s: DataAssignmentStatement => s.source match {
        case t : SpinalTagReady => t.addTag(spinal.core.allowOutOfRangeLiterals)
        case _ => ???
      }
      case _ => ???
    }
    this
  }
}

object noInit                        extends SpinalTag
object unusedTag                     extends SpinalTag
object noCombinatorialLoopCheck      extends SpinalTag
object noLatchCheck                  extends SpinalTag
object noBackendCombMerge            extends SpinalTag
object crossClockDomain              extends SpinalTag{ override def moveToSyncNode = true }
object crossClockBuffer              extends SpinalTag{ override def moveToSyncNode = true }
object randomBoot                    extends SpinalTag{ override def moveToSyncNode = true }
object tagAutoResize                 extends SpinalTag{ override def duplicative = true }
object tagTruncated                  extends SpinalTag{
  override def duplicative = true
  override def canSymplifyHost: Boolean = true
}
object tagAFixResized                   extends SpinalTag
class IfDefTag(val cond : String)       extends SpinalTag

class CommentTag(val comment : String) extends SpinalTag

class ExternalDriverTag(val driver : Data)             extends SpinalTag{
  override def allowMultipleInstance = false
}


class CrossClockBufferDepth(val value : Int) extends SpinalTag{
  override val allowMultipleInstance = false
}

object Driver {
  val startTime = System.currentTimeMillis()
  def executionTime: Double = (System.currentTimeMillis - startTime) / 1000.0
}

//Avoid having case class matching
trait OverridedEqualsHashCode{
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def hashCode(): Int = super.hashCode()
}


/**
  * Base operations for numbers
  * @tparam T the type which is associated with the base operation
  */
trait Num[T <: Data] {

  private[core] var Qtag: QFormat = null

  def Q: QFormat = Qtag

  def tag(q: QFormat): T

  private[core] def getfixSection(q: QFormat): Range.Inclusive = {
    require(this.Q != null, "init QFormat first")
    require(this.Q.fraction >= q.fraction, "fraction part exceed")
    val lpos = Q.fraction - q.fraction
    val hpos = lpos + q.width - 1
    hpos downto lpos
  }

  /** Addition */
  def + (right: T): T
  /** Safe Addition with 1 bit expand */
  def +^(right: T): T
  /** Safe Addition with saturation */
  def +| (right: T): T
  /** Substraction */
  def - (right: T): T
  /** Safe Substraction with 1 bit expand*/
  def -^ (right: T): T
  /** Safe Substraction with saturation*/
  def -| (right: T): T
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

  /** highest m bits Saturation Operation*/
  def sat(m: Int): T
  def trim(m: Int): T
  def sat(width: BitCount): T = sat(width.value)
  def trim(width: BitCount): T = trim(width.value)
  /**lowest n bits Round Operation */
  def floor(n: Int): T
  def ceil(n: Int, align: Boolean): T
  def floorToZero(n: Int): T
  def ceilToInf(n: Int, align: Boolean): T
  def roundUp(n: Int, align: Boolean): T
  def roundDown(n: Int, align: Boolean): T
  def roundToZero(n: Int, align: Boolean): T
  def roundToInf(n: Int, align: Boolean): T
  def roundToEven(n: Int, align: Boolean): T
  def roundToOdd(n: Int, align: Boolean): T
  def round(n: Int, align: Boolean): T
  /**lowest n bits Round Operation by BitCount */
  def ceil(width: BitCount, align: Boolean): T         = ceil(width.value, align)
  def floor(width: BitCount): T                        = floor(width.value)
  def floorToZero(width: BitCount): T                  = floorToZero(width.value)
  def ceilToInf(width: BitCount, align: Boolean): T    = ceilToInf(width.value, align)
  def roundUp(width: BitCount, align: Boolean): T      = roundUp(width.value, align)
  def roundDown(width: BitCount, align: Boolean): T    = roundDown(width.value, align)
  def roundToZero(width: BitCount, align: Boolean): T  = roundToZero(width.value, align)
  def roundToInf(width: BitCount, align: Boolean): T   = roundToInf(width.value, align)
  def roundToEven(width: BitCount, align: Boolean): T  = roundToEven(width.value, align)
  def roundToOdd(width: BitCount, align: Boolean): T   = roundToOdd(width.value, align)
  def round(width: BitCount, align: Boolean): T        = round(width.value, align)
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
