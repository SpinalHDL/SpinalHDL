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

import spinal.core.internals._
import spinal.idslplugin.Location

import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

trait TypeFactory{
  def postTypeFactory[T <: Data](that: T) = that
}

/**
  * Base type factory
  */
trait BaseTypeFactory extends BoolFactory with BitsFactory with UIntFactory with SIntFactory with VecFactory with SFixFactory with UFixFactory


/**
  * Base type Cast
  */
trait BaseTypeCast extends SFixCast with UFixCast

object BaseType{
  final val isRegMask      = 1
  final val isTypeNodeMask = 2
  final val isVitalMask    = 4
  final val isAnalogMask   = 8
  final val isFrozen       = 16
}

/**
  * Abstract base class of all Spinal types
  */
abstract class BaseType extends Data with DeclarationStatement with StatementDoubleLinkedContainer[BaseType, AssignmentStatement] with Expression {

  DslScopeStack.get match {
    case null =>
    case scope => scope.append(this)
  }

  var clockDomain = ClockDomain.current

  /** Type of the base type */
  private var btFlags = 0

  override def isAnalog = (btFlags & BaseType.isAnalogMask) != 0
  override def isReg    = (btFlags & BaseType.isRegMask) != 0
  override def isComb   = (btFlags & (BaseType.isRegMask | BaseType.isAnalogMask)) == 0

  override def setAsAnalog(): this.type = {
    btFlags |= BaseType.isAnalogMask; this
  }

  /** Set baseType to reg */
  override def setAsReg(): this.type = {
    btFlags |= BaseType.isRegMask; this
  }

  /** Set baseType to Combinatorial */
  override def setAsComb(): this.type = {
    btFlags &= ~(BaseType.isRegMask | BaseType.isAnalogMask); this
  }

  override def freeze(): this.type = {
    btFlags |= BaseType.isFrozen; this
  }

  override def unfreeze(): this.type = {
    btFlags &= ~BaseType.isFrozen; this
  }

  def isFrozen(): Boolean = {
    (btFlags & BaseType.isFrozen) != 0
  }

  /** Is the baseType a node */
  def isTypeNode = (btFlags & BaseType.isTypeNodeMask) != 0

  /** Set baseType to Node */
  def setAsTypeNode(): this.type = {
    btFlags |= BaseType.isTypeNodeMask; this
  }

  /** Check if the baseType is vital */
  def isVital = (btFlags & BaseType.isVitalMask) != 0

  /** Set the baseType to vital */
  def setAsVital(): this.type = {
    btFlags |= BaseType.isVitalMask; this
  }

  /** Is the basetype using reset signal */
  def isUsingResetSignal: Boolean = clockDomain.config.resetKind != BOOT && (clockDomain.reset != null || clockDomain.softReset == null) && hasInit

  /** Is the basetype using soft reset signal  */
  def isUsingSoftResetSignal: Boolean = clockDomain.softReset != null && hasInit

  override def normalizeInputs: Unit = {}

  /** Does the base type have initial value */
  def hasInit: Boolean = {
    require(isReg)
    foreachStatements(s => if (s.isInstanceOf[InitAssignmentStatement]) return true)
    false
  }

  def hasDataAssignment: Boolean = {
    foreachStatements(s => if (s.isInstanceOf[DataAssignmentStatement]) return true)
    false
  }

  def hasAssignement : Boolean = !this.dlcIsEmpty

  def initialFrom(that: AnyRef, target: AnyRef = this) = {
    compositAssignFrom(that,target,InitialAssign)
  }


  /** Don't remove/simplify this data during rtl generation */
  private[core] var dontSimplify = false

  /** Can this data be simplified ?? */
  private[core] def canSymplifyIt = !dontSimplify && isUnnamed && !existsTag(!_.canSymplifyHost)

  /** Remove all assignments of the base type */
  override def removeAssignments(data : Boolean = true, init : Boolean = true, initial : Boolean = true): this.type = {
    foreachStatements {
      case s : DataAssignmentStatement => if(data) s.removeStatement()
      case s : InitAssignmentStatement => if(init) s.removeStatement()
      case s : InitialAssignmentStatement => if(initial) s.removeStatement()
    }
    this
  }


  override def dontSimplifyIt(): this.type = {
    dontSimplify = true
    this
  }

  override def allowSimplifyIt(): this.type = {
    dontSimplify = false
    this
  }

  def getDrivingReg(reportError : Boolean = true) : this.type = {
    if (isReg) {
      this
    } else {
      this.getSingleDriver match {
        case Some(t) => t.getDrivingReg(reportError)
        case _       => if(reportError) SpinalError("Driver is not a register") else null
      }
    }
  }

  override def asInput(): this.type = {
    component.ioSet += this
    super.asInput()
  }

  override def asOutput(): this.type = {
    component.ioSet += this
    super.asOutput()
  }

  override def asInOut(): this.type = {
    component.ioSet += this
    super.asInOut()
  }


  override def copyDirectionOfImpl(that : Data): this.type = {
    component.ioSet += this
    super.copyDirectionOfImpl(that)
  }



  override def setAsDirectionLess(): BaseType.this.type = {
    if(dir == null) return this
    component.ioSet -= this
    super.setAsDirectionLess()
  }

  def getSingleDriver: Option[this.type] = {
    if (this.hasOnlyOneStatement) this.head match {
      case AssignmentStatement(target, driver: BaseType) if target == this && this.head.parentScope == this.rootScopeStatement =>
        Some(driver.asInstanceOf[this.type])
      case _ => None
    } else if(this.isInput && this.component.parent == null && this.hasTag(classOf[ExternalDriverTag])){
      Some(this.getTag(classOf[ExternalDriverTag]).get.driver.asInstanceOf[this.type])
    } else None
  }

  override protected def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef)(implicit loc: Location): Unit = {
    def statement(that : Expression) = kind match {
      case `DataAssign` =>
        DataAssignmentStatement(target = target.asInstanceOf[Expression], source = that).setLocation(loc)
      case `InitAssign` =>
        if(!isReg)
          LocatedPendingError(s"Try to set initial value of a data that is not a register ($this)")
        InitAssignmentStatement(target = target.asInstanceOf[Expression], source = that).setLocation(loc)
      case `InitialAssign` => InitialAssignmentStatement(target = target.asInstanceOf[Expression], source = that).setLocation(loc)
    }
    if(isFrozen()){
      LocatedPendingError(s"FROZEN ASSIGNED :\n$this := $that")
    }
    that match {
      case that : Expression if that.getTypeObject == target.asInstanceOf[Expression].getTypeObject =>
        DslScopeStack.get match {
          case null =>  SpinalError(s"Hardware assignement done outside any Component")
          case s => s.append(statement(that))
        }
      case _ => kind match {
        case `DataAssign` => LocatedPendingError(s"Assignment data type mismatch\n$this := $that")
        case `InitAssign` => LocatedPendingError(s"Register initialisation type mismatch\nReg($this) init($that)")
      }
    }
  }

  override def removeStatement(): Unit = {
    if(!isDirectionLess)
      component.ioSet.remove(this)
    super.removeStatement()
  }

  private[core] override def autoConnect(that: Data)(implicit loc: Location): Unit = autoConnectBaseImpl(that)

  override def flatten: Seq[BaseType] = Seq(this)

  override def flattenForeach(body: (BaseType) => Unit): Unit = body(this)

  override def flattenLocalName: Seq[String] = Seq("")

  override def addAttribute(attribute: Attribute): this.type = addTag(attribute)

  override def rootScopeStatement = if(isInput) component.parentScope else parentScope

  override def clone: this.type = {
    val res = this.getClass.newInstance
    res.asInstanceOf[this.type]
  }


  private[core] def newMultiplexerExpression() : Multiplexer
  /** Base function to create mux */
  private[core] def newMultiplexer[T <: Expression](select: UInt, inputs : ArrayBuffer[T]): Multiplexer = newMultiplexer(select,inputs,newMultiplexerExpression())


  /** Create a multiplexer */
  final private[core] def newMultiplexer[T <: Expression](select: Expression with WidthProvider, inputs : ArrayBuffer[T], mux: Multiplexer): Multiplexer = {
    assert(inputs != null)
    mux.select  = select
    mux.inputs  = inputs.asInstanceOf[ArrayBuffer[mux.T]]
    mux
  }

  private[core] def newBinaryMultiplexerExpression() : BinaryMultiplexer
  /** Base function to create mux */
  private[core] def newMultiplexer(sel: Bool, whenTrue: Expression, whenFalse: Expression): BinaryMultiplexer = newMultiplexer(sel,whenTrue, whenFalse, newBinaryMultiplexerExpression())

  /** Create a multiplexer */
  final private[core] def newMultiplexer(cond: Expression, whenTrue: Expression, whenFalse: Expression, mux: BinaryMultiplexer): BinaryMultiplexer = {
    assert(cond != null)
    assert(whenTrue != null)
    assert(whenFalse != null)
    mux.cond      = cond
    mux.whenTrue  = whenTrue.asInstanceOf[mux.T]
    mux.whenFalse = whenFalse.asInstanceOf[mux.T]
    mux
  }



  private[core] def wrapWithWeakClone(e: Expression): this.type = {
    val typeNode = weakClone.setAsTypeNode()
    typeNode.assignFrom(e)
    typeNode.asInstanceOf[this.type]
  }

  private[core] def wrapWithBool(e: Expression): Bool = {
    val typeNode = Bool().setAsTypeNode()
    typeNode.assignFrom(e)
    typeNode
  }

  private[core] def wrapUnaryWithBool(e: UnaryOperator): Bool = {
    e.source = this.asInstanceOf[e.T]
    val typeNode = Bool().setAsTypeNode()
    typeNode.assignFrom(e)
    typeNode
  }

  def wrapCast[T <: BaseType](result: T, node: Cast): T = {
    node.input = this.asInstanceOf[node.T]
    result.assignFrom(node)
    result.setAsTypeNode()
  }

  private[core] def wrapConstantOperator(op: ConstantOperator): this.type = {
    op.source = this.asInstanceOf[op.T]
    wrapWithWeakClone(op)
  }

  private[core] def wrapUnaryOperator(op: UnaryOperator): this.type = {
    op.source = this.asInstanceOf[op.T]
    wrapWithWeakClone(op)
  }


  private[core] def wrapBinaryOperator(right: BaseType, op: BinaryOperator): this.type = {
    assert(right != null)
    op.left = this.asInstanceOf[op.T]
    op.right = right.asInstanceOf[op.T]
    wrapWithWeakClone(op)
  }

  private[core] def wrapLogicalOperator(right: BaseType, op: BinaryOperator):  Bool = {
    assert(right != null)
    op.left = this.asInstanceOf[op.T]
    op.right = right.asInstanceOf[op.T]
    wrapWithBool(op)
  }

  /** Create a new instance of the same datatype without any configuration (width, direction) */
  private[core] def weakClone: this.type

  def muxList[T2 <: Data](mappings: Seq[(Any, T2)]): T2 = {
    SpinalMap.list(this,mappings)
  }

  def muxList[T2 <: Data](defaultValue: T2, mappings: Seq[(Any, T2)]): T2 = {
    SpinalMap.list(this, mappings :+ (spinal.core.default , defaultValue) )
  }

  def muxListDc[T2 <: Data](mappings: Seq[(Any, T2)]): T2 = {
    SpinalMap.listDc(this, mappings)
  }

  def mux[T2 <: Data](mappings: (Any, T2)*): T2 = {
    SpinalMap.list(this,mappings)
  }

  def muxDc[T2 <: Data](mappings: (Any, T2)*): T2 = {
    SpinalMap.listDc(this,mappings)
  }

  override def foreachClockDomain(func: (ClockDomain) => Unit): Unit = if(isReg) func(clockDomain)

  override def toString: String = {
    if (isNamed || !hasOnlyOneStatement || !head.source.isInstanceOf[Literal])
      s"(${(if (component != null) component.getPath() + "/" else "") + this.getDisplayName()} : ${dirString()} $getClassIdentifier)"
    else
      head.source.toString
  }

  override def getAheadValue() : this.type = {
    assert(this.isReg, "Next value is only for regs")
    val ret = this.parentScope.onHead(this.clone).asInstanceOf[this.type].setCompositeName(this, "aheadValue", true)
    this.addTag(new PhaseNextifyTag(ret))
    ret.freeze()
    ret.pull().asInstanceOf[this.type]
  }
}
