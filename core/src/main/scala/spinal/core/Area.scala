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

import spinal.core.Nameable.DATAMODEL_WEAK
import spinal.core.internals.Misc
import spinal.idslplugin.PostInitCallback





/**
  * Sometime, creating a Component to define some logic is overkill.
  * For this kind of cases you can use Area to define a group of signals/logic.
  *
  * @example {{{
  *     val tickConter = new Area{
  *       val tick = Reg(UInt(8 bits) init(0)
  *       tick := tick + 1
  *     }
  * }}}
  *  @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/area/ Area Documentation]]
  */

class Composite[T <: Nameable](val self : T, postfix : String = null, weak : Boolean = true) extends Area{
  override def childNamePriority = Nameable.USER_WEAK
  if(postfix == null) {
    setCompositeName(self, weak)
  } else {
    setCompositeName(self, postfix, weak)
  }
}

trait Area extends NameableByComponent with ContextUser with OwnableRef with ScalaLocated with ValCallbackRec with OverridedEqualsHashCode  {
  if(globalData != null) globalData.onAreaInit.foreach{f =>
    f(this)
    globalData.onAreaInit = None
  }
  def childNamePriority = DATAMODEL_WEAK
  val _context = ScopeProperty.capture() //TODO not as heavy
  def rework[T](body : => T): T = {
    val oldContext = ScopeProperty.captureNoClone()
    _context.restoreCloned()
    val b = body
    oldContext.restore()
    b
  }
  override private[core] def getComponent() = component

  override def valCallbackRec(obj: Any, name: String): Unit = {
    obj match {
      case component: Component =>
        if (component.parent == this.component) {
          component.setPartialName(name, childNamePriority, this)
        }
      case namable: Nameable =>
        if (!namable.isInstanceOf[ContextUser]) {
          namable.setPartialName(name, childNamePriority, this)
        } else if (namable.asInstanceOf[ContextUser].component == component){
          namable.setPartialName(name, childNamePriority, this)
        } else {
          if(component != null) for (kind <- component.children) {
            //Allow to name a component by his io reference into the parent component
            if (kind.reflectIo == namable) {
              kind.setPartialName(name, childNamePriority, this)
            }
          }
        }
      case _ =>
    }
  }

  override def toString: String = (if(component != null)component.getPath() + "/"  else "") + super.toString()
}




/**
  * Create an Area which can be assign to a data
  *
  * @example {{{
  *     class Counter extends ImplicitArea[UInt]{
  *        val cnt = Reg(UInt(8 bits)
  *        ...
  *        override def implicitValue: UInt = cnt
  *     }
  *     val myCounter = Counter()
  *     io.myUInt = myCounter
  * }}}
  */
abstract class ImplicitArea[T] extends Area {
  def implicitValue: T
}

object ImplicitArea{
  implicit def toImplicit[T](area: ImplicitArea[T]): T = area.implicitValue
}


/**
  * Clock domains could be applied to some area of the design and then all synchronous elements instantiated into
  * this area will then implicitly use this clock domain.
  *
  *  @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/clock_domain/ ClockDomain Documentation]]
  */
class ClockingArea(val clockDomain: ClockDomain) extends Area with PostInitCallback {
  val ctx = ClockDomainStack.set(clockDomain)

  override def postInitCallback(): this.type = {
    ctx.restore()
    this
  }
}


/**
  * Clock Area with a special clock enable
  */
class ClockEnableArea(clockEnable: Bool) extends Area with PostInitCallback {

  val newClockEnable : Bool = if (ClockDomain.current.config.clockEnableActiveLevel == HIGH)
    ClockDomain.current.readClockEnableWire & clockEnable
  else
    ClockDomain.current.readClockEnableWire | !clockEnable

  val clockDomain = ClockDomain.current.copy(clockEnable = newClockEnable)

  val ctx = ClockDomainStack.set(clockDomain)


  override def postInitCallback(): this.type = {
    ctx.restore()
    this
  }
}


/**
  * Define a clock domain which is x time slower than the current clock
  */
class SlowArea(val factor: BigInt, allowRounding : Boolean) extends ClockingArea(ClockDomain.current.newClockDomainSlowedBy(factor)){
  def this(factor: BigInt) = {
    this(factor, allowRounding = false)
  }

  def this(frequency: HertzNumber, allowRounding : Boolean) = {
    this((ClockDomain.current.frequency.getValue / frequency).toBigInt, allowRounding)

    val factor = ClockDomain.current.frequency.getValue / frequency
    require(allowRounding || factor.toBigInt == factor)
  }

  def this(frequency: HertzNumber) = {
    this(frequency, allowRounding = false)
  }

  def getFrequency() = (ClockDomain.current.frequency.getValue.toBigDecimal / BigDecimal(factor)) Hz
}


/**
  * ResetArea allow to reset an area with a special reset combining with the current reset (cumulative)
  */
class ResetArea(reset: Bool, cumulative: Boolean) extends Area with PostInitCallback {

  val newReset: Bool = if (ClockDomain.current.config.resetActiveLevel == LOW) {
    if(cumulative) (ClockDomain.current.readResetWire & !reset) else !reset
  }else {
    if(cumulative) (ClockDomain.current.readResetWire | reset) else reset
  }

  val clockDomain = ClockDomain.current.copy(reset = newReset)
  val ctx = ClockDomainStack.set(clockDomain)

  override def postInitCallback(): this.type = {
    ctx.restore()
    this
  }
}


trait AreaObject extends Area{
  setName(this.getClass.getSimpleName.replace("$",""))
}

trait AreaRoot extends Area{
  setName("")
}