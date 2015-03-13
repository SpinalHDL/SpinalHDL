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

/**
 * Created by PIC18F on 21.08.2014.
 */

trait EdgeKind;

object RISING extends EdgeKind;

object FALLING extends EdgeKind;

trait ResetKind;

object ASYNC extends ResetKind;

object SYNC extends ResetKind;




object ClockDomain {
  def apply(clock: Bool, reset: Bool = null, resetKind: ResetKind = ASYNC, edge: EdgeKind = RISING, clockEnable: Bool = null, resetActiveHigh: Boolean = true, clockEnableActiveHigh: Boolean = true): ClockDomain = {
    new ClockDomain(clock, edge, clockEnable, reset, resetKind, resetActiveHigh, clockEnableActiveHigh)
  }


  //  def apply(clock : Bool,resetKind : ResetKind = SYNC) : ClockDomain = {
  //    apply(clock,null,resetKind)
  //  }
  //  def apply(clock : Bool,reset : Bool,resetKind : ResetKind = SYNC) : ClockDomain = {
  //    new ClockDomain(clock,RISING,null,reset,resetKind,true)
  //  }
  //  def apply(clock : Bool,clockEnable : Bool,reset : Bool,resetKind : ResetKind = SYNC) : ClockDomain = {
  //    new ClockDomain(clock,RISING,clockEnable,reset,resetKind,true)
  //  }


  def push(c: ClockDomain): Unit = {
    GlobalData.get.clockDomainStack.push(c)
  }
  def pop(c: ClockDomain): Unit = {
    GlobalData.get.clockDomainStack.pop(c)
  }

  def current = GlobalData.get.clockDomainStack.head()

  def isResetActive = current.isResetActive
  def isClockEnableActive = current.isClockEnableActive


  def readClockWire = current.readClockWire
  def readResetWire = current.readResetWire
  def readClockEnableWire = current.readClockEnableWire

}

class ClockDomain(val clock: Bool, val edge: EdgeKind, var clockEnable: Bool, val reset: Bool, val resetKind: ResetKind, val resetActiveHigh: Boolean, val clockEnableActiveHigh: Boolean) {
  def hasClockEnable = clockEnable != null
  def hasReset = reset != null

  def push: Unit = ClockDomain.push(this)
  def pop: Unit = ClockDomain.pop(this)

  def isResetActive = if (resetActiveHigh) readResetWire else !readResetWire
  def isClockEnableActive = if (clockEnableActiveHigh) readClockEnableWire else !readClockEnableWire

  def readClockWire = if (null == clock) Bool(edge == FALLING) else Data.doPull(clock, Component.current, true, true)
  def readResetWire = if (null == reset) Bool(!resetActiveHigh) else Data.doPull(reset, Component.current, true, true)
  def readClockEnableWire = if (null == clockEnable) Bool(clockEnableActiveHigh) else Data.doPull(clockEnable, Component.current, true, true)


  def apply(block: => Unit): Unit = {
    push
    block
    pop
  }

  def apply[T <: Data](block: => T): T = {
    push
    val ret : T = block
    pop
    ret
  }


  override def clone() : this.type = new ClockDomain(clock,edge,clockEnable,reset,resetKind,resetActiveHigh,clockEnableActiveHigh).asInstanceOf[this.type]
}


//abstract class ClockDomainZone(clockDomain: ClockDomain){
//  clockDomain.push
//
//  clockDomain.pop
//}