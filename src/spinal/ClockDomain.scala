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

/**
 * Created by PIC18F on 21.08.2014.
 */

trait EdgeKind;

object RISING extends EdgeKind;
object FALLING extends EdgeKind;

trait ResetKind;

object ASYNC extends ResetKind;
object SYNC extends ResetKind;
object BOOT extends ResetKind;



object ClockDomain{
  def apply(clock : Bool,reset : Bool = null,resetKind : ResetKind = SYNC,edge : EdgeKind = RISING, clockEnable : Bool = null,resetActiveHigh : Boolean = true,clockEnableActiveHigh : Boolean = true) : ClockDomain = {
    new ClockDomain(clock,edge,clockEnable,reset,resetKind,resetActiveHigh,clockEnableActiveHigh)
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
  val stack = new SafeStack[ClockDomain]

  def push(c: ClockDomain): Unit = {
    stack.push(c)
  }
  def pop(c: ClockDomain): Unit = {
    stack.pop(c)
  }

  def current = stack.head()


  def readClock = current.readClock
  def readReset = current.readReset
  def readClockEnable = current.readClockEnable
}

class ClockDomain(val clock : Bool,val edge : EdgeKind,val clockEnable : Bool,val reset : Bool,val resetKind : ResetKind,val resetActiveHigh : Boolean,val clockEnableActiveHigh : Boolean){
  def hasClockEnable = clockEnable != null
  def hasReset = reset != null

  def push: Unit = ClockDomain.push(this)
  def pop: Unit = ClockDomain.pop(this)



  def readClock = if(null == clock) Bool(false) else Data.doPull(clock, Component.current,true,true)
  def readReset = if(null == reset) Bool(!resetActiveHigh) else Data.doPull(reset, Component.current,true,true)
  def readClockEnable = if(null == clockEnable) Bool(true) else Data.doPull(clockEnable, Component.current,true,true)
}
