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

import scala.collection.mutable.ArrayBuffer

/**
  * Created by PIC18F on 21.08.2014.
  */

trait EdgeKind
object RISING extends EdgeKind
object FALLING extends EdgeKind

trait ResetKind
object ASYNC extends ResetKind
object SYNC extends ResetKind
object BOOT extends ResetKind

trait Polarity
object HIGH extends Polarity
object LOW extends Polarity



// Default configuration of clock domain is :
// Rising edge clock with optional asyncronous reset active high and optional active high clockEnable
case class ClockDomainConfig(clockEdge: EdgeKind = RISING, resetKind: ResetKind = ASYNC, resetActiveLevel: Polarity = HIGH, softResetActiveLevel: Polarity = HIGH, clockEnableActiveLevel: Polarity = HIGH) {
  val useResetPin = resetKind match{
    case `ASYNC` | `SYNC` => true
    case _ => false
  }
}

//To use when you want to define a new clock domain by using internal signals
object ClockDomain {
  def apply(clock: Bool,
            reset: Bool = null, dummyArg : DummyTrait = null, // dummyArg is here to force the user to use an explicit argument specification
            softReset : Bool = null,
            clockEnable: Bool = null,
            frequency: IClockDomainFrequency = UnknownFrequency(),
            config: ClockDomainConfig = GlobalData.get.commonClockConfig): ClockDomain = {
    new ClockDomain(config, clock, reset,dummyArg,softReset,clockEnable, frequency)
  }

  /**
   *  Create a local clock domain with `name` as prefix. clock, reset, clockEnable signals should be assigned by your care.
   * @param name Name of the ClockDomain signals
   */
  def internal(name: String,
            config: ClockDomainConfig = GlobalData.get.commonClockConfig,
            withReset : Boolean = true, dummyArg : DummyTrait = null, // dummyArg is here to force the user to use an explicit argument specification
            withSoftReset : Boolean = false,
            withClockEnable : Boolean = false,
            frequency: IClockDomainFrequency = UnknownFrequency()): ClockDomain = {
    val clock = Bool()
    clock.setName(if (name != "") name + "_clk" else "clk")

    var reset: Bool = null
    if (withReset) {
      reset = Bool()
      reset.setName((if (name != "") name + "_reset" else "reset") + (if (config.resetActiveLevel == HIGH) "" else "n"))
    }

    var softReset: Bool = null
    if (withSoftReset) {
      softReset = Bool()
      softReset.setName((if (name != "") name + "_soft_reset" else "soft_reset") + (if (config.softResetActiveLevel == HIGH) "" else "n"))
    }

    var clockEnable: Bool = null
    if (withClockEnable) {
      clockEnable = Bool()
      clockEnable.setName((if (name != "") name + "_clkEn" else "clkEn") + (if (config.resetActiveLevel == HIGH) "" else "n"))
    }

    val clockDomain = ClockDomain(clock, reset,dummyArg,softReset, clockEnable, frequency,config)
    clockDomain
  }


  // To use when you want to define a new ClockDomain that thank signals outside the toplevel.
  // (it create input clock, reset, clockenable in the top level)
  def external(name: String,
               config: ClockDomainConfig = GlobalData.get.commonClockConfig,
               withReset : Boolean = true,
               dummyArg : DummyTrait = null, // dummyArg is here to force the user to use an explicit argument specification
               withSoftReset : Boolean = false,
               withClockEnable : Boolean = false,
               frequency: IClockDomainFrequency = UnknownFrequency()): ClockDomain = {
    Component.push(null)
    val clockDomain = internal(
      name = name,
      config = config,
      withReset = withReset,
      dummyArg = dummyArg,
      withSoftReset = withSoftReset,
      withClockEnable = withClockEnable,
      frequency = frequency
    )
    Component.pop(null)
    clockDomain
  }


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

  def getClockDomainDriver(that: Bool): Bool = {
    if (that.existsTag(_.isInstanceOf[ClockDomainBoolTag])) {
      that
    } else {
      that.input match {
        case input: Bool => getClockDomainDriver(input)
        case _ => null
      }
    }
  }

  def getClockDomainTag(that: Bool): ClockDomainBoolTag = {
    val driver = getClockDomainDriver(that)
    if (driver == null) {
      null
    } else {
      driver.findTag(_.isInstanceOf[ClockDomainBoolTag]).get.asInstanceOf[ClockDomainBoolTag]
    }
  }
}

case class ClockDomainTag(clockDomain: ClockDomain) extends SpinalTag

trait ClockDomainBoolTag extends SpinalTag
case class ClockTag(clockDomain: ClockDomain) extends ClockDomainBoolTag
case class ResetTag(clockDomain: ClockDomain) extends ClockDomainBoolTag
case class ClockEnableTag(clockDomain: ClockDomain) extends ClockDomainBoolTag

trait DummyTrait

class ClockDomain(val config: ClockDomainConfig, val clock: Bool, val reset: Bool = null,dummyArg : DummyTrait = null,val softReset : Bool = null, val clockEnable: Bool = null, val frequency: IClockDomainFrequency = UnknownFrequency()) {
  assert(!(reset != null && config.resetKind == BOOT),"A reset pin was given to a clock domain where the config.resetKind is 'BOOT'")
  val instanceCounter = GlobalData.get.getInstanceCounter
  clock.dontSimplifyIt()
  if(reset != null)reset.dontSimplifyIt()
  if(clockEnable != null)clockEnable.dontSimplifyIt()

  clock.addTag(ClockTag(this))
  if (reset != null) reset.addTag(ResetTag(this))
  if (clockEnable != null) clockEnable.addTag(ClockEnableTag(this))


  def hasClockEnableSignal = clockEnable != null
  def hasResetSignal = reset != null
  def hasSoftResetSignal = softReset != null
  def push() : Unit = ClockDomain.push(this)
  def pop(): Unit = ClockDomain.pop(this)
  def isResetActive = {
    if(config.useResetPin && reset != null)
      if (config.resetActiveLevel == HIGH) readResetWire else !readResetWire
    else
      False
  }
  def isSoftResetActive = {
    if(softReset != null)
      if (config.softResetActiveLevel == HIGH) readSoftResetWire else ! readSoftResetWire
    else
      False
  }
  def isClockEnableActive = {
    if(clockEnable != null)
      if (config.clockEnableActiveLevel == HIGH) readClockEnableWire else !readClockEnableWire
    else
      True
  }
  def readClockWire = if (null == clock) Bool(config.clockEdge == FALLING) else Data.doPull(clock, Component.current, true, true)
  def readResetWire = if (null == reset) Bool(config.resetActiveLevel == LOW) else Data.doPull(reset, Component.current, true, true)
  def readSoftResetWire = if (null == softReset) Bool(config.softResetActiveLevel == LOW) else Data.doPull(softReset, Component.current, true, true)
  def readClockEnableWire = if (null == clockEnable) Bool(config.clockEnableActiveLevel == HIGH) else Data.doPull(clockEnable, Component.current, true, true)

  val syncroneWith = ArrayBuffer[ClockDomain]()

  def isSyncronousWith(that: ClockDomain): Boolean = {
    if (this == that) return true
    if (this.clock == that.clock) return true
    if (syncroneWith.contains(that)) return true
    return false
  }

  def setSyncronousWith(that: ClockDomain) = {
    this.syncroneWith += that
    that.syncroneWith += this
  }

  def apply[T](block: => T): T = {
    push
    val ret: T = block
    pop
    ret
  }


  def clone(config: ClockDomainConfig = config, clock: Bool = clock, reset: Bool = reset, dummyArg : DummyTrait = null,softReset : Bool = null,clockEnable: Bool = clockEnable): ClockDomain = new ClockDomain(config, clock, reset, dummyArg, softReset,  clockEnable, frequency)
}

case class UnknownFrequency() extends IClockDomainFrequency {
  def getValue: HertzNumber = SpinalError("You are trying to get the frequency of a ClockDomain that dosen't know it")
  def getMax: HertzNumber   = SpinalError("You are trying to get the frequency of a ClockDomain that dosen't know it")
  def getMin: HertzNumber   = SpinalError("You are trying to get the frequency of a ClockDomain that dosen't know it")
}

case class FixedFrequency(value: HertzNumber) extends IClockDomainFrequency {
  def getValue: HertzNumber = value
  def getMax: HertzNumber = value
  def getMin: HertzNumber = value
}

trait IClockDomainFrequency {
  def getValue: HertzNumber
  def getMax: HertzNumber
  def getMin: HertzNumber
}