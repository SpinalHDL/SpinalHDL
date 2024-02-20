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

import spinal.core.ClockDomain.DivisionRate
import spinal.core.fiber.Handle

import scala.collection.mutable
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

sealed trait EdgeKind
object RISING  extends EdgeKind
object FALLING extends EdgeKind

sealed trait ResetKind
object ASYNC extends ResetKind
object SYNC  extends ResetKind
object BOOT  extends ResetKind

sealed trait Polarity{
  def assertedBool : Bool
  def deassertedBool : Bool
}
object HIGH extends Polarity{
  override def assertedBool: Bool = True
  override def deassertedBool: Bool = False
}
object LOW  extends Polarity{
  override def assertedBool: Bool = False
  override def deassertedBool: Bool = True
}

case class ClockDomainTag(clockDomain: ClockDomain) extends SpinalTag{
  override def toString = s"ClockDomainTag($clockDomain)"
  override def allowMultipleInstance = false
}

case class ClockDomainReportTag(clockDomain: ClockDomain) extends SpinalTag{
  override def toString = s"ClockDomainReportTag($clockDomain)"
  override def allowMultipleInstance = true
}

sealed trait ClockDomainBoolTag extends SpinalTag{
  override def allowMultipleInstance = true
}
case class ClockTag(clockDomain: ClockDomain)       extends ClockDomainBoolTag
case class ResetTag(clockDomain: ClockDomain)       extends ClockDomainBoolTag
case class ClockEnableTag(clockDomain: ClockDomain) extends ClockDomainBoolTag



// Default configuration of clock domain is :
// Rising edge clock with optional asynchronous reset active high and optional active high clockEnable
case class ClockDomainConfig(clockEdge: EdgeKind = RISING, resetKind: ResetKind = ASYNC, resetActiveLevel: Polarity = HIGH, softResetActiveLevel: Polarity = HIGH, clockEnableActiveLevel: Polarity = HIGH) {
  val useResetPin = resetKind match{
    case `ASYNC` | `SYNC` => true
    case _                => false
  }

  def resetAssertValue = resetActiveLevel match {
    case HIGH => True
    case LOW => False
  }
}


object ClockDomain {

  val crossClockBufferPushToPopResetGen = new ScopeProperty[Boolean]{
    override def default: Boolean = true
  }

  /**
    *  Create a local clock domain with `name` as prefix. clock, reset, clockEnable signals should be assigned by your care.
    */
  def internal(name            : String,
               config          : ClockDomainConfig = GlobalData.get.commonClockConfig,
               withReset       : Boolean = true,
               dummyArg        : DummyTrait = null, // dummyArg is here to force the user to use an explicit argument specification
               withSoftReset   : Boolean = false,
               withClockEnable : Boolean = false,
               frequency       : ClockFrequency = UnknownFrequency()): ClockDomain = {

    val clock = Bool()
    clock.setName(if (name != "") name + "_clk" else "clk")

    var reset: Bool = null
    if (withReset && config.resetKind != BOOT) {
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

    val clockDomain = ClockDomain(clock, reset, dummyArg, softReset, clockEnable, config,  frequency)
    clockDomain
  }


  def defaultConfig = GlobalData.get.commonClockConfig

  /**
    * To use when you want to define a new ClockDomain that thank signals outside the toplevel.
    * (it create input clock, reset, clockenable in the toplevel)
    */
  def external(name            : String,
               config          : ClockDomainConfig = GlobalData.get.commonClockConfig,
               withReset       : Boolean = true,
               dummyArg        : DummyTrait = null, // dummyArg is here to force the user to use an explicit argument specification
               withSoftReset   : Boolean = false,
               withClockEnable : Boolean = false,
               frequency       : ClockFrequency = UnknownFrequency()): ClockDomain = {

    val ctx = Component.push(null)

    val clockDomain = internal(
      name            = name,
      config          = config,
      withReset       = withReset,
      dummyArg        = dummyArg,
      withSoftReset   = withSoftReset,
      withClockEnable = withClockEnable,
      frequency       = frequency
    )

    ctx.restore()

    clockDomain
  }

  /** Push a clockdomain on the stack */
  def push(c: Handle[ClockDomain]) = ClockDomainStack.set(c)
  def push(c: ClockDomain) = ClockDomainStack.set(Handle.sync(c))

//  def push(c: Handle[ClockDomain]): Unit = {
//    ClockDomainStack.push(c)
//  }
//
//  def push(c: ClockDomain): Unit = {
//    ClockDomainStack.push(Handle.sync(c))
//  }


  /** Pop a clockdomain on the stack */
//  def pop(): Unit = {
//    ClockDomainStack.pop()
//  }

  /** Return the current clock Domain */
  def current: ClockDomain = {
    val h = currentHandle
    if(h != null) h.get else null
  }
  def currentHandle: Handle[ClockDomain] = ClockDomainStack.get

  def isResetActive       = current.isResetActive
  def isClockEnableActive = current.isClockEnableActive

  def readClockWire       = current.readClockWire
  def readResetWire       = current.readResetWire
  def readClockEnableWire = current.readClockEnableWire

  def getClockDomainDriver(that: Bool): Bool = {
    if (that.existsTag(_.isInstanceOf[ClockDomainBoolTag])) {
      that
    } else {
      that.getSingleDriver match {
        case Some(driver) => getClockDomainDriver(driver)
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


  trait DivisionRate {
    def getValue: BigInt
    def getMax:   BigInt
    def getMin:   BigInt
  }


  case class UnknownDivisionRate() extends DivisionRate {
    def getValue: BigInt = SpinalError("You are trying to get the frequency of a ClockDomain that doesn't know it")
    def getMax:   BigInt = SpinalError("You are trying to get the frequency of a ClockDomain that doesn't know it")
    def getMin:   BigInt = SpinalError("You are trying to get the frequency of a ClockDomain that doesn't know it")
  }


  case class FixedDivisionRate(value: BigInt) extends DivisionRate {
    def getValue: BigInt = value
    def getMax:   BigInt = value
    def getMin:   BigInt = value
  }


  trait ClockFrequency {
    def getValue: HertzNumber
    def getMax:   HertzNumber
    def getMin:   HertzNumber
  }


  case class UnknownFrequency() extends ClockFrequency {
    def getValue: HertzNumber = SpinalError("You are trying to get the frequency of a ClockDomain that doesn't know it")
    def getMax:   HertzNumber = SpinalError("You are trying to get the frequency of a ClockDomain that doesn't know it")
    def getMin:   HertzNumber = SpinalError("You are trying to get the frequency of a ClockDomain that doesn't know it")
  }


  case class FixedFrequency(value: HertzNumber) extends ClockFrequency {
    def getValue: HertzNumber = value
    def getMax:   HertzNumber = value
    def getMin:   HertzNumber = value
  }

  def getSyncronous(that: Bool)(solved: mutable.HashMap[Bool, immutable.Set[Bool]] = mutable.HashMap[Bool, immutable.Set[Bool]]()): immutable.Set[Bool] = {
    solved.get(that) match {
      case Some(sync) => sync
      case None => {
        var sync = scala.collection.immutable.Set[Bool]()

        //Collect all the directly syncronous Bool
        sync += that
        that.foreachTag {
          case tag: ClockSyncTag => sync += tag.a; sync += tag.b
          case tag: ClockDrivedTag => sync ++= getSyncronous(tag.driver)(solved)
          case _ =>
        }

        //Lock for driver inferation
        if (that.hasOnlyOneStatement && that.head.parentScope == that.rootScopeStatement && that.head.source.isInstanceOf[Bool] && that.head.source.asInstanceOf[Bool].isComb) {
          sync ++= getSyncronous(that.head.source.asInstanceOf[Bool])(solved)
        }

        //Cache result
        solved(that) = sync

        sync
      }
    }
  }

  def areSynchronousBool(a: Bool, b: Bool)(solved: mutable.HashMap[Bool, immutable.Set[Bool]]): Boolean = getSyncronous(a)(solved).contains(b) || getSyncronous(b)(solved).contains(a) || getSyncronous(a)(solved).intersect(getSyncronous(b)(solved)).nonEmpty

  def areSynchronous(a: ClockDomain, b: ClockDomain,solved: mutable.HashMap[Bool, immutable.Set[Bool]] = mutable.HashMap[Bool, immutable.Set[Bool]]()): Boolean = {
    a == b || a.clock == b.clock || areSynchronousBool(a.clock, b.clock)(solved)
  }

  def areSynchronous(a: ClockDomain, b: ClockDomain) : Boolean = areSynchronous(a,b,mutable.HashMap[Bool, immutable.Set[Bool]]())

}



case class ClockSyncTag(a : Bool, b : Bool) extends SpinalTag{
  override def canSymplifyHost: Boolean = false
}
case class ClockDrivedTag(driver : Bool) extends SpinalTag{
  override def canSymplifyHost: Boolean = false
}
case class ClockDriverTag(drived : Bool) extends SpinalTag{
  override def canSymplifyHost: Boolean = false
}

object Clock{
  def syncDrive(source : Bool, sink : Bool): Unit ={
    source.addTag(ClockDriverTag(sink))
    sink.addTag(ClockDrivedTag(source))
  }
  def sync(a : Bool, b : Bool): this.type ={
    val tag = new ClockSyncTag(a, b)
    a.addTag(tag)
    b.addTag(tag)
    this
  }
}

/**
  * clock and reset signals can be combined to create a clock domain.
  * Clock domains could be applied to some area of the design and then all synchronous elements instantiated into this
  * area will then implicitly use this clock domain.
  * Clock domain application work like a stack, which mean, if you are in a given clock domain, you can still apply another clock domain locally
  *
  * @see  [[http://spinalhdl.github.io/SpinalDoc/spinal/core/clock_domain ClockDomain Documentation]]
  */
case class ClockDomain(clock       : Bool,
                       reset       : Bool = null,
                       dummyArg    : DummyTrait = null, // dummyArg is here to force the user to use an explicit argument specification
                       softReset   : Bool = null,
                       clockEnable : Bool = null,
                       config      : ClockDomainConfig = GlobalData.get.commonClockConfig,
                       frequency   : ClockDomain.ClockFrequency = UnknownFrequency(),
                       clockEnableDivisionRate : ClockDomain.DivisionRate = ClockDomain.UnknownDivisionRate()) extends SpinalTagReady {

  assert(!(reset != null && config.resetKind == BOOT), "A reset pin was given to a clock domain where the config.resetKind is 'BOOT'")

  val instanceCounter = GlobalData.get.getInstanceCounter

  clock.addTag(ClockTag(this))

  if (reset != null) reset.addTag(ResetTag(this))
  if (clockEnable != null) clockEnable.addTag(ClockEnableTag(this))

  def hasClockEnableSignal = clockEnable != null
  def hasResetSignal       = reset != null
  def hasSoftResetSignal   = softReset != null
  def canInit = hasResetSignal || hasSoftResetSignal || config.resetKind == BOOT

  def push() = ClockDomain.push(this)
//  def pop(): Unit  = ClockDomain.pop()

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

  def readClockWire       = if (null == clock) Bool(config.clockEdge == FALLING)                 else Data.doPull(clock, Component.current, useCache = true, propagateName = true)
  def readResetWire       = if (null == reset) Bool(config.resetActiveLevel == LOW)              else Data.doPull(reset, Component.current, useCache = true, propagateName = true)
  def readSoftResetWire   = if (null == softReset) Bool(config.softResetActiveLevel == LOW)      else Data.doPull(softReset, Component.current, useCache = true, propagateName = true)
  def readClockEnableWire = if (null == clockEnable) Bool(config.clockEnableActiveLevel == HIGH) else Data.doPull(clockEnable, Component.current, useCache = true, propagateName = true)


//  def renameInCurrentComponent(clock : String = "clk",
//                               reset : String = if(config.resetActiveLevel == HIGH) "reset" else "resetn",
//                               softReset : String = if(config.softResetActiveLevel == HIGH) "soft_reset" else "soft_resetn",
//                               enable : String  = if(config.clockEnableActiveLevel == HIGH) "clk_en" else "clk_en"): this.type ={
def renamePulledWires(clock     : String = null,
                      reset     : String = null,
                      softReset : String = null,
                      enable    : String = null): this.type ={
    if(clock != null) readClockWire.setName(clock)
    if(reset != null && this.reset != null) readResetWire.setName(reset)
    if(softReset != null && this.softReset != null) readSoftResetWire.setName(softReset)
    if(enable != null && this.clockEnable != null) readClockEnableWire.setName(enable)
    this
  }

  def setSyncWith(that: ClockDomain) : this.type = {
    val tag = new ClockSyncTag(this.clock, that.clock)
    this.clock.addTag(tag)
    that.clock.addTag(tag)
    this
  }
  def setSynchronousWith(that: ClockDomain) : this.type = setSyncWith(that)
  @deprecated("misspelled method will be removed", "SpinalHDL 1.2.3")
  def setSyncronousWith(that: ClockDomain) = setSyncWith(that)

  def apply[T](block: => T): T = {
    val pop = this.push()
    val ret: T = block
    pop.restore()
    ret
  }

  def on [T](block : => T) : T = apply(block)

  def withoutReset() = GlobalData.get.userDatabase.getOrElseUpdate(this -> "withoutReset", copy(reset = null, softReset = null)).asInstanceOf[ClockDomain]

  def duringReset(body : => Unit): Unit ={
    when(ClockDomain.current.isResetActive) {
      ClockDomain.current.withoutReset() on body
    }
  }

  /** Slow down the current clock to factor time */
  def newClockDomainSlowedBy(factor: BigInt): ClockDomain = factor match {
    case x if x == 1 => this.copy()
    case x if x > 1  => this{
      val counter = Reg(UInt(log2Up(factor) bits)) init (0)
      val tick = counter === factor - 1
      counter := counter + 1
      when(tick) {
        counter := 0
      }

      val currentDivisionRate = if(ClockDomain.current.clockEnable == null) ClockDomain.FixedDivisionRate(1) else  ClockDomain.current.clockEnableDivisionRate
      val divisionRate = new DivisionRate {
        override def getValue: BigInt = currentDivisionRate.getValue*factor
        override def getMax: BigInt = currentDivisionRate.getMax*factor
        override def getMin: BigInt =  currentDivisionRate.getMin*factor
      }
      def syncResetFix(enable : Bool) = config.resetKind match {
        case `SYNC` if hasResetSignal => enable || isResetActive //Ensure that the area get a reset even if the enable isn't set
        case _ => enable
      }
      this.copy(clockEnable = syncResetFix(RegNext(tick) init(False)), clockEnableDivisionRate = divisionRate, config = ClockDomain.current.config.copy(clockEnableActiveLevel = HIGH))
    }
  }

  def newSlowedClockDomain(freq: HertzNumber): ClockDomain = {
    val currentFreq = ClockDomain.current.frequency.getValue.toBigDecimal
    freq match {
      case x if x.toBigDecimal > currentFreq => SpinalError("To high frequancy")
      case x                                 => newClockDomainSlowedBy((currentFreq/freq.toBigDecimal).toBigInt)
    }
  }

  @deprecated("Use copy instead of clone", "1.3.0")
  def clone(config      : ClockDomainConfig = config,
            clock       : Bool = clock,
            reset       : Bool = reset,
            dummyArg    : DummyTrait = null,
            softReset   : Bool = softReset,
            clockEnable : Bool = clockEnable): ClockDomain = {
    this.copy(clock, reset, dummyArg, softReset, clockEnable, config, frequency)
  }

  override def toString = clock.getName("???")

  def withRevertedClockEdge() = {
    copy(config = config.copy(clockEdge = if(config.clockEdge == RISING) FALLING else RISING))
  }

  def withAsyncReset() = {
    copy(config = config.copy(resetKind = ASYNC))
  }

  def withSyncReset() = {
    copy(config = config.copy(resetKind = SYNC))
  }

  def withBootReset() = {
    copy(reset = null, config = config.copy(resetKind = BOOT))
  }


  def samplingRate : IClockDomainFrequency = {
    if(clockEnable == null) return frequency
    try{
      val f = new IClockDomainFrequency{
        override def getValue: HertzNumber = frequency.getValue/BigDecimal(clockEnableDivisionRate.getValue)
        override def getMax: HertzNumber = frequency.getMax/BigDecimal(clockEnableDivisionRate.getMin)
        override def getMin: HertzNumber = frequency.getMin/BigDecimal(clockEnableDivisionRate.getMax)
      }
      //Test it
      f.getValue
      f.getMax
      f.getMin

      return f
    } catch {
      case _ : Throwable => return UnknownFrequency()
    }
  }
}





