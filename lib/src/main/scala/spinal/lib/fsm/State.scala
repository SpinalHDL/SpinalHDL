/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Lib                          **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/  MIT Licence                      **
**                                                                           **
** Permission is hereby granted, free of charge, to any person obtaining a   **
** copy of this software and associated documentation files (the "Software"),**
** to deal in the Software without restriction, including without limitation **
** the rights to use, copy, modify, merge, publish, distribute, sublicense,  **
** and/or sell copies of the Software, and to permit persons to whom the     **
** Software is furnished to do so, subject to the following conditions:      **
**                                                                           **
** The above copyright notice and this permission notice shall be included   **
** in all copies or substantial portions of the Software.                    **
**                                                                           **
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   **
** OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                **
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.    **
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY      **
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT **
** OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR  **
** THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                **
\*                                                                           */
package spinal.lib.fsm

import spinal.core._

import scala.collection.mutable.ArrayBuffer

/**
  * This trait indicate the entry point of the state machine
  */
trait EntryPoint


/**
  * Define the Entry point state
  */
object StateEntryPoint {
  def apply()(implicit stateMachineAccessor: StateMachineAccessor): State = new State with EntryPoint
}


trait StateCompletionTrait {
  val whenCompletedTasks = ArrayBuffer[() => Unit]()

  def whenCompleted(doThat: => Unit): this.type = {
    whenCompletedTasks += (() => doThat)
    this
  }

  protected def doWhenCompletedTasks() : Unit = whenCompletedTasks.foreach(_())
}


object State{
  def apply()(implicit stateMachineAccessor: StateMachineAccessor): State = new State
}


/**
  * State
  */

case class StateMachineTask(priority : Int, body : () => Unit)
class State(implicit stateMachineAccessor: StateMachineAccessor) extends Area with ScalaLocated {

  val onEntryTasks       = ArrayBuffer[() => Unit]()
  val onExitTasks        = ArrayBuffer[() => Unit]()
  val whenActiveTasks    = ArrayBuffer[StateMachineTask]()
  val whenInactiveTasks  = ArrayBuffer[() => Unit]()
  val whenIsNextTasks    = ArrayBuffer[() => Unit]()
  @dontName var innerFsm = ArrayBuffer[StateMachine]()

  def onEntry(doThat: => Unit): this.type = {
    onEntryTasks += (() => doThat)
    this
  }

  def onExit(doThat: => Unit): this.type = {
    onExitTasks += (() => doThat)
    this
  }

  def whenIsActive(doThat: => Unit): this.type = {
    whenIsActiveWithPriority(0)(doThat)
    this
  }

  def whenIsActiveWithPriority(priority : Int)(doThat: => Unit): this.type = {
    whenActiveTasks += StateMachineTask(priority, () => doThat)
    this
  }

  def whenIsInactive(doThat: => Unit): this.type = {
    whenInactiveTasks += (() => doThat)
    this
  }

  def whenIsNext(doThat: => Unit): this.type = {
    whenIsNextTasks += (() => doThat)
    this
  }

  def goto(state: State) = stateMachineAccessor.goto(state)

  def innerFsm(that: => StateMachine): Unit = innerFsm += that

  def exit(): Unit = stateMachineAccessor.exitFsm()

  def getStateMachineAccessor() = stateMachineAccessor

  val stateId = stateMachineAccessor.add(this)

  if(isInstanceOf[EntryPoint]) stateMachineAccessor.setEntry(this)
}


/**
  * Use to execute a State machine into a stateMachine
  */
class StateFsm[T <: StateMachineAccessor](val fsm:  T)(implicit stateMachineAccessor: StateMachineAccessor) extends State with StateCompletionTrait {
  valCallback(fsm, "fsm") //Provide naming to the fsm instance

  onEntry{
    fsm.startFsm()
  }

  whenIsActiveWithPriority(1){
    when(fsm.wantExit()){
      doWhenCompletedTasks()
    }
  }

  stateMachineAccessor.add(fsm)

  fsm.disableAutoStart()
}


/**
  * Run several state machine in Serie
  */
object StatesSerialFsm {

  def apply(_fsms:  StateMachineAccessor*)(doWhenCompleted: (State) =>  Unit)(implicit stateMachineAccessor: StateMachineAccessor): Seq[State] = {
    val fsms = _fsms
    var nextState: State = null

    val states = for(i <- fsms.size-1 downto 0) yield {
      val nextCpy = nextState
      nextState = new StateFsm(fsms(i)){
        if(nextState == null)
          whenCompleted(doWhenCompleted(this))
        else
          whenCompleted(this.goto(nextCpy))
      }
      nextState
    }
    states.reverse
  }
}


/**
  * Run several state machine in parallel
  */
class StateParallelFsm(val _fsms: StateMachineAccessor*)(implicit stateMachineAccessor: StateMachineAccessor) extends State with StateCompletionTrait {
  val fsms = _fsms
  
  onEntry{
    fsms.foreach(_.startFsm())
  }

  whenIsActiveWithPriority(1){
    val readys = fsms.map(fsm => fsm.isStateRegBoot() || fsm.wantExit())
    when(readys.reduce(_ && _)){
      doWhenCompletedTasks()
    }
  }

  for(fsm <- fsms){
    stateMachineAccessor.add(fsm)
    fsm.disableAutoStart()
  }
}


object StateMachineSharableUIntKey


class StateMachineSharableRegUInt {
  val value = Reg(UInt())
  var width = 0

  def addMinWidth(min: Int): Unit ={
    width = Math.max(width, min)
  }

  Component.current.addPrePopTask(() => value.setWidth(width))
}


/**
  * State Delay
  *
  * @example {{{
  *   val fsm = new StateMachine {
  *      ...
  *     val sDelay: State = new StateDelay(10 us){
  *       whenCompleted {
  *         goto(sIdle)
  *       }
  *     }
  *     ...
  *   }
  * }}}
  *
  */
class StateDelay(cyclesCount: AnyRef, minWidth: Int)(implicit stateMachineAccessor: StateMachineAccessor)
    extends State
    with StateCompletionTrait {

  def this(cyclesCount: BigInt)(implicit stateMachineAccessor: StateMachineAccessor) {
    this(cyclesCount, log2Up(cyclesCount + 1))
  }
  def this(cyclesCount: Int)(implicit stateMachineAccessor: StateMachineAccessor) {
    this(BigInt(cyclesCount))
  }
  /** Create a StateDelay with a TimeNumber */
  def this(time: TimeNumber)(implicit stateMachineAccessor: StateMachineAccessor) {
    this(((time.toBigDecimal * ClockDomain.current.frequency.getValue.toBigDecimal)
      .setScale(0, BigDecimal.RoundingMode.UP)).toBigInt)
  }
  def this(cyclesCount: CyclesCount)(implicit stateMachineAccessor: StateMachineAccessor) {
    this(cyclesCount.value)
  }
  def this(cyclesCount: UInt)(implicit stateMachineAccessor: StateMachineAccessor) {
    this(cyclesCount, cyclesCount.getWidth)
  }
  val cache = stateMachineAccessor
    .cacheGetOrElseUpdate(StateMachineSharableUIntKey, new StateMachineSharableRegUInt)
    .asInstanceOf[StateMachineSharableRegUInt]

  cache.addMinWidth(minWidth)
  cyclesCount match {
    case x: UInt => {
      onEntry {
        cache.value := x.resized
      }
    }
    case x: BigInt => {
      if (x < 0) SpinalError(s"StateDelay: ($cyclesCount) is negative")
      onEntry {
        cache.value := x
      }
    }
  }

  whenIsActiveWithPriority(1) {
    cache.value := cache.value - 1
    when(cache.value <= 1) {
      doWhenCompletedTasks()
    }
  }
}
