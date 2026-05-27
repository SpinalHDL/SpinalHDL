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
  * This trait indicate the entry point of the state machine.
  * 
  * @see [[setEntry]]
  * @see [[StateEntryPoint]] 
  */
trait EntryPoint


/** Syntactic sugar for `new State with EntryPoint`
  * @see [[EntryPoint]]
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


case class StateMachineTask(priority : Int, body : () => Unit)

/**
  * Represents a state within a state machine.
  * 
  * States can define behaviors for entry, exit, and active phases using methods like `onEntry`, `onExit`, and `whenIsActive`.
  * 
  * @example {{{
  *   val fsm = new StateMachine {
  *     val stateA: State = new State with EntryPoint {
  *       onEntry {
  *         counter := 0
  *       }
  *       whenIsActive {
  *         counter := counter + 1
  *         when(counter === 4) {
  *           goto(stateB)
  *         }
  *       }
  *       onExit {
  *         io.result := True
  *       }
  *     }
  *     val stateB: State = new State {
  *       whenIsActive {
  *         goto(stateA)
  *       }
  *     }
  *   }
  * }}}
  */
class State(implicit stateMachineAccessor: StateMachineAccessor) extends Area with ScalaLocated {

  val onEntryTasks       = ArrayBuffer[() => Unit]()
  val onExitTasks        = ArrayBuffer[() => Unit]()
  val whenActiveTasks    = ArrayBuffer[StateMachineTask]()
  val whenInactiveTasks  = ArrayBuffer[() => Unit]()
  val whenIsNextTasks    = ArrayBuffer[() => Unit]()
  @dontName var innerFsm = ArrayBuffer[StateMachine]()

  /**
    * Defines statements to apply when the state machine is not in this state 
    * but will be in it during the next cycle.
    *
    * @param doThat hardware statement(s)
    * 
    * @example {{{
    *   val stateA: State = new State with EntryPoint {
    *     onEntry {
    *       counter := 0
    *       io.led := True
    *     }
    *     whenIsActive {
    *       goto(stateB)
    *     }
    *   }
    * }}}
    */
  def onEntry(doThat: => Unit): this.type = {
    onEntryTasks += (() => doThat)
    this
  }

  /**
    * Defines statements applied when the state machine is in this state and
    * will be in another state the next cycle.
    *
    * @param doThat Hardware statement(s)
    * 
    * @example {{{
    *   val stateA: State = new State with EntryPoint {
    *     whenIsActive {
    *       goto(stateB)
    *     }
    *     onExit {
    *       io.result := True
    *       io.led := False
    *     }
    *   }
    * }}}
    */
  def onExit(doThat: => Unit): this.type = {
    onExitTasks += (() => doThat)
    this
  }

  /**
    * Defines statements applied when the state machine is in this state.
    *
    * @param doThat hardware statement(s)
    * 
    * @example {{{
    *   val stateA: State = new State with EntryPoint {
    *     whenIsActive {
    *       counter := counter + 1
    *       when(counter === 10) {
    *         goto(stateB)
    *       }
    *     }
    *   }
    * }}}
    */
  def whenIsActive(doThat: => Unit): this.type = {
    whenIsActiveWithPriority(0)(doThat)
    this
  }

  /** Used internally by the state machine library 
   * @see [[#whenIsActive()]]
   */
  def whenIsActiveWithPriority(priority : Int)(doThat: => Unit): this.type = {
    whenActiveTasks += StateMachineTask(priority, () => doThat)
    this
  }

  /**
    * Defines statements applied when the state machine is not in this state.
    *
    * @param doThat hardware statement(s)
    */
  def whenIsInactive(doThat: => Unit): this.type = {
    whenInactiveTasks += (() => doThat)
    this
  }

  /**
    * Defines statements applied when the state machine will be in state the 
    * next cycle (even if it is already in it).
    */
  def whenIsNext(doThat: => Unit): this.type = {
    whenIsNextTasks += (() => doThat)
    this
  }

  /**
    * Schedules the state machine to be in the provided [[State]] the next cycle.
    *
    * @param state [[State]] to transition to.
    * 
    * @example {{{
    *   val pingState: State = new State with EntryPoint {
    *     whenIsActive {
    *       goto(pongState)
    *     }
    *   }
    *   val pongState: State = new State {
    *     whenIsActive {
    *       goto(pingState)
    *     }
    *   }
    * }}}
    */
  def goto(state: State) = stateMachineAccessor.goto(state)

  /** Internal to the state machine library */
  def innerFsm(that: => StateMachine): Unit = innerFsm += that

  /** Schedules the state machine to be in the boot state the next cycle 
    * (or, in [[StateFsm]], to exit the current nested state machine).
    */
  def exit(): Unit = stateMachineAccessor.exitFsm()

  /** Used internally by the state machine library */
  def getStateMachineAccessor() = stateMachineAccessor

  val stateId = stateMachineAccessor.add(this)

  if(isInstanceOf[EntryPoint]) stateMachineAccessor.setEntry(this)
}


/**
  * Allows you to describe a state containing a nested state machine.
  * 
  * When the nested state machine is done (exited), statements in whenCompleted { ... } are executed.
  * 
  * This allows embedding a state machine as a state within another state machine. The embedded state machine starts on entry and completes on exit.
  * 
  * @example {{{
  *   // internalFsm is a function defined below
  *   val stateC = new StateFsm(fsm=internalFsm()) {
  *     whenCompleted {
  *       goto(stateD)
  *     }
  *   }
  *   
  *   def internalFsm() = new StateMachine {
  *     val counter = Reg(UInt(8 bits)) init(0)
  *   
  *     val stateA : State = new State with EntryPoint {
  *       whenIsActive {
  *         goto(stateB)
  *       }
  *     }
  *   
  *     val stateB : State = new State {
  *       onEntry (counter := 0)
  *       whenIsActive {
  *         when(counter === 4) {
  *           exit()
  *         }
  *         counter := counter + 1
  *       }
  *     }
  *   }
  * }}}
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


/** Runs several state machine in serially */
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
  * Allows you to handle multiple nested state machines. 
  * 
  * When all nested state machine are done, statements in whenCompleted { ... } are executed.
  * 
  * @example {{{
  *   val stateD = new StateParallelFsm (internalFsmA(), internalFsmB()) {
  *     whenCompleted {
  *       goto(stateE)
  *     }
  *   }
  * }}}
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
  * Allows you to create a state which waits for a fixed number of cycles
  * before executing statements in whenCompleted {...}.
  * 
  * The preferred way to use it is:
  *  
  * @example {{{
  *   val stateG : State = new StateDelay(cyclesCount=40) {
  *     whenCompleted {
  *       goto(stateH)
  *     }
  *   }
  * }}}
  * 
  * It can also be written in one line:
  *  
  * @example {{{
  *   val stateG : State = new StateDelay(40) { whenCompleted(goto(stateH)) }
  * }}}
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
