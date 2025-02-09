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
import spinal.lib._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


trait StateMachineAccessor {

  def setEntry(state: State): Unit
  def getEntry(): State

  def isActive(state: State): Bool
  def isEntering(state: State): Bool
  def isExiting(state: State): Bool

  def goto(state: State): Unit
  def forceGoto(state: State): Unit

  def add(state: State): Int
  def add(stateMachine: StateMachineAccessor): Unit

  def startFsm(): Unit
  def exitFsm(): Unit
  def wantExit(): Bool

  def disableAutoStart(): Unit

  def getName(): String

  def build(): Unit

  def setParentStateMachine(parent: StateMachineAccessor): Unit

  def cacheGet(key: Any): Option[Any]
  def cachePut(key: Any, value: Any): Unit

  def isStateNextBoot(): Bool
  def isStateRegBoot(): Bool

  def cacheGetOrElseUpdate(key: Any, op: => Any): Any = {
    cacheGet(key) match{
      case Some(value) => value
      case None  =>
        val value = op
        cachePut(key,value)
        value
    }
  }
}


class StateBoot(autoStart: Boolean)(implicit stateMachineAccessor: StateMachineAccessor) extends State {
  if(autoStart) {
    whenIsActive {
      stateMachineAccessor.startFsm()
    }
  }
}


class StateMachineEnum extends SpinalEnum


/**
  * State machine
  *
  * @example {{{
  *   val fsm = new StateMachine {
  *     val sIdle: State = StateEntryPoint{
  *       ...
  *     }
  *     val sState1: State = new State {
  *       whenIsActive{
  *         goto(sIdle)
  *       }
  *     }
  *     ...
  *   }
  * }}}
  */
class StateMachine extends Area with StateMachineAccessor with ScalaLocated {

  /**
    * Set the condition for state transitions
    *
    * goto() will only have an effect, if condition is True
    */
  def setTransitionCondition(condition : Bool): Unit = {
    this.transitionCond = Bool()
    this.transitionCond := condition
  }

  var inGeneration = false
  val alwaysTasks  = ArrayBuffer[() => Unit]()

  def always(doThat: => Unit): this.type = {
    alwaysTasks += (() => doThat)
    this
  }

  var corruptedState : Bool = null
  def inCorruptedState(): Bool ={
    if(corruptedState == null) corruptedState = Bool().setCompositeName(this, "corruptedState", weak = true)
    corruptedState
  }

  def setEncoding(encoding: SpinalEnumEncoding): Unit = enumDef.defaultEncoding = encoding
  def setEncoding(spec: (State, BigInt)*): Unit = {
    val mapping = spec.map(e => states.indexOf(e._1) -> e._2).toMapLinked()
    enumDef.defaultEncoding = SpinalEnumEncoding(mapping.apply)
  }

  @dontName val postBuildTasks = ArrayBuffer[() => Unit]()

  val cache = mutable.HashMap[Any,Any]()
  val enumDef = new StateMachineEnum()
  enumDef.name = ""
  var stateReg  : enumDef.C = null
  var stateNext : enumDef.C = null
  /* Candidate for next state */
  var stateNextCand : enumDef.C = null
  /* Condition for transition */
  var transitionCond : Bool = null
  override val wantExit  = False.allowPruning()
  val wantStart = False
  val wantKill = False
  var autoStart = true


  @dontName var parentStateMachine: StateMachineAccessor = null
  @dontName val childStateMachines = mutable.LinkedHashSet[StateMachineAccessor]()
  @dontName val states   = ArrayBuffer[State]()
  val stateToEnumElement = mutable.HashMap[State, enumDef.E]()
  @dontName var entryState: State = null

  def enumOf(state: State) = {
    checkState(state)
    stateToEnumElement(state)
  }

  def checkState(state: State) = assert(state.getStateMachineAccessor == this, s"A state machine ($this)is using a state ($state) that come from another state machine.\n\nState machine defined at ${this.getScalaLocationLong}\n State defined at ${state.getScalaLocationLong}")

  var stateBoot : State = new State()(this).setCompositeName(this, "BOOT", Nameable.DATAMODEL_WEAK)

  def makeInstantEntry(): State ={
    setEntry(stateBoot.unsetName())
    stateBoot
  }

  var builded = false
  override def build(): Unit = {
    if(builded) return
    builded = true
    inGeneration = true
    childStateMachines.foreach(_.build())
    if(autoStart) {
      stateBoot.whenIsActive {
        startFsm()
      }
    }
    stateReg  = Reg(enumDef())
    stateNext = enumDef().allowOverride
    /* Only synthesize, if conditional state machine */
    if (transitionCond != null)
      stateNextCand = enumDef().allowOverride

    OwnableRef.proposal(stateBoot, this)
    OwnableRef.proposal(stateReg, this)
    OwnableRef.proposal(stateNext, this)

    stateReg.setPartialName("stateReg")
    stateNext.setPartialName("stateNext")
    if (transitionCond != null)
      stateNextCand.setPartialName("stateNextCand")
    cacheGet(StateMachineSharableUIntKey).map(_.asInstanceOf[StateMachineSharableRegUInt].value.setPartialName(this,"cyclesRemain"))

    for(state <- states){
      checkState(state)
      val enumElement = enumDef.newElement().setLambdaName(state.isNamed && this.isNamed)(
        Nameable.getNameWithoutPrefix(prefix = this, from = state)
      )
      stateToEnumElement += (state -> enumElement)
    }

    stateReg init(enumOf(this.stateBoot))
    stateReg := stateNext

    if(transitionCond == null) {
      stateNext := stateReg
    } else {
      stateNextCand := stateReg
      stateNext := transitionCond ? stateNextCand | stateReg
    }

    switch(stateReg){
      for(state <- states){
        if(state == stateBoot) default {
          state.whenActiveTasks.sortBy(_.priority).foreach(_.body())
        } else is(enumOf(state)) {
          state.whenActiveTasks.sortBy(_.priority).foreach(_.body())
        }
      }
    }

    for(state <- states){
      when(!isActive(state)){
        state.whenInactiveTasks.foreach(_())
      }
      val exit = Bool().setLambdaName(this.isNamed && state.isNamed)(this.getName +  "_onExit_" + enumOf(state).getName)
      exit := isExiting(state)
      when(exit){
        state.onExitTasks.foreach(_())
      }
    }

    switch(stateNext){
      for(state <- states){
        if(state == stateBoot) default {
          state.whenIsNextTasks.foreach(_())
        } else is(enumOf(state)) {
          state.whenIsNextTasks.foreach(_())
        }
      }
    }

    for(state <- states){
      val entry = Bool().setLambdaName(this.isNamed && state.isNamed)(this.getName + "_onEntry_" + enumOf(state).getName)
      entry := isEntering(state) 
      when(entry) {
        state.onEntryTasks.foreach(_())
      }
    }

    // `always` and `postBuild` must be processed after states to get priority
    // in the behavior of the elaborated design.
    alwaysTasks.foreach(_())
    postBuildTasks.foreach(_())

    when(wantStart){
      if(entryState == null)
        globalData.pendingErrors += (() => (s"$this as no entry point set. val yourState : State = new State with EntryPoint{...}   should solve the situation at \n${getScalaLocationLong}"))
      else if(entryState != stateBoot)
        forceGoto(entryState)
    }
    when(wantKill){
      forceGoto(stateBoot)
    }

    if(corruptedState != null){
      switch(stateReg, coverUnreachable = true){
        for(e <- states) is(enumOf(e)){
          corruptedState := False
        }
        default{
          corruptedState := True
        }
      }
      for(state <- states){
        val entry = Bool().setLambdaName(this.isNamed && state.isNamed)(this.getName + "onEntry_" + enumOf(state).getName)
        entry := isEntering(state)
        when(entry) {
          state.onEntryTasks.foreach(_())
        }
      }
    }
  }

  Component.current.afterElaboration{
    if(parentStateMachine == null && !builded) {
      build()
    }
  }


  override def setEntry(state: State): Unit = {
    assert(entryState == null, "Entry point already set !")
    entryState = state
  }

  override def getEntry(): State = entryState

  override def goto(state: State): Unit = {
    assert(inGeneration, "You can't use the 'goto' function there. Maybe you should use an always{.. goto(x) ..} block ?")
    if (transitionCond != null)
      stateNextCand := enumOf(state)
    else
      stateNext := enumOf(state)
  }

  override def forceGoto(state: State): Unit = {
    assert(inGeneration, "You can't use the 'forceGoto' function there. Maybe you should use an always{.. forceGoto(x) ..} block ?")
    stateNext := enumOf(state)
  }

  def postBuild(body : => Unit){
    builded match {
      case false => postBuildTasks += (() => body)
      case true => body
    }
  }
  override def isActive(state: State): Bool = Component.current.onBody{
    val ret = Bool()
    postBuild{
      ret := stateReg === enumOf(state)
    }
    ret
  }

  override def isEntering(state: State): Bool = Component.current.onBody{
    val ret = Bool()
    postBuild{
      ret := stateNext === enumOf(state) && stateReg =/= enumOf(state)
    }
    ret
  }

  override def isExiting(state: State): Bool = Component.current.onBody{
    val ret = Bool()
    postBuild{
      ret := stateNext =/= enumOf(state) && stateReg === enumOf(state)
    }
    ret
  }

  override def add(state: State): Int = {
    if (state.isInstanceOf[StateBoot]) {
      states.+=:(state)
    } else {
      states += state
    }
    states.length-1
  }

  override def add(stateMachine: StateMachineAccessor): Unit = {
    childStateMachines += stateMachine
    stateMachine.setParentStateMachine(this)
  }

  override def startFsm(): Unit = {
    wantStart := True
  }

  def exit(): Unit = {
    exitFsm()
  }

  override def exitFsm(): Unit = {
    wantExit := True
    goto(stateBoot)
  }

  def killFsm() : Unit = {
    wantKill := True
  }

  @dontName implicit val implicitFsm : StateMachine = this

  override def disableAutoStart(): Unit = autoStart = false

  override def setParentStateMachine(parent: StateMachineAccessor): Unit = parentStateMachine = parent

  override def cacheGet(key: Any): Option[Any] = cache.get(key)
  override def cachePut(key: Any, value: Any): Unit = cache.put(key, value)

  override def isStateNextBoot(): Bool = stateNext === enumOf(stateBoot)
  override def isStateRegBoot():  Bool = stateReg === enumOf(stateBoot)

  def onStart(body : => Unit) = stateBoot.onExit(body)
  def isStarted = !isActive(stateBoot)
  def isStopped = isActive(stateBoot)
  def isRunning = isStarted
}


class StateMachineSlave extends StateMachine{
  disableAutoStart()
}