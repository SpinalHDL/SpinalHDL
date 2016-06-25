package spinal.lib.fsm

/**
 * Created by PIC32F_USER on 14/06/2016.
 */



import spinal.core._
import spinal.lib._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC32F_USER on 14/06/2016.
 */

trait StateMachineAccessor{
  def setEntry(state : State) : Unit
  def getEntry() : State
  def isActive(state : State) : Bool
  def isEntering(state : State) : Bool
  def goto(state : State) : Unit
//  def goto(state : SpinalEnumElement[StateMachineEnum]) : Unit
  def add(state : State) : Int
  def add(stateMachine: StateMachineAccessor) : Unit
  def start() : Unit
  def exit() : Unit
  def wantExit() : Bool
  def disableAutoStart() : Unit
  def getName() : String
  def build() : Unit
  def setParentStateMachine(parent : StateMachineAccessor) : Unit
  def cacheGet(key : Any) : Option[Any]
  def cachePut(key : Any,value : Any) : Unit
  def isStateNextBoot() : Bool
  def isStateRegBoot() : Bool
  def cacheGetOrElseUpdate(key: Any, op: => Any) : Any = {
    cacheGet(key) match{
      case Some(value) => value
      case None => {
        val value = op
        cachePut(key,value)
        value
      }
    }
  }
}


class StateBoot(autoStart : Boolean)(implicit stateMachineAccessor : StateMachineAccessor) extends State{
  if(autoStart) {
    whenIsActive {
      stateMachineAccessor.start()
    }
  }
}

class StateMachineEnum extends SpinalEnum

class StateMachine extends Area with StateMachineAccessor with ScalaLocated{
  var inGeneration = false
  val alwaysTasks = ArrayBuffer[() => Unit]()
  def always(doThat : => Unit) : this.type = {
    alwaysTasks += (() => doThat)
    this
  }
  def setEncoding(encoding : SpinalEnumEncoding) : Unit = enumDefinition.defaultEncoding = encoding

  @dontName val postBuildTasks = ArrayBuffer[() => Unit]()
  val cache = mutable.HashMap[Any,Any]()
  val enumDefinition = new StateMachineEnum
  var stateReg  : enumDefinition.C = null
  var stateNext : enumDefinition.C = null
  var stateBoot : State = null
  val wantExit = False
  var autoStart = true
  @dontName var parentStateMachine : StateMachineAccessor = null
  @dontName val childStateMachines = mutable.Set[StateMachineAccessor]()
  @dontName val states = ArrayBuffer[State]()
  val stateToEnumElement = mutable.HashMap[State,enumDefinition.E]()
  @dontName var entryState : State = null
  def enumOf(state : State) = {
    checkState(state)
    stateToEnumElement(state)
  }
  def checkState(state : State) = assert(state.getStateMachineAccessor == this,s"A state machine ($this)is using a state ($state) that come from another state machine.\n\nState machine defined at ${this.getScalaLocationLong}\n State defined at ${state.getScalaLocationLong}")
  def build() : Unit = {
    inGeneration = true
    childStateMachines.foreach(_.build())
    stateBoot = new StateBoot(autoStart).setName("boot") //TODO


    stateReg  = Reg(enumDefinition())
    stateNext = enumDefinition()

    if(this.isNamed){
      stateReg.setWeakName(this.getName() + "_stateReg")
      stateNext.setWeakName(this.getName() + "_stateNext")
    }

    for(state <- states){
      checkState(state)
      val enumElement = enumDefinition.newElement(if(state.isNamed) state.getName() else null)
      stateToEnumElement += (state -> enumElement)
    }

    stateReg init(enumOf(this.stateBoot))
    stateReg := stateNext

    val stateRegOneHotMap  = states.map(state => (state -> (stateReg === enumOf(state)))).toMap
    val stateNextOneHotMap = states.map(state => (state -> (stateNext === enumOf(state)))).toMap


    stateNext := stateReg
    switch(stateReg){
      for(state <- states){
        if(state == stateBoot) default {
          state.whenActiveTasks.foreach(_())
        } else is(enumOf(state)) {
          state.whenActiveTasks.foreach(_())
        }
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
      when(!stateRegOneHotMap(state) && stateNextOneHotMap(state)){
        state.onEntryTasks.foreach(_())
      }
      when(stateRegOneHotMap(state) && !stateNextOneHotMap(state)){
        state.onExitTasks.foreach(_())
      }
    }


    alwaysTasks.foreach(_())
    postBuildTasks.foreach(_())
  }

  Component.current.addPrePopTask(() => {
    if(parentStateMachine == null)
      build()
  })


  override def setEntry(state : State): Unit = {
    assert(entryState == null,"Entry point already set !")
    entryState = state
  }

  override def getEntry(): State = entryState

  override def goto(state: State): Unit = {
    assert(inGeneration,"You can't use the 'goto' function there. Maybe you should use an always{.. goto(x) ..} block ?")
    stateNext := enumOf(state)
  }
  def isActive(state : State) : Bool ={
    val ret = Bool
    postBuildTasks += {() => {
      ret := stateReg === enumOf(state)
    }}
    ret
  }

  def isEntering(state : State) : Bool ={
    val ret = Bool
    postBuildTasks += {() => {
      ret := stateNext === enumOf(state) && stateReg =/= enumOf(state)
    }}
    ret
  }
  //override def goto(state: SpinalEnumElement[StateMachineEnum]): Unit = stateNext := state.asInstanceOf[enumDefinition.E]

//  override def goto(state: SpinalEnumElement[StateMachineEnum]): Unit = ??? //stateNext := state

  override def add(state: State): Int = {
    states += state
    states.length-1
  }
  override def add(stateMachine : StateMachineAccessor) : Unit = {
    childStateMachines += stateMachine
    stateMachine.setParentStateMachine(this)
  }

  def start() : Unit = goto(entryState)
  def exit() : Unit = {
    wantExit := True
    goto(stateBoot)
  }
  @dontName implicit val implicitFsm = this

  override def disableAutoStart(): Unit = autoStart = false

  override def setParentStateMachine(parent: StateMachineAccessor): Unit = parentStateMachine = parent

  override def cacheGet(key: Any): Option[Any] = cache.get(key)
  override def cachePut(key: Any, value: Any): Unit = cache.put(key,value)
  def isStateNextBoot() : Bool = stateNext === enumOf(stateBoot)
  def isStateRegBoot() : Bool = stateReg === enumOf(stateBoot)
}
