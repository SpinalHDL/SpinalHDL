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
  def goto(state : State) : Unit
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
  val cache = mutable.HashMap[Any,Any]()
  val enumDefinition = new StateMachineEnum
  var stateReg  = Reg(enumDefinition())
  var stateNext = enumDefinition()
  val wantExit = False
  var autoStart = true
  @dontName var parentStateMachine : StateMachineAccessor = null
  @dontName val childStateMachines = ArrayBuffer[StateMachineAccessor]()
  val stateMachineToEnumElement = mutable.HashMap[StateMachineAccessor,enumDefinition.E]()
  @dontName val states = ArrayBuffer[State]()
  val stateToEnumElement = mutable.HashMap[State,enumDefinition.E]()
  @dontName var entryState : State = null
  def enumOf(state : State) = {
    checkState(state)
    stateToEnumElement(state)
  }
  def enumOf(stateMachine : StateMachineAccessor) = stateMachineToEnumElement(stateMachine)
  def checkState(state : State) = assert(state.getStateMachineAccessor == this,s"A state machine ($this)is using a state ($state) that come from another state machine.\n\nState machine defined at ${this.getScalaLocationLong}\n State defined at ${state.getScalaLocationLong}")
  def build() : Unit = {
    childStateMachines.foreach(_.build())
    val stateBoot = new StateBoot(autoStart).setName("boot") //TODO

    for(state <- states){
      checkState(state)
      val enumElement = enumDefinition.newElement(state.getName())
      stateToEnumElement += (state -> enumElement)
    }
    for(child <- childStateMachines){
      val enumElement = enumDefinition.newElement(child.getName())
      stateMachineToEnumElement += (child -> enumElement)
    }

    stateReg init(enumOf(stateBoot))
    stateReg := stateNext

    val stateRegOneHotMap  = states.map(state => (state -> (stateReg === enumOf(state)))).toMap
    val stateNextOneHotMap = states.map(state => (state -> (stateNext === enumOf(state)))).toMap


    stateNext := stateReg
    switch(stateReg){
      for(state <- states){
        state match {
          case `stateBoot` => default {
            state.whenActiveTasks.foreach(_())
          }
          case _ => is(enumOf(state)) {
            state.whenActiveTasks.foreach(_())
          }
        }
      }
    }

    switch(stateNext){
      for(state <- states){
        state match {
          case `stateBoot` => default {
            state.whenIsNextTasks.foreach(_())
          }
          case _ => is(enumOf(state)) {
            state.whenIsNextTasks.foreach(_())
          }
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

  override def goto(state: State): Unit = stateNext := enumOf(state)

  override def add(state: State): Int = {
    states += state
    states.length-1
  }
  override def add(stateMachine : StateMachineAccessor) : Unit = {
    childStateMachines += stateMachine
    stateMachine.setParentStateMachine(this)
  }

  def start() : Unit = goto(entryState)
  def exit() : Unit = wantExit := True
  @dontName implicit val implicitFsm = this

  override def disableAutoStart(): Unit = autoStart = false

  override def setParentStateMachine(parent: StateMachineAccessor): Unit = parentStateMachine = parent

  override def cacheGet(key: Any): Option[Any] = cache.get(key)
  override def cachePut(key: Any, value: Any): Unit = cache.put(key,value)
}
