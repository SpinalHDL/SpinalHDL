package spinal.lib.fsm

import spinal.core._
import spinal.lib._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC32F_USER on 14/06/2016.
 */

trait EntryPoint
class State(implicit stateMachineAccessor : StateMachineAccessor) extends Nameable{
  val onEntryTasks = ArrayBuffer[() => Unit]()
  val onExitTasks = ArrayBuffer[() => Unit]()
  val whenActiveTasks = ArrayBuffer[() => Unit]()

  def onEntry(doThat : => Unit) : Unit = onEntryTasks += (() => doThat)
  def onExit(doThat : => Unit) : Unit = onExitTasks += (() => doThat)
  def whenActive(doThat : => Unit) : Unit = whenActiveTasks += (() => doThat)
  def goto(state : State) = stateMachineAccessor.goto(state)


  val stateId = stateMachineAccessor.add(this)

  if(isInstanceOf[EntryPoint]) stateMachineAccessor.setEntry(this)
}

class StateMachineEnum extends SpinalEnum{
  def build(states : Seq[State],stateToEnumElement : mutable.HashMap[State,E]) : Unit = {
    for(state <- states){
      val enumElement = newElement(state.getName())
      stateToEnumElement += (state -> enumElement)
    }
  }
}

trait StateMachineAccessor{
  def setEntry(state : State) : Unit
  def goto(state : State) : Unit
  def add(state : State) : Int
}


class StateBoot(initialState : State)(implicit stateMachineAccessor : StateMachineAccessor) extends State{
  whenActive{
    goto(initialState)
  }
}

class StateMachine extends Area with StateMachineAccessor{
  val enumDefinition = new StateMachineEnum
  var stateReg  : enumDefinition.T = null
  var stateNext : enumDefinition.T = null

  val states = ArrayBuffer[State]()
  val stateToEnumElement = mutable.HashMap[State,enumDefinition.E]()
  var entryState : State = null
  def enumOf(state : State) = stateToEnumElement(state)
  def build() : Unit = {
    val stateBoot = new StateBoot(entryState)(this).setName("boot") //TODO

    enumDefinition.build(states,stateToEnumElement)
    
    stateReg  = RegInit(enumOf(stateBoot)).setName("STATE_REG") //TODO
    stateNext = enumDefinition().setName("STATE_NEXT")  //TODO
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

    for(state <- states){
      when(!stateRegOneHotMap(state) && stateNextOneHotMap(state)){
        state.onEntryTasks.foreach(_())
      }
      when(stateRegOneHotMap(state) && !stateNextOneHotMap(state)){
        state.onExitTasks.foreach(_())
      }
    }
  }

  component.addPrePopTask(() => build())


  override def setEntry(state : State): Unit = {
    assert(entryState == null,"Entry point already set !")
    entryState = state
  }
  override def goto(state: State): Unit = stateNext := enumOf(state)
  override def add(state: State): Int = {
    states += state
    states.length-1
  }
}


object StateMachineExample{
  class TopLevel extends Component{
    val io = new Bundle{
      val enter  = out UInt(8 bits)
      val active = out UInt(8 bits)
      val exit   = out UInt(8 bits)
    }

    io.enter  := 0xFF
    io.active := 0xFF
    io.exit   := 0xFF

    val counter = Reg(UInt(8 bits)) init(0)

    implicit val fsm = new StateMachine

    val stateA : State = new State with EntryPoint{
      onEntry{
        io.enter := stateId
      }
      whenActive{
        goto(stateB)
        io.active := stateId
      }
      onExit{
        io.exit := stateId
      }
    }

    val stateB : State = new State{
      onEntry {
        io.enter := stateId
        counter := 0
      }
      whenActive {
        when(counter === 9) {
          goto(stateA)
        }
        counter := counter + 1
        io.active := stateId
      }
      onExit {
        io.exit := stateId
      }
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}