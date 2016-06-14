package spinal.lib.fsm

import spinal.core._

import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC32F_USER on 14/06/2016.
 */
trait EntryPoint
class State(implicit stateMachineAccessor : StateMachineAccessor) extends Nameable{
  val onEntryTasks = ArrayBuffer[() => Unit]()
  val onExitTasks = ArrayBuffer[() => Unit]()
  val whenActiveTasks = ArrayBuffer[() => Unit]()
  val whenIsNextTasks = ArrayBuffer[() => Unit]()
  var innerFsm = ArrayBuffer[StateMachine]()

  def onEntry(doThat : => Unit) : Unit = onEntryTasks += (() => doThat)
  def onExit(doThat : => Unit) : Unit = onExitTasks += (() => doThat)
  def whenIsActive(doThat : => Unit) : Unit = whenActiveTasks += (() => doThat)
  def whenIsNext(doThat : => Unit) : Unit = whenIsNextTasks += (() => doThat)
  def goto(state : State) = stateMachineAccessor.goto(state)
  def innerFsm(that : => StateMachine) : Unit = innerFsm += that
  def exit() : Unit = stateMachineAccessor.exit()

  val stateId = stateMachineAccessor.add(this)

  if(isInstanceOf[EntryPoint]) stateMachineAccessor.setEntry(this)
}

class StateInnerFsm(inner : => StateMachine,returnIn : =>  State)(implicit stateMachineAccessor : StateMachineAccessor) extends State{
  onEntry{
    inner.start()
  }
  whenIsActive{
    when(inner.wantExit){
      goto(returnIn)
    }
  }
  stateMachineAccessor.add(inner)
  inner.autoStart = false
}

class StateExit(implicit stateMachineAccessor : StateMachineAccessor) extends State{
  whenIsActive{
    stateMachineAccessor.exit()
  }
}