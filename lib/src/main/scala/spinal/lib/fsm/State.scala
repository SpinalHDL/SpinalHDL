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

  def onEntry(doThat : => Unit) : Unit = onEntryTasks += (() => doThat)
  def onExit(doThat : => Unit) : Unit = onExitTasks += (() => doThat)
  def whenActive(doThat : => Unit) : Unit = whenActiveTasks += (() => doThat)
  def goto(state : State) = stateMachineAccessor.goto(state)
  def goto(stateMachine: StateMachine) = stateMachineAccessor.goto(stateMachine)


  val stateId = stateMachineAccessor.add(this)

  if(isInstanceOf[EntryPoint]) stateMachineAccessor.setEntry(this)
}
