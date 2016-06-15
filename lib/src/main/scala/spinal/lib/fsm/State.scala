package spinal.lib.fsm

import spinal.core._

import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC32F_USER on 14/06/2016.
 */
trait EntryPoint
class State(implicit stateMachineAccessor : StateMachineAccessor) extends Area with ScalaLocated{
  val onEntryTasks = ArrayBuffer[() => Unit]()
  val onExitTasks = ArrayBuffer[() => Unit]()
  val whenActiveTasks = ArrayBuffer[() => Unit]()
  val whenIsNextTasks = ArrayBuffer[() => Unit]()
  @dontName var innerFsm = ArrayBuffer[StateMachine]()

  def onEntry(doThat : => Unit) : this.type = {
    onEntryTasks += (() => doThat)
    this
  }
  def onExit(doThat : => Unit) : this.type = {
    onExitTasks += (() => doThat)
    this
  }
  def whenIsActive(doThat : => Unit) : this.type = {
    whenActiveTasks += (() => doThat)
    this
  }
  def whenIsNext(doThat : => Unit) : this.type = {
    whenIsNextTasks += (() => doThat)
    this
  }
  def goto(state : State) = stateMachineAccessor.goto(state)
  def innerFsm(that : => StateMachine) : Unit = innerFsm += that
  def exit() : Unit = stateMachineAccessor.exit()
  def getStateMachineAccessor() = stateMachineAccessor
  val stateId = stateMachineAccessor.add(this)

  if(isInstanceOf[EntryPoint]) stateMachineAccessor.setEntry(this)
}

object StateFsm{
  def apply(exitState : =>  State)(fsm :  StateMachineAccessor)(implicit stateMachineAccessor : StateMachineAccessor) : StateFsm = new StateFsm(exitState,fsm)
}

class StateFsm(exitState : =>  State,val fsm :  StateMachineAccessor)(implicit stateMachineAccessor : StateMachineAccessor) extends State{
  onEntry{
    fsm.start()
  }
  whenIsActive{
    when(fsm.wantExit()){
      goto(exitState)
    }
  }
  stateMachineAccessor.add(fsm)
  fsm.disableAutoStart()
}

object StatesSerialFsm{
  def apply(exitState : =>  State)(fsms :  StateMachineAccessor*)(implicit stateMachineAccessor : StateMachineAccessor) : Seq[State] = {
    var outState = exitState
    val states = for(i <- fsms.size-1 downto 0) yield{
      val outStateCpy = outState
      outState = StateFsm(if(i == fsms.size-1) exitState else outStateCpy)(fsms(i))
      outState
    }
    states.reverse
  }
}


object StateParallelFsm{
  def apply(exitState : =>  State)(fsms :  StateMachineAccessor*)(implicit stateMachineAccessor : StateMachineAccessor) : StateParallelFsm = new StateParallelFsm(exitState,fsms)
}

class StateParallelFsm(exitState : =>  State,val fsms :  Seq[StateMachineAccessor])(implicit stateMachineAccessor : StateMachineAccessor) extends State{
  onEntry{
    fsms.foreach(_.start())
  }
  whenIsActive{
    when(fsms.map(_.wantExit).reduce(_ && _)){
      goto(exitState)
    }
  }
  for(fsm <- fsms){
    stateMachineAccessor.add(fsm)
    fsm.disableAutoStart()
  }
}

class StateExit(implicit stateMachineAccessor : StateMachineAccessor) extends State{
  whenIsActive{
    stateMachineAccessor.exit()
  }
}
object StateMachineSharableUIntKey
class StateMachineSharableRegUInt{
  val value = Reg(UInt())
  var width = 0
  def addMinWidth(min : Int): Unit ={
    width = Math.max(width,min)
  }

  Component.current.addPrePopTask(() => value.setWidth(width))
}

class StateDelay(exitIn : =>  State,cyclesCount : BigInt)(implicit stateMachineAccessor : StateMachineAccessor)  extends State{
  val cache = stateMachineAccessor.cacheGetOrElseUpdate(StateMachineSharableUIntKey,new StateMachineSharableRegUInt).asInstanceOf[StateMachineSharableRegUInt]
  cache.addMinWidth(log2Up(cyclesCount))

  onEntry{
    cache.value := cyclesCount - 1
  }
  whenIsActive{
    cache.value := cache.value - 1
    when(cache.value === 0){
      goto(exitIn)
    }
  }
}