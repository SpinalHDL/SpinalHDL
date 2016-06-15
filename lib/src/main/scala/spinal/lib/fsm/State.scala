package spinal.lib.fsm

import spinal.core._

import scala.collection.mutable.ArrayBuffer

/**
 * Created by PIC32F_USER on 14/06/2016.
 */
trait EntryPoint
trait OnDonable{
  val onDoneTasks = ArrayBuffer[() => Unit]()

  def onDone(doThat : => Unit) : this.type = {
    onDoneTasks += (() => doThat)
    this
  }

  protected def doOnDoneTasks() : Unit = onDoneTasks.foreach(_())
}
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
  def goto(state : SpinalEnumElement[StateMachineEnum]) = ???
  def goto(state : SpinalEnumCraft[StateMachineEnum]) = ??? //stateMachineAccessor.goto(state)
  def enumOf(state : State) : SpinalEnumElement[StateMachineEnum] = ???
  def innerFsm(that : => StateMachine) : Unit = innerFsm += that
  def exit() : Unit = stateMachineAccessor.exit()
  def getStateMachineAccessor() = stateMachineAccessor
  val stateId = stateMachineAccessor.add(this)

  if(isInstanceOf[EntryPoint]) stateMachineAccessor.setEntry(this)
}

//object StateFsm{
//  def apply(fsm :  StateMachineAccessor)(implicit stateMachineAccessor : StateMachineAccessor) : StateFsm = new StateFsm(exitState,fsm)
//}

class StateFsm(val fsm :  StateMachineAccessor)(implicit stateMachineAccessor : StateMachineAccessor) extends State with OnDonable{
  onEntry{
    fsm.start()
  }
  whenIsActive{
    when(fsm.wantExit()){
      doOnDoneTasks()
    }
  }
  stateMachineAccessor.add(fsm)
  fsm.disableAutoStart()
}

object StatesSerialFsm{
  def apply(fsms :  StateMachineAccessor*)(doOnDone : (State) =>  Unit)(implicit stateMachineAccessor : StateMachineAccessor) : Seq[State] = {
    var nextState : State = null
    val states = for(i <- fsms.size-1 downto 0) yield{
      val nextCpy = nextState
      nextState = new StateFsm(fsms(i)){
        if(nextState == null)
          onDone(doOnDone(this))
        else
          onDone(this.goto(nextCpy))
      }
      nextState
    }
    states.reverse
  }
}


//object StateParallelFsm{
//  def apply(fsms :  StateMachineAccessor*)(implicit stateMachineAccessor : StateMachineAccessor) : StateParallelFsm = new StateParallelFsm(fsms)
//}

class StateParallelFsm(val fsms :  StateMachineAccessor*)(implicit stateMachineAccessor : StateMachineAccessor) extends State with OnDonable{
  onEntry{
    fsms.foreach(_.start())
  }
  whenIsActive{
    val readys = fsms.map(fsm => fsm.isStateRegBoot() || fsm.wantExit())
    when(readys.reduce(_ && _)){
      doOnDoneTasks()
    }
  }
  for(fsm <- fsms){
    stateMachineAccessor.add(fsm)
    fsm.disableAutoStart()
  }
}

//class StateExit(implicit stateMachineAccessor : StateMachineAccessor) extends State{
//  whenIsActive{
//    stateMachineAccessor.exit()
//  }
//}
object StateMachineSharableUIntKey
class StateMachineSharableRegUInt{
  val value = Reg(UInt())
  var width = 0
  def addMinWidth(min : Int): Unit ={
    width = Math.max(width,min)
  }

  Component.current.addPrePopTask(() => value.setWidth(width))
}

class StateDelay(cyclesCount : BigInt)(implicit stateMachineAccessor : StateMachineAccessor)  extends State with OnDonable{
  val cache = stateMachineAccessor.cacheGetOrElseUpdate(StateMachineSharableUIntKey,new StateMachineSharableRegUInt).asInstanceOf[StateMachineSharableRegUInt]
  cache.addMinWidth(log2Up(cyclesCount))

  onEntry{
    cache.value := cyclesCount - 1
  }
  whenIsActive{
    cache.value := cache.value - 1
    when(cache.value === 0){
      doOnDoneTasks()
    }
  }
}