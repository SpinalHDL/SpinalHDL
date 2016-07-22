package spinal.lib.fsm

import spinal.core._

/**
 * Created by PIC32F_USER on 14/06/2016.
 */

object StateMachineStyle1 {
  class TopLevel extends Component {
    val io = new Bundle{
      val result = out Bool
    }

    val fsm = new StateMachine{
      val counter = Reg(UInt(8 bits)) init (0)
      io.result := False

      val stateA : State = new State with EntryPoint{
        whenIsActive (goto(stateB))
      }
      val stateB : State = new State{
        onEntry(counter := 0)
        whenIsActive {
          counter := counter + 1
          when(counter === 4){
            goto(stateC)
          }
        }
        onExit(io.result := True)
      }
      val stateC : State = new State{
        whenIsActive (goto(stateA))
      }
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object StateMachineStyle2 {
  class TopLevel extends Component {
    val io = new Bundle{
      val result = out Bool
    }

    val fsm = new StateMachine{
      val stateA = new State with EntryPoint
      val stateB = new State
      val stateC = new State

      val counter = Reg(UInt(8 bits)) init (0)
      io.result := False

      stateA
        .whenIsActive (goto(stateB))

      stateB
        .onEntry(counter := 0)
        .whenIsActive {
          counter := counter + 1
          when(counter === 4){
            goto(stateC)
          }
        }
        .onExit(io.result := True)

      stateC
        .whenIsActive (goto(stateA))
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}

object StateMachineStyle3 {
  class TopLevel extends Component {
    val io = new Bundle{
      val result = out Bool
    }

    val fsm = new StateMachine{
      val counter = Reg(UInt(8 bits)) init (0)
      io.result := False

      val stateA  : State = StateEntryPoint()
        .whenIsActive(goto(stateB))

      val stateB  : State = State()
        .onEntry(counter := 0)
        .whenIsActive {
          counter := counter + 1
          when(counter === 4){
            goto(stateC)
          }
        }
        .onExit(io.result := True)

      val stateC : State = State()
        .whenIsActive (goto(stateA))
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}





object StateMachineSimpleExample {
  class TopLevel extends Component {
    val io = new Bundle {
      val enter = out UInt (8 bits)
      val active = out UInt (8 bits)
      val exit = out UInt (8 bits)
    }

    io.enter := 0xFF
    io.active := 0xFF
    io.exit := 0xFF

    val counter = Reg(UInt(8 bits)) init (0)

    val fsm = new StateMachine {

      val stateA: State = new State with EntryPoint {
        onEntry {
          io.enter := stateId
        }
        whenIsActive {
          goto(stateB)
          io.active := stateId
        }
        onExit {
          io.exit := stateId
        }
      }

      val stateB: State = new State {
        onEntry {
          io.enter := stateId
          counter := 0
        }
        whenIsActive {
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
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}









object StateMachineWithInnerExample {
  class TopLevel extends Component {

    def simpleFsm(countTo : Int) = new StateMachine {
      val counter = out(Reg(UInt(8 bits)) init (0))

      val stateA: State = new State with EntryPoint {
        whenIsActive {
          goto(stateB)
        }
      }

      val stateB: State = new State {
        onEntry (counter := 0)
        whenIsActive {
          when(counter === countTo) {
            exit()
          }
          counter := counter + 1
        }
      }
    }

    val coreFsm = new StateMachine {
      val stateA: State = new State with EntryPoint {
        whenIsActive {
          goto(stateB)
        }
      }

      val stateB: State = new StateFsm(
        new StateMachine {
          val counter = Reg(UInt(8 bits)) init (0)

          val stateA: State = new State with EntryPoint {
            whenIsActive {
              goto(stateB)
            }
          }

          val stateB: State = new State {
            onEntry {
              counter := 0
            }
            whenIsActive {
              when(counter === 3) {
                exit()
              }
              counter := counter + 1
            }
          }

        }
      ){
        whenCompleted(goto(stateC))
      }
      val stateC = new StateFsm(simpleFsm(5)){
        whenCompleted{
          goto(stateD)
        }
      }
      val stateD = new StateParallelFsm (simpleFsm(5), simpleFsm(8), simpleFsm(3)){
        whenCompleted{
          goto(stateE.head)
        }
      }
      val stateE = StatesSerialFsm (
        simpleFsm(8),
        simpleFsm(12),
        simpleFsm(16)
      )(_.goto(stateF))
      val stateF : State = new StateDelay(30){whenCompleted(goto(stateG))}
      val stateG : State = new StateDelay(40){
        whenCompleted{
          goto(stateH)
        }
      }
      val stateH : State = new StateDelay(50){whenCompleted(goto(stateI))}
      val stateI: State = new State {
        whenIsActive {
          goto(stateA)
        }
      }
    }


  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object StateMachineTryExample {
  class TopLevel extends Component {


    val fsm = new StateMachine{
      val counter = Reg(UInt(8 bits)) init (0)

      val stateA: State = new State {
        whenIsActive {
          goto(stateB)
        }
      }
      val stateB: State = new State {
        whenIsActive {
          goto(stateC)
        }
      }
      val stateC: State = new State {
        whenIsActive {
          goto(stateA)
          counter := counter + 1
        }
      }
    }
    fsm.counter.keep
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}



object StateMachineTry2Example {
  class TopLevel extends Component {


    def InnerFsm(stateBDelay : Int) = new StateMachine {
      val counter = Reg(UInt(8 bits)) init (0)

      val stateA: State = new State with EntryPoint {
        whenIsActive {
          goto(stateB)
        }
      }

      val stateB: State = new State {
        onEntry {
          counter := 0
        }
        whenIsActive {
          when(counter === stateBDelay) {
            exit()
          }
          counter := counter + 1
        }
      }
    }

    val fsm = new StateMachine{
      val stateA: State = new State with EntryPoint {
        whenIsActive {
          goto(stateB)
        }
      }
      val stateB: State = new StateParallelFsm(InnerFsm(9),InnerFsm(18)){
        whenCompleted{
          goto(stateC)
        }
      }
      val stateC: State = new State {
        whenIsActive {
          goto(stateA)
        }
      }
    }
    fsm.stateReg.keep()
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}









object StateMachineTry3Example {
  class TopLevel extends Component {

    val fsm = new StateMachine{
      val counter = Reg(UInt(8 bits)) init (0)
      counter := counter + 1

      always{
        when(counter === 9){
          counter := 0
          goto(stateB)
        }
      }
      val stateA: State = new State with EntryPoint {
        whenIsActive {
          goto(stateB)
        }
      }
      val stateB: State = new State{
        whenIsActive{
          goto(stateC)
        }
      }
      val stateC: State = new State {
        whenIsActive {
          goto(stateA)
        }
      }
      out(isActive(stateB))
    }

//    val isInStateB = out(fsm.isActive(fsm.stateB))
//    val isEnteringStateB = out(fsm.isEntering(fsm.stateB))
//
//    fsm.stateReg.keep()
    val toto = "asd"
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


