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
      val stateA = new State with EntryPoint
      val stateB = new State
      val stateC = new State

      val counter = Reg(UInt(8 bits)) init (0)
      io.result := False

      stateA.whenIsActive (goto(stateB))
      stateB.onEntry(counter := 0)
      stateB.whenIsActive {
        counter := counter + 1
        when(counter === 4){
          goto(stateC)
        }
      }
      stateB.onExit(io.result := True)
      stateC.whenIsActive (goto(stateA))
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
          when(counter === countTo) {
            goto(stateExit)
          }
          counter := counter + 1
        }
      }

      val stateExit: State = new StateExit()
    }

    val coreFsm = new StateMachine {
      val stateA: State = new State with EntryPoint {
        whenIsActive {
          goto(stateB)
        }
      }

      val stateB: State = StateFsm(exitState = stateC) {
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
                goto(stateExit)
              }
              counter := counter + 1
            }
          }

          val stateExit: State = new StateExit()
        }
      }
      val stateC = StateParallelFsm(exitState = stateD) (
        simpleFsm(5),
        simpleFsm(8),
        simpleFsm(3)
      )
      val stateD = StateParallelFsm(exitState = stateE.head) (
        simpleFsm(5),
        simpleFsm(8),
        simpleFsm(3)
      )
      val stateE = StatesSerialFsm(exitState = stateF) (
        simpleFsm(8),
        simpleFsm(12),
        simpleFsm(16)
      )
      val stateF : State = new StateDelay(stateG,30)
      val stateG : State = new StateDelay(stateH,40)
      val stateH : State = new StateDelay(stateI,50)
      val stateI: State = new State {
        whenIsActive {
          goto(stateA)
        }
      }
    }

    out(coreFsm.stateReg)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object StateMachineTryExample {
  class TopLevel extends Component {


    val fsm = new StateMachine{
      val counter = Reg(UInt(8 bits)) init (0)

      val stateA: State = new State with EntryPoint {
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
        }
      }
    }
    fsm.stateReg.keep()
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}












