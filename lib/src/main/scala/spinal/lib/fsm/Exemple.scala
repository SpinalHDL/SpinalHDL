package spinal.lib.fsm

import spinal.core._

/**
 * Created by PIC32F_USER on 14/06/2016.
 */


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

    implicit val fsm = new StateMachine

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

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object StateMachineWithInnerExample {
  class TopLevel extends Component {
    val coreFsm = new StateMachine {
      val counter = Reg(UInt(8 bits)) init (0)

      val stateA: State = new State with EntryPoint {
        whenIsActive {
          goto(stateB)
        }
      }
      val stateB: State = new StateInnerFsm(
        fsm = new StateMachine {
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
        },
        returnIn = stateC
      )
      val stateC: State = new State {
        whenIsActive {
          goto(stateA)
        }
      }
    }

    coreFsm.stateReg.keep
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