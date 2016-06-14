package spinal.lib.fsm

import spinal.core._

/**
 * Created by PIC32F_USER on 14/06/2016.
 */


object StateMachineSimpleExample{
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


//
//object StateMachineWithInnerExample{
//  class TopLevel extends Component{
//    val io = new Bundle{
//      val enter  = out UInt(8 bits)
//      val active = out UInt(8 bits)
//      val exit   = out UInt(8 bits)
//    }
//
//    io.enter  := 0xFF
//    io.active := 0xFF
//    io.exit   := 0xFF
//
//
//
//    val counter = Reg(UInt(8 bits)) init(0)
//
//    implicit val fsm = new StateMachine
//    val stateA : State = new State with EntryPoint{
//      whenActive{
//        goto(stateB)
//        io.active := stateId
//      }
//    }
//    val stateB : State = new State{
//      onEntry{
//        counter := 0
//      }
//      whenActive {
//        when(counter === 6) {
//          goto(inner.fsm)
//        }
//        counter := counter + 1
//      }
//    }
//
//
//    val inner = new Area{
//      val counter = Reg(UInt(8 bits)) init(0)
//
//      implicit val fsm = new StateMachine
//      val stateA : State = new State with EntryPoint{
//        whenActive{
//          goto(stateB)
//        }
//      }
//      val stateB : State = new State{
//        onEntry{
//          counter := 0
//        }
//        whenActive {
//          when(counter === 3) {
//            goto(stateA)
//          }
//          counter := counter + 1
//        }
//      }
//    }
//  }
//
//  def main(args: Array[String]) {
//    SpinalVhdl(new TopLevel)
//  }
//}