/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Lib                          **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/  MIT Licence                      **
**                                                                           **
** Permission is hereby granted, free of charge, to any person obtaining a   **
** copy of this software and associated documentation files (the "Software"),**
** to deal in the Software without restriction, including without limitation **
** the rights to use, copy, modify, merge, publish, distribute, sublicense,  **
** and/or sell copies of the Software, and to permit persons to whom the     **
** Software is furnished to do so, subject to the following conditions:      **
**                                                                           **
** The above copyright notice and this permission notice shall be included   **
** in all copies or substantial portions of the Software.                    **
**                                                                           **
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   **
** OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                **
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.    **
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY      **
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT **
** OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR  **
** THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                **
\*                                                                           */
package spinal.lib.fsm

import spinal.core._


object StateMachineStyle1 {

  class TopLevel extends Component {

    val io = new Bundle{
      val result = out Bool
    }

    val fsm = new StateMachine {
      val counter = Reg(UInt(8 bits)) init (0)
      io.result := False

      val stateA: State = new State with EntryPoint {
        whenIsActive (goto(stateB))
      }
      val stateB: State = new State {
        onEntry(counter := 0)
        whenIsActive {
          counter := counter + 1
          when(counter === 4){
            goto(stateC)
          }
        }
        onExit(io.result := True)
      }
      val stateC: State = new State {
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

      val stateA: State = StateEntryPoint()
        .whenIsActive(goto(stateB))

      val stateB: State = State()
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
      val enter  = out UInt (8 bits)
      val active = out UInt (8 bits)
      val exit   = out UInt (8 bits)
    }

    io.enter  := 0xFF
    io.active := 0xFF
    io.exit   := 0xFF

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
      val stateF : State = new StateDelay(30){ whenCompleted(goto(stateG)) }
      val stateG : State = new StateDelay(40){
        whenCompleted{
          goto(stateH)
        }
      }
      val stateH: State = new StateDelay(50){ whenCompleted(goto(stateI)) }
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

    val fsm = new StateMachine {
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

    def InnerFsm(stateBDelay: Int) = new StateMachine {
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
      val stateB: State = new StateParallelFsm(InnerFsm(9), InnerFsm(18)){
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



object StateMachineTry6Example {
  class TopLevel extends Component {

    def InnerFsm(stateBDelay: Int) = new StateMachine {
      //...
    }

    val fsm = new StateMachine{
      val stateA = new State with EntryPoint
      val stateB = new StateFsm(InnerFsm(5))
    }
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

    val toto = "asd"
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}




object StateMachineSimExample {
  class TopLevel extends Component {
    val counter = out(Reg(UInt(8 bits)) init (0))

    val fsm = new StateMachine {
      val stateA, stateB, stateC = new State
      setEntry(stateA)
      stateA.whenIsActive {
        goto(stateB)
      }
      stateB.whenIsActive {
        goto(stateC)
      }
      stateC.onEntry(counter := 0)
      stateC.whenIsActive {
        counter := counter + 1
        when(counter === 3) {
          goto(stateA)
        }
      }
    }
  }

  def main(args: Array[String]) {
    import spinal.core.sim._
    SimConfig.compile{
      val dut = new TopLevel
      dut.fsm.stateReg.simPublic()
      dut
    }.doSim{dut =>
      dut.clockDomain.forkStimulus(10)

      for(i <- 0 until 20){
        dut.clockDomain.waitSampling()
        println(dut.fsm.stateToEnumElement(dut.fsm.stateC) == dut.fsm.stateReg.toEnum)
      }
    }
  }
}

