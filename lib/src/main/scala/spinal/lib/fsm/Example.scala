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
import spinal.lib.com.uart._

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

object StateMachineCondTransExample {
  class TopLevel extends Component {
    val counter = out(Reg(UInt(8 bits)) init (0))
    val cond = in Bool
    val fsm = new StateMachine {
      setTransitionCondition(cond)
      val stateA, stateB, stateC = new State
      setEntry(stateA)
      stateA.whenIsActive {
        goto(stateB)
      }
      stateB.whenIsActive {
        forceGoto(stateC)
      }
      stateC.onEntry(counter := 0)
      stateC.whenIsActive {
        when(counter === 3) {
          goto(stateA)
        }.otherwise {
          counter := counter + 1
        }
      }
    }
  }

  def main(args: Array[String]) {
    import spinal.core.sim._
    SimConfig.compile{
      val dut = new TopLevel
      dut.fsm.stateReg.simPublic()
      dut.fsm.stateNext.simPublic()
      dut.fsm.stateNextCand.simPublic()
      dut
    }.doSim{dut =>
      dut.clockDomain.forkStimulus(10)

      dut.cond #= false
      for(i <- 0 until 20){
        dut.clockDomain.waitSampling()
        dut.cond #= !dut.cond.toBoolean
        println(f"State: ${dut.fsm.stateReg.toEnum} StateNext: ${dut.fsm.stateNext.toEnum} Cand: ${dut.fsm.stateNextCand.toEnum} Cond: ${dut.cond.toBoolean}")
      }
    }
  }
}

object StateMachineCondLargeExample {
  class LargeExample extends Component {
    val io = new Bundle {
      val clk = in Bool
      val txd = out Bool
    }

    val clkDomain = ClockDomain(
      clock = io.clk,
      frequency = FixedFrequency(100 MHz),
      config = ClockDomainConfig(resetKind = BOOT)
    )

    val logic = new ClockingArea(clkDomain) {
      val uart = new UartCtrl()
      uart.io.config.setClockDivider(115200 Hz)
      uart.io.config.frame.dataLength := 7
      uart.io.config.frame.stop := UartStopType.ONE
      uart.io.config.frame.parity := UartParityType.NONE
      uart.io.uart.rxd := True
      uart.io.uart.txd <> io.txd

      val fsm = new StateMachine {
        setTransitionCondition(uart.io.write.ready)
        uart.io.write.payload := ' '
        uart.io.write.valid := True

        val stateNL = new State with EntryPoint
        val stateH1 = new State
        val stateE1 = new State
        val stateL1 = new State
        val stateL2 = new State
        val stateO = new State
        val stateS = new State
        val stateT1 = new State
        val stateA1 = new State
        val stateT2 = new State
        val stateE2 = new State
        val stateM = new State
        val stateA2 = new State
        val stateC = new State
        val stateH2 = new State
        val stateI = new State
        val stateN = new State
        val stateE3 = new State

        @dontName val textStates = Array(stateNL, stateH1, stateE1, stateL1, stateL2, stateO, stateS,
                                         stateT1, stateA1, stateT2, stateE2, stateM, stateA2, stateC,
                                         stateH2, stateI, stateN, stateE3)
        val text = "\nHelloStatemachine"
        for ((s, t) <- states.zip(text)) {
          s.whenIsActive {
            uart.io.write.payload := t
          }
        }
        for ((s1, s2) <- states.zip(states.slice(1, states.length-1) ++ states.slice(0, 1))) {
          s1.whenIsActive {
            if(transitionCond == null) {
              when(uart.io.write.ready) {
                goto(s2)
              }
            } else {
              goto(s2)
            }
          }
        }
      }
    }
  }

  def main(args: Array[String]) : Unit = {
    SpinalVerilog(new LargeExample()).printPruned().printUnused()
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

