package spinal.lib

import spinal.core._
import spinal.core.sim._
import spinal.lib.fsm._
import spinal.tester.SpinalAnyFunSuite

class SpinalFsmTester extends SpinalAnyFunSuite {
  test("nested fsm onEntry") {
    SimConfig
      .compile(new Component {
        val io = new Bundle {
          val go = in Bool()
          val a = out Bool()
          val b = out Bool()
          val aReg = out(Reg(Bool()) init(False))
          val bReg = out(Reg(Bool()) init(False))
        }

        io.a := False
        io.b := False
        io.aReg := False
        io.bReg := False

        val fsm = new StateMachine() {
          val idle: State = new State with EntryPoint() {
            whenIsActive {
              io.b := False
              io.bReg := False
              when (io.go) { goto(par) }
            }
          }
          val par = new StateFsm(
            new StateMachine {
              val nested = new State() with EntryPoint {
                onEntry {
                  io.a := True
                  io.b := True
                  io.aReg := True
                  io.bReg := True
                }
                whenIsActive { exit() }
              }
            }
          ) {
            whenCompleted { goto(idle) }
          }
        }
      })
      .doSim(dut => {
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling()
        fork {
          while (true) {
            sleep(1)
            assert(dut.io.a.toBoolean == dut.io.b.toBoolean)
          }
        }
        sleep(5)
        dut.io.go #= true
        dut.clockDomain.waitSampling()
        sleep(5)
        dut.io.go #= false
        dut.clockDomain.waitSampling(5)
        simSuccess()
      })
  }
}
