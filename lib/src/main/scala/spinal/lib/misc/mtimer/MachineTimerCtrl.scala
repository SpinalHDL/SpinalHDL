package spinal.lib.misc.mtimer

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory


object MachineTimerCtrl {
  def apply(busCtrl: BusSlaveFactory, p: Parameter = Parameter.default) = MachineTimerCtrl(busCtrl, p)

  case class Parameter(
    width: Int
  ) {
    assert(width > 32, "MachineTimer needs two compare registers. Timer width should be more than 32.")
  }
  object Parameter {
    def default = Parameter(64)
    def lightweight = Parameter(40)
  }

  case class Io(p: Parameter) extends Bundle {
    val interrupt = out(Bool)
  }

  case class MachineTimerCtrl(
    busCtrl: BusSlaveFactory,
    p: Parameter
  ) extends Area {
    val io = Io(p)

    val counter = Reg(UInt(p.width bits)) init(0)
    val compare = Reg(UInt(p.width bits))
    val hit = RegNext(!(counter - compare).msb)
    counter := counter + 1
    io.interrupt := hit

    busCtrl.readMultiWord(counter, 0x0)
    busCtrl.writeMultiWord(compare, 0x8)
  }
}
