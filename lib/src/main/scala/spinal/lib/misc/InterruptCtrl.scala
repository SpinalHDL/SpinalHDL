package spinal.lib.misc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.misc.BusSlaveFactory

case class InterruptCtrl(width : Int) extends Component{
  val io = new Bundle{
    val inputs = in Bits(width bits)
    val clears = in Bits(width bits)
    val masks = in Bits(width bits)
    val pendings = out Bits(width bits)
  }
  val pendings= Reg(io.inputs) init(0)
  pendings := (pendings & ~io.clears) | io.inputs

  io.pendings := pendings & io.masks

  def driveFrom(busCtrl : BusSlaveFactory,baseAddress : Int) = new Area{
    io.clears := 0
    busCtrl.read(io.pendings, baseAddress + 0)
    busCtrl.write(io.clears, baseAddress + 0)
    busCtrl.driveAndRead(io.masks, baseAddress + 4) init(0)
  }
}


case class Apb3InterruptCtrl(width : Int) extends Component{
  val io = new Bundle{
    val bus = slave(Apb3(4, 32))
    val inputs = in Bits(width bits)
    val pendings = out Bits(width bits)
  }

  val ctrl = InterruptCtrl(width)
  ctrl.io.inputs   <> io.inputs
  ctrl.io.pendings <> io.pendings

  val factory = Apb3SlaveFactory(io.bus)
  ctrl.driveFrom(factory,0)
}