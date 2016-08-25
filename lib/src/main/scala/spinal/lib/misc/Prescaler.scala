package spinal.lib.misc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory

case class Prescaler(width : Int) extends Component{
  val io = new    Bundle{
    val clear    = in Bool
    val limit    = in UInt(width bits)
    val overflow = out Bool
  }

  val counter = Reg(UInt(width bits))
  counter := counter + 1

  when(io.clear || io.overflow){
    counter := 0
  }

  io.overflow := counter === io.limit


  def driveFrom(busCtrl : BusSlaveFactory,baseAddress : BigInt) = new Area {
    busCtrl.driveAndRead(io.limit,baseAddress)
    io.clear := busCtrl.isWriting(baseAddress)
  }
}