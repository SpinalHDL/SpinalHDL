package spinal.lib.misc

import spinal.core._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.slave

case class MachineTimer() extends Component{
  val io = new Bundle{
    val bus = slave(Apb3(4, 32))
    val mTimeInterrupt = out Bool()
  }

  val mapper = Apb3SlaveFactory(io.bus, dontCareReadData = true)
  val counter = Reg(UInt(64 bits)) init(0)
  val cmp = Reg(UInt(64 bits))
  val interrupt = RegNext(counter >= cmp)
  counter := counter + 1
  io.mTimeInterrupt := interrupt
  mapper.readMultiWord(counter, 0x0)
  mapper.writeMultiWord(cmp, 0x8)
}
