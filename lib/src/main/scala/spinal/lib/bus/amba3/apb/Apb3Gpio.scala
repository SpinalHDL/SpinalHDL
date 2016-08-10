package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib._
import spinal.lib.io.{TriStateArray, TriState}

case class Apb3Gpio(apbConfig : Apb3Config,gpioWidth : Int) extends Component{
  assert(apbConfig.dataWidth == 32)
  val io = new Bundle{
    val apb  = slave(Apb3(apbConfig))
    val gpio = master(TriStateArray(gpioWidth))
  }

  val ctrl = Apb3SlaveFactory(io.apb)
  ctrl.drive(io.gpio.write,0)
  ctrl.read(io.gpio.read,0)
  ctrl.read(io.gpio.write,4)
  ctrl.driveAndRead(io.gpio.writeEnable,8)
  io.gpio.writeEnable.getDrivingReg init(0)
}
