package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib._
import spinal.lib.io.{TriStateArray, TriState}

object Apb3Gpio{
  def getApb3Config() = {
    Apb3Config(addressWidth = 4,dataWidth = 32)
  }
}

case class Apb3Gpio(gpioWidth : Int) extends Component{
  val io = new Bundle{
    val apb  = slave(Apb3(Apb3Gpio.getApb3Config()))
    val gpio = master(TriStateArray(gpioWidth bits))
  }

  val ctrl = Apb3SlaveFactory(io.apb)
  ctrl.drive(io.gpio.write,0)
  ctrl.read(io.gpio.read,0)
  ctrl.read(io.gpio.write,4)
  ctrl.driveAndRead(io.gpio.writeEnable,8)
  io.gpio.writeEnable.getDrivingReg init(0)
}
