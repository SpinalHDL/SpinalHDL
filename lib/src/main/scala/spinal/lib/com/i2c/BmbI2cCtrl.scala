package spinal.lib.com.i2c

import spinal.core._
import spinal.lib.bus.bmb._
import spinal.lib.{master, slave}


object BmbI2cCtrl {
  def getBmbCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = addressWidth,
    dataWidth = 32
  )
  def addressWidth = 8
}

case class BmbI2cCtrl(generics : I2cSlaveMemoryMappedGenerics, bmbParameter: BmbParameter) extends Component{
  val io = new Bundle{
    val ctrl =  slave(Bmb(bmbParameter))
    val i2c = master(I2c())
    val interrupt = out Bool
  }

  val i2cCtrl = new I2cSlave(generics.ctrlGenerics)

  val busCtrl = BmbSlaveFactory(io.ctrl)
  val bridge = i2cCtrl.io.driveFrom(busCtrl,0)(generics)

  //Phy
  io.i2c.scl.write := RegNext(bridge.i2cBuffer.scl.write) init(True)
  io.i2c.sda.write := RegNext(bridge.i2cBuffer.sda.write) init(True)
  bridge.i2cBuffer.scl.read := io.i2c.scl.read
  bridge.i2cBuffer.sda.read := io.i2c.sda.read

  io.interrupt := bridge.interruptCtrl.interrupt
}


