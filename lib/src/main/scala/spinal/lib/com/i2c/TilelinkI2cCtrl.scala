package spinal.lib.com.i2c

import spinal.core._
import spinal.core.fiber.Fiber
import spinal.lib.bus.tilelink._
import spinal.lib.misc.InterruptNode
import spinal.lib.{master, slave}


object TilelinkI2cCtrl {
  def getSupported(proposed : M2sSupport) = SlaveFactory.getSupported(
    addressWidth = addressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed = proposed
  )
  def addressWidth = 8
}

case class TilelinkI2cCtrl(generics : I2cSlaveMemoryMappedGenerics, ctrlParam: BusParameter) extends Component{
  val io = new Bundle{
    val ctrl =  slave(Bus(ctrlParam))
    val i2c = master(I2c())
    val interrupt = out Bool()
  }

  val i2cCtrl = new I2cSlave(generics.ctrlGenerics)

  val busCtrl = new SlaveFactory(io.ctrl, false)
  val bridge = i2cCtrl.io.driveFrom(busCtrl,0)(generics)

  //Phy
  io.i2c.scl.write := RegNext(bridge.i2cBuffer.scl.write) init(True)
  io.i2c.sda.write := RegNext(bridge.i2cBuffer.sda.write) init(True)
  bridge.i2cBuffer.scl.read := io.i2c.scl.read
  bridge.i2cBuffer.sda.read := io.i2c.sda.read

  io.interrupt := bridge.interruptCtrl.interrupt
}


case class TilelinkI2cCtrlFiber(generics : I2cSlaveMemoryMappedGenerics) extends Area{
  val ctrl = fabric.Node.up()
  val interrupt = InterruptNode.master()

  val logic = Fiber build new Area{
    ctrl.m2s.supported.load(TilelinkI2cCtrl.getSupported(ctrl.m2s.proposed))
    ctrl.s2m.none()

    val core = TilelinkI2cCtrl(generics, ctrl.bus.p)
    core.io.ctrl <> ctrl.bus
    core.io.interrupt <> interrupt.flag
    val i2c = core.io.i2c.toIo
  }
}

