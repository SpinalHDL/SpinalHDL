package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._

/**
  * Demo SoC-level wrapper showing I2cAutoMaster and I2cAutoSlave
  * integrated with a BMB bus for register access.
  */
object I2cAutoDemo {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new I2cAutoDemo())
  }
}

class I2cAutoDemo extends Component {

  // === Master side ===
  val master = new I2cAutoMaster(I2cAutoMaster.Config(
    fifoDepth  = 64,
    timerWidth = 12
  ))

  val masterBus = spinal.lib.slave(Bmb(BmbParameter(
    addressWidth = 8,
    dataWidth    = 32,
    sourceWidth  = 0,
    contextWidth = 0,
    lengthWidth  = 2
  )))
  val masterBmf = BmbSlaveFactory(masterBus)
  master.driveFrom(masterBmf, 0)

  // === Slave side ===
  val slaveCtrl = new I2cAutoSlave(I2cAutoSlave.Config(
    ramDepth   = 64,
    validAddrs = (0 until 64).toSeq
  ))

  val slaveBus = spinal.lib.slave(Bmb(BmbParameter(
    addressWidth = 8,
    dataWidth    = 32,
    sourceWidth  = 0,
    contextWidth = 0,
    lengthWidth  = 2
  )))
  val slaveBmf = BmbSlaveFactory(slaveBus)
  slaveCtrl.driveFrom(slaveBmf, 0)

  // === External I2C pins ===
  val io = new Bundle {
    val i2cMaster = spinal.lib.master(I2c())
    val i2cSlave  = spinal.lib.master(I2c())
    val irqMaster = out Bool()
    val irqSlave  = out Bool()
  }

  io.i2cMaster <> master.io.i2c
  io.i2cSlave  <> slaveCtrl.io.i2c
  io.irqMaster := master.io.irq
  io.irqSlave  := slaveCtrl.io.irq
}
