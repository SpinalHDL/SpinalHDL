package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._

/**
  * Demo SoC-level wrapper showing I2cAutoMaster and I2cAutoSlave
  * with flat register bus interfaces.
  */
object I2cAutoDemo {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new I2cAutoDemo())
  }
}

/**
  * 10-bit address variant demo.
  */
class I2cAutoDemo10bit extends Component {

  val master = new I2cAutoMaster(I2cAutoMaster.Config(
    fifoDepth    = 64,
    support10bit = true,
    timerWidth   = 12
  ))

  val slave = new I2cAutoSlave(I2cAutoSlave.Config(
    ramDepth     = 64,
    validAddrs   = (0 until 64).toSeq,
    support10bit = true
  ))

  val io = new Bundle {
    val i2cMaster    = spinal.lib.master(I2c())
    val i2cSlave     = spinal.lib.slave(I2c())
    val irqMaster    = out Bool()
    val irqSlave     = out Bool()
    val sclStretch   = out Bool()
    val masterRegBus = new I2cAutoMaster.RegBus()
    val slaveRegBus  = new I2cAutoSlave.RegBus()
  }

  io.i2cMaster <> master.io.i2c
  io.i2cSlave  <> slave.io.i2c
  io.irqMaster := master.io.irq
  io.irqSlave  := slave.io.irq
  io.sclStretch := slave.io.sclStretch
  io.masterRegBus <> master.io.regBus
  io.slaveRegBus  <> slave.io.regBus
}

class I2cAutoDemo extends Component {

  val master = new I2cAutoMaster(I2cAutoMaster.Config(
    fifoDepth  = 64,
    timerWidth = 12
  ))

  val slaveCtrl = new I2cAutoSlave(I2cAutoSlave.Config(
    ramDepth   = 64,
    validAddrs = (0 until 64).toSeq
  ))

  val io = new Bundle {
    val i2cMaster    = spinal.lib.master(I2c())
    val i2cSlave     = spinal.lib.slave(I2c())
    val irqMaster    = out Bool()
    val irqSlave     = out Bool()
    val masterRegBus = new I2cAutoMaster.RegBus()
    val slaveRegBus  = new I2cAutoSlave.RegBus()
  }

  io.i2cMaster <> master.io.i2c
  io.i2cSlave  <> slaveCtrl.io.i2c
  io.irqMaster := master.io.irq
  io.irqSlave  := slaveCtrl.io.irq
  io.masterRegBus <> master.io.regBus
  io.slaveRegBus  <> slaveCtrl.io.regBus
}

object I2cAutoDemo10bitGen {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new I2cAutoDemo10bit)
  }
}
