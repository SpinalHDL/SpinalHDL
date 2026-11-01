package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbAccessParameter, BmbSourceParameter, BmbSlaveFactory}

/**
  * Demo SoC-level wrapper: I2cAutoMaster + I2cAutoSlave.
  *
  * Each controller's register map is instantiated at this SoC level via
  * [[I2cAutoMaster.driveFrom]] / [[I2cAutoSlave.driveFrom]] on a top-level
  * Bmb slave port. In a real SoC the fabric would connect to those ports;
  * here we tie them off so the demo focuses on I2C pin behaviour for
  * SpinalVerilog generation.
  */
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
    val i2cMaster = spinal.lib.master(I2c())
    val i2cSlave  = spinal.lib.slave(I2c())
    val irqMaster = out Bool()
    val irqSlave  = out Bool()
  }

  io.i2cMaster <> master.io.i2c
  io.i2cSlave  <> slaveCtrl.io.i2c
  io.irqMaster := master.io.irq
  io.irqSlave  := slaveCtrl.io.irq

  // Register map: top-level Bmb slave ports + factories bound to the cores
  // Register-bus port geometry: this top's choice (single-beat; see ADR-0004)
  val regBusParam = BmbAccessParameter(8, 32)
    .addSources(1, BmbSourceParameter(contextWidth = 0, lengthWidth = 2))
    .toBmbParameter()
  val masterRegBus = slave(Bmb(regBusParam))
  val slaveRegBus  = slave(Bmb(regBusParam))
  val masterRegMap = BmbSlaveFactory(masterRegBus)
  val slaveRegMap  = BmbSlaveFactory(slaveRegBus)
  master.io.driveFrom(masterRegMap, 0)
  slaveCtrl.io.driveFrom(slaveRegMap, 0)

  // Note: the Bmb register ports are top-level inputs; in a real SoC the
  // fabric connects to them. Left undriven here (focus on I2C pin behaviour).
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

  val slaveCtrl = new I2cAutoSlave(I2cAutoSlave.Config(
    ramDepth     = 64,
    validAddrs   = (0 until 64).toSeq,
    support10bit = true
  ))

  val io = new Bundle {
    val i2cMaster  = spinal.lib.master(I2c())
    val i2cSlave   = spinal.lib.slave(I2c())
    val irqMaster  = out Bool()
    val irqSlave   = out Bool()
    val sclStretch = out Bool()
  }

  io.i2cMaster  <> master.io.i2c
  io.i2cSlave   <> slaveCtrl.io.i2c
  io.irqMaster  := master.io.irq
  io.irqSlave   := slaveCtrl.io.irq
  io.sclStretch := slaveCtrl.io.sclStretch

  // Register map: top-level Bmb slave ports + factories bound to the cores
  // Register-bus port geometry: this top's choice (single-beat; see ADR-0004)
  val regBusParam = BmbAccessParameter(8, 32)
    .addSources(1, BmbSourceParameter(contextWidth = 0, lengthWidth = 2))
    .toBmbParameter()
  val masterRegBus = slave(Bmb(regBusParam))
  val slaveRegBus  = slave(Bmb(regBusParam))
  val masterRegMap = BmbSlaveFactory(masterRegBus)
  val slaveRegMap  = BmbSlaveFactory(slaveRegBus)
  master.io.driveFrom(masterRegMap, 0)
  slaveCtrl.io.driveFrom(slaveRegMap, 0)

  // Note: the Bmb register ports are top-level inputs; in a real SoC the
  // fabric connects to them. Left undriven here (focus on I2C pin behaviour).
}

object I2cAutoDemo {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new I2cAutoDemo())
  }
}

object I2cAutoDemo10bitGen {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new I2cAutoDemo10bit)
  }
}
