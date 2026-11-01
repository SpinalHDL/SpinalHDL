package spinal.lib.com.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.Bmb

/**
  * Demo SoC-level wrapper: I2cAutoMaster + I2cAutoSlave.
  *
  * Each controller's register map is served over an internal Bmb slave port
  * (mapped by a [[spinal.lib.bus.bmb.BmbSlaveFactory]] inside the controller).
  * In a real SoC the fabric would connect to those ports; here we tie them off
  * so the demo focuses on I2C pin behaviour for SpinalVerilog generation.
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

  // Tie off Bmb register ports (no external fabric in this demo)
  def tieOffBmb(bus: Bmb): Unit = {
    bus.cmd.valid := False
    bus.cmd.opcode := 0
    bus.cmd.address := 0
    bus.cmd.length := 0
    bus.cmd.data := 0
    bus.cmd.mask := 0
    bus.rsp.ready := False
  }
  tieOffBmb(master.io.regBus)
  tieOffBmb(slaveCtrl.io.regBus)
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
    val i2cMaster  = spinal.lib.master(I2c())
    val i2cSlave   = spinal.lib.slave(I2c())
    val irqMaster  = out Bool()
    val irqSlave   = out Bool()
    val sclStretch = out Bool()
  }

  io.i2cMaster  <> master.io.i2c
  io.i2cSlave   <> slave.io.i2c
  io.irqMaster  := master.io.irq
  io.irqSlave   := slave.io.irq
  io.sclStretch := slave.io.sclStretch

  // Tie off Bmb register ports
  def tieOffBmb(bus: Bmb): Unit = {
    bus.cmd.valid := False
    bus.cmd.opcode := 0
    bus.cmd.address := 0
    bus.cmd.length := 0
    bus.cmd.data := 0
    bus.cmd.mask := 0
    bus.rsp.ready := False
  }
  tieOffBmb(master.io.regBus)
  tieOffBmb(slave.io.regBus)
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
