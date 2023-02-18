/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package spinal.tester.scalatest

import spinal.core._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.com.i2c._


class Apb3I2cSlaveTester extends SpinalTesterCocotbBase {
  override def getName: String = "Apb3I2cSlaveTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/I2CTester2/Apb3I2cSlaveTester"
  override def createToplevel: Component = new Apb3I2cCtrl(
    I2cSlaveMemoryMappedGenerics(
      ctrlGenerics = I2cSlaveGenerics(
        samplingWindowSize = 3,
        samplingClockDividerWidth = 10 bits,
        timeoutWidth = 20 bits
      ),
      addressFilterCount = 4
    )
  ).setDefinitionName("Apb3I2cSlave")
}

object Apb3I2cSlaveTesterSpinalSim extends App {
  import spinal.core.sim._
  SimConfig.withFstWave.compile(new Apb3I2cCtrl(
    I2cSlaveMemoryMappedGenerics(
      ctrlGenerics = I2cSlaveGenerics(
        samplingWindowSize = 3,
        samplingClockDividerWidth = 10 bits,
        timeoutWidth = 20 bits
      ),
      addressFilterCount = 4,
      masterGenerics = I2cMasterMemoryMappedGenerics(
        timerWidth = 16
      )
    )
  )).doSim(seed = 42){dut =>
    val cd = dut.clockDomain
    cd.forkStimulus(10)
    val apb = Apb3Driver(dut.io.apb, cd)
    val samplingClockDivider = 0x28
    val timeout = 0x2C
    val tsuDat = 0x30
    val tLow = 0x50
    val tHigh = 0x54
    val tBuf = 0x58
    val masterStatus = 0x40
    val txData = 0x00
    val sda, scl = new OpenDrainInterconnect(cd)
    sda.addHardDriver(dut.io.i2c.sda.write)
    sda.addHardReader(dut.io.i2c.sda.read)
    scl.addHardDriver(dut.io.i2c.scl.write)
    scl.addHardReader(dut.io.i2c.scl.read)


    val softSda = sda.newSoftConnection()
//    softSda.write(false)

//    val isBusy = R[0]
//    val start = RW[4]
//    val stop = RW[5]
//    val drop = RW[6]
    cd.waitSampling(100)
    apb.write(samplingClockDivider, 5)
    apb.write(timeout, 1000)
    apb.write(tsuDat, 10)
    apb.write(tLow, 100)
    apb.write(tHigh, 100)
    apb.write(tBuf, 100)

    cd.waitSampling(10000)
    apb.write(masterStatus, 1 << 4) //start

    cd.waitSampling(1000)
    apb.write(txData, 0x300)

    cd.waitSampling(10000)
  }
}
