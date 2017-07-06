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
import spinal.lib._
import spinal.lib.io._
import spinal.lib.com.i2c._

/*
class I2CHALTester extends Component{

    val slaveGeneric  = I2CIoSlaveGenerics()
    val masterGeneric = I2CIoMasterGenerics()

    val io = new Bundle{
      val ioSlave = new Bundle {
        val cmd  = master  Flow ( I2CIoSlaveCmd() )
        val rsp  = slave Stream ( I2CIoSlaveRsp() )
      }
      val ioMaster = new Bundle {
        val cmd    = slave Stream(I2CIoMasterCmd())
        val rsp    = master Flow (I2CIoMasterRsp ())
      }

      val sda = out Bool
      val scl = out Bool
    }

    val i2cSlave  = new I2CIoSlave(slaveGeneric)
    val i2cMaster = new I2CIoMaster(masterGeneric)

    i2cSlave.io.cmd  <> io.ioSlave.cmd
    i2cSlave.io.rsp  <> io.ioSlave.rsp
    i2cMaster.io.cmd <> io.ioMaster.cmd
    i2cMaster.io.rsp <> io.ioMaster.rsp
    i2cMaster.io.config.setTimerFrequency(1 MHz)
    i2cMaster.io.config.setSamplingFrequency(5 MHz)

    io.sda := i2cMaster.io.i2c.sda.read
    io.scl := i2cMaster.io.i2c.scl.read
    i2cSlave.io.config.setFrequencySampling(5 MHz)
    i2cSlave.io.config.setTimeoutPeriod(100 us)

    interconnect(Seq(i2cMaster.io.i2c.scl, i2cSlave.io.i2c.scl))
    interconnect(Seq(i2cMaster.io.i2c.sda, i2cSlave.io.i2c.sda))

    def interconnect(elements : Seq[ReadableOpenDrain[Bool]]) : Unit = {
      val readValue = elements.map(_.write).reduce(_ & _)
      elements.foreach(_.read := readValue)
    }
}

class I2CHALCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "I2CHALTest"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/I2CTester/HAL"
  override def createToplevel: Component = new I2CHALTester
  override def backendConfig(config: SpinalConfig) : SpinalConfig = {
    config.copy(defaultClockDomainFrequency  = FixedFrequency(50 MHz),
      defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW))
  }
}*/

