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
import spinal.lib.com.i2c._

class I2CMasterHALTester extends Component {

  val generic = I2CMasterHALGenerics()

  val io = new Bundle {
    val i2c    = master( I2C() )
    val config = in( I2CMasterHALConfig(generic) )
    val cmd    = slave Stream(I2CMasteHALCmd(generic))
    val rsp    = master Flow(I2CMasterHALRsp (generic))
  }

  val myMasterI2C = new I2CMasterHAL(generic)
  myMasterI2C.io.config.setFrequency(4e6)

  io <> myMasterI2C.io
}


class I2CMasterHALCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "I2CMasterHALTest"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/I2CTester/Master/HAL"
  override def createToplevel: Component = new I2CMasterHALTester
  override def backendConfig(config: SpinalConfig) : SpinalConfig = {
    config.copy(defaultClockDomainFrequency  = FixedFrequency(50e6),
                defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW))
  }
}


class I2CSlaveHALTester extends Component {

  val generic = I2CSlaveHALGenerics()

  val io = new Bundle {
    val i2c  = slave( I2C() )
    val cmd  = master  Flow ( I2CSlaveHALCmd(generic) )
    val rsp  = slave Stream ( I2CSlaveHALRsp(generic) )
  }

  val mySlave = new I2CSlaveHAL(generic)
  io <> mySlave.io
}


class I2CSlaveHALCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "I2CSlaveHALTest"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/I2CTester/Slave/HAL"
  override def createToplevel: Component = new I2CSlaveHALTester
  override def backendConfig(config: SpinalConfig) : SpinalConfig = {
    config.copy(defaultClockDomainFrequency  = FixedFrequency(50e6),
      defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW))
  }
}
