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

