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
import spinal.lib.com.spi.{SpiMasterCtrlGenerics, Apb3SpiMasterCtrl, SpiMasterCtrlMemoryMappedConfig}


class Apb3SpiMasterCtrlTester extends SpinalTesterCocotbBase {
  override def getName: String = "Apb3SpiMasterCtrlTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/SpiTester/Apb3SpiMasterCtrlTester"
  override def createToplevel: Component = new Apb3SpiMasterCtrl(
    SpiMasterCtrlMemoryMappedConfig(
      ctrlGenerics = SpiMasterCtrlGenerics(
        ssWidth     = 4,
        timerWidth  = 12,
        dataWidth   = 8
      ),
      cmdFifoDepth = 32,
      rspFifoDepth = 32
    )
  )
}

