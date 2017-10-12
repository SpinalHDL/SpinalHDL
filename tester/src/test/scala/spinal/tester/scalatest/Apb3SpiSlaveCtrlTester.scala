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
import spinal.lib.com.spi.{Apb3SpiSlaveCtrl, SpiSlaveCtrlGenerics, SpiSlaveCtrlMemoryMappedConfig}


class Apb3SpiSlaveCtrlTester extends SpinalTesterCocotbBase {
  override def getName: String = "Apb3SpiSlaveCtrlTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/SpiTester/Apb3SpiSlaveCtrlTester"
  override def createToplevel: Component = new Apb3SpiSlaveCtrl(
    SpiSlaveCtrlMemoryMappedConfig(
      ctrlGenerics = SpiSlaveCtrlGenerics(
        dataWidth   = 8
      ),
      rxFifoDepth = 32,
      txFifoDepth = 32
    )
  )
}

