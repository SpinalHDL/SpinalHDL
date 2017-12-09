/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Lib                          **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.lib.bus.simple

import spinal.core._
import spinal.lib._


case class AsyncMemoryBusConfig(dataWidth: Int, addrWidth: Int)


case class AsyncMemoryBus(config: AsyncMemoryBusConfig) extends Bundle with IMasterSlave {

  val valid      = Bool
  val rwn        = Bool
  val ready      = Bool
  val address    = UInt(config.addrWidth bits)
  val writeData  = Bits(config.dataWidth bits)
  val readData   = Bits(config.dataWidth bits)

  override def asMaster(): Unit = {
    out(valid, rwn, address, writeData)
    in(readData, ready)
  }
}