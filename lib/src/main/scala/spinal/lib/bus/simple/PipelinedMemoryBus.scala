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


case class PipelinedMemoryBusConfig(dataWidth: Int, addressWidth: Int)


case class PipelinedMemoryBusCmd(config: PipelinedMemoryBusConfig) extends Bundle {
  val write    = Bool
  val address  = UInt(config.addressWidth bits)
  val data     = Bits(config.dataWidth bits)
  val maskByte = Bits(scala.math.ceil(config.dataWidth / 8.0).toInt bits)
}


case class PipelinedMemoryBusRsp(config: PipelinedMemoryBusConfig) extends Bundle {
  val data = Bits(config.dataWidth bits)
}


case class PipelinedMemoryBus(config: PipelinedMemoryBusConfig) extends Bundle with IMasterSlave {

  val cmd = Stream(PipelinedMemoryBusCmd(config))
  val rsp = Flow(PipelinedMemoryBusRsp(config))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}