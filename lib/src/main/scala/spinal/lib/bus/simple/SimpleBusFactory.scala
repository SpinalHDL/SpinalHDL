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
import spinal.lib.bus.misc._

/*
class SimpleBusFactory(bus: SimpleBus, incAddress: Int = 0) extends BusSlaveFactoryDelayed {

  override def readHalt  = bus.cmd.ready := False
  override def writeHalt = bus.cmd.ready := False

  override def readAddress()   = bus.cmd.address
  override def writeAddress() = bus.cmd.address

  override def busDataWidth: Int   = bus.cmd.data.getWidth
  override def wordAddressInc: Int = if(incAddress == 0) super.wordAddressInc else incAddress

  def build(): Unit ={

    val askWrite = bus.cmd.valid & bus.cmd.write
    val doWrite  = bus.cmd.valid & bus.cmd.write & bus.cmd.ready
    val askRead  = bus.cmd.valid & !bus.cmd.write
    val doRead   = bus.cmd.valid & !bus.cmd.write & bus.rsp.valid


    val readValue  = B(0, bus.config.dataWidth  bits)
    bus.rsp.data  := RegNext(readValue)
    bus.rsp.valid := RegNext(bus.cmd.fire) init(False)
    bus.cmd.ready := True

    def doMappedElements(jobs: Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs      = jobs,
      askWrite  = askWrite,
      askRead   = askRead,
      doWrite   = doWrite,
      doRead    = doRead,
      writeData = bus.cmd.data,
      readData  = readValue
    )

    switch(bus.cmd.address) {
      for((address, primitives) <- elementsPerAddress if address.isInstanceOf[SingleMapping]){
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedElements(primitives)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(bus.cmd.address)){
        doMappedElements(jobs)
      }
    }
  }
}
*/