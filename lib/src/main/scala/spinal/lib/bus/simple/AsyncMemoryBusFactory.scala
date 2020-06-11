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


class AsyncMemoryBusFactory(bus: AsyncMemoryBus, incAddress: Int = 0) extends BusSlaveFactoryDelayed {

  override def readHalt  = bus.ready := False
  override def writeHalt = bus.ready := False

  override def readAddress()  = bus.address
  override def writeAddress() = bus.address

  override def busDataWidth: Int   = bus.writeData.getWidth
  override def wordAddressInc: Int = if(incAddress == 0) super.wordAddressInc else incAddress

  def build(): Unit = {

    super.doNonStopWrite(bus.writeData)
    
    val askWrite = (bus.valid & !bus.rwn).allowPruning()
    val doWrite  = (bus.valid & !bus.rwn & bus.ready).allowPruning()
    val askRead  = (bus.valid & bus.rwn).allowPruning()
    val doRead   = (bus.valid & bus.rwn & bus.ready).allowPruning()

    bus.readData := 0
    bus.ready := True

    def doMappedElements(jobs: Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs      = jobs,
      askWrite  = askWrite,
      askRead   = askRead,
      doWrite   = doWrite,
      doRead    = doRead,
      writeData = bus.writeData,
      readData  = bus.readData
    )

    switch(bus.address) {
      for((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]){
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedElements(jobs)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(bus.address)){
        doMappedElements(jobs)
      }
    }
  }
}
