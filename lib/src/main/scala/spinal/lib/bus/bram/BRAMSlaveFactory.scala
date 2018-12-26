/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Lib                          **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/  MIT Licence                      **
**                                                                           **
** Permission is hereby granted, free of charge, to any person obtaining a   **
** copy of this software and associated documentation files (the "Software"),**
** to deal in the Software without restriction, including without limitation **
** the rights to use, copy, modify, merge, publish, distribute, sublicense,  **
** and/or sell copies of the Software, and to permit persons to whom the     **
** Software is furnished to do so, subject to the following conditions:      **
**                                                                           **
** The above copyright notice and this permission notice shall be included   **
** in all copies or substantial portions of the Software.                    **
**                                                                           **
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   **
** OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                **
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.    **
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY      **
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT **
** OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR  **
** THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                **
\*                                                                           */
package spinal.lib.bus.bram

import spinal.core._
import spinal.lib.bus.misc._


/**
  * BRAM bus slave factory
  * @param bus          : BRAM bus
  * @param incAddress   : Incr address (default + dataWidth / 4)
  */
class BRAMSlaveFactory(bus: BRAM, incAddress: Int = 0) extends BusSlaveFactoryDelayed{

  override def readHalt  = {}
  override def writeHalt = {}

  override def readAddress()  = bus.addr
  override def writeAddress() = bus.addr

  override def busDataWidth: Int   = bus.wrdata.getWidth
  override def wordAddressInc: Int = if(incAddress == 0) super.wordAddressInc else incAddress


  override def build(): Unit = {

    val isReading = bus.we === 0

    val doWrite    = (bus.en & !isReading).allowPruning()
    val doRead     = (bus.en & isReading).allowPruning()
    val doReadNext = RegNext(doRead, False)

    val address = RegNextWhen(bus.addr, doRead)

    bus.rddata := 0


    /**
      * Read operation
      */
    switch(address){
      default{ bus.rddata := 0 }

      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          for (element <- jobs) element match {
            case element: BusSlaveFactoryRead =>
              bus.rddata(element.bitOffset, element.that.getBitsWidth bits) := element.that.asBits
              elementsOk += element
            case element: BusSlaveFactoryOnReadAtAddress if element.haltSensitive =>
              when(doReadNext){ element.doThat()}
              elementsOk += element
            case _ =>
          }
        }
      }
    }


    /**
      * Write operation
      */
    for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
      when(doWrite & address.hit(bus.addr)) {
        for (element <- jobs) element match {
          case element: BusSlaveFactoryWrite =>
            element.that.assignFromBits(bus.wrdata(element.bitOffset, element.that.getBitsWidth bits))
            elementsOk += element
          case element: BusSlaveFactoryOnWriteAtAddress if element.haltSensitive =>
            element.doThat()
            elementsOk += element
          case _ =>
        }
      }
    }
  }
}


