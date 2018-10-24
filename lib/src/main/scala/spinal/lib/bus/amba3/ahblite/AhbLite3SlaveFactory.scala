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
package spinal.lib.bus.amba3.ahblite

import spinal.core._
import spinal.lib.bus.misc.{BusSlaveFactoryDelayed, BusSlaveFactoryElement, SingleMapping}



class AhbLite3SlaveFactory(bus: AhbLite3, incAddress: Int = 0) extends BusSlaveFactoryDelayed{

  override def readHalt  = {}
  override def writeHalt = {}

  override def readAddress()  = bus.HADDR
  override def writeAddress() = bus.HADDR

  override def busDataWidth: Int   = bus.HWDATA.getWidth
  override def wordAddressInc: Int = if(incAddress == 0) super.wordAddressInc else incAddress


  override def build(): Unit = {

    val askWrite    = bus.HSEL & bus.HTRANS === 2 & bus.HWRITE
    val askRead     = bus.HSEL & bus.HTRANS === 2 & !bus.HWRITE
    val doWrite     = RegNext(askWrite, False )
    val doRead      = RegNext(askRead, False )

    val addressDelay = RegNextWhen(bus.HADDR, askRead | askWrite)

    bus.HREADYOUT := True
    bus.HRESP     := False
    bus.HRDATA    := 0

    def doMappedElements(jobs: Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs      = jobs,
      askWrite  = askWrite,
      askRead   = askRead,
      doWrite   = doWrite,
      doRead    = doRead,
      writeData = bus.HWDATA,
      readData  = bus.HRDATA
    )

    /** Read/Write operation */
    switch(addressDelay){
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address){
          doMappedElements(jobs)
        }
      }
    }
  }
}
