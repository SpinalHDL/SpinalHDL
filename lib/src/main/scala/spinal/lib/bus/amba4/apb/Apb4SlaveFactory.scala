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
package spinal.lib.bus.amba4.apb

import spinal.core._
import spinal.lib.bus.misc._

import scala.collection.Seq


class Apb4SlaveFactory(bus: Apb4, selId: Int, dontCareReadData : Boolean = false) extends BusSlaveFactoryDelayed {

  bus.PREADY := True
  if(dontCareReadData) bus.PRDATA.assignDontCare() else bus.PRDATA := 0

  val askWrite = (bus.PSEL(selId) && bus.PENABLE && bus.PWRITE).allowPruning()
  val askRead  = (bus.PSEL(selId) && bus.PENABLE && !bus.PWRITE).allowPruning()
  val doWrite  = (bus.PSEL(selId) && bus.PENABLE && bus.PREADY &&  bus.PWRITE).allowPruning()
  val doRead   = (bus.PSEL(selId) && bus.PENABLE && bus.PREADY && !bus.PWRITE).allowPruning()

  if (bus.c.useSlaveError) bus.PSLVERR := (doWrite && writeErrorFlag) || (doRead && readErrorFlag)

  override def readAddress()  = bus.PADDR
  override def writeAddress() = bus.PADDR

  override def readHalt()  = bus.PREADY := False
  override def writeHalt() = bus.PREADY := False

  override def busDataWidth   = bus.c.dataWidth
  override def wordAddressInc = busDataWidth / 8

  override def build(): Unit = {

    super.doNonStopWrite(bus.PWDATA)

    val writeData = Bits(busDataWidth bit)
    (0 until bus.c.strbWidth).foreach{ i =>
      writeData((i+1)*8-1 downto i*8) := Mux(bus.PSTRB(i), bus.PWDATA((i+1)*8-1 downto i*8), B(0, 8 bit))
    }

    def doMappedElements(jobs : Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs      = jobs,
      askWrite  = askWrite,
      askRead   = askRead,
      doWrite   = doWrite,
      doRead    = doRead,
      writeData = writeData,
      readData  = bus.PRDATA
    )

    switch(bus.PADDR) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedElements(jobs)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(bus.PADDR)){
        doMappedElements(jobs)
      }
    }
  }
}
