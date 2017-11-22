package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib.bus.misc._

object Apb3SlaveFactory {
  def apply(bus: Apb3, selId: Int = 0) = new Apb3SlaveFactory(bus, selId)
}

class Apb3SlaveFactory(bus: Apb3, selId: Int) extends BusSlaveFactoryDelayed{
  bus.PREADY := True
  bus.PRDATA := 0
  if(bus.config.useSlaveError) bus.PSLVERROR := False

  val askWrite = bus.PSEL(selId) && bus.PENABLE && bus.PWRITE
  val askRead = bus.PSEL(selId) && bus.PENABLE && !bus.PWRITE
  val doWrite = bus.PSEL(selId) && bus.PENABLE && bus.PREADY &&  bus.PWRITE
  val doRead  = bus.PSEL(selId) && bus.PENABLE && bus.PREADY && !bus.PWRITE


  def readAddress() : UInt = bus.PADDR
  def writeAddress() : UInt = bus.PADDR

  override def readHalt(): Unit = bus.PREADY := False
  override def writeHalt(): Unit = bus.PREADY := False

  override def build(): Unit = {
    super.doNonStopWrite(bus.PWDATA)

    def doMappedElements(jobs : Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs = jobs,
      askWrite = askWrite,
      askRead = askRead,
      doWrite = doWrite,
      doRead = doRead,
      writeData = bus.PWDATA,
      readData = bus.PRDATA
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

  override def busDataWidth: Int = bus.config.dataWidth

  override def wordAddressInc: Int = busDataWidth / 8
}
