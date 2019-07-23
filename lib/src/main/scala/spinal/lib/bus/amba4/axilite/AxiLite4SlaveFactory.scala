package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._


class AxiLite4SlaveFactory(bus : AxiLite4) extends BusSlaveFactoryDelayed{

  val readHaltRequest = False
  val writeHaltRequest = False

  val writeJoinEvent = StreamJoin.arg(bus.writeCmd,bus.writeData)
  val writeRsp = AxiLite4B(bus.config)
  bus.writeRsp <-< writeJoinEvent.translateWith(writeRsp).haltWhen(writeHaltRequest)

  val readDataStage = bus.readCmd.stage()
  val readRsp = AxiLite4R(bus.config)
  bus.readRsp << readDataStage.haltWhen(readHaltRequest).translateWith(readRsp)


  writeRsp.setOKAY()
  readRsp.setOKAY()
  readRsp.data := 0

  def readAddress() : UInt = readDataStage.addr
  def writeAddress() : UInt = bus.writeCmd.addr

  override def readHalt(): Unit = readHaltRequest := True
  override def writeHalt(): Unit = writeHaltRequest := True

  val writeOccur = writeJoinEvent.fire
  val readOccur = bus.readRsp.fire

  override def build(): Unit = {
    super.doNonStopWrite(bus.writeData.data)

    switch(bus.writeCmd.addr) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedWriteElements(jobs,writeJoinEvent.valid, writeOccur, bus.writeData.data)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(bus.writeCmd.addr)){
        doMappedWriteElements(jobs,writeJoinEvent.valid, writeOccur, bus.writeData.data)
      }
    }


    switch(readDataStage.addr) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedReadElements(jobs,readDataStage.valid, readOccur, readRsp.data)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(readDataStage.addr)){
        doMappedReadElements(jobs,readDataStage.valid, readOccur, readRsp.data)
      }
    }
  }

  override def busDataWidth: Int = bus.config.dataWidth

  override def wordAddressInc: Int = busDataWidth / 8
}
