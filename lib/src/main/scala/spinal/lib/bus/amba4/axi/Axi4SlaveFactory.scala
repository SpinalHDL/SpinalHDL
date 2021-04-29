package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

class Axi4SlaveFactory(bus: Axi4) extends BusSlaveFactoryDelayed {

  val readHaltRequest = False
  val writeHaltRequest = False

  var writeCmd = bus.writeCmd.unburstify
  val writeJoinEvent = StreamJoin.arg(writeCmd, bus.writeData)
  val writeRsp = Axi4B(bus.config)
  bus.writeRsp.payload := writeRsp
  when(bus.writeData.last) {
    // backpressure in last beat
    writeJoinEvent.ready := bus.writeRsp.ready
    bus.writeRsp.valid := writeJoinEvent.fire
  } otherwise {
    // otherwise, stall W channel when writeHaltRequest
    writeJoinEvent.ready := !writeHaltRequest
    bus.writeRsp.valid := False
  }

  val readCmd = bus.ar.unburstify
  var readDataStage = readCmd.stage()
  val readRsp = Axi4R(bus.config)
  bus.readRsp << readDataStage.haltWhen(readHaltRequest).translateWith(readRsp)

  // only one outstanding request is supported
  writeRsp.setOKAY()
  writeRsp.id := writeCmd.id
  readRsp.setOKAY()
  readRsp.data := 0
  readRsp.last := readDataStage.last
  readRsp.id := readDataStage.id

  val writeOccur = writeJoinEvent.fire
  val readOccur = bus.readRsp.fire

  override def readAddress(): UInt = readDataStage.addr
  override def writeAddress(): UInt = writeCmd.addr

  override def writeByteEnable(): Bits = bus.writeData.strb

  override def readHalt(): Unit = readHaltRequest := True
  override def writeHalt(): Unit = writeHaltRequest := True

  override def build(): Unit = {
    super.doNonStopWrite(bus.writeData.data)

    switch(writeCmd.addr) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedWriteElements(jobs, writeJoinEvent.valid, writeOccur, bus.writeData.data)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(writeCmd.addr)) {
        doMappedWriteElements(jobs, writeJoinEvent.valid, writeOccur, bus.writeData.data)
      }
    }


    switch(readDataStage.addr) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedReadElements(jobs, readDataStage.valid, readOccur, readRsp.data)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(readDataStage.addr)) {
        doMappedReadElements(jobs, readDataStage.valid, readOccur, readRsp.data)
      }
    }
  }

  override def wordAddressInc: Int = busDataWidth / 8

  override def busDataWidth: Int = bus.config.dataWidth
}
