package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

import scala.collection.mutable.ArrayBuffer

object Axi4SlaveFactory {
  def apply(bus: Axi4) = new Axi4SlaveFactory(bus)
}

class Axi4SlaveFactory(bus: Axi4) extends BusSlaveFactoryDelayed {

  val readHaltRequest = False
  val writeHaltRequest = False


  var writeCmd = bus.writeCmd.unburstify
  val writeJoinEvent = StreamJoin.arg(writeCmd, bus.writeData)
  val writeRsp = Stream(Axi4B(bus.config))
  bus.writeRsp << writeRsp.stage()
  when(bus.writeData.last) {
    // backpressure in last beat
    writeJoinEvent.ready := writeRsp.ready && !writeHaltRequest
    writeRsp.valid := writeJoinEvent.fire
  } otherwise {
    // otherwise, stall W channel when writeHaltRequest
    writeJoinEvent.ready := !writeHaltRequest
    writeRsp.valid := False
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

  var prohibitedReadAddress,prohibitedWriteAddress=ArrayBuffer[BigInt]()
  def  prohibitRead(address  : BigInt=null)={if(address != null)prohibitedReadAddress += address}
  def  prohibitWrite(address : BigInt=null)={if(address != null)prohibitedWriteAddress += address}

  val writeOccur = writeJoinEvent.fire
  val readOccur = bus.readRsp.fire

  def maskAddress(addr : UInt) = addr & ~U(bus.config.dataWidth/8 -1, bus.config.addressWidth bits)
  val readAddressMasked = maskAddress(readDataStage.addr)
  val writeAddressMasked = maskAddress(writeCmd.addr)

  override def readAddress(): UInt  = readAddressMasked
  override def writeAddress(): UInt = writeAddressMasked

  override def writeByteEnable(): Bits = bus.writeData.strb

  override def readHalt(): Unit = readHaltRequest := True
  override def writeHalt(): Unit = writeHaltRequest := True

  override def build(): Unit = {
    super.doNonStopWrite(bus.writeData.data)

    switch(writeAddress()) {
      for ((address, jobs) <- elementsPerAddress ) address match {
        case address : SingleMapping =>
          assert(address.address % log2Up(bus.config.dataWidth/8) == 0)
          is(address.address) {
            doMappedWriteElements(jobs, writeJoinEvent.valid, writeOccur, bus.writeData.data)
            if(prohibitedWriteAddress contains address.address) {
              bus.b.resp:=Axi4.resp.SLVERR.resized //TODO  do the same for !address.isInstanceOf[SingleMapping]
            }
          }
        case _ =>
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(writeAddress())) {
        doMappedWriteElements(jobs, writeJoinEvent.valid, writeOccur, bus.writeData.data)
      }
    }


    switch(readAddress()) {
      for ((address, jobs) <- elementsPerAddress) address match {
        case address : SingleMapping =>
          assert(address.address % log2Up(bus.config.dataWidth/8) == 0)
          is(address.address) {
            doMappedReadElements(jobs, readDataStage.valid, readOccur, readRsp.data)
            if(prohibitedReadAddress contains address.address) {
              bus.r.resp:=Axi4.resp.SLVERR.resized
            }
          }

        case _ =>
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(readAddress())) {
        doMappedReadElements(jobs, readDataStage.valid, readOccur, readRsp.data)
      }
    }
  }

 override def wordAddressInc: Int = busDataWidth / 8

  override def busDataWidth: Int = bus.config.dataWidth
}
