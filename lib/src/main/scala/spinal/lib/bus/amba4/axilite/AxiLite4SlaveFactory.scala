package spinal.lib.bus.amba4.axilite

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

object AxiLite4SlaveFactory {
  def apply(bus: AxiLite4) = new AxiLite4SlaveFactory(bus)
}

class AxiLite4SlaveFactory(bus : AxiLite4, useWriteStrobes : Boolean = false) extends BusSlaveFactoryDelayed{

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

  override def writeByteEnable() : Bits = useWriteStrobes generate(bus.writeData.strb)

  def maskAddress(addr : UInt) = addr & ~U(bus.config.dataWidth/8 -1, bus.config.addressWidth bits)
  val readAddressMasked  = maskAddress(readDataStage.addr)
  val writeAddressMasked = maskAddress(bus.writeCmd.addr)

  override def readAddress(): UInt  = readAddressMasked
  override def writeAddress(): UInt = writeAddressMasked


  override def readHalt(): Unit = readHaltRequest := True
  override def writeHalt(): Unit = writeHaltRequest := True

  val writeOccur = writeJoinEvent.fire
  val readOccur = bus.readRsp.fire

  override def build(): Unit = {
    super.doNonStopWrite(bus.writeData.data)

    switch(writeAddress()) {
      for ((address, jobs) <- elementsPerAddress)address match {
        case address : SingleMapping =>
          assert(address.address % (bus.config.dataWidth/8) == 0)
          is(address.address) {
            doMappedWriteElements(jobs, writeJoinEvent.valid, writeOccur, bus.writeData.data)
          }
        case _ =>
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(writeAddress())){
        doMappedWriteElements(jobs,writeJoinEvent.valid, writeOccur, bus.writeData.data)
      }
    }


    switch(readAddress()) {
      for ((address, jobs) <- elementsPerAddress) address match {
        case address : SingleMapping =>
          assert(address.address % (bus.config.dataWidth/8) == 0)
          is(address.address) {
            doMappedReadElements(jobs, readDataStage.valid, readOccur, readRsp.data)
          }
        case _ =>
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(readAddress())){
        doMappedReadElements(jobs,readDataStage.valid, readOccur, readRsp.data)
      }
    }
  }

  override def busDataWidth: Int = bus.config.dataWidth

  override def wordAddressInc: Int = busDataWidth / 8
}
