package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

case class Axi4SlaveFactory(bus: Axi4, cmdPipeline: StreamPipe = StreamPipe.M2S, addrRemapFunc: UInt => UInt = null) extends BusSlaveFactoryDelayed {
  implicit class RichMultiData[T <: MultiData](b: T) {
    def mapElement[E <: Data](locator: T => E)(f: E => E): T = {
      val ret = b.clone.asInstanceOf[T].allowOverride()
      ret := b
      locator(ret) := f(locator(b))
      ret
    }
  }

  implicit class RichStreamMultiData[T <: MultiData](s: Stream[T]) {
    def mapPayloadElement[E <: Data](locator: T => E)(f: E => E): Stream[T] = {
      val ret = s.clone
      ret.translateFrom(s) { case (r, ss) =>
        r := ss.mapElement(locator)(f)
      }
    }
  }

  bus.setCompositeName(this, "bus", true)

  val readHaltRequest = False
  val writeHaltRequest = False

  val writeCmd = bus.writeCmd.unburstify.mapPayloadElement(_.addr)(remap)
  val writeCmdStage = writeCmd.pipelined(cmdPipeline)
  val writeJoinEvent = StreamJoin.arg(writeCmdStage, bus.writeData)
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

  val readCmd = bus.readCmd.unburstify.mapPayloadElement(_.addr)(remap)
  var readCmdStage = readCmd.pipelined(cmdPipeline)
  val readRsp = Axi4R(bus.config)
  bus.readRsp << readCmdStage.haltWhen(readHaltRequest).translateWith(readRsp)

  // only one outstanding request is supported
  if(bus.config.useId) writeRsp.id := writeCmdStage.id
  if (writeRsp.config.useResp) {
    when(writeErrorFlag) {
      writeRsp.setSLVERR()
    } otherwise {
      writeRsp.setOKAY()
    }
  }
  if (readRsp.config.useResp) {
    when(readErrorFlag) {
      readRsp.setSLVERR()
    } otherwise {
      readRsp.setOKAY()
    }
  }
  readRsp.data := 0
  readRsp.last := readCmdStage.last
  if(bus.config.useId) readRsp.id := readCmdStage.id

  val writeOccur = writeJoinEvent.fire
  val readOccur = bus.readRsp.fire

  def maskAddress(addr : UInt) = addr & ~U(bus.config.dataWidth/8 -1, bus.config.addressWidth bits)
  private def remap(addr: UInt) = if (addrRemapFunc != null) addrRemapFunc(addr) else addr

  def readAddressMasked = maskAddress(readCmdStage.addr)
  def writeAddressMasked = maskAddress(writeCmdStage.addr)

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
          assert(address.address % (bus.config.dataWidth/8) == 0)
          is(address.address) {
            doMappedWriteElements(jobs, writeJoinEvent.valid, writeOccur, bus.writeData.data)
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
          assert(address.address % (bus.config.dataWidth/8) == 0)
          is(address.address) {
            doMappedReadElements(jobs, readCmdStage.valid, readOccur, readRsp.data)
          }
        case _ =>
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(readAddress())) {
        doMappedReadElements(jobs, readCmdStage.valid, readOccur, readRsp.data)
      }
    }
  }

  override def wordAddressInc: Int = busDataWidth / 8

  override def busDataWidth: Int = bus.config.dataWidth
}
