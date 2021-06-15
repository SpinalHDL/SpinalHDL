package spinal.lib.bus.avalon

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

object AvalonMMSlaveFactory{
  def getAvalonConfig(addressWidth: Int,
                      dataWidth: Int) = {
    AvalonMMConfig.pipelined(
      addressWidth = addressWidth,
      dataWidth = dataWidth
    ).copy(
      useByteEnable = false,
      useWaitRequestn = true
    )
  }

  def apply(bus: AvalonMM) = new AvalonMMSlaveFactory(bus)
}


class AvalonMMSlaveFactory(bus: AvalonMM) extends BusSlaveFactoryDelayed{
  assert(bus.config == AvalonMMSlaveFactory.getAvalonConfig(bus.config.addressWidth, bus.config.dataWidth))

  bus.waitRequestn := True

  val readAtCmd = Flow(Bits(bus.config.dataWidth bits))
  val readAtRsp = readAtCmd.stage()

  val askWrite = (bus.write).allowPruning()
  val askRead  = (bus.read).allowPruning()
  val doWrite  = (bus.waitRequestn &&  bus.write).allowPruning()
  val doRead   = (bus.waitRequestn &&  bus.read).allowPruning()

  bus.readDataValid := readAtRsp.valid
  bus.readData := readAtRsp.payload

  readAtCmd.valid := doRead
  readAtCmd.payload := 0

  override def readAddress() : UInt = bus.address
  override def writeAddress() : UInt = bus.address

  override def writeByteEnable(): Bits = bus.byteEnable

  override def readHalt(): Unit = bus.waitRequestn := False
  override def writeHalt(): Unit = bus.waitRequestn := False

  override def build(): Unit = {
    super.doNonStopWrite(bus.writeData)

    def doMappedElements(jobs : Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs = jobs,
      askWrite = askWrite,
      askRead = askRead,
      doWrite = doWrite,
      doRead = doRead,
      writeData = bus.writeData,
      readData = readAtCmd.payload
    )

    switch(bus.address) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
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

  override def busDataWidth: Int = bus.config.dataWidth

  override def wordAddressInc: Int = bus.config.addressUnits match {
    case WORDS   => 1
    case SYMBOLS => busDataWidth / 8
  }
}
