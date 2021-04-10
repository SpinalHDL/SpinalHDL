package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactoryDelayed, BusSlaveFactoryElement, SingleMapping}

object BmbSlaveFactory{
  def getBmbCapabilities(accessSource : BmbAccessCapabilities,
                         addressWidth : Int,
                         dataWidth : Int) = accessSource.copy(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
    lengthWidthMax = log2Up(dataWidth/8),
    alignment = BmbParameter.BurstAlignement.LENGTH
  )
}

case class BmbSlaveFactory(bus: Bmb) extends BusSlaveFactoryDelayed{
  val readHaltTrigger, writeHaltTrigger = False
  val rsp = Stream(Fragment(BmbRsp(bus.p)))
  bus.rsp << rsp.haltWhen(readHaltTrigger || writeHaltTrigger).stage()


  val askWrite = (bus.cmd.valid && bus.cmd.isWrite).allowPruning()
  val askRead  = (bus.cmd.valid && bus.cmd.isRead).allowPruning()
  val doWrite  = (bus.cmd.fire && bus.cmd.isWrite).allowPruning()
  val doRead   = (bus.cmd.fire && bus.cmd.isRead).allowPruning()

  rsp.arbitrationFrom(bus.cmd)
  rsp.last := True
  rsp.setSuccess()
  rsp.data := 0
  rsp.context := bus.cmd.context
  rsp.source := bus.cmd.source

  override def readAddress() : UInt = bus.cmd.address
  override def writeAddress() : UInt = bus.cmd.address

  override def writeByteEnable(): Bits = bus.cmd.mask

  override def readHalt(): Unit = readHaltTrigger := True
  override def writeHalt(): Unit = writeHaltTrigger := True

  override def build(): Unit = {
    super.doNonStopWrite(bus.cmd.data)

    def doMappedElements(jobs : Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs = jobs,
      askWrite = askWrite,
      askRead = askRead,
      doWrite = doWrite,
      doRead = doRead,
      writeData = bus.cmd.data,
      readData = rsp.data
    )

    switch(bus.cmd.address) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedElements(jobs)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(bus.cmd.address)){
        doMappedElements(jobs)
      }
    }
  }

  override def busDataWidth: Int = bus.p.access.dataWidth
}
