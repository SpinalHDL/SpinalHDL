package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import scala.collection.Seq

object SlaveFactory{
  def getSupported(addressWidth : Int,
                   dataWidth : Int,
                   proposed : M2sSupport) = proposed.copy(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
    transfers = proposed.transfers.intersect(
      M2sTransfers(
        get = SizeRange.upTo(dataWidth/8),
        putFull = SizeRange(dataWidth/8)
      )
    )
  )
}

class SlaveFactory(bus: Bus) extends BusSlaveFactoryDelayed{

  val rspAsync = cloneOf(bus.d)

  val askWrite = (bus.a.valid && Opcode.A.isPut(bus.a.opcode)).allowPruning()
  val askRead  = (bus.a.valid && Opcode.A.isGet(bus.a.opcode)).allowPruning()
  val doWrite  = (askWrite && bus.a.ready).allowPruning()
  val doRead   = (askRead && bus.a.ready).allowPruning()


  override def readAddress() : UInt = bus.a.address
  override def writeAddress() : UInt = bus.a.address

//  override def writeByteEnable(): Bits = bus.a.mask

  val halt = False
  override def readHalt(): Unit = halt := True
  override def writeHalt(): Unit = halt := True

  override def build(): Unit = {
    super.doNonStopWrite(bus.a.data)
    bus.a.ready := rspAsync.ready && !halt
    rspAsync.valid := bus.a.valid && !halt
    rspAsync.data := 0
    rspAsync.opcode := Opcode.A.isGet(bus.a.opcode).mux(Opcode.D.ACCESS_ACK_DATA(), Opcode.D.ACCESS_ACK())
    rspAsync.param := 0
    rspAsync.source := bus.a.source
    rspAsync.sink := 0
    rspAsync.size := bus.a.size
    rspAsync.corrupt := False
    rspAsync.denied := False

    bus.d << rspAsync.stage()

    def doMappedElements(jobs : Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs = jobs,
      askWrite = askWrite,
      askRead = askRead,
      doWrite = doWrite,
      doRead = doRead,
      writeData = bus.a.data,
      readData = rspAsync.data
    )

    switch(bus.a.address) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedElements(jobs)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(bus.a.address)){
        doMappedElements(jobs)
      }
    }
  }

  override def busDataWidth: Int = bus.p.dataWidth
}
