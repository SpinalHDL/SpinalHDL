package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import scala.collection.Seq

object SlaveFactory{
  def getSupported(addressWidth : Int,
                   dataWidth : Int,
                   allowBurst : Boolean,
                   proposed : M2sSupport) = proposed.copy(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
    transfers = proposed.transfers.intersect(
      allowBurst match {
        case false => M2sTransfers(
          get = SizeRange.upTo(dataWidth / 8),
          putFull = SizeRange(dataWidth / 8)
        )
        case true => M2sTransfers(
          get = SizeRange.all,
          putFull = SizeRange.downTo(dataWidth / 8)
        )
      }
    )
  )
}

class SlaveFactory(bus: Bus, allowBurst : Boolean) extends BusSlaveFactoryDelayed{
  val implementBurst = allowBurst && bus.p.beatMax > 1
  val unburstify = implementBurst generate new Area {
    val isGet = bus.a.opcode === Opcode.A.GET
    val counter = Reg(bus.p.beat) init (0)
    val last = counter === bus.a.sizeToBeatMinusOne()
    val busA = bus.a.forkSerial(!isGet || last)
    when(busA.fire) {
      counter := counter + 1
      when(bus.a.fire && (isGet || last)) {
        counter := 0
      }
    }
    busA.address.removeAssignments() := bus.a.address | (counter << log2Up(bus.p.dataBytes)).resized
    val withRsp = isGet || last
  }


  val rspAsync = cloneOf(bus.d)

  def busA = if (implementBurst) unburstify.busA else bus.a
  def withRsp = if (implementBurst) unburstify.withRsp else True


  val askWrite = (busA.valid && Opcode.A.isPut(busA.opcode)).allowPruning()
  val askRead  = (busA.valid && Opcode.A.isGet(busA.opcode)).allowPruning()
  val doWrite  = (askWrite && busA.ready).allowPruning()
  val doRead   = (askRead && busA.ready).allowPruning()

  val address = (busA.address >> bus.p.dataBytesLog2Up) << bus.p.dataBytesLog2Up
  override def readAddress()  : UInt  = address
  override def writeAddress() : UInt = address

//  override def writeByteEnable(): Bits = busA.mask

  val halt = False
  override def readHalt(): Unit = halt := True
  override def writeHalt(): Unit = halt := True

  override def build(): Unit = {
    super.doNonStopWrite(busA.data)
    busA.ready := rspAsync.ready && !halt
    rspAsync.valid := busA.valid && !halt && withRsp
    rspAsync.data := 0
    rspAsync.opcode := Opcode.A.isGet(busA.opcode).mux(Opcode.D.ACCESS_ACK_DATA(), Opcode.D.ACCESS_ACK())
    rspAsync.param := 0
    rspAsync.source := busA.source
    rspAsync.sink := 0
    rspAsync.size := busA.size
    rspAsync.corrupt := False
    rspAsync.denied := False

    bus.d << rspAsync.stage()

    def doMappedElements(jobs : Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs = jobs,
      askWrite = askWrite,
      askRead = askRead,
      doWrite = doWrite,
      doRead = doRead,
      writeData = busA.data,
      readData = rspAsync.data
    )

    switch(address) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedElements(jobs)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(this.address)){
        doMappedElements(jobs)
      }
    }
  }

  override def busDataWidth: Int = bus.p.dataWidth
}
