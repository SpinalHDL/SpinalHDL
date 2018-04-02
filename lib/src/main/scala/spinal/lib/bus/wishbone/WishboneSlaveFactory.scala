//TODO: Make selection logic for Syncronous/asyncronous/pipelined interface
//TODO: Support ERR signal
//TODO: Support RTY signal
//TODO: SUpport Burts mode (specially for memory)

package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib.bus.misc._

/** Factory for [[spinal.lib.bus.wishbone.WishboneSlaveFactory]] instances. */
object WishboneSlaveFactory {
  /** This is the slave facotory fot the wishbone bus
    * @param bus the wishbone bus istance that will connect with the module
    * @return an istanciated class of [[spinal.lib.bus.wishbone.WishboneSlaveFactory]]
    */
  def apply(bus: Wishbone) = new WishboneSlaveFactory(bus)
}

/** This is the slave facotory fot the wishbone bus
  * @param bus the wishbone bus istance that will connect with the module
  */
class WishboneSlaveFactory(bus: Wishbone) extends BusSlaveFactoryDelayed{
  bus.DAT_MISO := 0

  val askWrite =  if(bus.config.isPipelined)
                    (bus.CYC && bus.STB && !bus.STALL && bus.WE).allowPruning()
                  else
                    (bus.CYC && bus.STB && bus.WE).allowPruning()

  val askRead =   if(bus.config.isPipelined)
                    (bus.CYC && bus.STB && !bus.STALL && !bus.WE).allowPruning()
                  else
                    (bus.CYC && bus.STB && !bus.WE).allowPruning()

  val doWrite =   if(bus.config.isPipelined)
                    (bus.CYC && bus.STB && !bus.STALL && bus.ACK && bus.WE).allowPruning()
                  else
                    (bus.CYC && bus.STB && bus.ACK && bus.WE).allowPruning()

  val doRead =    if(bus.config.isPipelined)
                    (bus.CYC && bus.STB && !bus.STALL && bus.ACK && !bus.WE).allowPruning()
                  else
                    (bus.CYC && bus.STB && bus.ACK && !bus.WE).allowPruning()

    // val pip_feedback = RegNext(bus.STB) init(False)
    // bus.ACK := pip_feedback || (bus.STALL && bus.CYC)

    // val reg_feedback = RegNext(bus.STB && bus.CYC) init(False)
    // bus.ACK := reg_feedback && bus.STB

  if(bus.config.isPipelined){
    val pip_feedback = RegNext(bus.STB) init(False)
    bus.ACK := pip_feedback || (bus.STALL && bus.CYC)
  } else {
    val reg_feedback = RegNext(bus.STB && bus.CYC) init(False)
    bus.ACK := reg_feedback && bus.STB
  }
//   if(bus.config.isPipelined){
//     //Wishbone Pipelined
//     val askWrite = (bus.CYC && bus.STB && !bus.STALL && bus.WE).allowPruning()
//     val askRead = (bus.CYC && bus.STB && !bus.STALL && !bus.WE).allowPruning()
//     val doWrite = (bus.CYC && bus.STB && !bus.STALL && bus.ACK && bus.WE).allowPruning()
//     val doRead = (bus.CYC && bus.STB && !bus.STALL && bus.ACK && !bus.WE).allowPruning()

//     val pip_feedback = RegNext(bus.STB) init(False)
//     bus.ACK := pip_feedback || (bus.STALL && bus.CYC)
//   } else {
//     //Wishbone Classic
//     val askWrite = (bus.CYC && bus.STB && bus.WE).allowPruning()
//     val askRead = (bus.CYC && bus.STB && !bus.WE).allowPruning()
//     val doWrite = (bus.CYC && bus.STB && bus.ACK && bus.WE).allowPruning()
//     val doRead = (bus.CYC && bus.STB && bus.ACK && !bus.WE).allowPruning()

//     val reg_feedback = RegNext(bus.STB && bus.CYC) init(False)
//     bus.ACK := reg_feedback && bus.STB
// }

  //Wishbone Classic Asynchronous//
  //bus.ACK := bus.STB && bus.CYC  //
  /////////////////////////////////

  override def readAddress() = bus.ADR
  override def writeAddress() = bus.ADR

  override def readHalt() = bus.ACK := False
  override def writeHalt() = bus.ACK := False

  override def busDataWidth = bus.config.dataWidth
  override def wordAddressInc = busDataWidth / 8

  override def build(): Unit = {
    super.doNonStopWrite(bus.DAT_MOSI)

    def doMappedElements(jobs : Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs = jobs,
      askWrite = askWrite,
      askRead = askRead,
      doWrite = doWrite,
      doRead = doRead,
      writeData = bus.DAT_MOSI,
      readData = bus.DAT_MISO
    )

    switch(bus.ADR) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedElements(jobs)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(bus.ADR)){
        doMappedElements(jobs)
      }
    }
  }
}
