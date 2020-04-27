/**@todo: Support RTY signal*/
/**@todo: SUpport Burts mode (specially for memory)*/

package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib.bus.misc._

/** Factory for [[spinal.lib.bus.wishbone.WishboneSlaveFactory]] instances. */
object WishboneSlaveFactory {
  /** This is the slave facotory fot the wishbone bus
    * @param bus the wishbone bus istance that will connect with the module
    * @return an istanciated class of [[spinal.lib.bus.wishbone.WishboneSlaveFactory]]
    */
  def apply(bus: Wishbone,reg_fedback: Boolean = true) = new WishboneSlaveFactory(bus,reg_fedback)
}

/** This is the slave facotory fot the wishbone bus
  * @param bus the wishbone bus istance that will connect with the module
  * @param reg_fedback if set to false, the slave will acknoledge as soon as possible otherwise will wait the next clock cycle(default)
  */
class WishboneSlaveFactory(bus: Wishbone,reg_fedback: Boolean = true) extends BusSlaveFactoryDelayed{
  bus.DAT_MISO := 0
  if(bus.config.isPipelined) bus.STALL := False

  val askWrite = bus.isWrite.allowPruning()
  val askRead = bus.isRead.allowPruning()
  val doWrite = bus.doWrite.allowPruning()
  val doRead = bus.doRead.allowPruning()

  if(!reg_fedback){
    bus.ACK := bus.STB && bus.CYC                 //Acknoledge as fast as possible
  } else if(bus.config.isPipelined){
    val pip_reg = RegNext(bus.STB) init(False)
    bus.ACK := pip_reg || (bus.STALL && bus.CYC)  //Pipelined: Acknoledge at the next clock cycle
  } else {
    val reg_reg = RegNext(bus.STB && bus.CYC) init(False)
    bus.ACK := reg_reg && bus.STB                 //Classic: Acknoledge at the next clock cycle
  }

  val byteAddress = bus.ADR << log2Up(bus.config.dataWidth/8)

  override def readAddress()  = byteAddress
  override def writeAddress() = byteAddress

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

    switch(byteAddress) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedElements(jobs)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(byteAddress)){
        doMappedElements(jobs)
      }
    }
  }
}
