/**@todo: Support RTY signal*/
/**@todo: SUpport Burts mode (specially for memory)*/

package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib.bus.misc._
import scala.collection.Seq

/** Factory for [[spinal.lib.bus.wishbone.WishboneSlaveFactory]] instances. */
object WishboneSlaveFactory {
  /** This is the slave facotory fot the wishbone bus
    * @param bus the wishbone bus istance that will connect with the module
    * @return an instanciated class of [[spinal.lib.bus.wishbone.WishboneSlaveFactory]]
    */
  def apply(bus: Wishbone,reg_fedback: Boolean = true) = new WishboneSlaveFactory(bus,reg_fedback)
}

/** This is the slave facotory fot the wishbone bus
  * @param bus the wishbone bus istance that will connect with the module
  * @param reg_fedback if set to false, the slave will acknowledge as soon as possible otherwise will wait the next clock cycle(default)
  */
class WishboneSlaveFactory(bus: Wishbone,reg_fedback: Boolean = true) extends BusSlaveFactoryDelayed{

  if(bus.config.isPipelined) bus.STALL := False

  val askWrite = bus.isWrite.allowPruning()
  val askRead = bus.isRead.allowPruning()
  val doWrite = bus.doWrite.allowPruning()
  val doRead = bus.doRead.allowPruning()

  val readData = bus.DAT_MISO.clone
  readData.assignDontCare()
  if(!reg_fedback){
    bus.ACK := bus.STB && bus.CYC                 //Acknowledge as fast as possible
  } else if(bus.config.isPipelined){
    //Pipelined: Acknowledge one cycle after accepting
    val ack = RegNext(bus.isRequestAck) init(False)
    bus.ACK := ack

    // Data must be registered too; after we accept the read the address can change. This also lets us pipeline another
    // read or write
    readData.setAsReg()
  } else {
    val ack = Reg(Bool()) init(False)
    ack := bus.STB && bus.CYC && !ack

    //Classic: Acknowledge at the next clock cycle
    bus.ACK := ack

    // From the spec: 3.1.3.1 (B4)
    //  the MASTER asserts [STB_O] when it is ready to transfer data.
    //   [STB_O] remains asserted until the SLAVE asserts one of the cycle terminating signals
    //   [ACK_I], [ERR_I] or [RTY_I].
    // Since it used to be that bus.ACK := ack && bus.STB here, we can assert this for simulations.
    assert(!ack || bus.STB, "Bus dropped the STB signal prematurely")
  }
  bus.DAT_MISO := readData

  val byteAddress = bus.byteAddress(AddressGranularity.WORD)

  override def readAddress()  = byteAddress
  override def writeAddress() = byteAddress

  override def readHalt() = {
    if(bus.config.isPipelined) {
      bus.STALL := True
    }
    bus.ACK := False
  }
  override def writeHalt() = readHalt()

  override def busDataWidth = bus.config.dataWidth

  override def writeByteEnable() = bus.SEL

  override def build(): Unit = {
    super.doNonStopWrite(bus.DAT_MOSI)

    def doMappedElements(jobs : Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs = jobs,
      askWrite = askWrite,
      askRead = askRead,
      doWrite = doWrite,
      doRead = doRead,
      writeData = bus.DAT_MOSI,
      readData = readData
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
