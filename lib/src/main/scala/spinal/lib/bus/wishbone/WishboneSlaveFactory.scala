/**@todo: Support RTY signal*/
/**@todo: SUpport Burts mode (specially for memory)*/

package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib.bus.misc._
import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

/** Factory for [[spinal.lib.bus.wishbone.WishboneSlaveFactory]] instances. */
object WishboneSlaveFactory {
  /** This is the slave factory fot the wishbone bus.
    * @param bus the wishbone bus instance that will connect with the module
    * @return an instantiated class of [[spinal.lib.bus.wishbone.WishboneSlaveFactory]]
    */
  def apply(bus: Wishbone, reg_feedback: Boolean = true, errorOnUnmapped: Boolean = true) = new WishboneSlaveFactory(bus, reg_feedback, errorOnUnmapped)
}

/** This is the slave facotory fot the wishbone bus
  * @param bus the wishbone bus istance that will connect with the module
  * @param reg_feedback if set to false, the slave will acknowledge as soon as possible otherwise will wait the next clock cycle(default)
  * @param errorOnUnmapped if set to true (default), accesses to unmapped addresses assert ERR. Requires useERR in the config.
  */
class WishboneSlaveFactory(bus: Wishbone, reg_feedback: Boolean = true, errorOnUnmapped: Boolean = true) extends BusSlaveFactoryDelayed{
  bus.DAT_MISO := 0
  if(bus.config.isPipelined) bus.STALL := False
  if(bus.config.useERR) bus.ERR := False

  val askWrite = bus.isWrite.allowPruning()
  val askRead = bus.isRead.allowPruning()
  val doWrite = bus.doWrite.allowPruning()
  val doRead = bus.doRead.allowPruning()

  if(!reg_feedback){
    bus.ACK := bus.STB && bus.CYC                 // Acknowledge as fast as possible.
  } else if(bus.config.isPipelined){
    val pip_reg = RegNext(bus.STB) init(False)
    bus.ACK := pip_reg || (bus.STALL && bus.CYC)  // Pipelined: Acknowledge at the next clock cycle.
  } else {
    val reg_reg = RegNext(bus.STB && bus.CYC) init(False)
    bus.ACK := reg_reg && bus.STB                 // Classic: Acknowledge at the next clock cycle.
  }

  val byteAddress = bus.byteAddress(AddressGranularity.WORD)

  override def readAddress()  = byteAddress
  override def writeAddress() = byteAddress

  override def readHalt() = bus.ACK := False
  override def writeHalt() = bus.ACK := False

  override def busDataWidth = bus.config.dataWidth

  override def writeByteEnable() = bus.SEL

  private val customErrors = ArrayBuffer[Bool]()

  /** Register a custom error condition on this slave.
    * When [[condition]] is True during an active transfer, the bus will respond
    * with ERR and suppress ACK, with timing matched to the configured feedback
    * mode. Requires [[bus.config.useERR]] to be set.
    * @param condition combinatorial Bool that is True when an error is present
    */
  def setError(condition: Bool): Unit = {
    require(bus.config.useERR, "setError requires useERR to be enabled in the WishboneConfig")
    customErrors += condition
  }

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

    val hit = Bool()
    hit := False

    switch(byteAddress) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          hit := True
          doMappedElements(jobs)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(byteAddress)){
        hit := True
        doMappedElements(jobs)
      }
    }

    if(bus.config.useERR) {
      val unmappedErr = if(errorOnUnmapped) !hit else { hit.allowPruning(); False }
      val errCondition = customErrors.foldLeft(unmappedErr)(_ || _)
      if(!reg_feedback) {
        when(errCondition && bus.STB && bus.CYC) {
          bus.ERR := True
          bus.ACK := False
        }
      } else if(bus.config.isPipelined) {
        val pip_err = RegNext(bus.STB && errCondition) init(False)
        when(pip_err) {
          bus.ERR := True
          bus.ACK := False
        }
      } else {
        val reg_err = RegNext(bus.STB && bus.CYC && errCondition) init(False)
        when(reg_err && bus.STB) {
          bus.ERR := True
          bus.ACK := False
        }
      }
    } else {
      hit.allowPruning()
    }
  }
}
