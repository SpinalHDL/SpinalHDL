//TODO: Make selection logic for Syncronous/asyncronous/pipelined interface
//TODO: Support ERR signal
//TODO: Support RTY signal
//TODO: Create Testbench with SpinalSim
//TODO: SUpport Burts mode (specially for memory)

package spinal.lib.bus.wishbone

import spinal.core._
import spinal.lib.bus.misc._

object WishboneSlaveFactory {
  def apply(bus: Wishbone) = new WishboneSlaveFactory(bus)
}

class WishboneSlaveFactory(bus: Wishbone) extends BusSlaveFactoryDelayed{

  //bus.ACK := False
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
                      (bus.CYC && bus.STB && !bus.STALL && bus.ACK && !bus.WE).allowPruning()

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

  //val readDATA = Stream(Bits(bus.getConfig.dataWidth bits))
  //readDATA.io.payload := bus.DATA_MISO
  //readDATA.io.valid := bus.STB
  //readDATA.io.ready := bus.ACK

  //val writeDATA = Stream(Bits(bus.getConfig.dataWidth bits))
  //writeDATA.payload := bus.DATA_MOSI
  //writeDATA.valid := bus.STB
  //writeDATA.ready := bus.ACK

  //val DAT_MISOBundle = new Bundle {
  //  val DAT = bus.DAT_MISO
  //  val TGD = bus.TGD_MISO
  //}

  //val DAT_MOSIBundle = new Bundle {
  //  val DAT = bus.DAT_MOSI
  //  val TGD = bus.TGD_MOSI
  //}

  //val ADRBundle = new Bundle {
  //  val ADR = bus.ADR
  //  val TGA = bus.TGA
  //}

  //val CYCBundle = new Bundle {
  //  val CYC = bus.CYC
  //  val TGC = bus.TGC
  //}


  //val doWrite = bus.CYC && bus.STB &&  bus.WE
  //val doRead  = bus.CYC && bus.STB && !bus.WE

 ///* override def build(): Unit = {

  //  for(element <- elements) element match {
  //    case element: BusSlaveFactoryNonStopWrite => element.that.assignFromBits(bus.DAT_MISO(element.bitOffset, element.that.getBitsWidth bits))
  //    case _ =>
  //  }

  //  for((address, jobs) <- elementsPerAddress){
  //    when(bus.ADR === address){
  //      when(doWrite){
  //        for(element <- jobs) element match{
  //          case element: BusSlaveFactoryWrite   => element.that.assignFromBits(bus.DAT_MISO(element.bitOffset, element.that.getBitsWidth bits))
  //          case element: BusSlaveFactoryOnWriteAtAddress => element.doThat()
  //          case _ =>
  //        }
  //      }
  //      when(doRead){
  //        for(element <- jobs) element match{
  //          case element: BusSlaveFactoryRead   => bus.DAT_MOSI(element.bitOffset, element.that.getBitsWidth bits) := element.that.asBits
  //          case element: BusSlaveFactoryOnReadAtAddress => element.doThat()
  //          case _ =>
  //        }
  //      }
  //    }
  //  }

  //  when(doWrite){
  //    for(jobs <- elements) jobs match{
  //      case element: BusSlaveFactoryOnWriteAnyAddress => element.doThat()
  //      case _ =>
  //    }
  //  }

  //  when(doRead){
  //    for(jobs <- elements) jobs match{
  //      case element: BusSlaveFactoryOnReadAnyAddress => element.doThat()
  //      case _ =>
  //    }
  //  }

  //}*/

  //override def busDataWidth: Int = bus.getConfig.dataWidth

  //override def wordAddressInc: Int = busDataWidth / 8
}
