package spinal.lib.cpu.riscv.debug

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import scala.collection.Seq

object DebugUpdateOp extends SpinalEnum(binarySequential){
  val NOP, READ, WRITE, RESERVED = newElement()
}

object DebugCaptureOp extends SpinalEnum(binarySequential){
  val SUCCESS, RESERVED, FAILED, OVERRUN = newElement()
}

case class DebugUpdate(addressWidth : Int) extends Bundle{
  val op = DebugUpdateOp()
  val data = Bits(32 bits)
  val address = UInt(addressWidth bits)
}

case class DebugCapture(addressWidth : Int) extends Bundle{
  val op = DebugCaptureOp()
  val data = Bits(32 bits)
  val padding = UInt(addressWidth bits)
}

case class DebugCmd(addressWidth : Int) extends Bundle{
  val write = Bool()
  val data = Bits(32 bits)
  val address = UInt(addressWidth bits)
}

case class DebugRsp() extends Bundle{
  val error = Bool()
  val data = Bits(32 bits)
}


case class DebugBus(addressWidth : Int) extends Bundle with IMasterSlave {
  val cmd = Stream(DebugCmd(addressWidth))
  val rsp = Flow(DebugRsp())

  override def asMaster() = {
    master(cmd)
    slave(rsp)
  }
}


class DebugBusSlaveFactory(bus: DebugBus) extends BusSlaveFactoryDelayed{
  bus.cmd.ready := True

  val cmdToRsp = Flow(DebugRsp())
  val rspBuffer = cmdToRsp.stage()

  val askWrite = (bus.cmd.valid && bus.cmd.write).allowPruning()
  val askRead  = (bus.cmd.valid && !bus.cmd.write).allowPruning()
  val doWrite  = (askWrite && bus.cmd.ready).allowPruning()
  val doRead   = (askRead && bus.cmd.ready).allowPruning()
//  val forceError = False

//  def error() = forceError := True

  bus.rsp.valid := rspBuffer.valid
  bus.rsp.payload  := rspBuffer.payload

  cmdToRsp.valid := bus.cmd.fire
  cmdToRsp.error := False //Can be turned to true in order to see if a given access isn't implemented
  cmdToRsp.data := 0

  override def readAddress() : UInt = bus.cmd.address
  override def writeAddress() : UInt = bus.cmd.address

  override def readHalt(): Unit = bus.cmd.ready := False
  override def writeHalt(): Unit = bus.cmd.ready := False

  override def build(): Unit = {
    super.doNonStopWrite(bus.cmd.data)

    def doMappedElements(jobs : Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs = jobs,
      askWrite = askWrite,
      askRead = askRead,
      doWrite = doWrite,
      doRead = doRead,
      writeData = bus.cmd.data,
      readData = cmdToRsp.data
    )

    switch(bus.cmd.address) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          cmdToRsp.error := False
          doMappedElements(jobs)
        }
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(bus.cmd.address)){
        cmdToRsp.error := False
        doMappedElements(jobs)
      }
    }

//    cmdToRsp.error setWhen(forceError)
  }

  override def busDataWidth: Int = 32
  override def wordAddressInc: Int = 1
}
