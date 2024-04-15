package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.bram.BRAM
import spinal.lib.Delay

case class BRAMBusInterface(bus: BRAM, sizeMap: SizeMapping, regPre: String = "", readDelay: Int = 1)(implicit moduleName: ClassName) extends BusIf {
  override val withStrb: Boolean = true
  override val wstrb: Bits = Bits(strbWidth bit)
  override val wmask: Bits = Bits(busDataWidth bit)
  override val wmaskn: Bits = Bits(busDataWidth bit)
  wstrb := bus.we
  initStrbMasks()

  override val askWrite: Bool = bus.we.orR.allowPruning()

  override val askRead: Bool = (!askWrite).allowPruning()

  override val doWrite: Bool = (askWrite && bus.en).allowPruning()

  override val doRead: Bool = (askRead && bus.en).allowPruning()

  override val readData: Bits = Bits(busDataWidth bits)

  bus.rddata := Delay(readData, readDelay) init B(0)

  override val writeData: Bits = bus.wrdata

  override val readError: Bool = False

  override def readAddress(): UInt = bus.addr << underbitWidth // BRAM uses word-aligned addresses

  override def writeAddress(): UInt = bus.addr << underbitWidth

  override def readHalt(): Unit = assert(false, "BRAM bus does not support halting")

  override def writeHalt(): Unit = assert(false, "BRAM bus does not support halting")

  override def busDataWidth: Int = bus.config.dataWidth

  override def getModuleName: String = moduleName.name
}
