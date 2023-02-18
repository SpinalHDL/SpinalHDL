package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.wishbone.Wishbone

case class WishboneBusInterface(
    bus: Wishbone,
    sizeMap: SizeMapping,
    selId: Int = 0,
    override val readSync: Boolean = true,
    regPre: String = ""
)(implicit moduleName: ClassName)
    extends BusIf {
  override val withStrb: Boolean = false
  val wstrb: Bits  = withStrb generate (Bits(strbWidth bit))
  val wmask: Bits  = withStrb generate (Bits(busDataWidth bit))
  val wmaskn: Bits = withStrb generate (Bits(busDataWidth bit))
  initStrbMasks()

  override def getModuleName = moduleName.name

  val readError = Bool()
  val readData = Bits(bus.config.dataWidth bits)

  if (readSync) {
    readError.setAsReg() init False
    readData.setAsReg() init 0
  } else {
    readError := False
    readData := 0
  }

  bus.ACK := True
  bus.DAT_MISO := readData

  val selMatch = if (bus.config.useSEL) bus.SEL(selId) else True
  val askWrite = (selMatch && bus.CYC && bus.STB && bus.WE).allowPruning()
  val askRead = (selMatch && bus.CYC && bus.STB && !bus.WE).allowPruning()
  val doWrite =
    (selMatch && bus.CYC && bus.STB && bus.ACK && bus.WE).allowPruning()
  val doRead =
    (selMatch && bus.CYC && bus.STB && bus.ACK && !bus.WE).allowPruning()
  val writeData = bus.DAT_MOSI

  if (bus.config.useERR) bus.ERR := readError
  override def readAddress() = bus.ADR
  override def writeAddress() = bus.ADR

  override def readHalt() = bus.ACK := False
  override def writeHalt() = bus.ACK := False

  override def busDataWidth = bus.config.dataWidth
}
