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

  val halted = Bool()
  halted := False
  val readError = Bool()
  val readData = Bits(bus.config.dataWidth bits)
  val ack = RegNext(bus.CYC && bus.STB) init(False)

  if (readSync) {
    readError.setAsReg() init False
    readData.setAsReg() init 0
  } else {
    readError := False
    readData := 0
  }

  // TODO: Possibly assert retry if halted && STB?
  bus.ACK := ack && !halted
  bus.DAT_MISO := readData

  val askWrite = (bus.CYC && bus.STB && bus.WE).allowPruning()
  val askRead = (bus.CYC && bus.STB && !bus.WE).allowPruning()
  val doWrite =
    (bus.CYC && bus.STB && !halted && bus.WE).allowPruning()
  val doRead =
    (bus.CYC && bus.STB && !halted && !bus.WE).allowPruning()
  val writeData = bus.DAT_MOSI

  if (bus.config.useERR) bus.ERR := readError
  override def readAddress() = bus.ADR
  override def writeAddress() = bus.ADR

  override def readHalt() = halted := True
  override def writeHalt() = halted := True

  override def busDataWidth = bus.config.dataWidth
}
