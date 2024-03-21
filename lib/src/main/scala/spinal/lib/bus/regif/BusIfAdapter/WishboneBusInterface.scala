package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.wishbone._

case class WishboneBusInterface(
    bus: Wishbone,
    sizeMap: SizeMapping,
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
  override val readError = Bool()
  override val readData = Bits(bus.config.dataWidth bits)
  val ack = Bool()

  if (readSync) {
    readError.setAsReg() init False
    readData.setAsReg() init 0
    ack.setAsReg() init False

    // Force ack down between bursts; avoids misread when write cycle proceeds a read cycle
    ack := bus.CYC && bus.STB && !ack
  } else {
    readError := False
    readData := 0
    ack := bus.CYC && bus.STB
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

  val byteAddress = bus.byteAddress(AddressGranularity.BYTE)
  override def readAddress() = byteAddress
  override def writeAddress() = byteAddress

  override def readHalt() = halted := True
  override def writeHalt() = halted := True

  override def busDataWidth = bus.config.dataWidth
}
