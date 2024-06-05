package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.wishbone._

case class WishboneBusInterface( bus: Wishbone, sizeMap: SizeMapping, override val readSync: Boolean = true, regPre: String = "", withSecFireWall: Boolean = false)(implicit moduleName: ClassName) extends BusIf {
  override val busDataWidth: Int = bus.config.dataWidth
  override val busAddrWidth: Int = bus.config.addressWidth
  override val withStrb: Boolean = false

  val bus_rderr: Bool = Bool()
  val bus_rdata: Bits  = Bits(busDataWidth bits)
  val reg_rderr: Bool = Bool()
  val reg_rdata: Bits = Bits(busDataWidth bits)

  val wstrb: Bits  = withStrb generate (Bits(strbWidth bit))
  val wmask: Bits  = withStrb generate (Bits(busDataWidth bit))
  val wmaskn: Bits = withStrb generate (Bits(busDataWidth bit))
  initStrbMasks()

  override lazy val bus_nsbit: Bool = False
  override def getModuleName = moduleName.name

  val halted = Bool()
  halted := False

  val ack = Bool()

  if (readSync) {
    reg_rderr.setAsReg() init False
    reg_rdata.setAsReg() init 0
    ack.setAsReg() init False
    // Force ack down between bursts; avoids misread when write cycle proceeds a read cycle
    ack := bus.CYC && bus.STB && !ack
  } else {
    ack := bus.CYC && bus.STB
  }

  // TODO: Possibly assert retry if halted && STB?
  bus.ACK := ack && !halted
  bus.DAT_MISO := bus_rdata

  val askWrite = (bus.CYC && bus.STB && bus.WE).allowPruning()
  val askRead = (bus.CYC && bus.STB && !bus.WE).allowPruning()
  val doWrite =
    (bus.CYC && bus.STB && !halted && bus.WE).allowPruning()
  val doRead =
    (bus.CYC && bus.STB && !halted && !bus.WE).allowPruning()
  val writeData = bus.DAT_MOSI

  if (bus.config.useERR) bus.ERR := bus_rderr

  val byteAddress = bus.byteAddress(AddressGranularity.BYTE)
  override def readAddress() = byteAddress
  override def writeAddress() = byteAddress

  override def readHalt() = halted := True
  override def writeHalt() = halted := True
}
