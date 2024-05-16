package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.bus.misc.SizeMapping

case class Apb3BusInterface(bus: Apb3, sizeMap: SizeMapping, selId: Int = 0, regPre: String = "")(implicit moduleName: ClassName) extends BusIf{
  val busDataWidth: Int = bus.config.dataWidth
  val busAddrWidth: Int = bus.config.addressWidth

  val readError: Bool = Bool()
  val readData: Bits  = Bits(busDataWidth bits)
  val reg_rderr: Bool = Reg(Bool(), init = False)
  val reg_rdata: Bits = Reg(Bits(busDataWidth bits), init = defualtReadBits)

  override val withStrb: Boolean = false
  val wstrb: Bits = withStrb generate(Bits(strbWidth bit))
  val wmask: Bits = withStrb generate(Bits(busDataWidth bit))
  val wmaskn: Bits = withStrb generate(Bits(busDataWidth bit))
  initStrbMasks()

  override def getModuleName = moduleName.name

  bus.PREADY := True
  bus.PRDATA := readData

  val askWrite  = (bus.PSEL(selId) && bus.PWRITE).allowPruning()
  val askRead   = (bus.PSEL(selId) && !bus.PWRITE).allowPruning()
  val doWrite   = (askWrite && bus.PENABLE && bus.PREADY).allowPruning()
  val doRead    = (askRead  && bus.PENABLE && bus.PREADY).allowPruning()
  val writeData = bus.PWDATA

  if(bus.config.useSlaveError) bus.PSLVERROR := readError
  override def readAddress()  = bus.PADDR
  override def writeAddress() = bus.PADDR

  override def readHalt()  = bus.PREADY := False
  override def writeHalt() = bus.PREADY := False
}
