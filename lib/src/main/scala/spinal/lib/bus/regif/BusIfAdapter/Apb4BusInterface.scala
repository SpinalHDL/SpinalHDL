package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba4.apb.Apb4
import spinal.lib.bus.misc.SizeMapping

case class Apb4BusInterface(bus: Apb4, sizeMap: SizeMapping, selId: Int = 0, regPre: String = "")(implicit moduleName: ClassName) extends BusIf{
  override val withStrb: Boolean = bus.c.useStrb
  override val busDataWidth: Int = bus.c.dataWidth
  override val busAddrWidth: Int = bus.c.addressWidth
  override def getModuleName = moduleName.name

  val bus_rderr: Bool = Bool()
  val bus_rdata: Bits  = Bits(busDataWidth bits)
  val reg_rderr: Bool = Reg(Bool(), init = False)
  val reg_rdata: Bits = Reg(Bits(busDataWidth bits), init = defualtReadBits)

  val wstrb: Bits  = withStrb generate(Bits(strbWidth bit))
  val wmask: Bits  = withStrb generate(Bits(busDataWidth bit))
  val wmaskn: Bits = withStrb generate(Bits(busDataWidth bit))
  withStrb generate(wstrb := bus.PSTRB)

  bus.PREADY := True
  bus.PRDATA := bus_rdata
  if(bus.c.useSlaveError) bus.PSLVERR := bus_rderr

  val askWrite  = (bus.PSEL(selId) && bus.PWRITE).allowPruning()
  val askRead   = (bus.PSEL(selId) && !bus.PWRITE).allowPruning()
  val doWrite   = (askWrite && bus.PENABLE && bus.PREADY).allowPruning()
  val doRead    = (askRead  && bus.PENABLE && bus.PREADY).allowPruning()
  val writeData = bus.PWDATA
  override val cg_en: Bool = bus.PSEL(selId)

  initStrbMasks()

  override def readAddress()  = if(withStrb) (bus.PADDR.drop(underbitWidth) ## B(0, underbitWidth bit)).asUInt else bus.PADDR
  override def writeAddress() = bus.PADDR

  override def readHalt()  = bus.PREADY := False
  override def writeHalt() = bus.PREADY := False
}
