package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba4.apb.Apb4
import spinal.lib.bus.misc.SizeMapping

case class Apb4BusInterface(bus: Apb4, sizeMap: SizeMapping, selId: Int = 0, regPre: String = "")(implicit moduleName: ClassName) extends BusIf{
  override val withStrb: Boolean = bus.c.useStrb
  override def getModuleName = moduleName.name

  val readError = Bool()
  val readData  = Bits(bus.c.dataWidth bits)
  val wstrb: Bits  = withStrb generate(Bits(strbWidth bit))
  val wmask: Bits  = withStrb generate(Bits(busDataWidth bit))
  val wmaskn: Bits = withStrb generate(Bits(busDataWidth bit))
  withStrb generate(wstrb := bus.PSTRB)

  readError.setAsReg() init False
  readData.setAsReg()  init 0

  bus.PREADY := True
  bus.PRDATA := readData

  val askWrite  = (bus.PSEL(selId) && bus.PWRITE).allowPruning()
  val askRead   = (bus.PSEL(selId) && !bus.PWRITE).allowPruning()
  val doWrite   = (askWrite && bus.PENABLE && bus.PREADY).allowPruning()
  val doRead    = (askRead  && bus.PENABLE && bus.PREADY).allowPruning()
  val writeData = bus.PWDATA

  initStrbMasks()

  if(bus.c.useSlaveError) bus.PSLVERR := readError
  override def readAddress()  = if(withStrb) (bus.PADDR.drop(underbitWidth) ## B(0, underbitWidth bit)).asUInt else bus.PADDR
  override def writeAddress() = bus.PADDR

  override def readHalt()  = bus.PREADY := False
  override def writeHalt() = bus.PREADY := False

  override def busDataWidth   = bus.c.dataWidth
}
