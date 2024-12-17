package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.localbus._
import spinal.lib.bus.misc.SizeMapping

case class MemBusInterface(bus: MemBus, sizeMap: SizeMapping, regPre: String = "", withSecFireWall: Boolean = false)(implicit moduleName: ClassName) extends BusIf{
  override val busDataWidth: Int = bus.c.dw
  override val busAddrWidth: Int = bus.c.aw
  override val withStrb: Boolean = false
  override def getModuleName = moduleName.name

  lazy val reg_wrerr: Bool = Reg(Bool(), init = False)
  val bus_rdata: Bits = Bits(busDataWidth bits)
  val reg_rderr: Bool = Reg(Bool(), init = False)
  val reg_rdata: Bits = Reg(Bits(busDataWidth bits), init = defaultReadBits)

  val wstrb: Bits  = null
  val wmask: Bits  = null
  val wmaskn: Bits = null

  bus.rdat := bus_rdata

  val askWrite  = bus.ce && bus.wr
  val askRead   = bus.ce && !bus.wr
  val doWrite   = askWrite.allowPruning()
  val doRead    = askRead.allowPruning()
  val writeData = bus.wdat

  override lazy val cg_en: Bool = bus.ce|| RegNext(bus.ce, False) // dleay 1 cycle for W1P clear
  override lazy val bus_nsbit: Bool = False

  initStrbMasks()

  override def readAddress()  = bus.addr
  override def writeAddress() = bus.addr

  override def readHalt()  = assert(false, "MemBus does not support halting")
  override def writeHalt() = assert(false, "MemBus does not support halting")
}
