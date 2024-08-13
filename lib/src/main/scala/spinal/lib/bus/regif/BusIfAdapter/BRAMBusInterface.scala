package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.bram.BRAM
import spinal.lib.Delay

case class BRAMBusInterface(bus: BRAM, sizeMap: SizeMapping, regPre: String = "", withSecFireWall: Boolean = false)(implicit moduleName: ClassName) extends BusIf {
  override val busDataWidth: Int = bus.config.dataWidth
  override val busAddrWidth: Int = bus.config.addressWidth

  lazy val reg_wrerr: Bool = Reg(Bool(), init = False)
  val bus_rdata: Bits  = Bits(busDataWidth bits)
  val reg_rderr: Bool = Reg(Bool(), init = False)
  val reg_rdata: Bits = Reg(Bits(busDataWidth bits), init = defualtReadBits)

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

  override lazy val cg_en: Bool = bus.en || RegNext(bus.en, False) // dleay 1 cycle for W1P clear
  override lazy val bus_nsbit: Bool = False
//  override val readData: Bits = Bits(busDataWidth bits)

  if(bus.config.readLatency == 1 ){
    bus.rddata := bus_rdata
  } else {
    bus.rddata := Delay(bus_rdata, bus.config.readLatency - 1) init B(0)
  }

  override val writeData: Bits = bus.wrdata

  override def readAddress(): UInt = bus.addr << underbitWidth // BRAM uses word-aligned addresses

  override def writeAddress(): UInt = bus.addr << underbitWidth

  override def readHalt(): Unit = assert(false, "BRAM bus does not support halting")

  override def writeHalt(): Unit = assert(false, "BRAM bus does not support halting")

  override def getModuleName: String = moduleName.name
}
