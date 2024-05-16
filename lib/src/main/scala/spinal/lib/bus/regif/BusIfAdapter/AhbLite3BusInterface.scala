package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba3.ahblite.AhbLite3
import spinal.lib.bus.misc.SizeMapping

case class AhbLite3BusInterface(bus: AhbLite3, sizeMap: SizeMapping, regPre: String = "")(implicit moduleName: ClassName)  extends BusIf{
  override val withStrb: Boolean = false
  override val busDataWidth: Int = bus.config.dataWidth
  override val busAddrWidth: Int = bus.config.addressWidth

  val readError: Bool = Bool()
  val readData: Bits  = Bits(busDataWidth bits)
  val reg_rderr: Bool = Reg(Bool(), init = False)
  val reg_rdata: Bits = Reg(Bits(busDataWidth bits), init = defualtReadBits)

  val wstrb: Bits = withStrb generate (Bits(strbWidth bit))
  val wmask: Bits = withStrb generate (Bits(busDataWidth bit))
  val wmaskn: Bits = withStrb generate (Bits(busDataWidth bit))
  initStrbMasks()
  override def getModuleName = moduleName.name

  val writeData: Bits  = bus.HWDATA

  val askWrite    = bus.HSEL & bus.HTRANS(1) &  bus.HWRITE
  val askRead     = bus.HSEL & bus.HTRANS(1) & !bus.HWRITE
  val doWrite     = bus.HREADY & RegNext(askWrite, False)
  val doRead      = bus.HREADY & askRead

  val addressDelay = RegNextWhen(bus.HADDR, askRead | askWrite)

  bus.HRDATA    := readData

  readError.clearWhen(readError)
  val readError_2ndcycle = RegNext(readError) init False

  when(readError_2ndcycle){
    bus.HREADYOUT := True
    bus.HRESP     := True
  } elsewhen (readError){
    bus.HREADYOUT := False
    bus.HRESP     := True
  } otherwise {
    bus.HREADYOUT := True
    bus.HRESP     := False
  }

  def readAddress():UInt   = bus.HADDR
  def writeAddress(): UInt = addressDelay

  def readHalt(): Unit  = bus.HREADY === False
  def writeHalt(): Unit = bus.HREADY === False
}
