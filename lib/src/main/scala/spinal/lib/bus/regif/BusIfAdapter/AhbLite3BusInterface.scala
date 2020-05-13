package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba3.ahblite.AhbLite3
import spinal.lib.bus.misc.SizeMapping

case class AhbLite3BusInterface(bus: AhbLite3, sizeMap: SizeMapping, readSync: Boolean = true, regPre: String = "")(implicit moduleName: ClassName)  extends BusIf{

  override def getModuleName = moduleName.name

  val readError = Bool()
  val readData  = Bits(bus.config.dataWidth bits)

  val writeData: Bits  = bus.HWDATA

  if(readSync) {
    readError.setAsReg() init False
    readData.setAsReg()  init 0
  } else {
    readError := False
    readData  := 0
  }

  val askWrite    = bus.HSEL & bus.HTRANS(1) === True &  bus.HWRITE
  val askRead     = bus.HSEL & bus.HTRANS(1) === True & !bus.HWRITE
  val doWrite     = bus.HREADY & RegNext(askWrite, False)
  val doRead      = bus.HREADY & askRead

  val addressDelay = RegNextWhen(bus.HADDR, askRead | askWrite)
  bus.HREADYOUT := True
  bus.HRESP     := False
  bus.HRDATA    := readData

  def readAddress():UInt   = bus.HADDR
  def writeAddress(): UInt = addressDelay

  def readHalt(): Unit  = bus.HREADY === False
  def writeHalt(): Unit = bus.HREADY === False

  def busDataWidth: Int = bus.config.dataWidth
}
