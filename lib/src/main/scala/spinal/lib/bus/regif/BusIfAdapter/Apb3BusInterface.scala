package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.bus.misc.SizeMapping

case class Apb3BusInterface(bus: Apb3, sizeMap: SizeMapping, selId: Int = 0, readSync: Boolean = true, regPre: String = "")(implicit moduleName: ClassName) extends BusIf{

  override def getModuleName = moduleName.name

  val readError = Bool()
  val readData  = Bits(bus.config.dataWidth bits)

  if(readSync) {
    readError.setAsReg() init False
    readData.setAsReg()  init 0
  } else {
    readError := False
    readData  := 0
  }

  bus.PREADY := True
  bus.PRDATA := readData
  if(bus.config.useSlaveError) bus.PSLVERROR := readError

  val askWrite  = (bus.PSEL(selId) && bus.PENABLE && bus.PWRITE).allowPruning()
  val askRead   = (bus.PSEL(selId) && bus.PENABLE && !bus.PWRITE).allowPruning()
  val doWrite   = (bus.PSEL(selId) && bus.PENABLE && bus.PREADY &&  bus.PWRITE).allowPruning()
  val doRead    = (bus.PSEL(selId) && bus.PENABLE && bus.PREADY && !bus.PWRITE).allowPruning()
  val writeData = bus.PWDATA

  override def readAddress()  = bus.PADDR
  override def writeAddress() = bus.PADDR

  override def readHalt()  = bus.PREADY := False
  override def writeHalt() = bus.PREADY := False

  override def busDataWidth   = bus.config.dataWidth
}
