package spinal.lib.bus.regif

import spinal.core._
import spinal.lib.bus.amba4.apb.Apb4
import spinal.lib.bus.misc.SizeMapping

case class Apb4BusInterface(bus: Apb4, sizeMap: SizeMapping, selId: Int = 0, regPre: String = "")(implicit moduleName: ClassName) extends BusIf{

  override def getModuleName = moduleName.name

  val readError = Bool()
  val readData  = Bits(bus.c.dataWidth bits)

  readError.setAsReg() init False
  readData.setAsReg()  init 0

  bus.PREADY := True
  bus.PRDATA := readData

  val askWrite  = (bus.PSEL(selId) && bus.PWRITE).allowPruning()
  val askRead   = (bus.PSEL(selId) && !bus.PWRITE).allowPruning()
  val doWrite   = (askWrite && bus.PENABLE && bus.PREADY).allowPruning()
  val doRead    = (askRead  && bus.PENABLE && bus.PREADY).allowPruning()
  val writeData = Bits(busDataWidth bit)

//  (0 until bus.c.strbWidth).foreach{ i =>
//    writeData((i+1)*8-1 downto i*8) := Mux(bus.PSTRB(i), bus.PWDATA((i+1)*8-1 downto i*8), B(0, 8 bit))
//  }
  if(withstrb) {
    (0 until bus.c.strbWidth).foreach{i =>
      wmask((i+1)*8-1 downto i*8) := Mux(bus.PSTRB(i), B(0xFF, 8 bit), B(0, 8 bit))
      wmask((i+1)*8-1 downto i*8) := Mux(bus.PSTRB(i), B(0, 8 bit), B(0xFF, 8 bit))
    }
  }

  if(bus.c.useSlaveError) bus.PSLVERR := readError
  override def readAddress()  = bus.PADDR
  override def writeAddress() = bus.PADDR

  override def readHalt()  = bus.PREADY := False
  override def writeHalt() = bus.PREADY := False

  override def busDataWidth   = bus.c.dataWidth
}
