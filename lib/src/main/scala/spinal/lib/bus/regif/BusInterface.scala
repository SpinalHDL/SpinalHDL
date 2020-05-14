package spinal.lib.bus.regif

import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.ahblite.AhbLite3
import spinal.lib.bus.amba4.axi.Axi4
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.misc.SizeMapping

object BusInterface {
  def apply(bus: Apb3, sizeMap: SizeMapping, selID: Int)(implicit moduleName: ClassName): BusIf = Apb3BusInterface(bus, sizeMap, selID)(moduleName)
  def apply(bus: Apb3, sizeMap: SizeMapping, selID: Int, regPre: String)(implicit moduleName: ClassName): BusIf = Apb3BusInterface(bus, sizeMap, selID, regPre = regPre)(moduleName)
  def apply(bus: Apb3, sizeMap: SizeMapping, selID: Int, readSync: Boolean)(implicit moduleName: ClassName): BusIf = Apb3BusInterface(bus, sizeMap, selID, readSync)(moduleName)
  def apply(bus: Apb3, sizeMap: SizeMapping, selID: Int, readSync: Boolean, regPre: String)(implicit moduleName: ClassName): BusIf = Apb3BusInterface(bus, sizeMap, selID, readSync, regPre = regPre)(moduleName)

  def apply(bus: AhbLite3, sizeMap: SizeMapping)(implicit moduleName: ClassName): BusIf = AhbLite3BusInterface(bus, sizeMap)(moduleName)
  def apply(bus: AhbLite3, sizeMap: SizeMapping, regPre: String)(implicit moduleName: ClassName): BusIf = AhbLite3BusInterface(bus, sizeMap, regPre = regPre)(moduleName)
  def apply(bus: AhbLite3, sizeMap: SizeMapping, readSync: Boolean)(implicit moduleName: ClassName): BusIf = AhbLite3BusInterface(bus, sizeMap, readSync)(moduleName)
  def apply(bus: AhbLite3, sizeMap: SizeMapping, readSync: Boolean, regPre: String)(implicit moduleName: ClassName): BusIf = AhbLite3BusInterface(bus, sizeMap, readSync, regPre = regPre)(moduleName)

//  def apply(bus: Axi4, sizeMap: SizeMapping): BusIf = Axi4BusInterface(bus, sizeMap)
//  def apply(bus: Axi4, sizeMap: SizeMapping, readSync: Boolean): BusIf = Axi4BusInterface(bus, sizeMap)
//
//  def apply(bus: AxiLite4, sizeMap: SizeMapping): BusIf = AxiLite4BusInterface(bus, sizeMap)
//  def apply(bus: AxiLite4, sizeMap: SizeMapping, readSync: Boolean): BusIf = AxiLite4BusInterface(bus, sizeMap)
}
