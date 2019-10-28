package spinal.lib.bus.regif

import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.ahblite.AhbLite3
import spinal.lib.bus.amba4.axi.Axi4
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.misc.SizeMapping

object BusInterface {
  def apply(bus: Apb3, sizeMap: SizeMapping, selID: Int): BusIf = Apb3BusInterface(bus, sizeMap, selID)
  def apply(bus: Apb3, sizeMap: SizeMapping, selID: Int, readSync: Boolean): BusIf = Apb3BusInterface(bus, sizeMap, selID, readSync)

//  def apply(bus: AhbLite3, sizeMap: SizeMapping): BusIf = AhbLite3BusInterface(bus, sizeMap)
//  def apply(bus: AhbLite3, sizeMap: SizeMapping, readSync: Boolean): BusIf = AhbLite3BusInterface(bus, sizeMap, readSync)
//
//  def apply(bus: Axi4, sizeMap: SizeMapping): BusIf = Axi4BusInterface(bus, sizeMap)
//  def apply(bus: Axi4, sizeMap: SizeMapping, readSync: Boolean): BusIf = Axi4BusInterface(bus, sizeMap)
//
//  def apply(bus: AxiLite4, sizeMap: SizeMapping): BusIf = AxiLite4BusInterface(bus, sizeMap)
//  def apply(bus: AxiLite4, sizeMap: SizeMapping, readSync: Boolean): BusIf = AxiLite4BusInterface(bus, sizeMap)
}
