package spinal.lib.bus.regif

import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.ahblite.AhbLite3
import spinal.lib.bus.amba4.apb.Apb4
import spinal.lib.bus.amba4.axi.Axi4
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.wishbone.Wishbone
import spinal.lib.bus.bram.BRAM
import spinal.lib.bus.localbus.{MemBus, MinBus}
import spinal.lib.bus.regif.BusIfAdapter.MinBusInterface

object BusInterface {
  def apply(bus: Apb3, sizeMap: SizeMapping, selID: Int)(implicit moduleName: ClassName): BusIf = Apb3BusInterface(bus, sizeMap)(moduleName)
  def apply(bus: Apb3, sizeMap: SizeMapping, selID: Int, regPre: String)(implicit moduleName: ClassName): BusIf = Apb3BusInterface(bus, sizeMap, regPre = regPre)(moduleName)
  def apply(bus: Apb3, sizeMap: SizeMapping, selID: Int, regPre: String, withSecFireWall: Boolean)(implicit moduleName: ClassName): BusIf = Apb3BusInterface(bus, sizeMap, regPre = regPre, withSecFireWall = withSecFireWall)(moduleName)
  def apply(bus: Apb4, sizeMap: SizeMapping, selID: Int)(implicit moduleName: ClassName): BusIf = Apb4BusInterface(bus, sizeMap)(moduleName)
  def apply(bus: Apb4, sizeMap: SizeMapping, selID: Int, regPre: String)(implicit moduleName: ClassName): BusIf = Apb4BusInterface(bus, sizeMap, regPre = regPre)(moduleName)
  def apply(bus: Apb4, sizeMap: SizeMapping, selID: Int, regPre: String, withSecFireWall: Boolean)(implicit moduleName: ClassName): BusIf = Apb4BusInterface(bus, sizeMap, regPre = regPre, withSecFireWall = withSecFireWall)(moduleName)

  def apply(bus: Apb3, sizeMap: SizeMapping)(implicit moduleName: ClassName): BusIf = Apb3BusInterface(bus, sizeMap)(moduleName)
  def apply(bus: Apb3, sizeMap: SizeMapping, regPre: String)(implicit moduleName: ClassName): BusIf = Apb3BusInterface(bus, sizeMap, regPre = regPre)(moduleName)
  def apply(bus: Apb3, sizeMap: SizeMapping, regPre: String, withSecFireWall: Boolean)(implicit moduleName: ClassName): BusIf = Apb3BusInterface(bus, sizeMap, regPre = regPre, withSecFireWall = withSecFireWall)(moduleName)
  def apply(bus: Apb4, sizeMap: SizeMapping)(implicit moduleName: ClassName): BusIf = Apb4BusInterface(bus, sizeMap)(moduleName)
  def apply(bus: Apb4, sizeMap: SizeMapping, regPre: String)(implicit moduleName: ClassName): BusIf = Apb4BusInterface(bus, sizeMap, regPre = regPre)(moduleName)
  def apply(bus: Apb4, sizeMap: SizeMapping, regPre: String, withSecFireWall: Boolean)(implicit moduleName: ClassName): BusIf = Apb4BusInterface(bus, sizeMap, regPre = regPre, withSecFireWall = withSecFireWall)(moduleName)

  def apply(bus: AhbLite3, sizeMap: SizeMapping)(implicit moduleName: ClassName): BusIf = AhbLite3BusInterface(bus, sizeMap)(moduleName)
  def apply(bus: AhbLite3, sizeMap: SizeMapping, regPre: String)(implicit moduleName: ClassName): BusIf = AhbLite3BusInterface(bus, sizeMap, regPre = regPre)(moduleName)
  def apply(bus: AhbLite3, sizeMap: SizeMapping, regPre: String, withSecFireWall: Boolean)(implicit moduleName: ClassName): BusIf = AhbLite3BusInterface(bus, sizeMap, regPre = regPre, withSecFireWall = withSecFireWall)(moduleName)

  def apply(bus: Wishbone, sizeMap: SizeMapping)(implicit moduleName: ClassName): BusIf = WishboneBusInterface(bus, sizeMap)(moduleName)
  def apply(bus: Wishbone, sizeMap: SizeMapping, regPre: String)(implicit moduleName: ClassName): BusIf = WishboneBusInterface(bus, sizeMap, regPre = regPre)(moduleName)
  def apply(bus: Wishbone, sizeMap: SizeMapping, readSync: Boolean)(implicit moduleName: ClassName): BusIf = WishboneBusInterface(bus, sizeMap, readSync)(moduleName)
  def apply(bus: Wishbone, sizeMap: SizeMapping, readSync: Boolean, regPre: String)(implicit moduleName: ClassName): BusIf = WishboneBusInterface(bus, sizeMap, readSync, regPre = regPre)(moduleName)
  def apply(bus: Wishbone, sizeMap: SizeMapping, readSync: Boolean, regPre: String, withSecFireWall: Boolean)(implicit moduleName: ClassName): BusIf = WishboneBusInterface(bus, sizeMap, readSync, regPre = regPre, withSecFireWall = withSecFireWall)(moduleName)

  def apply(bus: AxiLite4, sizeMap: SizeMapping)(implicit moduleName: ClassName): BusIf = AxiLite4BusInterface(bus, sizeMap)(moduleName)
  def apply(bus: AxiLite4, sizeMap: SizeMapping, regPre: String)(implicit moduleName: ClassName): BusIf = AxiLite4BusInterface(bus, sizeMap, regPre)(moduleName)
  def apply(bus: AxiLite4, sizeMap: SizeMapping, regPre: String, withSecFireWall: Boolean)(implicit moduleName: ClassName): BusIf = AxiLite4BusInterface(bus, sizeMap, regPre)(moduleName)

  def apply(bus: BRAM, sizeMap: SizeMapping)(implicit moduleName: ClassName): BusIf = BRAMBusInterface(bus, sizeMap)(moduleName)
  def apply(bus: BRAM, sizeMap: SizeMapping, regPre: String)(implicit moduleName: ClassName): BusIf = BRAMBusInterface(bus, sizeMap, regPre)(moduleName)
  def apply(bus: BRAM, sizeMap: SizeMapping, regPre: String, withSecFireWall: Boolean)(implicit moduleName: ClassName): BusIf = BRAMBusInterface(bus, sizeMap, regPre, withSecFireWall)(moduleName)

  def apply(bus: MemBus, sizeMap: SizeMapping)(implicit moduleName: ClassName): BusIf = MemBusInterface(bus, sizeMap)(moduleName)
  def apply(bus: MemBus, sizeMap: SizeMapping, regPre: String)(implicit moduleName: ClassName): BusIf = MemBusInterface(bus, sizeMap, regPre)(moduleName)
  def apply(bus: MemBus, sizeMap: SizeMapping, regPre: String, withSecFireWall: Boolean)(implicit moduleName: ClassName): BusIf = MemBusInterface(bus, sizeMap, regPre, withSecFireWall)(moduleName)

  def apply(bus: MinBus, sizeMap: SizeMapping)(implicit moduleName: ClassName): BusIf = MinBusInterface(bus, sizeMap)(moduleName)
  def apply(bus: MinBus, sizeMap: SizeMapping, regPre: String)(implicit moduleName: ClassName): BusIf = MinBusInterface(bus, sizeMap, regPre)(moduleName)
  def apply(bus: MinBus, sizeMap: SizeMapping, regPre: String, withSecFireWall: Boolean)(implicit moduleName: ClassName): BusIf = MinBusInterface(bus, sizeMap, regPre, withSecFireWall)(moduleName)
}
