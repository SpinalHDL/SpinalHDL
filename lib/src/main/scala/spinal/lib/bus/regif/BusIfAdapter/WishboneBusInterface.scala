package spinal.lib.bus.regif

import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.wishbone.{Wishbone, WishboneSlaveFactory}

object WishboneBusInterface {
  @deprecated
  def apply(bus: Wishbone,
            sizeMap: SizeMapping,
            selId: Int = 0,
            readSync: Boolean = true,
            regPre: String = ""
            )(implicit moduleName: ClassName): BusIf = {
    BusInterface(WishboneSlaveFactory(bus), sizeMap, regPre = regPre)(moduleName)
  }
}