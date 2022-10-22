package spinal.lib.bus.regif

import spinal.lib.bus.amba3.ahblite.{AhbLite3, AhbLite3SlaveFactory}
import spinal.lib.bus.misc.SizeMapping

object AhbLite3BusInterface {
  @deprecated
  def apply(bus: AhbLite3, sizeMap: SizeMapping, regPre: String = "")(implicit moduleName: ClassName): BusIf = BusInterface(AhbLite3SlaveFactory(bus), sizeMap, regPre = regPre)(moduleName)
}