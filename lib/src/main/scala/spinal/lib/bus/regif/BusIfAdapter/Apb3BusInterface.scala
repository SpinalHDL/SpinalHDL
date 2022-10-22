package spinal.lib.bus.regif

import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.misc.SizeMapping

object Apb3BusInterface {
  @deprecated
  def apply(bus: Apb3, sizeMap: SizeMapping, selId: Int = 0, regPre: String = "")(implicit moduleName: ClassName): BusIf = BusInterface(Apb3SlaveFactory(bus, selId = selId), regPre = regPre)(moduleName)
}