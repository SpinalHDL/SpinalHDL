package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4B, AxiLite4R, AxiLite4SlaveFactory}
import spinal.lib.bus.misc.SizeMapping

object AxiLite4BusInterface {
  @deprecated
  def apply(bus: AxiLite4, sizeMap: SizeMapping, regPre: String = "")(implicit moduleName: ClassName) = BusInterface(AxiLite4SlaveFactory(bus), sizeMap, regPre = regPre)(moduleName)
}