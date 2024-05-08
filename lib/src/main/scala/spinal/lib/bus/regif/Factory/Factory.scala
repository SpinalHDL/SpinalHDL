package spinal.lib.bus.regif

import language.experimental.macros
import spinal.core.Bool

object InterruptFactory{
  @deprecated(message = "busif.interruptFactory() is recommended", since = "2022-12-31")
  def apply(busif: BusIf, regNamePre: String, triggers: Bool*): Bool = macro Macros.interruptFactoryImpl
}
