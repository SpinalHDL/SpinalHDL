package spinal.lib.bus.regif

import language.experimental.macros
import spinal.core.Bool

object InterruptFactory{
  def apply(busif: BusIf, regNamePre: String, triggers: Bool*): Bool = macro Macros.interruptFactoryImpl
}
