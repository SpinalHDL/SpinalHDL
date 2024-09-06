package spinal.lib.tools.IPXACTGenerator

import spinal.core._


class IPXACTGenerator[T <: Component](rtl: SpinalReport[T]) {
  private val toplevelModule = rtl.toplevel
  private val toplevelName = rtl.toplevelName

  def generate2022Component(vendor: String = "SpinalHDL"): Unit = {
    V2022ComponentGenerator.generate(toplevelVendor = vendor, toplevelName = toplevelName, module = toplevelModule)
  }

  def generateVivadoComponent(vendor: String = "SpinalHDL"): Unit = {
    VivadoComponentGenerator.generate(toplevelVendor = vendor, toplevelName = toplevelName, module = toplevelModule)
  }
}


