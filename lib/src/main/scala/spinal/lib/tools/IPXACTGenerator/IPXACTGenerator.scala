package spinal.lib.tools.IPXACTGenerator

import spinal.core._

class IPXACTGenerator[T <: Component](rtl: SpinalReport[T]) {
  private val toplevelModule = rtl.toplevel
  private val toplevelName = rtl.toplevelName

  def generateIPXACT2022Component(vendor: String = "SpinalHDL", version:String="1.0"): Unit = {
    IPXACT2022ComponentGenerator.generate(toplevelVendor = vendor, toplevelName = toplevelName, module = toplevelModule,version=version)
  }

  def generateIPXACTVivadoComponent(vendor: String = "SpinalHDL", version:String="1.0"): Unit = {
    IPXACTVivadoComponentGenerator.generate(toplevelVendor = vendor, toplevelName = toplevelName, module = toplevelModule,version=version)
  }
}
