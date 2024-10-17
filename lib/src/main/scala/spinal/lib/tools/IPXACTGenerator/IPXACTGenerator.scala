package spinal.lib.tools.IPXACTGenerator

import spinal.core._

import java.nio.file.Paths

class IPXACTGenerator[T <: Component](rtl: SpinalReport[T]) {
  private val toplevelModule = rtl.toplevel
  private val toplevelName = rtl.toplevelName
  private val sourcesPaths = rtl.generatedSourcesPaths
  private val filePath = Paths.get(rtl.generatedSourcesPaths.head)
  private val parentPath = filePath.getParent.toString
  private val fileName = filePath.getFileName.toString
  private val fileType = if (fileName.endsWith(".vhd")) {
    "VHDL"
  } else {
    "Verilog"
  }

  def generateIPXACT2022Component(vendor: String = "SpinalHDL", version: String = "1.0"): Unit = {
    if (sourcesPaths.size > 1) {
      println("Warning: Avoid using the 'oneFilePerComponent' option in SpinalConfig.")
    } else {
      IPXACT2022ComponentGenerator.generate(toplevelVendor = vendor, toplevelName = toplevelName, module = toplevelModule, version = version, generatePath = parentPath, fileType = fileType)
    }
  }

  def generateIPXACTVivadoComponent(vendor: String = "SpinalHDL", version: String = "1.0"): Unit = {
    if (sourcesPaths.size > 1) {
      println("Warning: Avoid using the 'oneFilePerComponent' option in SpinalConfig.")
    } else {
      IPXACTVivadoComponentGenerator.generate(toplevelVendor = vendor, toplevelName = toplevelName, module = toplevelModule, version = version, generatePath = parentPath, fileType = fileType)
    }
  }

  def generateDesignView(vendor: String = "SpinalHDL", version: String = "1.0"): Unit = {
    if (sourcesPaths.size <= 1) {
      println("Warning: You must use the 'oneFilePerComponent' option in SpinalConfig.")
    } else {
      IPXACT2022ComponentGenerator.generate(toplevelVendor = vendor, toplevelName = toplevelName, module = toplevelModule, version = version, generateDesign = true, generatePath = parentPath, fileType = fileType)
    }
  }
}
