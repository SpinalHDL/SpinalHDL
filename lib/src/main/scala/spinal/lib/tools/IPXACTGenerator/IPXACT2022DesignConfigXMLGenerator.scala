package spinal.lib.tools.IPXACTGenerator

import IPXACT2022ScalaCases._
import IPXACT2022scalaxb._
import spinal.core._

import java.nio.file.{Files, Paths}
import scala.language.implicitConversions
import scala.xml._

class IPXACT2022DesignConfigXMLGenerator(toplevelVendor: String = "SpinalHDL", toplevelName: String, module: Component, version: String = "1.0", generatePath: String = "./") {
  private val moduleDefinitionName = module.definitionName

  private def createDesignConfigData: DesignConfiguration = {
    val vendorRecord = DataRecord(Some(""), Some("vendor"), toplevelVendor)
    val libraryRecord = DataRecord(Some(""), Some("library"), toplevelName)
    val nameRecord = DataRecord(Some(""), Some("name"), moduleDefinitionName + ".design")
    val versionRecord = DataRecord(Some(""), Some("version"), version)
    val designConfigLibraryRefTypeAttributes = Map("vendor" -> vendorRecord, "library" -> libraryRecord, "name" -> nameRecord, "version" -> versionRecord)
    val designConfigLibraryRefType = LibraryRefType(attributes = designConfigLibraryRefTypeAttributes)

    val versionedIdentifierSequence = VersionedIdentifierSequence(toplevelVendor, toplevelName, moduleDefinitionName + ".designcfg", version)
    val documentNameGroupSequence = DocumentNameGroupSequence(versionedIdentifierSequence)
    val designConfiguration = DesignConfiguration(documentNameGroupSequence, designRef = Some(designConfigLibraryRefType))
    designConfiguration
  }

  def beginGenerate = {
    val fileDirectory = s"$generatePath/$toplevelVendor/$toplevelName/$moduleDefinitionName/$version/"
    val filePath = s"$fileDirectory${moduleDefinitionName}.designcfg.$version.xml"
    if (module.children.nonEmpty) {
      Files.createDirectories(Paths.get(fileDirectory))

      val designConfiguration = createDesignConfigData
      val xml: NodeSeq = toXML[DesignConfiguration](designConfiguration, "ipxact:designConfiguration", defaultScope)

//      val prettyPrinter = new PrettyPrinter(width = 80, step = 2)
//      val formattedXml: String = prettyPrinter.format(xml.head)
      //      println(formattedXml)
      //    Files.write(Paths.get(filePath), formattedXml.getBytes("UTF-8"))
      XML.save(filePath, xml.head, "UTF-8", xmlDecl = true, doctype = null)
    }
  }
}

object IPXACT2022DesignConfigXMLGenerator {
  def generate(Vendor: String = "SpinalHDL",
               toplevelName: String,
               version: String = "1.0",
               module: Component,
               generatePath: String): Unit = {
    val generator = new IPXACT2022DesignConfigXMLGenerator(toplevelVendor = Vendor, toplevelName = toplevelName, version = version, module = module, generatePath = generatePath)
    generator.beginGenerate
  }
}
