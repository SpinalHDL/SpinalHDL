package spinal.lib.tools.IPXACTGenerator

import IPXACT2022ScalaCases._
import IPXACT2022scalaxb._
import spinal.lib._

import java.nio.file.{Files, Paths}
import scala.language.implicitConversions
import scala.xml._

class IPXACT2022BusDefinitionGenerator(toplevelVendor: String = "SpinalHDL", toplevelName: String, version: String = "1.0", bus: IMasterSlave) {
  private val busDefinitionName = bus match {
    case stream: Stream[_] =>
      s"Stream_${stream.payload.getClass.getSimpleName}"
    case flow: Flow[_] =>
      s"Flow_${flow.payload.getClass.getSimpleName}"
    case _ => bus.getClass.getSimpleName
  }

  private def createVLNV: DocumentNameGroupSequence = {
    val busVersionedIdentifierSequence = VersionedIdentifierSequence(toplevelVendor, toplevelName, busDefinitionName, version)
    val busDocumentNameGroupSequence = DocumentNameGroupSequence(busVersionedIdentifierSequence)
    busDocumentNameGroupSequence
  }

  def beginGenerate(): Unit = {
    //    val maxInitiators=UnsignedIntExpression("1")
    //    val maxTargets=UnsignedIntExpression("1")
    val vlnv = createVLNV
    val BusType = BusDefinition(vlnv, directConnection = true, isAddressable = true, broadcast = Some(true))

    val xml: NodeSeq = toXML[BusDefinition](BusType, "ipxact:busDefinition", defaultScope)
    val fileDirectory = s"./$toplevelVendor/$toplevelName/$busDefinitionName/$version/"
    val filePath = s"$fileDirectory$busDefinitionName.$version.xml"
    Files.createDirectories(Paths.get(fileDirectory))
    //    val prettyPrinter = new PrettyPrinter(width = 80, step = 2)
    //    val formattedXml: String = prettyPrinter.format(xml.head)
    //    println(formattedXml)
    XML.save(filePath, xml.head, "UTF-8", xmlDecl = true, doctype = null)
  }

}

object IPXACT2022BusDefinitionGenerator {
  def generate(toplevelVendor: String = "SpinalHDL",
               toplevelName: String,
               version: String = "1.0",
               bus: IMasterSlave): Unit = {
    val generator = new IPXACT2022BusDefinitionGenerator(toplevelVendor, toplevelName, version, bus)
    generator.beginGenerate()
  }
}
