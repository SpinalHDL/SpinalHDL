package spinal.lib.tools.IPXACTGenerator
import IPXACT2022scalaxb._
import IPXACT2022ScalaCases._
import spinal.lib._

import java.nio.file.{Files, Paths}
import scala.language.implicitConversions
import scala.xml._

class V2022BusDefinitionGenerator(toplevelVendor: String = "SpinalHDL", toplevelName: String, version: String = "1.0", thisBus: IMasterSlave) {
  private val busDefinitionName = thisBus match {
    case thisStream: Stream[_] =>
      s"Stream_${thisStream.payload.getClass.getSimpleName}"
    case thisFlow: Flow[_] =>
      s"Flow_${thisFlow.payload.getClass.getSimpleName}"
    case _ => thisBus.getClass.getSimpleName
  }

  private def generateVLNV: DocumentNameGroupSequence = {
    val busVersionedIdentifierSequence = VersionedIdentifierSequence(toplevelVendor, toplevelName, busDefinitionName, version)
    val busDocumentNameGroupSequence = DocumentNameGroupSequence(busVersionedIdentifierSequence)
    busDocumentNameGroupSequence
  }

  def beginGenerate(): Unit = {
    //    val maxInitiators=UnsignedIntExpression("1")
    //    val maxTargets=UnsignedIntExpression("1")
    val thisVLNV = generateVLNV
    val BusType = BusDefinition(thisVLNV, directConnection = true, isAddressable = true, broadcast = Some(true))

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

object V2022BusDefinitionGenerator {
  def generate(toplevelVendor: String = "SpinalHDL",
               toplevelName: String,
               version: String = "1.0",
               thisBus: IMasterSlave): Unit = {
    val generator = new V2022BusDefinitionGenerator(toplevelVendor, toplevelName, version, thisBus)
    generator.beginGenerate()
  }
}
