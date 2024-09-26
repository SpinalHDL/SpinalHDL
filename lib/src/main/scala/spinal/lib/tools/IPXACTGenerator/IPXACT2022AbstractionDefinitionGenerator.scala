package spinal.lib.tools.IPXACTGenerator

import IPXACT2022ScalaCases._
import IPXACT2022scalaxb._
import spinal.core._
import spinal.lib._

import java.nio.file.{Files, Paths}
import scala.language.implicitConversions
import scala.xml._

class IPXACT2022AbstractionDefinitionGenerator(toplevelVendor: String = "SpinalHDL", toplevelName: String, version: String = "1.0", bus: IMasterSlave, generatePath: String = "./") {
  private val busDefinitionName = bus match {
    case stream: Stream[_] =>
      s"Stream_${stream.payload.getClass.getSimpleName}"
    case flow: Flow[_] =>
      s"Flow_${flow.payload.getClass.getSimpleName}"
    case _ => bus.getClass.getSimpleName
  }

  private def createBusType: LibraryRefType = {
    val busTypeVendorRecord = DataRecord(Some(""), Some("vendor"), toplevelVendor)
    val busTypeLibraryRecord = DataRecord(Some(""), Some("library"), toplevelName)
    val busTypeNameRecord = DataRecord(Some(""), Some("name"), busDefinitionName)
    val busTypeVersionRecord = DataRecord(Some(""), Some("version"), version)
    val abstractionBusTypeAttributes = Map("vendor" -> busTypeVendorRecord, "library" -> busTypeLibraryRecord, "name" -> busTypeNameRecord, "version" -> busTypeVersionRecord)
    val abstractionBusType = LibraryRefType(abstractionBusTypeAttributes)
    abstractionBusType
  }

  private def createVLNV: DocumentNameGroupSequence = {
    val name = busDefinitionName + ".absDef"
    val abstractionVersionedIdentifierSequence = VersionedIdentifierSequence(toplevelVendor, toplevelName, name, version)
    val abstractionDocumentNameGroupSequence = DocumentNameGroupSequence(abstractionVersionedIdentifierSequence)
    abstractionDocumentNameGroupSequence
  }


  private def createPorts(isMaster: Boolean): Ports3 = {
    val busData = bus.asInstanceOf[Data]
    val logicalSignalList = busData.flattenLocalName
    val physicalSignalList = busData.flatten
    val signalNum = logicalSignalList.length
    var portsSeq: Seq[Port] = Seq()
    for (i <- 0 until signalNum) {
      val signalName = logicalSignalList(i)
      val signalDirection = physicalSignalList(i).isInput
      var directionAsMaster = if (signalDirection) {
        Out
      } else {
        In
      }
      var directionAsSlave = if (signalDirection) {
        In
      } else {
        Out
      }
      if (isMaster) {
        val temp = directionAsMaster
        directionAsMaster = directionAsSlave
        directionAsSlave = temp
      }

      val InitiatorWirePortSequence = WirePortSequence(direction = Some(directionAsMaster))
      val TargetWirePortSequence = WirePortSequence(direction = Some(directionAsSlave))
      val onInitiator = OnInitiator2(InitiatorWirePortSequence)
      val onTarget = OnTarget2(TargetWirePortSequence)
      val wire = Wire(onInitiator = Some(onInitiator), onTarget = Some(onTarget))
      val wireRecord = DataRecord(Some(""), Some("ipxact:wire"), wire)
      val portSequence = PortSequence1(wireRecord)
      val abstractionBusPort = Port(logicalName = signalName, portsequence1 = portSequence)
      portsSeq = portsSeq :+ abstractionBusPort
    }
    val abstractionBusPorts = Ports3(portsSeq)
    abstractionBusPorts
  }

  def beginGenerate(): Unit = {
    bus match {
      case iMasterSlaveBus: IMasterSlave =>
        val isMaster = iMasterSlaveBus.isMasterInterface
        val vlnv = createVLNV
        val abstractionBusType = createBusType
        val ports = createPorts(isMaster)
        val abstractionDefinition = AbstractionDefinition(vlnv, abstractionBusType, ports = ports)
        val xml: NodeSeq = toXML[AbstractionDefinition](abstractionDefinition, "ipxact:abstractionDefinition", defaultScope)
        val fileDirectory = s"$generatePath/$toplevelVendor/$toplevelName/$busDefinitionName/$version/"
        val filePath = s"$fileDirectory$busDefinitionName.absDef.$version.xml"
        Files.createDirectories(Paths.get(fileDirectory))
        //        val prettyPrinter = new PrettyPrinter(width = 80, step = 2)
        //        val formattedXml: String = prettyPrinter.format(xml.head)
        //        println(formattedXml)
        XML.save(filePath, xml.head, "UTF-8", xmlDecl = true, doctype = null)
    }
  }
}

object IPXACT2022AbstractionDefinitionGenerator {
  def generate(toplevelVendor: String = "SpinalHDL",
               toplevelName: String,
               version: String = "1.0",
               bus: IMasterSlave,
               generatePath: String = "./"): Unit = {
    val generator = new IPXACT2022AbstractionDefinitionGenerator(toplevelVendor, toplevelName, version, bus, generatePath = generatePath)
    generator.beginGenerate()
  }
}

