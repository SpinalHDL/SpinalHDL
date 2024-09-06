package spinal.lib.tools.IPXACTGenerator

import spinal.core._
import spinal.lib._
import IPXACT2022scalaxb._
import IPXACT2022ScalaCases._
import java.nio.file.{Files, Paths}
import scala.language.implicitConversions
import scala.xml._

class V2022AbstractionDefinitionGenerator(toplevelVendor: String = "SpinalHDL", toplevelName: String, version: String = "1.0", thisBus: IMasterSlave) {
  private val busDefinitionName = thisBus match {
    case thisStream: Stream[_] =>
      s"Stream_${thisStream.payload.getClass.getSimpleName}"
    case thisFlow: Flow[_] =>
      s"Flow_${thisFlow.payload.getClass.getSimpleName}"
    case _ => thisBus.getClass.getSimpleName
  }

  private def getBusType: LibraryRefType = {
    val busTypeRecord1 = DataRecord(Some("namespace1"), Some("vendor"), toplevelVendor)
    val busTypeRecord2 = DataRecord(Some("namespace2"), Some("library"), toplevelName)
    val busTypeRecord3 = DataRecord(Some("namespace3"), Some(" name"), busDefinitionName)
    val busTypeRecord4 = DataRecord(Some("namespace4"), Some("version"), version)
    val abstractionBusTypeAttributes = Map("key1" -> busTypeRecord1, "key2" -> busTypeRecord2, "key3" -> busTypeRecord3, "key4" -> busTypeRecord4)
    val abstractionBusType = LibraryRefType(abstractionBusTypeAttributes)
    abstractionBusType
  }

  private def getVLNV: DocumentNameGroupSequence = {
    val thisName = busDefinitionName + ".absDef"
    val abstractionVersionedIdentifierSequence = VersionedIdentifierSequence(toplevelVendor, toplevelName, thisName, version)
    val abstractionDocumentNameGroupSequence = DocumentNameGroupSequence(abstractionVersionedIdentifierSequence)
    abstractionDocumentNameGroupSequence
  }


  private def getPorts(isMaster: Boolean): Ports3 = {
    val thisBusData = thisBus.asInstanceOf[Data]
    val logicalSignalList = thisBusData.flattenLocalName
    val physicalSignalList = thisBusData.flatten
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
      val thisWire = Wire(onInitiator = Some(onInitiator), onTarget = Some(onTarget))
      val thisWireRecord = DataRecord(Some("namespace1"), Some("ipxact:wire"), thisWire)
      val portSequence = PortSequence1(thisWireRecord)
      val abstractionBusPort = Port(logicalName = signalName, portsequence1 = portSequence)
      portsSeq = portsSeq :+ abstractionBusPort
    }
    val abstractionBusPorts = Ports3(portsSeq)
    abstractionBusPorts
  }

  def beginGenerate(): Unit = {
    thisBus match {
      case iMasterSlaveBus: IMasterSlave =>
        val isMaster = iMasterSlaveBus.isMasterInterface
        val thisVLNV = getVLNV
        val abstractionBusType = getBusType
        val ports = getPorts(isMaster)
        val abstractionDefinition = AbstractionDefinition(thisVLNV, abstractionBusType, ports = ports)
        val xml: NodeSeq = toXML[AbstractionDefinition](abstractionDefinition, "ipxact:abstractionDefinition", defaultScope)
        val fileDirectory = s"./$toplevelVendor/$toplevelName/$busDefinitionName/$version/"
        val filePath = s"$fileDirectory$busDefinitionName.absDef.$version.xml"
        Files.createDirectories(Paths.get(fileDirectory))
        //        val prettyPrinter = new PrettyPrinter(width = 80, step = 2)
        //        val formattedXml: String = prettyPrinter.format(xml.head)
        //        println(formattedXml)
        XML.save(filePath, xml.head, "UTF-8", xmlDecl = true, doctype = null)
    }
  }
}

object V2022AbstractionDefinitionGenerator {
  def generate(toplevelVendor: String = "SpinalHDL",
               toplevelName: String,
               version: String = "1.0",
               thisBus: IMasterSlave): Unit = {
    val generator = new V2022AbstractionDefinitionGenerator(toplevelVendor, toplevelName, version, thisBus)
    generator.beginGenerate()
  }
}

