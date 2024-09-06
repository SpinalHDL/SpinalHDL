package spinal.lib.tools.IPXACTGenerator

import spinal.core._
import spinal.core.tools.ModuleAnalyzer
import spinal.lib.{IMasterSlave, _}

import java.nio.file.{Files, Paths, StandardCopyOption}
import scala.language.implicitConversions
import scala.util.control.Breaks.{break, breakable}
import scala.xml._
import IPXACT2022scalaxb._
import IPXACT2022ScalaCases._
class V2022ComponentGenerator(toplevelVendor: String = "SpinalHDL", toplevelName: String, version: String = "1.0", module: Component) {


  private val moduleDefinitionName = module.definitionName
  private var busTypeStringSet: Set[String] = Set()
  private var busStringSet: Set[String] = Set()
  private val moduleAnalyzer = new ModuleAnalyzer(module)
  private val inPorts = moduleAnalyzer.getInputs(_ => true)
  private val outPorts = moduleAnalyzer.getOutputs(_ => true)
  private val allPorts = inPorts ++ outPorts


  private def getBusInterfaces: BusInterfaces = {
    var busInterfacesSeq: Seq[BusInterfaceType] = Seq()
    for (thisPort <- allPorts) {
      val thisPortParentChain = thisPort.getRefOwnersChain()
      breakable {
        for (thisPortParent <- thisPortParentChain) {
          thisPortParent match {
            case iMasterSlaveParent: IMasterSlave with Data =>
              val thisParentName = iMasterSlaveParent.name
              val isMaster = iMasterSlaveParent.isMasterInterface
              val busDefinitionName = iMasterSlaveParent match {
                case thisStream: Stream[_] =>
                  s"Stream_${thisStream.payload.getClass.getSimpleName}"
                case thisFlow: Flow[_] =>
                  s"Flow_${thisFlow.payload.getClass.getSimpleName}"
                case _ => iMasterSlaveParent.getClass.getSimpleName
              }
              val thisBusName = busDefinitionName + "_" + thisParentName
              val thisBusInterfaceMode = if (isMaster) {
                "ipxact:initiator"
              } else {
                "ipxact:target"
              }
              if (!busTypeStringSet.contains(busDefinitionName)) {
                V2022AbstractionDefinitionGenerator.generate(toplevelName = toplevelName, thisBus = iMasterSlaveParent)
                V2022BusDefinitionGenerator.generate(toplevelName = toplevelName, thisBus = iMasterSlaveParent)
                busTypeStringSet = busTypeStringSet + busDefinitionName
              }
              if (!busStringSet.contains(thisBusName)) {
                val physicalSignalList = iMasterSlaveParent.flatten
                val logicalSignalList = iMasterSlaveParent.flattenLocalName
                var portMapSeq: Seq[PortMap] = Seq()
                for (thisSignal <- physicalSignalList) {
                  if (allPorts.contains(thisSignal)) {
                    val index = physicalSignalList.indexOf(thisSignal)
                    val thisSignalPhysicalName = thisSignal.name
                    val thisSignalLogicalName = logicalSignalList(index)
                    val logicalPort = LogicalPort(thisSignalLogicalName)
                    val physicalPort = PhysicalPort(thisSignalPhysicalName)
                    val physicalPortRecord = DataRecord(Some("namespace1"), Some("ipxact:physicalPort"), physicalPort)
                    val portMap = PortMap(logicalPort = logicalPort, portmapoption = physicalPortRecord)
                    portMapSeq = portMapSeq :+ portMap
                  }
                }
                val portMaps = PortMaps(portMapSeq)
                val busAbstractionTypeRecord1 = DataRecord(Some("namespace1"), Some("vendor"), toplevelVendor)
                val busAbstractionTypeRecord2 = DataRecord(Some("namespace2"), Some("library"), toplevelName)
                val busAbstractionTypeRecord3 = DataRecord(Some("namespace3"), Some(" name"), busDefinitionName + ".absDef")
                val busAbstractionTypeRecord4 = DataRecord(Some("namespace4"), Some("version"), version)
                val busAbstractionAttributes = Map("key1" -> busAbstractionTypeRecord1, "key2" -> busAbstractionTypeRecord2, "key3" -> busAbstractionTypeRecord3, "key4" -> busAbstractionTypeRecord4)
                val abstractionRef = ConfigurableLibraryRefType(attributes = busAbstractionAttributes)

                val busAbstractionType1 = AbstractionType(abstractionRef = abstractionRef, portMaps = Some(portMaps))
                val busAbstractionTypes = AbstractionTypes(Seq(busAbstractionType1))
                val busTypeRecord1 = DataRecord(Some("namespace1"), Some("vendor"), toplevelVendor)
                val busTypeRecord2 = DataRecord(Some("namespace2"), Some("library"), toplevelName)
                val busTypeRecord3 = DataRecord(Some("namespace3"), Some(" name"), busDefinitionName)
                val busTypeRecord4 = DataRecord(Some("namespace4"), Some("version"), version)
                val busTypeAttributes = Map("key1" -> busTypeRecord1, "key2" -> busTypeRecord2, "key3" -> busTypeRecord3, "key4" -> busTypeRecord4)
                val busType = ConfigurableLibraryRefType(attributes = busTypeAttributes)
                val interfaceModeOptionRecord = DataRecord(Some("namespace1"), Some(thisBusInterfaceMode), "")
                val busInterfaceNameGroupSequence = NameGroupSequence(thisBusName)
                val thisBusInterface = BusInterfaceType(busInterfaceNameGroupSequence, busType, abstractionTypes = Some(busAbstractionTypes), interfaceModeOption4 = interfaceModeOptionRecord)
                busInterfacesSeq = busInterfacesSeq :+ thisBusInterface
                busStringSet += thisBusName
              }
              break
            case _ =>
          }
        }
      }
    }
    val componentBusInterfaces = BusInterfaces(busInterfacesSeq)
    componentBusInterfaces
  }


  private def getPorts: Option[Ports] = {
    var portsSeq: Seq[Port2] = Seq()
    for (thisPort <- inPorts ++ outPorts) {
      val inOutValue =
        if (thisPort.isInput) {
          InValue
        } else if (thisPort.isOutput) {
          OutValue
        } else {
          InoutValue
        }
      val leftUnsignedIntExpression = UnsignedIntExpression((thisPort.getBitsWidth - 1).toString)
      val rightUnsignedIntExpression = UnsignedIntExpression("0")
      val thisVector1 = Vector4(left = leftUnsignedIntExpression, right = rightUnsignedIntExpression)
      val thisVectorSeq: Seq[IPXACT2022ScalaCases.Vector4] = Seq(thisVector1)
      val thisPortVectors = ExtendedVectorsType(thisVectorSeq)
      val thisAbstractorPortWireType = PortWireType(direction = inOutValue, vectors = Some(thisPortVectors))
      val thisWireRecord = DataRecord(Some("namespace1"), Some("ipxact:wire"), thisAbstractorPortWireType)
      val thisNameGroupPortSequence = NameGroupPortSequence(thisPort.name)
      val thisPort2 = Port2(thisNameGroupPortSequence, thisWireRecord)
      portsSeq = portsSeq :+ thisPort2
    }

    val thisPorts: Option[Ports] = if (portsSeq.nonEmpty) Some(Ports(portsSeq)) else None
    thisPorts
  }

  private def getViews: Option[Views] = {
    var viewSep: Seq[View] = Seq()
    val componentViewNameGroupNMTOKEN = NameGroupNMTOKENSequence("componentView")
    val componentViewEnvIdentifier = Seq(EnvIdentifier("verilog:Spinal:"))
    val componentViewComponentInstantiationRef = Some("verilog_implementation")
    val componentView = View(componentViewNameGroupNMTOKEN, envIdentifier = componentViewEnvIdentifier, componentInstantiationRef = componentViewComponentInstantiationRef)
    viewSep = viewSep :+ componentView
    //    if (module.children.nonEmpty) {
    //      val designConfigRefName = moduleDefinitionName + ".designcfg_1.0"
    //      val designConfigurationViewNameGroupNMTOKEN = NameGroupNMTOKENSequence("designConfigurationView")
    //      val designConfigurationView = View(designConfigurationViewNameGroupNMTOKEN, designConfigurationInstantiationRef = Some(designConfigRefName))
    //      viewSep = viewSep :+ designConfigurationView
    //    }
    val thisViews: Option[Views] = Some(Views(viewSep))
    thisViews
  }

  private def getInstantiations: Option[Instantiations] = {
    var instantiationsSeq: Seq[IPXACT2022scalaxb.DataRecord[Any]] = Seq()
    val componentNameGroupNMTOKENSequence = NameGroupNMTOKENSequence("verilog_implementation")
    val componentInstantiationLanguage = LanguageType("verilog")
    val componentInstantiationModuleName = Some(module.definitionName)
    val componentInstantiationFileSetRef = FileSetRef("verilogSource")
    val componentInstantiation = ComponentInstantiationType(componentNameGroupNMTOKENSequence, language = Some(componentInstantiationLanguage), moduleName = componentInstantiationModuleName, fileSetRef = Seq(componentInstantiationFileSetRef))
    val componentInstantiationRecord = DataRecord(Some("namespace1"), Some("ipxact:componentInstantiation"), componentInstantiation)
    instantiationsSeq = instantiationsSeq :+ componentInstantiationRecord
    //    if (module.children.nonEmpty) {
    //      val designConfigurationRecord1 = DataRecord(Some("namespace1"), Some("vendor"), toplevelVendor)
    //      val designConfigurationRecord2 = DataRecord(Some("namespace2"), Some("library"), toplevelName)
    //      val designConfigurationRecord3 = DataRecord(Some("namespace3"), Some("name"), moduleDefinitionName + ".designcfg")
    //      val designConfigurationRecord4 = DataRecord(Some("namespace4"), Some("version"), "1.0")
    //      val designConfigurationBusAbstractionAttributes = Map("key1" -> designConfigurationRecord1, "key2" -> designConfigurationRecord2, "key3" -> designConfigurationRecord3, "key4" -> designConfigurationRecord4)
    //      val donfigurableLibraryRefType = ConfigurableLibraryRefType(attributes = designConfigurationBusAbstractionAttributes)
    //      val designConfigurationnameGroupNMTOKENSequence = NameGroupNMTOKENSequence(moduleDefinitionName + ".designcfg_1.0")
    //      val designConfigurationInstantiation = DesignConfigurationInstantiationType(designConfigurationnameGroupNMTOKENSequence , designConfigurationRef = donfigurableLibraryRefType)
    //      val designConfigurationInstantiationRecord = DataRecord(Some("namespace1"), Some("ipxact:designConfigurationInstantiation"), designConfigurationInstantiation)
    //      instantiationsSeq=instantiationsSeq:+designConfigurationInstantiationRecord
    //    }
    val thisInstantiations: Option[Instantiations] = Some(Instantiations(instantiationsSeq))
    thisInstantiations
  }

  private def getModel: ModelType = {
    val thisthisInstantiations = getInstantiations
    val thisViews = getViews
    val thisPorts = getPorts
    val thisModel = ModelType(thisViews, ports = thisPorts, instantiations = thisthisInstantiations)
    thisModel
  }

  private def getFileSets: FileSets = {
    val thisFileName = IpxactURI(module.definitionName + ".v")
    val thisFileType = FileType(VerilogSource)
    val thisFileDescription = "Generated by Spinal HDL function:SpinalVerilog()"
    val thisFile = File(thisFileName, fileType = Seq(thisFileType), description = Some(thisFileDescription))
    val fileSetNameGroupSequence = NameGroupSequence("verilogSource")
    val fileSet = FileSetType(fileSetNameGroupSequence, file = Seq(thisFile))
    val fileSetsSeq = Seq(fileSet)
    val thisFileSets = FileSets(fileSetsSeq)
    thisFileSets
  }

  private def getVLNV: DocumentNameGroupSequence = {
    val versionedIdentifierSequence11 = VersionedIdentifierSequence(toplevelVendor, toplevelName, moduleDefinitionName, "1.0")
    val documenNameGroup = DocumentNameGroupSequence(versionedIdentifierSequence1 = versionedIdentifierSequence11)
    documenNameGroup
  }

  def beginGenerate(): Unit = {

    val fileDirectory = s"./$toplevelVendor/$toplevelName/$moduleDefinitionName/1.0/"
    val filePath = s"$fileDirectory$moduleDefinitionName.1.0.xml"
    Files.createDirectories(Paths.get(fileDirectory))

    val inputFilePath = s"./$toplevelName.v"
    val sourcePath = Paths.get(inputFilePath)
    val targetPath = Paths.get(fileDirectory + sourcePath.getFileName)
    Files.copy(sourcePath, targetPath, StandardCopyOption.REPLACE_EXISTING)

    val documentNameGroup = getVLNV
    val thisModel = getModel
    val thisFileSets = getFileSets
    val thisBusInterfaces = getBusInterfaces
    val thisComponent = ComponentType(
      documentNameGroupSequence1 = documentNameGroup,
      busInterfaces = Some(thisBusInterfaces),
      model = Some(thisModel),
      fileSets = Some(thisFileSets)
    )
    val xml: NodeSeq = toXML[ComponentType](thisComponent, "ipxact:component", defaultScope)
//    val prettyPrinter = new PrettyPrinter(width = 80, step = 2)
//    val formattedXml: String = prettyPrinter.format(xml.head)
//    println(formattedXml)
    XML.save(filePath, xml.head, "UTF-8", xmlDecl = true, doctype = null)
  }
}

object V2022ComponentGenerator {
  def generate(toplevelVendor: String = "SpinalHDL",
               toplevelName: String,
               version: String = "1.0",
               module: Component): Unit = {
    val generator = new V2022ComponentGenerator(toplevelVendor, toplevelName, version, module)
    generator.beginGenerate()
  }
}

