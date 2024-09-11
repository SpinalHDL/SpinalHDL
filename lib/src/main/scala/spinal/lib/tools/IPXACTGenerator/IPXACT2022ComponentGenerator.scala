package spinal.lib.tools.IPXACTGenerator

import IPXACT2022ScalaCases._
import IPXACT2022scalaxb._
import spinal.core._
import spinal.core.tools.ModuleAnalyzer
import spinal.lib.{IMasterSlave, _}

import java.nio.file.{Files, Paths, StandardCopyOption}
import scala.language.implicitConversions
import scala.util.control.Breaks.{break, breakable}
import scala.xml._

class IPXACT2022ComponentGenerator(toplevelVendor: String = "SpinalHDL", toplevelName: String, version: String = "1.0", module: Component) {

  private val moduleDefinitionName = module.definitionName
  private var busTypeStringSet: Set[String] = Set()
  private var busStringSet: Set[String] = Set()
  private val moduleAnalyzer = new ModuleAnalyzer(module)
  private val inPorts = moduleAnalyzer.getInputs(_ => true)
  private val outPorts = moduleAnalyzer.getOutputs(_ => true)
  private val allPorts = inPorts ++ outPorts

  private def createConfigurableLibraryRefType(name:String):ConfigurableLibraryRefType={
    val busAbstractionTypeVendorRecord = DataRecord(Some(""), Some("vendor"), toplevelVendor)
    val busAbstractionTypeLibraryRecord = DataRecord(Some(""), Some("library"), toplevelName)
    val busAbstractionTypeNameRecord = DataRecord(Some(""), Some("name"),name)
    val busAbstractionTypeVersionRecord = DataRecord(Some(""), Some("version"), version)
    val busAbstractionAttributes = Map("vendor" -> busAbstractionTypeVendorRecord, "library" -> busAbstractionTypeLibraryRecord, "name" -> busAbstractionTypeNameRecord , "version" ->busAbstractionTypeVersionRecord)
    val configurableLibraryRefType = ConfigurableLibraryRefType(attributes = busAbstractionAttributes)
    configurableLibraryRefType
  }

  private def getBusInterfaces: BusInterfaces = {
    var busInterfacesSeq: Seq[BusInterfaceType] = Seq()
    for (port <- allPorts) {
      val portParentChain = port.getRefOwnersChain()
      breakable {
        for (portParent <- portParentChain) {
          portParent match {
            case iMasterSlaveParent: IMasterSlave with Data =>
              val parentName = iMasterSlaveParent.name
              val isMaster = iMasterSlaveParent.isMasterInterface
              val busDefinitionName = iMasterSlaveParent match {
                case stream: Stream[_] =>
                  s"Stream_${stream.payload.getClass.getSimpleName}"
                case flow: Flow[_] =>
                  s"Flow_${flow.payload.getClass.getSimpleName}"
                case _ => iMasterSlaveParent.getClass.getSimpleName
              }
              val busName = busDefinitionName + "_" + parentName
              val busInterfaceMode = if (isMaster) {
                "ipxact:initiator"
              } else {
                "ipxact:target"
              }
              if (!busTypeStringSet.contains(busDefinitionName)) {
                IPXACT2022AbstractionDefinitionGenerator.generate(toplevelName = toplevelName, bus = iMasterSlaveParent,version=version)
                IPXACT2022BusDefinitionGenerator.generate(toplevelName = toplevelName, bus = iMasterSlaveParent,version = version)
                busTypeStringSet = busTypeStringSet + busDefinitionName
              }
              if (!busStringSet.contains(busName)) {
                val physicalSignalList = iMasterSlaveParent.flatten
                val logicalSignalList = iMasterSlaveParent.flattenLocalName
                var portMapSeq: Seq[PortMap] = Seq()
                for (signal <- physicalSignalList) {
                  if (allPorts.contains(signal)) {
                    val index = physicalSignalList.indexOf(signal)
                    val signalPhysicalName = signal.name
                    val signalLogicalName = logicalSignalList(index)
                    val logicalPort = LogicalPort(signalLogicalName)
                    val physicalPort = PhysicalPort(signalPhysicalName)
                    val physicalPortRecord = DataRecord(Some(""), Some("ipxact:physicalPort"), physicalPort)
                    val portMap = PortMap(logicalPort = logicalPort, portmapoption = physicalPortRecord)
                    portMapSeq = portMapSeq :+ portMap
                  }
                }
                val portMaps = PortMaps(portMapSeq)
                val abstractionRef=createConfigurableLibraryRefType(busDefinitionName + ".absDef")
                val busType=createConfigurableLibraryRefType(busDefinitionName)
                val busAbstractionType = AbstractionType(abstractionRef = abstractionRef, portMaps = Some(portMaps))
                val busAbstractionTypes = AbstractionTypes(Seq(busAbstractionType))
                val interfaceModeOptionRecord = DataRecord(Some(""), Some(busInterfaceMode), "")
                val busInterfaceNameGroupSequence = NameGroupSequence(busName)
                val busInterface = BusInterfaceType(busInterfaceNameGroupSequence, busType, abstractionTypes = Some(busAbstractionTypes), interfaceModeOption4 = interfaceModeOptionRecord)
                busInterfacesSeq = busInterfacesSeq :+ busInterface
                busStringSet += busName
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
    for (port <- inPorts ++ outPorts) {
      val inOutValue =
        if (port.isInput) {
          InValue
        } else if (port.isOutput) {
          OutValue
        } else {
          InoutValue
        }
      val leftUnsignedIntExpression = UnsignedIntExpression((port.getBitsWidth - 1).toString)
      val rightUnsignedIntExpression = UnsignedIntExpression("0")
      val vector1 = Vector4(left = leftUnsignedIntExpression, right = rightUnsignedIntExpression)
      val vectorSeq: Seq[IPXACT2022ScalaCases.Vector4] = Seq(vector1)
      val portVectors = ExtendedVectorsType(vectorSeq)
      val abstractorPortWireType = PortWireType(direction = inOutValue, vectors = Some(portVectors))
      val wireRecord = DataRecord(Some(""), Some("ipxact:wire"), abstractorPortWireType)
      val nameGroupPortSequence = NameGroupPortSequence(port.name)
      val port2 = Port2(nameGroupPortSequence, wireRecord)
      portsSeq = portsSeq :+ port2
    }

    val ports: Option[Ports] = if (portsSeq.nonEmpty) Some(Ports(portsSeq)) else None
    ports
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
    val views: Option[Views] = Some(Views(viewSep))
    views
  }

  private def getInstantiations: Option[Instantiations] = {
    var instantiationsSeq: Seq[IPXACT2022scalaxb.DataRecord[Any]] = Seq()
    val componentNameGroupNMTOKENSequence = NameGroupNMTOKENSequence("verilog_implementation")
    val componentInstantiationLanguage = LanguageType("verilog")
    val componentInstantiationModuleName = Some(module.definitionName)
    val componentInstantiationFileSetRef = FileSetRef("verilogSource")
    val componentInstantiation = ComponentInstantiationType(componentNameGroupNMTOKENSequence, language = Some(componentInstantiationLanguage), moduleName = componentInstantiationModuleName, fileSetRef = Seq(componentInstantiationFileSetRef))
    val componentInstantiationRecord = DataRecord(Some(""), Some("ipxact:componentInstantiation"), componentInstantiation)
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
    val instantiations: Option[Instantiations] = Some(Instantiations(instantiationsSeq))
    instantiations
  }

  private def getModel: ModelType = {
    val instantiations = getInstantiations
    val views = getViews
    val ports = getPorts
    val model = ModelType(views, ports = ports, instantiations = instantiations)
    model
  }

  private def getFileSets: FileSets = {
    val fileName = IpxactURI(module.definitionName + ".v")
    val fileType = FileType(VerilogSource)
    val fileDescription = "Generated by Spinal HDL function:SpinalVerilog()"
    val file = File(fileName, fileType = Seq(fileType), description = Some(fileDescription))
    val fileSetNameGroupSequence = NameGroupSequence("verilogSource")
    val fileSet = FileSetType(fileSetNameGroupSequence, file = Seq(file))
    val fileSetsSeq = Seq(fileSet)
    val fileSets = FileSets(fileSetsSeq)
    fileSets
  }

  private def getVLNV: DocumentNameGroupSequence = {
    val versionedIdentifierSequence11 = VersionedIdentifierSequence(toplevelVendor, toplevelName, moduleDefinitionName, version)
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
    val model = getModel
    val fileSets = getFileSets
    val busInterfaces = getBusInterfaces
    val component = ComponentType(
      documentNameGroupSequence1 = documentNameGroup,
      busInterfaces = Some(busInterfaces),
      model = Some(model),
      fileSets = Some(fileSets)
    )
    val xml: NodeSeq = toXML[ComponentType](component, "ipxact:component", defaultScope)
//    val prettyPrinter = new PrettyPrinter(width = 80, step = 2)
//    val formattedXml: String = prettyPrinter.format(xml.head)
//    println(formattedXml)
    XML.save(filePath, xml.head, "UTF-8", xmlDecl = true, doctype = null)
  }
}

object IPXACT2022ComponentGenerator {
  def generate(toplevelVendor: String = "SpinalHDL",
               toplevelName: String,
               version: String = "1.0",
               module: Component): Unit = {
    val generator = new IPXACT2022ComponentGenerator(toplevelVendor, toplevelName, version, module)
    generator.beginGenerate()
  }
}

