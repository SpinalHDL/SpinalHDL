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

class IPXACT2022ComponentGenerator(toplevelVendor: String = "SpinalHDL", toplevelName: String, version: String = "1.0", module: Component, generateDesign: Boolean = false, generatePath: String = "./", fileType: String = "Verilog") {

  private val moduleDefinitionName = module.definitionName
  private var busTypeStringSet: Set[String] = Set()
  private val moduleAnalyzer = new ModuleAnalyzer(module)
  private val inPorts = moduleAnalyzer.getInputs(_ => true)
  private val outPorts = moduleAnalyzer.getOutputs(_ => true)
  private val allPorts = inPorts ++ outPorts

  private def createConfigurableLibraryRefType(name: String): ConfigurableLibraryRefType = {
    val busAbstractionTypeVendorRecord = DataRecord(Some(""), Some("vendor"), toplevelVendor)
    val busAbstractionTypeLibraryRecord = DataRecord(Some(""), Some("library"), toplevelName)
    val busAbstractionTypeNameRecord = DataRecord(Some(""), Some("name"), name)
    val busAbstractionTypeVersionRecord = DataRecord(Some(""), Some("version"), version)
    val busAbstractionAttributes = Map("vendor" -> busAbstractionTypeVendorRecord, "library" -> busAbstractionTypeLibraryRecord, "name" -> busAbstractionTypeNameRecord, "version" -> busAbstractionTypeVersionRecord)
    val configurableLibraryRefType = ConfigurableLibraryRefType(attributes = busAbstractionAttributes)
    configurableLibraryRefType
  }

  private def getBusInterfaces: BusInterfaces = {
    var busStringSet: Set[String] = Set()
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
                IPXACT2022AbstractionDefinitionGenerator.generate(toplevelName = toplevelName, bus = iMasterSlaveParent, version = version, generatePath = generatePath)
                IPXACT2022BusDefinitionGenerator.generate(toplevelName = toplevelName, bus = iMasterSlaveParent, version = version, generatePath = generatePath)
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
                val abstractionRef = createConfigurableLibraryRefType(busDefinitionName + ".absDef")
                val busType = createConfigurableLibraryRefType(busDefinitionName)
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
    for (pin <- inPorts ++ outPorts) {
      val inOutValue =
        if (pin.isInput) {
          InValue
        } else if (pin.isOutput) {
          OutValue
        } else {
          InoutValue
        }
      val leftUnsignedIntExpression = UnsignedIntExpression((pin.getBitsWidth - 1).toString)
      val rightUnsignedIntExpression = UnsignedIntExpression("0")
      val vector = Vector4(left = leftUnsignedIntExpression, right = rightUnsignedIntExpression)
      val vectorSeq: Seq[IPXACT2022ScalaCases.Vector4] = Seq(vector)
      val portVectors = ExtendedVectorsType(vectorSeq)
      val abstractorPortWireType = PortWireType(direction = inOutValue, vectors = Some(portVectors))
      val wireRecord = DataRecord(Some(""), Some("ipxact:wire"), abstractorPortWireType)
      val nameGroupPortSequence = NameGroupPortSequence(pin.name)
      val port = Port2(nameGroupPortSequence, wireRecord)
      portsSeq = portsSeq :+ port
    }

    val ports: Option[Ports] = if (portsSeq.nonEmpty) Some(Ports(portsSeq)) else None
    ports
  }

  private def getViews: Option[Views] = {
    var viewSep: Seq[View] = Seq()
    val componentViewNameGroupNMTOKEN = NameGroupNMTOKENSequence("componentView")
    val componentViewEnvIdentifier = Seq(EnvIdentifier("Spinal"))
    val componentViewComponentInstantiationRef = Some("implementation")
    val componentView = View(componentViewNameGroupNMTOKEN, envIdentifier = componentViewEnvIdentifier, componentInstantiationRef = componentViewComponentInstantiationRef)
    viewSep = viewSep :+ componentView
    if (generateDesign && module.children.nonEmpty) {
      val designConfigRefName = moduleDefinitionName + s".designcfg_$version"
      val designConfigurationViewNameGroupNMTOKEN = NameGroupNMTOKENSequence("designConfigurationView")
      val designConfigurationView = View(designConfigurationViewNameGroupNMTOKEN, designConfigurationInstantiationRef = Some(designConfigRefName))
      viewSep = viewSep :+ designConfigurationView
    }
    val views: Option[Views] = Some(Views(viewSep))
    views
  }

  private def getInstantiations: Option[Instantiations] = {
    var instantiationsSeq: Seq[IPXACT2022scalaxb.DataRecord[Any]] = Seq()
    val componentNameGroupNMTOKENSequence = NameGroupNMTOKENSequence("implementation")
    val (componentInstantiationLanguage, componentInstantiationFileSetRef) = if (fileType == "VHDL") {
      (LanguageType("vhdl"), FileSetRef("vhdlSource"))
    } else {
      (LanguageType("verilog"), FileSetRef("verilogSource"))
    }
    val componentInstantiationModuleName = Some(module.definitionName)
    val componentInstantiation = ComponentInstantiationType(componentNameGroupNMTOKENSequence, language = Some(componentInstantiationLanguage), moduleName = componentInstantiationModuleName, fileSetRef = Seq(componentInstantiationFileSetRef))
    val componentInstantiationRecord = DataRecord(Some(""), Some("ipxact:componentInstantiation"), componentInstantiation)
    instantiationsSeq = instantiationsSeq :+ componentInstantiationRecord
    if (generateDesign && module.children.nonEmpty) {
      val designConfigurationVendorRecord = DataRecord(Some(""), Some("vendor"), toplevelVendor)
      val designConfigurationLibraryRecord = DataRecord(Some(""), Some("library"), toplevelName)
      val designConfigurationNameRecord = DataRecord(Some(""), Some("name"), moduleDefinitionName + ".designcfg")
      val designConfigurationVersionRecord = DataRecord(Some(""), Some("version"), version)
      val designConfigurationBusAbstractionAttributes = Map("vendor" -> designConfigurationVendorRecord, "library" -> designConfigurationLibraryRecord, "name" -> designConfigurationNameRecord, "version" -> designConfigurationVersionRecord)
      val designConfigurableLibraryRefType = ConfigurableLibraryRefType(attributes = designConfigurationBusAbstractionAttributes)
      val designConfigurationNameGroupNMTOKENSequence = NameGroupNMTOKENSequence(moduleDefinitionName + s".designcfg_$version")
      val designConfigurationInstantiation = DesignConfigurationInstantiationType(designConfigurationNameGroupNMTOKENSequence, designConfigurationRef = designConfigurableLibraryRefType)
      val designConfigurationInstantiationRecord = DataRecord(Some(""), Some("ipxact:designConfigurationInstantiation"), designConfigurationInstantiation)
      instantiationsSeq = instantiationsSeq :+ designConfigurationInstantiationRecord
    }
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
    val (fileName, languageType, fileSetNameGroupSequence) = if (fileType == "VHDL") {
      (IpxactURI(module.definitionName + ".vhd"), FileType(VhdlSource), NameGroupSequence("vhdlSource"))
    } else {
      (IpxactURI(module.definitionName + ".v"), FileType(VerilogSource), NameGroupSequence("verilogSource"))
    }
    val fileDescription = "Generated by Spinal HDL"
    val file = File(fileName, fileType = Seq(languageType), description = Some(fileDescription))
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

    if (generateDesign) {
      for (children <- module.children) {
        if (children.children.nonEmpty) {
          IPXACT2022ComponentGenerator.generate(toplevelVendor = toplevelVendor, toplevelName = toplevelName, module = children, version = version, generateDesign = true, generatePath = generatePath, fileType = fileType)
        }
        else {
          IPXACT2022ComponentGenerator.generate(toplevelVendor = toplevelVendor, toplevelName = toplevelName, module = children, version = version, generatePath = generatePath, fileType = fileType)
        }
      }
      IPXACT2022DesignConfigXMLGenerator.generate(Vendor = toplevelVendor, toplevelName = toplevelName, module = module, version = version, generatePath = generatePath)
      IPXACT2022DesignXMLGenerator.generate(Vendor = toplevelVendor, toplevelName = toplevelName, module = module, version = version, generatePath = generatePath)
    }
    val ipxactPath = s"$generatePath/IPXACT/"
    val fileDirectory = s"$ipxactPath$toplevelVendor/$toplevelName/$moduleDefinitionName/$version/"
    val filePath = s"$fileDirectory$moduleDefinitionName.$version.xml"
    Files.createDirectories(Paths.get(fileDirectory))
    val sourceFilePath = if (fileType == "VHDL") {
      s"$generatePath/${module.definitionName}.vhd"
    } else {
      s"$generatePath/${module.definitionName}.v"
    }
    val sourcePath = Paths.get(sourceFilePath)
    val targetPath = Paths.get(fileDirectory + sourcePath.getFileName)
    if (Files.exists(sourcePath)) {
      Files.copy(sourcePath, targetPath, StandardCopyOption.REPLACE_EXISTING)
    }
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
    if (module.definitionName == toplevelName) {
      println(s"Generate 2022 IPXACT at $ipxactPath")
    }
  }
}

object IPXACT2022ComponentGenerator {

  def generate(toplevelVendor: String = "SpinalHDL",
               toplevelName: String,
               version: String = "1.0",
               module: Component,
               generateDesign: Boolean = false,
               generatePath: String,
               fileType: String = "Verilog"): Unit = {
    val generator = new IPXACT2022ComponentGenerator(toplevelVendor, toplevelName, version, module, generateDesign, generatePath = generatePath, fileType = fileType)
    generator.beginGenerate()
  }
}

