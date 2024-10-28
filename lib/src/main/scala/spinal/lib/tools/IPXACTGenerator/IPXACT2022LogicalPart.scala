package spinal.lib.tools.IPXACTGenerator

import IPXACT2022ScalaCases._
import IPXACT2022scalaxb._
import spinal.core._
import spinal.lib.{IMasterSlave, _}

import java.nio.file.{Files, Paths}
import scala.language.implicitConversions
import scala.util.control.Breaks.{break, breakable}
import scala.xml._

class IPXACT2022LogicalPart {
  var outputSet: Set[BaseType] = Set()
  var inputSet: Set[BaseType] = Set()
  private var busSet: Set[Data] = Set()
  var name: String = ""
  var logicalSignalSet: Set[Data] = Set()

  def createBus(): Unit = {
    for (port <- outputSet ++ inputSet) {
      val parentChain = port.getRefOwnersChain()
      for (parent <- parentChain) {
        parent match {
          case iMasterSlaveParent: IMasterSlave with Data =>
            if (!busSet.contains(iMasterSlaveParent)) {
              busSet += iMasterSlaveParent
              val signalSeq = iMasterSlaveParent.flatten
              for (signal <- signalSeq) {
                if (signal.isInput) {
                  inputSet += signal
                } else {
                  outputSet += signal
                }
              }
            }
          case _ =>
        }
      }
    }
    checkSignal()
  }

  private def checkSignal(): Unit = {
    for (pin <- inputSet ++ outputSet) {
      if (pin.getComponent() == null) {
        val parentChain = pin.getRefOwnersChain()
        for (parent <- parentChain) {
          parent match {
            case iMasterSlaveParent: IMasterSlave with Data =>
              if (busSet.contains(iMasterSlaveParent)) {
                busSet -= iMasterSlaveParent
                val signalSeq = iMasterSlaveParent.flatten
                for (signal <- signalSeq) {
                  if (signal.isInput) {
                    inputSet -= signal
                  } else {
                    outputSet -= signal
                  }
                }
                return
              }
            case _ =>
          }
        }
        if (pin.isInput) {
          inputSet -= pin
        } else {
          outputSet -= pin
        }
      }
    }
  }

  private def createConfigurableLibraryRefType(name: String, toplevelVendor: String = "SpinalHDL", toplevelName: String, version: String = "1.0"): ConfigurableLibraryRefType = {
    val busAbstractionTypeVendorRecord = DataRecord(Some(""), Some("vendor"), toplevelVendor)
    val busAbstractionTypeLibraryRecord = DataRecord(Some(""), Some("library"), toplevelName)
    val busAbstractionTypeNameRecord = DataRecord(Some(""), Some("name"), name)
    val busAbstractionTypeVersionRecord = DataRecord(Some(""), Some("version"), version)
    val busAbstractionAttributes = Map("vendor" -> busAbstractionTypeVendorRecord, "library" -> busAbstractionTypeLibraryRecord, "name" -> busAbstractionTypeNameRecord, "version" -> busAbstractionTypeVersionRecord)
    val configurableLibraryRefType = ConfigurableLibraryRefType(attributes = busAbstractionAttributes)
    configurableLibraryRefType
  }

  private def getBusInterfaces(toplevelVendor: String = "SpinalHDL", toplevelName: String, version: String = "1.0", module: Component): BusInterfaces = {
    var busStringSet: Set[String] = Set()
    var busInterfacesSeq: Seq[BusInterfaceType] = Seq()
    val allPorts = inputSet ++ outputSet
    for (port <- allPorts) {
      val portComponent = port.getComponent()
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
              val busInterfaceMode = if ((isMaster && portComponent == module) || ((!isMaster) && portComponent != module)) {
                "ipxact:initiator"
              } else {
                "ipxact:target"
              }
              if (!busStringSet.contains(busName)) {
                val physicalSignalList = iMasterSlaveParent.flatten
                val logicalSignalList = iMasterSlaveParent.flattenLocalName
                var portMapSeq: Seq[PortMap] = Seq()
                for (signal <- physicalSignalList) {
                  if (allPorts.contains(signal)) {
                    val index = physicalSignalList.indexOf(signal)
                    val signalComponentName = Option(signal.getComponent()).map(_.name).getOrElse("null")
                    val signalPhysicalName = signalComponentName + "_" + signal.name
                    val signalLogicalName = logicalSignalList(index)
                    val logicalPort = LogicalPort(signalLogicalName)
                    val physicalPort = PhysicalPort(signalPhysicalName)
                    val physicalPortRecord = DataRecord(Some(""), Some("ipxact:physicalPort"), physicalPort)
                    val portMap = PortMap(logicalPort = logicalPort, portmapoption = physicalPortRecord)
                    portMapSeq = portMapSeq :+ portMap
                  }
                }
                val portMaps = PortMaps(portMapSeq)
                val abstractionRef = createConfigurableLibraryRefType(busDefinitionName + ".absDef", toplevelVendor, toplevelName, version)
                val busType = createConfigurableLibraryRefType(busDefinitionName, toplevelVendor, toplevelName, version)
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

  private def getPorts(module: Component): Option[Ports] = {
    var portsSeq: Seq[Port2] = Seq()
    for (pin <- inputSet ++ outputSet) {
      val pinComponent = pin.getComponent()
      val pinComponentName = Option(pinComponent).map(_.name).getOrElse("null")
      val inOutValue =
        if ((pin.isInput && pinComponent == module) || (pin.isOutput && pinComponent != module)) {
          InValue
        } else if ((pin.isOutput && pinComponent == module) || (pin.isInput && pinComponent != module)) {
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
      val nameGroupPortSequence = NameGroupPortSequence(pinComponentName + "_" + pin.name)
      val port = Port2(nameGroupPortSequence, wireRecord)
      portsSeq = portsSeq :+ port
    }
    val ports: Option[Ports] = if (portsSeq.nonEmpty) Some(Ports(portsSeq)) else None
    ports
  }

  private def getModel(module: Component): ModelType = {
    val ports = getPorts(module)
    val model = ModelType(ports = ports)
    model
  }

  def generateIPXACTComponent(toplevelVendor: String = "SpinalHDL",
                              toplevelName: String,
                              version: String = "1.0",
                              module: Component,
                              generatePath: String): Unit = {
    val fileDirectory = s"$generatePath/IPXACT/$toplevelVendor/$toplevelName/$name/$version/"
    val filePath = s"$fileDirectory$name.$version.xml"
    Files.createDirectories(Paths.get(fileDirectory))
    val versionedIdentifierSequence11 = VersionedIdentifierSequence(toplevelVendor, toplevelName, name, version)
    val documentNameGroup = DocumentNameGroupSequence(versionedIdentifierSequence1 = versionedIdentifierSequence11)
    val model = getModel(module)
    val busInterfaces = getBusInterfaces(toplevelVendor, toplevelName, version, module)
    val component = ComponentType(
      documentNameGroupSequence1 = documentNameGroup,
      busInterfaces = Some(busInterfaces),
      model = Some(model)
    )
    val xml: NodeSeq = toXML[ComponentType](component, "ipxact:component", defaultScope)
    //    val prettyPrinter = new PrettyPrinter(width = 80, step = 2)
    //    val formattedXml: String = prettyPrinter.format(xml.head)
    //    println(formattedXml)
    XML.save(filePath, xml.head, "UTF-8", xmlDecl = true, doctype = null)
  }
}
