package spinal.lib.tools.IPXACTGenerator

import IPXACT2022ScalaCases._
import IPXACT2022scalaxb._
import spinal.core._
import spinal.core.tools.{DataAnalyzer, ModuleAnalyzer}
import spinal.lib._

import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.util.control.Breaks.{break, breakable}
import scala.xml._

class IPXACT2022DesignXMLGenerator(toplevelVendor: String = "SpinalHDL", toplevelName: String, version: String = "1.0", module: Component, generatePath: String = "./") {
  private val moduleDefinitionName = module.definitionName
  private var busConnectionStringSet: Set[String] = Set()
  private var logicalPartSet: Set[IPXACT2022LogicalPart] = Set()

  //return (isBus,isInitiator,name,busDefinitionName)
  private def getPinInformation(pin: BaseType): (Boolean, Boolean, String, String) = {
    val pinComponent = pin.getComponent()
    val pinParentChain = pin.getRefOwnersChain()
    for (pinParent <- pinParentChain) {
      pinParent match {
        case iMasterSlaveParent: IMasterSlave with Data =>
          val parentName = iMasterSlaveParent.name
          val isInitiator = if ((iMasterSlaveParent.isSlaveInterface && pinComponent == module) || (iMasterSlaveParent.isMasterInterface && pinComponent != module)) {
            true
          } else {
            false
          }
          val busDefinitionName = iMasterSlaveParent match {
            case stream: Stream[_] =>
              s"Stream_${stream.payload.getClass.getSimpleName}"
            case flow: Flow[_] =>
              s"Flow_${flow.payload.getClass.getSimpleName}"
            case _ => iMasterSlaveParent.getClass.getSimpleName
          }
          val busName = busDefinitionName + "_" + parentName
          return (true, isInitiator, busName, busDefinitionName)
        case _ =>
      }
    }
    val pinName = pin.getName()
    val isInitiator = if ((pin.isInput && pinComponent == module) || (pin.isOutput && pinComponent != module)) {
      true
    } else {
      false
    }
    (false, isInitiator, pinName, "")
  }

  private def createActiveInterface(pinComponentName: String, busName: String): ActiveInterface = {
    val componentRefRecord = DataRecord(Some(""), Some("componentInstanceRef"), pinComponentName)
    val busRefRecord = DataRecord(Some(""), Some("busRef"), busName)
    val activeInterfaceMap = Map("busRef" -> busRefRecord, "componentInstanceRef" -> componentRefRecord)
    val pinActiveInterface = ActiveInterface(attributes = activeInterfaceMap)
    pinActiveInterface
  }

  private def createInterconnectionOption(pinComponentName: String, busName: String): Seq[DataRecord[InterconnectionOption]] = {
    val interconnectionSequence = if (pinComponentName == module.name) {
      val hierInterface = createHierInterfaceType(busName)
      InterconnectionSequence1(hierInterface = Seq(hierInterface))
    } else {
      val activeInterface = createActiveInterface(pinComponentName, busName)
      InterconnectionSequence1(activeInterface = Seq(activeInterface))
    }
    val interconnectionSequenceRecord = DataRecord(Some(""), Some(""), interconnectionSequence)
    Seq(interconnectionSequenceRecord)
  }

  private def createHierInterfaceType(busName: String): HierInterfaceType = {
    val busRefRecord = DataRecord(Some(""), Some("busRef"), busName)
    val hierInterfaceMap = Map("busRef" -> busRefRecord)
    val hierInterface = HierInterfaceType(attributes = hierInterfaceMap)
    hierInterface
  }

  private def createComponentInstances: ComponentInstances = {
    //ComponentInstances
    var designComponentInstancesSeq: Seq[ComponentInstance] = Seq()
    for (childrenComponent <- module.children) {
      val vendorRecord = DataRecord(Some(""), Some("vendor"), toplevelVendor)
      val libraryRecord = DataRecord(Some(""), Some("library"), toplevelName)
      val nameRecord = DataRecord(Some(""), Some("name"), childrenComponent.definitionName)
      val versionRecord = DataRecord(Some(""), Some("version"), version)
      val configurableLibraryRefTypeAttributes = Map("vendor" -> vendorRecord, "library" -> libraryRecord, "name" -> nameRecord, "version" -> versionRecord)
      val configurableLibraryRefType = ConfigurableLibraryRefType(attributes = configurableLibraryRefTypeAttributes)
      val designComponentInstance = ComponentInstance(childrenComponent.getName(), componentRef = configurableLibraryRefType)
      designComponentInstancesSeq = designComponentInstancesSeq :+ designComponentInstance
    }
    for (logicalPart <- logicalPartSet) {
      val vendorRecord = DataRecord(Some(""), Some("vendor"), toplevelVendor)
      val libraryRecord = DataRecord(Some(""), Some("library"), toplevelName)
      val nameRecord = DataRecord(Some(""), Some("name"), logicalPart.name)
      val versionRecord = DataRecord(Some(""), Some("version"), version)
      val configurableLibraryRefTypeAttributes = Map("vendor" -> vendorRecord, "library" -> libraryRecord, "name" -> nameRecord, "version" -> versionRecord)
      val configurableLibraryRefType = ConfigurableLibraryRefType(attributes = configurableLibraryRefTypeAttributes)
      val designComponentInstance = ComponentInstance(logicalPart.name, componentRef = configurableLibraryRefType)
      designComponentInstancesSeq = designComponentInstancesSeq :+ designComponentInstance
    }
    ComponentInstances(designComponentInstancesSeq)
  }

   private def fillLogicalPart(fanOuts: mutable.LinkedHashSet[BaseType], logicalPart: IPXACT2022LogicalPart, pinComponent: Component, allComponent: ArrayBuffer[Component], allPins: mutable.LinkedHashSet[BaseType]): IPXACT2022LogicalPart = {
    var currentLogicalPart = logicalPart
    for (fanOut <- fanOuts if !currentLogicalPart.logicalSignalSet.contains(fanOut) && allComponent.contains(fanOut.getComponent())) {
      if (fanOut.getComponent() == pinComponent && !allPins.contains(fanOut)) {
        currentLogicalPart.logicalSignalSet += fanOut
        val fanOutAnalyzer = new DataAnalyzer(fanOut)
        val allFanOuts = fanOutAnalyzer.getFanOut
        currentLogicalPart = fillLogicalPart(allFanOuts, currentLogicalPart, pinComponent, allComponent,allPins)
      } else {
        currentLogicalPart.outputSet += fanOut
      }
    }
    currentLogicalPart
  }

  private def createLogicalPart: Set[IPXACT2022LogicalPart] = {
    val moduleAnalyzer = new ModuleAnalyzer(module)
    val childrenComponent = module.children
    val allInputs = moduleAnalyzer.getInputs
    val allComponent = childrenComponent :+ module
    val allInnerOutputs = moduleAnalyzer.getPins(pin => pin.isOutput && pin.getComponent().name != toplevelName && childrenComponent.contains(pin.getComponent()))
    val allPins = moduleAnalyzer.getPins(_ => true)
    for (pin <- allInputs ++ allInnerOutputs) {
      val pinComponent = pin.getComponent()
      val pinAnalyzer = new DataAnalyzer(pin)
      //      val validFanOuts = pinAnalyzer.getFanOut(otherComponentPin => otherComponentPin.getComponent() != pinComponent && allPins.contains(otherComponentPin) && allComponent.contains(otherComponentPin.getComponent()))
      val fanOuts = pinAnalyzer.getFanOut(fanOut => fanOut.getComponent()== pinComponent && !allPins.contains(fanOut))
      if (fanOuts.nonEmpty) {
        var logicalPart = new IPXACT2022LogicalPart()
        logicalPart.inputSet += pin
        logicalPart = fillLogicalPart(fanOuts, logicalPart, pinComponent, allComponent,allPins)
        logicalPartSet += logicalPart
      }
    }
    logicalPartSet = mergeLogicalPart(logicalPartSet)
    var index = 0
    for (logicalPart <- logicalPartSet) {
      logicalPart.name = module.definitionName+"_logicalPart" + index.toString
      logicalPart.createBus()
      index += 1
    }
    logicalPartSet
  }

  private def mergeLogicalPartTogether(newLogicalPart:IPXACT2022LogicalPart, otherLogicalPart:IPXACT2022LogicalPart):IPXACT2022LogicalPart={
    newLogicalPart.logicalSignalSet ++= otherLogicalPart.logicalSignalSet
    newLogicalPart.inputSet ++= otherLogicalPart.inputSet
    newLogicalPart.outputSet ++= otherLogicalPart.outputSet
    newLogicalPart
  }

  private def mergeLogicalPart(logicalPartSet: Set[IPXACT2022LogicalPart]): Set[IPXACT2022LogicalPart] = {
    var oldLogicalPartSet = logicalPartSet
    var newLogicalPartSet: Set[IPXACT2022LogicalPart] = Set()
    while (oldLogicalPartSet.nonEmpty) {
      val logicalPart = oldLogicalPartSet.head
      val newLogicalPart = logicalPart
      val toMerge = oldLogicalPartSet.filter(_ != logicalPart)
      for (otherLogicalPart <- toMerge) {
        breakable {
          for (otherLogicalSignal <- otherLogicalPart.logicalSignalSet) {
            if (newLogicalPart.logicalSignalSet.contains(otherLogicalSignal)) {
              mergeLogicalPartTogether(newLogicalPart,otherLogicalPart)
              oldLogicalPartSet -= otherLogicalPart
              break
            }
          }
          for(otherInput<-otherLogicalPart.inputSet){
            val (_,_,otherInputName,_)=getPinInformation(otherInput)
            for(input<-newLogicalPart.inputSet){
              val (_,_,inputName,_)=getPinInformation(input)
              if(inputName==otherInputName){
                mergeLogicalPartTogether(newLogicalPart,otherLogicalPart)
                oldLogicalPartSet -= otherLogicalPart
                break
              }
            }
          }
          for(otherInput<-otherLogicalPart.outputSet) {
            val (_, _, otherOutputName, _) = getPinInformation(otherInput)
            for (output <- newLogicalPart.outputSet) {
              val (_, _, outputName, _) = getPinInformation(output)
              if (outputName == otherOutputName) {
                mergeLogicalPartTogether(newLogicalPart, otherLogicalPart)
                oldLogicalPartSet -= otherLogicalPart
                break
              }
            }
          }
        }
      }
      newLogicalPartSet += newLogicalPart
      oldLogicalPartSet -= logicalPart
    }
    newLogicalPartSet
  }

  private def createConnections: (AdHocConnections, Interconnections) = {
    //adHocConnections
    val moduleAnalyzer = new ModuleAnalyzer(module)
    val childrenComponent = module.children
    val allComponent = childrenComponent :+ module
    var adHocConnectionSeq: Seq[AdHocConnection] = Seq()
    var InterconnectionSeq: Seq[DataRecord[InterconnectionsOption]] = Seq()
    val allInputs = moduleAnalyzer.getInputs
    val allInnerOutputs = moduleAnalyzer.getPins(pin => pin.isOutput && pin.getComponent().name != toplevelName && childrenComponent.contains(pin.getComponent()))
    val allPins = moduleAnalyzer.getPins(_ => true)
    for (pin <- allInputs ++ allInnerOutputs if pin.getComponent()!=null) {
      val pinComponent = pin.getComponent()
      val pinComponentName=Option(pinComponent).map(_.name).getOrElse("null")
      val pinAnalyzer = new DataAnalyzer(pin)
      val (pinIsBus, pinIsInitiator, pinName, pinBusDefinitionName) = getPinInformation(pin)
      val validFanOuts = pinAnalyzer.getFanOut(otherComponentPin => otherComponentPin.getComponent() != pinComponent && allPins.contains(otherComponentPin) && allComponent.contains(otherComponentPin.getComponent()))
      for (fanOut <- validFanOuts) {

        val fanOutComponent = fanOut.getComponent()
        val (fanOutIsBus, fanOutIsInitiator, fanOutName, fanOutBusDefinitionName) = getPinInformation(fanOut)

        if (pinIsBus && fanOutIsBus && (pinIsInitiator ^ fanOutIsInitiator) && pinBusDefinitionName == fanOutBusDefinitionName) {
          val completePinName = if (pinComponent == module) {
            pinName
          } else {
            pinComponentName + "_" + pinName
          }
          val completeFanOutName = if (fanOutComponent == module) {
            fanOutName
          } else {
            fanOutComponent.name + "_" + fanOutName
          }
          val interconnectionName = if (pinIsInitiator) {
            completePinName + "_to_" + completeFanOutName
          }
          else {
            completeFanOutName + "_to_" + completePinName
          }
          if (!busConnectionStringSet.contains(interconnectionName)) {
            busConnectionStringSet += interconnectionName
            val activeInterface = if (pinComponent != module) {
              createActiveInterface(pinComponent.name, pinName)
            } else {
              createActiveInterface(fanOutComponent.name, fanOutName)
            }
            val interconnectionOption = if (pinComponent != module) {
              createInterconnectionOption(fanOutComponent.name, fanOutName)
            } else {
              createInterconnectionOption(pinComponent.name, pinName)
            }
            val interconnectionNameGroupSequence = NameGroupSequence(interconnectionName)
            val interconnection = Interconnection(interconnectionNameGroupSequence, activeInterface, interconnectionOption)
            val interconnectionRecord = DataRecord(Some(""), Some("ipxact:interconnection"), interconnection)
            InterconnectionSeq :+= interconnectionRecord
          }
        } else if ((!pinIsBus) && (!fanOutIsBus) && (pinIsInitiator ^ fanOutIsInitiator)) {
          var internalPortReferenceSeq: Seq[InternalPortReference] = Seq()
          var externalPortReferenceSeq: Seq[ExternalPortReference] = Seq()
          if (module != pinComponent) {
            val componentInstanceRefRecord = DataRecord(Some(""), Some("componentInstanceRef"),pinComponentName)
            val portRefRecord = DataRecord(Some(""), Some("portRef"), pin.getName())
            val internalPortReferenceAttributes = Map("componentInstanceRef" -> componentInstanceRefRecord, "portRef" -> portRefRecord)
            val internalPortReference = InternalPortReference(attributes = internalPortReferenceAttributes)
            internalPortReferenceSeq = internalPortReferenceSeq :+ internalPortReference
          }
          else {
            val portRefRecord = DataRecord(Some(""), Some("portRef"), pin.getName())
            val externalPortReferenceAttributes = Map("portRef" -> portRefRecord)
            val externalPortReference = ExternalPortReference(attributes = externalPortReferenceAttributes)
            externalPortReferenceSeq = externalPortReferenceSeq :+ externalPortReference
          }
          if (fanOutComponent != module) {
            val componentInstanceRefRecord = DataRecord(Some(""), Some("componentInstanceRef"), fanOut.getComponent().getName())
            val portRefRecord = DataRecord(Some(""), Some("portRef"), fanOut.getName())
            val internalPortReferenceAttributes = Map("componentInstanceRef" -> componentInstanceRefRecord, "portRef" -> portRefRecord)
            val internalPortReference = InternalPortReference(attributes = internalPortReferenceAttributes)
            internalPortReferenceSeq = internalPortReferenceSeq :+ internalPortReference
          }
          else {
            val portRefRecord = DataRecord(Some(""), Some("portRef"), fanOut.getName())
            val externalPortReferenceAttributes = Map("portRef" -> portRefRecord)
            val externalPortReference = ExternalPortReference(attributes = externalPortReferenceAttributes)
            externalPortReferenceSeq = externalPortReferenceSeq :+ externalPortReference
          }
          val portReferencesSequence = PortReferencesSequence1(internalPortReference = internalPortReferenceSeq, externalPortReference = externalPortReferenceSeq)
          val portReferencesSequenceRecord = DataRecord(Some(""), Some(""), portReferencesSequence)
          val adHocConnectionPortReferences = PortReferences(Seq(portReferencesSequenceRecord))
          val sourceName = pinComponentName + "_" + pin.getName()
          val targetName = fanOutComponent.name + "_" + fanOut.getName()
          val adHocConnectionName = sourceName + "_to_" + targetName
          val adHocConnectionNameGroupPortSequence = NameGroupPortSequence(adHocConnectionName)
          val adHocConnection = AdHocConnection(adHocConnectionNameGroupPortSequence, portReferences = adHocConnectionPortReferences)
          adHocConnectionSeq = adHocConnectionSeq :+ adHocConnection
        }
      }
    }
    for (logicalPart <- logicalPartSet) {
      for (pin <- logicalPart.inputSet ++ logicalPart.outputSet if pin.getComponent()!=null) {
        val pinComponent = pin.getComponent()
        val pinComponentName=Option(pinComponent).map(_.name).getOrElse("null")
        val (pinIsBus, pinIsInitiator, pinName, _) = getPinInformation(pin)
        if (pinIsBus) {
          val busConnectionName = if (pinComponent == module) {
            if (pinIsInitiator) {
              pinName + "_to_" + logicalPart.name + "_" + pinName
            }
            else {
              logicalPart.name + "_" + pinName + "_to_" + pinName
            }
          } else {
            if (pinIsInitiator) {
              pinComponent+"_" + pinName + "_to_" + logicalPart.name + "_" + pinName
            } else {
              logicalPart.name + "_" + pinName + "_to_" + pinComponentName + "_" + pinName
            }
          }
          if (!busConnectionStringSet.contains(busConnectionName)) {
            busConnectionStringSet += busConnectionName
            val activeInterface = createActiveInterface(logicalPart.name , pinName)
            val interconnectionOption = createInterconnectionOption(pinComponentName, pinName)
            val interconnectionNameGroupSequence = NameGroupSequence(busConnectionName)
            val interconnection = Interconnection(interconnectionNameGroupSequence, activeInterface, interconnectionOption)
            val interconnectionRecord = DataRecord(Some(""), Some("ipxact:interconnection"), interconnection)
            InterconnectionSeq :+= interconnectionRecord
          }
        } else {
          var internalPortReferenceSeq: Seq[InternalPortReference] = Seq()
          var externalPortReferenceSeq: Seq[ExternalPortReference] = Seq()
          if (module != pinComponent) {
            val componentInstanceRefRecord = DataRecord(Some(""), Some("componentInstanceRef"), pin.getComponent().getName())
            val portRefRecord = DataRecord(Some(""), Some("portRef"), pin.getName())
            val internalPortReferenceAttributes = Map("componentInstanceRef" -> componentInstanceRefRecord, "portRef" -> portRefRecord)
            val internalPortReference = InternalPortReference(attributes = internalPortReferenceAttributes)
            internalPortReferenceSeq = internalPortReferenceSeq :+ internalPortReference
          }
          else {
            val portRefRecord = DataRecord(Some(""), Some("portRef"), pin.getName())
            val externalPortReferenceAttributes = Map("portRef" -> portRefRecord)
            val externalPortReference = ExternalPortReference(attributes = externalPortReferenceAttributes)
            externalPortReferenceSeq = externalPortReferenceSeq :+ externalPortReference
          }
          val componentInstanceRefRecord = DataRecord(Some(""), Some("componentInstanceRef"), logicalPart.name)
          val portRefRecord = DataRecord(Some(""), Some("portRef"), pin.getComponent().name+"_"+pin.getName())
          val internalPortReferenceAttributes = Map("componentInstanceRef" -> componentInstanceRefRecord, "portRef" -> portRefRecord)
          val internalPortReference = InternalPortReference(attributes = internalPortReferenceAttributes)
          internalPortReferenceSeq = internalPortReferenceSeq :+ internalPortReference

          val portReferencesSequence = PortReferencesSequence1(internalPortReference = internalPortReferenceSeq, externalPortReference = externalPortReferenceSeq)
          val portReferencesSequenceRecord = DataRecord(Some(""), Some(""), portReferencesSequence)
          val adHocConnectionPortReferences = PortReferences(Seq(portReferencesSequenceRecord))
          val sourceName = pinComponentName + "_" + pin.getName()
          val targetName = logicalPart.name + "_" + pin.getName()
          val adHocConnectionName = sourceName + "_to_" + targetName
          val adHocConnectionNameGroupPortSequence = NameGroupPortSequence(adHocConnectionName)
          val adHocConnection = AdHocConnection(adHocConnectionNameGroupPortSequence, portReferences = adHocConnectionPortReferences)
          adHocConnectionSeq = adHocConnectionSeq :+ adHocConnection
        }
      }
    }
    val designAdHocConnections = AdHocConnections(adHocConnectionSeq)
    val designInterconnections = Interconnections(InterconnectionSeq)
    (designAdHocConnections, designInterconnections)

  }

  private def createLogicalPartComponent(): Unit = {
    for (logicalPart <- logicalPartSet) {
      logicalPart.generateIPXACTComponent(toplevelVendor, toplevelName, version, module,generatePath)
    }
  }

  private def createDesign: Design = {
    logicalPartSet = createLogicalPart
    val (designAdHocConnections, designInterconnections) = createConnections
    createLogicalPartComponent()
    val designVersionedIdentifierSequence = VersionedIdentifierSequence(toplevelVendor, toplevelName, moduleDefinitionName + ".design", version)
    val designDocumentNameGroupSequence = DocumentNameGroupSequence(designVersionedIdentifierSequence)
    val designComponentInstances = createComponentInstances
    val design = Design(designDocumentNameGroupSequence, componentInstances = Some(designComponentInstances), adHocConnections = Some(designAdHocConnections), interconnections = Some(designInterconnections))
    design
  }

  def beginGenerate(): Unit = {

    val fileDirectory = s"$generatePath/IPXACT/$toplevelVendor/$toplevelName/$moduleDefinitionName/$version/"
    val filePath = s"$fileDirectory$moduleDefinitionName.design.$version.xml"
    if (module.children.nonEmpty) {
      Files.createDirectories(Paths.get(fileDirectory))
      val design = createDesign
      val xml: NodeSeq = toXML[Design](design, "ipxact:design", defaultScope)
      //      val prettyPrinter = new PrettyPrinter(width = 80, step = 2)
      //      val formattedXml: String = prettyPrinter.format(xml.head)
      //      println(formattedXml)
      //      Files.write(Paths.get(filePath), formattedXml.getBytes("UTF-8"))
      XML.save(filePath, xml.head, "UTF-8", xmlDecl = true, doctype = null)
    }
  }
}

object IPXACT2022DesignXMLGenerator {
  def generate(Vendor: String = "SpinalHDL",
               toplevelName: String,
               version: String = "1.0",
               module: Component,
               generatePath: String = "./"): Unit = {
    val generator = new IPXACT2022DesignXMLGenerator(toplevelVendor = Vendor, toplevelName = toplevelName, version = version, module = module, generatePath = generatePath)
    generator.beginGenerate()
  }
}