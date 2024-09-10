package spinal.lib.tools.IPXACTGenerator

import spinal.core._
import spinal.core.tools.ModuleAnalyzer
import spinal.lib._
import spinal.lib.bus.amba3.ahblite.AhbLite3
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.bus.amba4.apb.Apb4
import spinal.lib.bus.amba4.axi.Axi4
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.amba4.axis.Axi4Stream.Axi4StreamBundle
import spinal.lib.bus.avalon.AvalonMM
import spinal.lib.bus.bram.BRAM
import spinal.lib.com.uart.Uart
import spinal.lib.graphic.vga.Vga

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardCopyOption, StandardOpenOption}
import scala.language.implicitConversions
import scala.util.control.Breaks._
import scala.xml._
import IPXACT2009scalaxb._
import IPXACT2009ScalaCases._
class VivadoComponentGenerator(toplevelVendor: String = "SpinalHDL", toplevelName: String, version: String = "1.0", module: Component) {
  private val moduleDefinitionName = module.definitionName
  private var busStringSet: Set[String] = Set()
  private val moduleAnalyzer = new ModuleAnalyzer(module)
  private val inPorts = moduleAnalyzer.getInputs(_ => true)
  private var busClockMap: Map[String, String] = Map()
  private val outPorts = moduleAnalyzer.getOutputs(_ => true)
  private val allPorts = inPorts ++ outPorts
  private val versionSuffix = version.split("\\.").mkString("_")


  private def appendBusElement[T <: Data](thisBus: T): Unit = {
    busStringSet = busStringSet + thisBus.name
    busClockMap = busClockMap + (thisBus.name -> thisBus.flatten.head.clockDomain.clock.name)
  }

  private def getBusInterfaces: Option[BusInterfaces] = {
    var busInterfacesSeq: Seq[BusInterfaceType] = Seq()
    val allClocks = moduleAnalyzer.getClocks(_ => true)
    var clockNameSet: Set[String] = Set()
    var clockSet: Set[ClockDomain] = Set()
    for (thisClock <- allClocks) {
      val thisClockName = thisClock.clock.name
      if (!clockNameSet.contains(thisClockName)) {
        clockNameSet += thisClockName
        clockSet = clockSet + thisClock
      }
    }
    for (thisPort <- allPorts) {
      val parentChain = thisPort.getRefOwnersChain()
      breakable {
        for (parentElement <- parentChain) {
          parentElement match {
            case busStream: Stream[_] =>
              busStream.payload match {
                case _: Axi4StreamBundle =>
                  if (!busStringSet.contains(busStream.name)) {
                    busInterfacesSeq = busInterfacesSeq :+ VivadoBusReference.referenceAxis4(busStream.asInstanceOf[Stream[Axi4StreamBundle]])
                    appendBusElement(busStream)
                    break
                  }
//                case normalType if normalType.isInstanceOf[Bool]
//                  || normalType.isInstanceOf[UInt] ||
//                  normalType.isInstanceOf[SInt] ||
//                  normalType.isInstanceOf[Bits] ||
//                  normalType.isInstanceOf[UFix] ||
//                  normalType.isInstanceOf[SFix] ||
//                  normalType.isInstanceOf[AFix] =>
//                  if (!busStringSet.contains(busStream.name)) {
//                    busInterfacesSeq = busInterfacesSeq :+ VivadoBusReference.referenceNormalStream(busStream)
//                    appendBusElement(busStream)
//                    break
//                  }
                case _ =>
              }
            //            case busFlow: Flow[_] =>
            //              busFlow.payload match {
            //                case normalType if normalType.isInstanceOf[Bool]
            //                  || normalType.isInstanceOf[UInt] ||
            //                  normalType.isInstanceOf[SInt] ||
            //                  normalType.isInstanceOf[Bits] ||
            //                  normalType.isInstanceOf[UFix] ||
            //                  normalType.isInstanceOf[SFix] ||
            //                  normalType.isInstanceOf[AFix] =>
            //                  if (!busStringSet.contains(busFlow.name)) {
            //                    busInterfacesSeq = busInterfacesSeq :+ vivadoBusReference.referenceNormalFlow(busFlow)
            //                    appendBusElement(busFlow)
            //                    break
            //                  }
            //                case _ =>
            //              }
            case busAxi4: Axi4 =>
              if (!busStringSet.contains(busAxi4.name)) {
                busInterfacesSeq = busInterfacesSeq :+ VivadoBusReference.referenceAxi4(busAxi4)
                appendBusElement(busAxi4)
                break
              }
            case axiLite4: AxiLite4 =>
              if (!busStringSet.contains(axiLite4.name)) {
                busInterfacesSeq = busInterfacesSeq :+ VivadoBusReference.referenceAxiLite4(axiLite4)
                appendBusElement(axiLite4)
                break
              }
            case bram: BRAM =>
              if (!busStringSet.contains(bram.name)) {
                busInterfacesSeq = busInterfacesSeq :+ VivadoBusReference.referenceBRAM(bram)
                appendBusElement(bram)
                break
              }
            case vga: Vga =>
              if (!busStringSet.contains(vga.name)) {
                busInterfacesSeq = busInterfacesSeq :+ VivadoBusReference.referenceVga(vga)
                appendBusElement(vga)
                break
              }
            case uart: Uart =>
              if (!busStringSet.contains(uart.name)) {
                busInterfacesSeq = busInterfacesSeq :+ VivadoBusReference.referenceUART(uart)
                appendBusElement(uart)
                break
              }
            case busApb3: Apb3 =>
              if (!busStringSet.contains(busApb3.name)) {
                busInterfacesSeq = busInterfacesSeq :+ VivadoBusReference.referenceApb3(busApb3)
                appendBusElement(busApb3)
                break
              }
            case busApb4: Apb4 =>
              if (!busStringSet.contains(busApb4.name)) {
                busInterfacesSeq = busInterfacesSeq :+ VivadoBusReference.referenceApb4(busApb4)
                appendBusElement(busApb4)
                break
              }
            case ahbLite3: AhbLite3 =>
              if (!busStringSet.contains(ahbLite3.name)) {
                busInterfacesSeq = busInterfacesSeq :+ VivadoBusReference.referenceAhbLite3(ahbLite3)
                appendBusElement(ahbLite3)
                break
              }
            case avalonMM: AvalonMM =>
              if (!busStringSet.contains(avalonMM.name)) {
                busInterfacesSeq = busInterfacesSeq :+ VivadoBusReference.referenceAvalonMM(avalonMM)
                appendBusElement(avalonMM)
                break
              }
            case _ =>
          }
        }
      }
    }
    for (thisPort <- allPorts) {
      if (clockNameSet.contains(thisPort.name)) {
        for (thisClk <- clockSet) {
          if (thisClk.clock.name == thisPort.name) {
            busInterfacesSeq = busInterfacesSeq :+ VivadoBusReference.referenceClock(thisClk, busClockMap)
            if (thisClk.reset != null) {
              busInterfacesSeq = busInterfacesSeq :+ VivadoBusReference.referenceReset(thisClk.reset)
            }
          }
        }
      }
    }
    val componentBusInterfaces: Option[BusInterfaces] = if (busInterfacesSeq.nonEmpty) Some(BusInterfaces(busInterfacesSeq)) else None
    componentBusInterfaces
  }


  private def getPorts: Option[Ports] = {
    var portsSeq: Seq[PortTypable] = Seq()
    for (thisPort <- inPorts ++ outPorts) {
      val wireTypeDef = if (thisPort.isReg) {
        TypeName4("register")
      } else {
        TypeName4("wire")
      }
      val thisViewNameRef = Seq("xilinx_anylanguagesynthesis", "xilinx_anylanguagebehavioralsimulation")
      val thisWireTypeDef = WireTypeDef(typeName = wireTypeDef, viewNameRef = thisViewNameRef)
      val wireTypeDefSeq: Seq[WireTypeDef] = Seq(thisWireTypeDef)
      val thisWireTypeDefs = WireTypeDefs(wireTypeDefSeq)


      val thisPortdeclarationtypablesequence = PortDeclarationTypableSequence1()

      val inOutValue =
        if (thisPort.isInput) {
          In
        } else if (thisPort.isOutput) {
          Out
        } else {
          Inout
        }
      val longRecord = DataRecord(Some("namespace"), Some("spirit:format"), "long")
      val longTypeMap = Map("key1" -> longRecord)
      val thisVector = if (thisPort.getBitsWidth != 1) {
        val leftUnsignedIntExpression = LeftType4(thisPort.getBitsWidth - 1, attributes = longTypeMap)
        val rightUnsignedIntExpression = RightType4(0, attributes = longTypeMap)
        Some(Vector4(left = leftUnsignedIntExpression, right = rightUnsignedIntExpression))
      } else None

      val thisPortWireType = PortWireType(direction = inOutValue, vector = thisVector, wireTypeDefs = Some(thisWireTypeDefs))
      val thisWireRecord = DataRecord(Some("namespace1"), Some("spirit:wire"), thisPortWireType)
      val thisNameGroupPortSequence = NameGroupPortSequence(thisPort.name)

      val thisPort2 = PortType(thisNameGroupPortSequence, thisWireRecord, thisPortdeclarationtypablesequence)
      portsSeq = portsSeq :+ thisPort2
    }
    val thisPorts: Option[Ports] = if (portsSeq.nonEmpty) Some(Ports(portsSeq)) else None
    thisPorts
  }

  private def getViews: Option[Views] = {
    var viewSep: Seq[ViewType] = Seq()

    val guiComponentViewNameGroupNMTOKEN = NameGroupNMTOKENSequence("xilinx_xpgui")
    val guiComponentViewEnvIdentifier = Seq(":vivado.xilinx.com:xgui.ui")
    val guiComponentViewFileSetRefRecord = DataRecord(Some("namespace"), Some("spirit:localName"), "xilinx_xpgui_view_fileset")
    val guiFileSetRef = FileSetRef(guiComponentViewFileSetRefRecord)
    val guiViewTypeSequence = ViewTypeSequence1(fileSetRef = Seq(guiFileSetRef))
    val guiViewTypeSequenceRecord = DataRecord(Some("namespace"), Some("noInfluence"), guiViewTypeSequence)
    val guiComponentView = ViewType(guiComponentViewNameGroupNMTOKEN, envIdentifier = guiComponentViewEnvIdentifier, viewtypeoption = guiViewTypeSequenceRecord)
    viewSep = viewSep :+ guiComponentView

    val simulationComponentViewNameGroupNMTOKEN = NameGroupNMTOKENSequence("xilinx_anylanguagebehavioralsimulation")
    val simulationComponentViewEnvIdentifier = Seq(":vivado.xilinx.com:simulation")
    val simulationLanguage = IPXACT2009ScalaCases.Language("Verilog")
    val simulationModuleName = moduleDefinitionName
    val simulationComponentViewFileSetRefRecord = DataRecord(Some("namespace"), Some("spirit:localName"), "xilinx_anylanguagebehavioralsimulation_view_fileset")
    val simulationFileSetRef = FileSetRef(simulationComponentViewFileSetRefRecord)
    val simulationViewTypeSequence = ViewTypeSequence1(Some(simulationLanguage), Some(simulationModuleName), fileSetRef = Seq(simulationFileSetRef))
    val simulationViewTypeSequenceRecord = DataRecord(Some("namespace"), Some("noInfluence"), simulationViewTypeSequence)
    val simulationComponentView = ViewType(simulationComponentViewNameGroupNMTOKEN, envIdentifier = simulationComponentViewEnvIdentifier, viewtypeoption = simulationViewTypeSequenceRecord)
    viewSep = viewSep :+ simulationComponentView

    val synthesisComponentViewNameGroupNMTOKEN = NameGroupNMTOKENSequence("xilinx_anylanguagesynthesis")
    val synthesisComponentViewEnvIdentifier = Seq(":vivado.xilinx.com:synthesis")
    val synthesisLanguage = IPXACT2009ScalaCases.Language("Verilog")
    val synthesisModuleName = moduleDefinitionName
    val synthesisComponentViewFileSetRefRecord = DataRecord(Some("namespace"), Some("spirit:localName"), "xilinx_anylanguagesynthesis_view_fileset")
    val synthesisFileSetRef = FileSetRef(synthesisComponentViewFileSetRefRecord)
    val synthesisViewTypeSequence = ViewTypeSequence1(Some(synthesisLanguage), Some(synthesisModuleName), fileSetRef = Seq(synthesisFileSetRef))
    val synthesisViewTypeSequenceRecord = DataRecord(Some("namespace"), Some("noInfluence"), synthesisViewTypeSequence)
    val synthesisComponentView = ViewType(synthesisComponentViewNameGroupNMTOKEN, envIdentifier = synthesisComponentViewEnvIdentifier, viewtypeoption = synthesisViewTypeSequenceRecord)
    viewSep = viewSep :+ synthesisComponentView

    //    if (module.children.nonEmpty) {
    //      val designConfigRefName = moduleDefinitionName + ".designcfg_1.0"
    //      val designConfigurationViewNameGroupNMTOKEN = NameGroupNMTOKENSequence("designConfigurationView")
    //      val designConfigurationView = View(designConfigurationViewNameGroupNMTOKEN, designConfigurationInstantiationRef = Some(designConfigRefName))
    //      viewSep = viewSep :+ designConfigurationView
    //    }
    val thisViews: Option[Views] = Some(Views(viewSep))
    thisViews
  }


  private def getModel: ModelType = {
    val thisViews = getViews
    val thisPorts = getPorts
    val thisModel = ModelType(views = thisViews, ports = thisPorts)
    thisModel
  }


  private def getVLNV: VersionedIdentifierSequence = {
    val documentNameGroupSequence = VersionedIdentifierSequence(toplevelVendor, toplevelName, moduleDefinitionName, version)
    documentNameGroupSequence
  }

  private def getFileSets: FileSets = {
    val xpguiFileName = Name(s"xgui/${moduleDefinitionName}_v$versionSuffix.tcl")
    val xpguiFileTypeRecord = DataRecord(Some("namespace"), Some("spirit:fileType"), "tclSource")
    //    val xpguiUserFileTypeRecord = DataRecord(Some("namespace"), Some("spirit:userFileType"), "XGUI_VERSION_2")
    val xpguiFileSequence = FileSequence1(xpguiFileTypeRecord)

    val xpguiFileSetFile = IPXACT2009ScalaCases.File(name = xpguiFileName, filesequence1 = xpguiFileSequence)
    val xpguiFileSetNameGroupSequence = NameGroupSequence("xilinx_xpgui_view_fileset")
    val xpguiFileSet = FileSetType(xpguiFileSetNameGroupSequence, file = Seq(xpguiFileSetFile))

    val simulationFileName = Name(module.definitionName + ".v")
    val simulationFileTypeRecord = DataRecord(Some("namespace"), Some("spirit:fileType"), "verilogSource")
    val simulationFileSequence = FileSequence1(simulationFileTypeRecord)
    val simulationFileSetFile = IPXACT2009ScalaCases.File(name = simulationFileName, filesequence1 = simulationFileSequence)
    val simulationFileSetNameGroupSequence = NameGroupSequence("xilinx_anylanguagebehavioralsimulation_view_fileset")
    val simulationFileSet = FileSetType(simulationFileSetNameGroupSequence, file = Seq(simulationFileSetFile))

    val synthesisFileName = Name(module.definitionName + ".v")
    val synthesisFileTypeRecord = DataRecord(Some("namespace"), Some("spirit:fileType"), "verilogSource")
    val synthesisFileSequence = FileSequence1(synthesisFileTypeRecord)
    val synthesisFileSetFile = IPXACT2009ScalaCases.File(name = synthesisFileName, filesequence1 = synthesisFileSequence)
    val synthesisFileSetNameGroupSequence = NameGroupSequence("xilinx_anylanguagesynthesis_view_fileset")
    val synthesisFileSet = FileSetType(synthesisFileSetNameGroupSequence, file = Seq(synthesisFileSetFile))


    val fileSetsSeq = Seq(synthesisFileSet, simulationFileSet, xpguiFileSet)
    val thisFileSets = FileSets(fileSetsSeq)
    thisFileSets
  }


  private def getExtensions = {
    val vendorExtensionsXml: Elem =
      <spirit:vendorExtensions>
        <xilinx:coreExtensions>
          <xilinx:supportedFamilies>
            <xilinx:family xilinx:lifeCycle="Production">kintex7</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">kintex7l</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">artix7</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">artix7l</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">aartix7</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">zynq</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">azynq</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">spartan7</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">aspartan7</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">virtexuplus</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">virtexuplusHBM</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">virtexuplus58g</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">kintexuplus</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">artixuplus</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">zynquplus</xilinx:family>
            <xilinx:family xilinx:lifeCycle="Production">kintexu</xilinx:family>
          </xilinx:supportedFamilies>
          <xilinx:taxonomies>
            <xilinx:taxonomy>/SpinalAutoGenerate</xilinx:taxonomy>
          </xilinx:taxonomies>
          <xilinx:displayName>{moduleDefinitionName}</xilinx:displayName>
        </xilinx:coreExtensions>
      </spirit:vendorExtensions>
    vendorExtensionsXml
  }

  private def generateTcl(fileDirectory: String) = {
    val tclFileDirectory = fileDirectory + "xgui/"
    Files.createDirectories(Paths.get(tclFileDirectory))
    val filePath = s"$tclFileDirectory${moduleDefinitionName}_v$versionSuffix.tcl"
    val tclContent =
      """# Definitional proc to organize widgets for parameters.
        |proc init_gui { IPINST } {
        |  ipgui::add_param $IPINST -name "Component_Name"
        |  #Adding Page
        |  ipgui::add_page $IPINST -name "Page 0"
        |}
        |
        |""".stripMargin
    java.nio.file.Files.write(Paths.get(filePath), tclContent.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
  }

  def beginGenerate(): Unit = {
    val fileDirectory = s"./VivadoIpFolder/$toplevelName/"
    generateTcl(fileDirectory)
    val filePath = s"${fileDirectory}component.xml"
    val verilogSourcePath = s"./$toplevelName.v"
    val verilogTargetPath = fileDirectory + verilogSourcePath
    Files.createDirectories(Paths.get(fileDirectory))

    Files.copy(
      Paths.get(verilogSourcePath),
      Paths.get(verilogTargetPath),
      StandardCopyOption.REPLACE_EXISTING
    )
    val thisFileSets = getFileSets
    val thisModel = getModel
    val versionedIdentifierSequence = getVLNV
    val vivadoExtensions = getExtensions
    val thisBusInterfaces = getBusInterfaces
    val thisDescription = "This IP is generated automatically by SpinalHDL"
    val thisComponent = ComponentType(
      versionedIdentifierSequence1 = versionedIdentifierSequence,
      thisBusInterfaces,
      fileSets = Some(thisFileSets),
      model = Some(thisModel),
      description = Some(thisDescription)
    )


    val xml: NodeSeq = toXML[ComponentType](thisComponent, "spirit:component", defaultScope)
    val updatedXml = xml.head match {
      case elem: Elem =>
        val newElem = elem % new UnprefixedAttribute("xmlns:xilinx", "http://www.xilinx.com", Null)
        newElem.copy(child = newElem.child ++ vivadoExtensions)
      case _ => xml.head
    }
//    val prettyPrinter = new PrettyPrinter(width = 80, step = 2)
//    val formattedXml: String = prettyPrinter.format(updatedXml.head)
//    println(formattedXml)
    XML.save(filePath, updatedXml.head, "UTF-8", xmlDecl = true, doctype = null)
  }
}


object VivadoComponentGenerator {
  def generate(toplevelVendor: String = "SpinalHDL",
               toplevelName: String,
               version: String = "1.0",
               module: Component): Unit = {
    val generator = new VivadoComponentGenerator(toplevelVendor, toplevelName, version, module)
    generator.beginGenerate()
  }
}