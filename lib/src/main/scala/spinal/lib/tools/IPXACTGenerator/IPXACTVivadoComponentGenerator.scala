package spinal.lib.tools.IPXACTGenerator

import IPXACT2009ScalaCases._
import IPXACT2009scalaxb._
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

class IPXACTVivadoComponentGenerator(toplevelVendor: String = "SpinalHDL", toplevelName: String, version: String = "1.0", module: Component, generatePath: String) {
  private val moduleDefinitionName = module.definitionName
  private var busStringSet: Set[String] = Set()
  private val moduleAnalyzer = new ModuleAnalyzer(module)
  private val inPorts = moduleAnalyzer.getInputs(_ => true)
  private var busClockMap: Map[String, String] = Map()
  private val outPorts = moduleAnalyzer.getOutputs(_ => true)
  private val allPorts = inPorts ++ outPorts
  private val versionSuffix = version.split("\\.").mkString("_")

  private def appendBusElement[T <: IMasterSlave with Data](bus: T): Unit = {
    busStringSet = busStringSet + bus.name
    busClockMap = busClockMap + (bus.name -> bus.flatten.head.clockDomain.clock.name)
  }

  private def createBusInterfaces: Option[BusInterfaces] = {
    var busInterfacesSeq: Seq[BusInterfaceType] = Seq()
    val allClocks = moduleAnalyzer.getClocks(_ => true)
    var clockNameSet: Set[String] = Set()
    var clockSet: Set[ClockDomain] = Set()
    for (clock <- allClocks) {
      val clockName = clock.clock.name
      if (!clockNameSet.contains(clockName)) {
        clockNameSet += clockName
        clockSet = clockSet + clock
      }
    }
    for (port <- allPorts) {
      val parentChain = port.getRefOwnersChain()
      breakable {
        for (parentElement <- parentChain) {
          parentElement match {
            case busStream: Stream[_] =>
              busStream.payload match {
                case _: Axi4StreamBundle =>
                  if (!busStringSet.contains(busStream.name)) {
                    busInterfacesSeq = busInterfacesSeq :+ IPXACTVivadoBusReference.referenceAxis4(busStream.asInstanceOf[Stream[Axi4StreamBundle]])
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
            case bus: IMasterSlave with Data
              if bus.isInstanceOf[Axi4] ||
                bus.isInstanceOf[AxiLite4] ||
                bus.isInstanceOf[BRAM] ||
                bus.isInstanceOf[Vga] ||
                bus.isInstanceOf[Uart] ||
                bus.isInstanceOf[Apb3] ||
                bus.isInstanceOf[Apb4] ||
                bus.isInstanceOf[AhbLite3] ||
                bus.isInstanceOf[AvalonMM] =>
              if (!busStringSet.contains(bus.name)) {
                busInterfacesSeq = busInterfacesSeq :+ IPXACTVivadoBusReference.referenceMatchedBus(bus)
                appendBusElement(bus)
                break
              }
            case _ =>
          }
        }
      }
    }
    for (port <- allPorts) {
      if (clockNameSet.contains(port.name)) {
        for (clk <- clockSet) {
          if (clk.clock.name == port.name) {
            busInterfacesSeq = busInterfacesSeq :+ IPXACTVivadoBusReference.referenceClock(clk, busClockMap)
            if (clk.reset != null) {
              busInterfacesSeq = busInterfacesSeq :+ IPXACTVivadoBusReference.referenceReset(clk.reset)
            }
          }
        }
      }
    }
    val componentBusInterfaces: Option[BusInterfaces] = if (busInterfacesSeq.nonEmpty) Some(BusInterfaces(busInterfacesSeq)) else None
    componentBusInterfaces
  }


  private def createPorts: Option[Ports] = {
    var portsSeq: Seq[PortTypable] = Seq()
    for (port <- inPorts ++ outPorts) {
      val wireTypeNameDef = if (port.isReg) {
        TypeName4("register")
      } else {
        TypeName4("wire")
      }
      val viewNameRef = Seq("xilinx_anylanguagesynthesis", "xilinx_anylanguagebehavioralsimulation")
      val wireTypeDef = WireTypeDef(typeName = wireTypeNameDef, viewNameRef = viewNameRef)
      val wireTypeDefSeq: Seq[WireTypeDef] = Seq(wireTypeDef)
      val wireTypeDefs = WireTypeDefs(wireTypeDefSeq)
      val portDeclarationTypableSequence = PortDeclarationTypableSequence1()
      val inOutValue =
        if (port.isInput) {
          In
        } else if (port.isOutput) {
          Out
        } else {
          Inout
        }
      val longRecord = DataRecord(Some(""), Some("spirit:format"), "long")
      val longTypeMap = Map("format" -> longRecord)
      val vector = if (port.getBitsWidth != 1) {
        val leftUnsignedIntExpression = LeftType4(port.getBitsWidth - 1, attributes = longTypeMap)
        val rightUnsignedIntExpression = RightType4(0, attributes = longTypeMap)
        Some(Vector4(left = leftUnsignedIntExpression, right = rightUnsignedIntExpression))
      } else None

      val portWireType = PortWireType(direction = inOutValue, vector = vector, wireTypeDefs = Some(wireTypeDefs))
      val wireRecord = DataRecord(Some(""), Some("spirit:wire"), portWireType)
      val nameGroupPortSequence = NameGroupPortSequence(port.name)

      val portType = PortType(nameGroupPortSequence, wireRecord, portDeclarationTypableSequence)
      portsSeq = portsSeq :+ portType
    }
    val ports: Option[Ports] = if (portsSeq.nonEmpty) Some(Ports(portsSeq)) else None
    ports
  }

  private def createViews: Option[Views] = {
    var viewSep: Seq[ViewType] = Seq()

    val guiComponentViewNameGroupNMTOKEN = NameGroupNMTOKENSequence("xilinx_xpgui")
    val guiComponentViewEnvIdentifier = Seq(":vivado.xilinx.com:xgui.ui")
    val guiComponentViewFileSetRefRecord = DataRecord(Some(""), Some("spirit:localName"), "xilinx_xpgui_view_fileset")
    val guiFileSetRef = FileSetRef(guiComponentViewFileSetRefRecord)
    val guiViewTypeSequence = ViewTypeSequence1(fileSetRef = Seq(guiFileSetRef))
    val guiViewTypeSequenceRecord = DataRecord(Some(""), Some(""), guiViewTypeSequence)
    val guiComponentView = ViewType(guiComponentViewNameGroupNMTOKEN, envIdentifier = guiComponentViewEnvIdentifier, viewtypeoption = guiViewTypeSequenceRecord)
    viewSep = viewSep :+ guiComponentView

    val simulationComponentViewNameGroupNMTOKEN = NameGroupNMTOKENSequence("xilinx_anylanguagebehavioralsimulation")
    val simulationComponentViewEnvIdentifier = Seq(":vivado.xilinx.com:simulation")
    val simulationLanguage = IPXACT2009ScalaCases.Language("Verilog")
    val simulationModuleName = moduleDefinitionName
    val simulationComponentViewFileSetRefRecord = DataRecord(Some(""), Some("spirit:localName"), "xilinx_anylanguagebehavioralsimulation_view_fileset")
    val simulationFileSetRef = FileSetRef(simulationComponentViewFileSetRefRecord)
    val simulationViewTypeSequence = ViewTypeSequence1(Some(simulationLanguage), Some(simulationModuleName), fileSetRef = Seq(simulationFileSetRef))
    val simulationViewTypeSequenceRecord = DataRecord(Some(""), Some(""), simulationViewTypeSequence)
    val simulationComponentView = ViewType(simulationComponentViewNameGroupNMTOKEN, envIdentifier = simulationComponentViewEnvIdentifier, viewtypeoption = simulationViewTypeSequenceRecord)
    viewSep = viewSep :+ simulationComponentView

    val synthesisComponentViewNameGroupNMTOKEN = NameGroupNMTOKENSequence("xilinx_anylanguagesynthesis")
    val synthesisComponentViewEnvIdentifier = Seq(":vivado.xilinx.com:synthesis")
    val synthesisLanguage = IPXACT2009ScalaCases.Language("Verilog")
    val synthesisModuleName = moduleDefinitionName
    val synthesisComponentViewFileSetRefRecord = DataRecord(Some(""), Some("spirit:localName"), "xilinx_anylanguagesynthesis_view_fileset")
    val synthesisFileSetRef = FileSetRef(synthesisComponentViewFileSetRefRecord)
    val synthesisViewTypeSequence = ViewTypeSequence1(Some(synthesisLanguage), Some(synthesisModuleName), fileSetRef = Seq(synthesisFileSetRef))
    val synthesisViewTypeSequenceRecord = DataRecord(Some(""), Some(""), synthesisViewTypeSequence)
    val synthesisComponentView = ViewType(synthesisComponentViewNameGroupNMTOKEN, envIdentifier = synthesisComponentViewEnvIdentifier, viewtypeoption = synthesisViewTypeSequenceRecord)
    viewSep = viewSep :+ synthesisComponentView

    //    if (module.children.nonEmpty) {
    //      val designConfigRefName = moduleDefinitionName + ".designcfg_1.0"
    //      val designConfigurationViewNameGroupNMTOKEN = NameGroupNMTOKENSequence("designConfigurationView")
    //      val designConfigurationView = View(designConfigurationViewNameGroupNMTOKEN, designConfigurationInstantiationRef = Some(designConfigRefName))
    //      viewSep = viewSep :+ designConfigurationView
    //    }
    val views: Option[Views] = Some(Views(viewSep))
    views
  }

  private def createModel: ModelType = {
    val views = createViews
    val ports = createPorts
    val model = ModelType(views = views, ports = ports)
    model
  }

  private def createVLNV: VersionedIdentifierSequence = {
    val documentNameGroupSequence = VersionedIdentifierSequence(toplevelVendor, toplevelName, moduleDefinitionName, version)
    documentNameGroupSequence
  }

  private def createFileSets: FileSets = {
    val xpguiFileName = Name(s"xgui/${moduleDefinitionName}_v$versionSuffix.tcl")
    val xpguiFileTypeRecord = DataRecord(Some(""), Some("spirit:fileType"), "tclSource")
    //    val xpguiUserFileTypeRecord = DataRecord(Some("namespace"), Some("spirit:userFileType"), "XGUI_VERSION_2")
    val xpguiFileSequence = FileSequence1(xpguiFileTypeRecord)

    val xpguiFileSetFile = IPXACT2009ScalaCases.File(name = xpguiFileName, filesequence1 = xpguiFileSequence)
    val xpguiFileSetNameGroupSequence = NameGroupSequence("xilinx_xpgui_view_fileset")
    val xpguiFileSet = FileSetType(xpguiFileSetNameGroupSequence, file = Seq(xpguiFileSetFile))

    val simulationFileName = Name(module.definitionName + ".v")
    val simulationFileTypeRecord = DataRecord(Some(""), Some("spirit:fileType"), "verilogSource")
    val simulationFileSequence = FileSequence1(simulationFileTypeRecord)
    val simulationFileSetFile = IPXACT2009ScalaCases.File(name = simulationFileName, filesequence1 = simulationFileSequence)
    val simulationFileSetNameGroupSequence = NameGroupSequence("xilinx_anylanguagebehavioralsimulation_view_fileset")
    val simulationFileSet = FileSetType(simulationFileSetNameGroupSequence, file = Seq(simulationFileSetFile))

    val synthesisFileName = Name(module.definitionName + ".v")
    val synthesisFileTypeRecord = DataRecord(Some(""), Some("spirit:fileType"), "verilogSource")
    val synthesisFileSequence = FileSequence1(synthesisFileTypeRecord)
    val synthesisFileSetFile = IPXACT2009ScalaCases.File(name = synthesisFileName, filesequence1 = synthesisFileSequence)
    val synthesisFileSetNameGroupSequence = NameGroupSequence("xilinx_anylanguagesynthesis_view_fileset")
    val synthesisFileSet = FileSetType(synthesisFileSetNameGroupSequence, file = Seq(synthesisFileSetFile))


    val fileSetsSeq = Seq(synthesisFileSet, simulationFileSet, xpguiFileSet)
    val fileSets = FileSets(fileSetsSeq)
    fileSets
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
    val fileDirectory = s"$generatePath/IPXACT/GeneratedVivadoIpFolder/$toplevelName/"
    // 确保目录存在
    generateTcl(fileDirectory)
    val filePath = s"${fileDirectory}component.xml"
    val verilogSourcePath = s"$generatePath/$toplevelName.v"
    val verilogTargetPath = s"$fileDirectory$toplevelName.v"
    Files.createDirectories(Paths.get(fileDirectory))

    Files.copy(
      Paths.get(verilogSourcePath),
      Paths.get(verilogTargetPath),
      StandardCopyOption.REPLACE_EXISTING
    )
    val fileSets = createFileSets
    val model = createModel
    val versionedIdentifierSequence = createVLNV
    val vivadoExtensions = getExtensions
    val busInterfaces = createBusInterfaces
    val description = "This IP is generated automatically by SpinalHDL"
    val component = ComponentType(
      versionedIdentifierSequence1 = versionedIdentifierSequence,
      busInterfaces,
      fileSets = Some(fileSets),
      model = Some(model),
      description = Some(description)
    )


    val xml: NodeSeq = toXML[ComponentType](component, "spirit:component", defaultScope)
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

object IPXACTVivadoComponentGenerator {
  def generate(toplevelVendor: String = "SpinalHDL",
               toplevelName: String,
               version: String = "1.0",
               module: Component,
               generatePath: String): Unit = {
    val generator = new IPXACTVivadoComponentGenerator(toplevelVendor, toplevelName, version, module, generatePath: String)
    generator.beginGenerate()
  }
}