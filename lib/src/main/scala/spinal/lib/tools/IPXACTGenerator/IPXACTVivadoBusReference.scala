package spinal.lib.tools.IPXACTGenerator

import IPXACT2009ScalaCases._
import IPXACT2009scalaxb._
import spinal.core._
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

object IPXACTVivadoBusReference {
  private def createLibraryRefType(busInterfaceName: String, libraryName: String = "interface", version: String = "1.0"): LibraryRefType = {
    val busTypeVendorRecord = DataRecord(Some(""), Some("spirit:vendor"), "xilinx.com")
    val busTypeLibraryRecord = DataRecord(Some(""), Some("spirit:library"), libraryName)
    val busTypeNameRecord = DataRecord(Some(""), Some("spirit:name"), busInterfaceName)
    val busTypeVersionRecord = DataRecord(Some(""), Some("spirit:version"), version)
    val busTypeAttributes = Map(
      "vendor" -> busTypeVendorRecord,
      "library" -> busTypeLibraryRecord,
      "name" -> busTypeNameRecord,
      "version" -> busTypeVersionRecord
    )
    val busLibraryRefType = LibraryRefType(busTypeAttributes)
    busLibraryRefType
  }

  private def createDirectionRecord(iMasterSlaveBus: IMasterSlave): IPXACT2009scalaxb.DataRecord[InterfaceModeOption] = {
    if (iMasterSlaveBus.isMasterInterface) {
      DataRecord(Some(""), Some("spirit:master"), Master())
    } else {
      DataRecord(Some(""), Some("spirit:slave"), Slave())
    }
  }

  def referenceReset(resetSignal: BaseType): BusInterfaceType = {
    val busName = resetSignal.name
    val busDirectionRecord = if (resetSignal.isOutput) {
      DataRecord(Some(""), Some("spirit:master"), Master())
    } else {
      DataRecord(Some(""), Some("spirit:slave"), Slave())
    }
    var portSeqMap: Seq[PortMap] = Seq()
    val signalPhysicalName = busName
    val signalLogicalName = "RST"
    val physicalPort = PhysicalPort(signalPhysicalName)
    val logicalPort = LogicalPort(signalLogicalName)
    val portMap = PortMap(logicalPort, physicalPort)
    portSeqMap = portSeqMap :+ portMap
    val portMaps = PortMaps(portSeqMap)
    val busType = createLibraryRefType(busInterfaceName = "reset", libraryName = "signal")
    val abstractionType = createLibraryRefType(busInterfaceName = "reset_rtl", libraryName = "signal")
    val busNameGroupSequence = NameGroupSequence(busName)
    BusInterfaceType(busNameGroupSequence, busType, Some(abstractionType), busDirectionRecord, portMaps = Some(portMaps))
  }

  def referenceClock(clock: ClockDomain, busClockMap: Map[String, String]): BusInterfaceType = {
    val clockSignal = clock.clock
    var parametersSeq: Seq[NameValuePairTypable] = Seq()
    if (clock.reset != null) {
      val resetNameGroup = NameGroupStringSequence("ASSOCIATED_RESET")
      val resetValueRecord = DataRecord(Some(""), Some("spirit:id"), "BUSIFPARAM_VALUE.CLK.ASSOCIATED_RESET")
      val resetAbstractionAttributes = Map("id" -> resetValueRecord)
      val resetValue = Value(value = clock.reset.name, attributes = resetAbstractionAttributes)
      val resetNameValue = NameValueTypeType(resetNameGroup, resetValue)
      parametersSeq = parametersSeq :+ resetNameValue
    }
    var busName = ""
    for (element <- busClockMap) {
      if (element._2 == clockSignal.name) {
        busName = busName + ":" + element._1
      }
    }
    if (busName != "") {
      busName = busName.stripPrefix(":")
      val busNameGroup = NameGroupStringSequence("ASSOCIATED_BUSIF")
      val busValueRecord = DataRecord(Some(""), Some("spiritid"), "BUSIFPARAM_VALUE.CLK.ASSOCIATED_BUSIF")
      val busAbstractionAttributes = Map("id" -> busValueRecord)
      val busValue = Value(value = busName, attributes = busAbstractionAttributes)
      val busNameValue = NameValueTypeType(busNameGroup, busValue)
      parametersSeq = parametersSeq :+ busNameValue
    }
    val parameters = Parameters(parametersSeq)
    val busDirectionRecord = if (clockSignal.isOutput) {
      DataRecord(Some(""), Some("spirit:master"), Master())
    } else {
      DataRecord(Some(""), Some("spirit:slave"), Slave())
    }
    var portSeqMap: Seq[PortMap] = Seq()
    val signalPhysicalName = clockSignal.name
    val signalLogicalName = "CLK"
    val physicalPort = PhysicalPort(signalPhysicalName)
    val logicalPort = LogicalPort(signalLogicalName)
    val portMap = PortMap(logicalPort, physicalPort)
    portSeqMap = portSeqMap :+ portMap
    val portMaps = PortMaps(portSeqMap)
    val busType = createLibraryRefType(busInterfaceName = "clock", libraryName = "signal")
    val abstractionType = createLibraryRefType(busInterfaceName = "clock_rtl", libraryName = "signal")
    val busNameGroupSequence = NameGroupSequence(signalPhysicalName)
    BusInterfaceType(busNameGroupSequence, busType, Some(abstractionType), busDirectionRecord, portMaps = Some(portMaps), parameters = Some(parameters))
  }

  def referenceMatchedBus[T <: IMasterSlave with Data](bus: T): BusInterfaceType = {
    val busName = bus.getClass.getSimpleName + "_" + bus.name
    val busDirectionRecord = createDirectionRecord(bus)
    val (portSeqMap, vivadoDefinitionName) = bus match {
      case axi4: Axi4 =>
        referenceAxi4(axi4)
      case axiLite4: AxiLite4 =>
        referenceAxiLite4(axiLite4)
      case vga: Vga =>
        referenceVga(vga)
      case avalonMM: AvalonMM =>
        referenceAvalonMM(avalonMM)
      case ahbLite3: AhbLite3 =>
        referenceAhbLite3(ahbLite3)
      case apb3: Apb3 =>
        referenceApb3(apb3)
      case apb4: Apb4 =>
        referenceApb4(apb4)
      case uart: Uart =>
        referenceUART(uart)
      case bram: BRAM =>
        referenceBRAM(bram)
      case _ =>
        val portSeqMap: Seq[PortMap] = Seq()
        (portSeqMap, "")
    }
    val portMaps = PortMaps(portSeqMap)
    val busType = createLibraryRefType(vivadoDefinitionName)
    val abstractionType = createLibraryRefType(vivadoDefinitionName + "_rtl")
    val busNameGroupSequence = NameGroupSequence(busName)
    BusInterfaceType(busNameGroupSequence, busType, Some(abstractionType), busDirectionRecord, portMaps = Some(portMaps))
  }


  private def referenceAvalonMM(avalonMM: AvalonMM): (Seq[PortMap], String) = {
    val busChildren=avalonMM.flatten
    var portSeqMap: Seq[PortMap] = Seq()
    for (signal <- busChildren) {
      val signalPhysicalName = signal.name
      val physicalPort = PhysicalPort(signalPhysicalName)
      var portMapCanAdd = true
      if (signalPhysicalName.split("_").last == "debugAccess") {
        portMapCanAdd = false
      }
      val signalLogicalName = if (signalPhysicalName.split("_").last == "waitRequestn") {
        "WAITREQUEST"
      } else {
        signalPhysicalName.split("_").last.toUpperCase
      }
      val logicalPort = LogicalPort(signalLogicalName)
      val portMap = PortMap(logicalPort, physicalPort)
      if (portMapCanAdd) {
        portSeqMap = portSeqMap :+ portMap
      }
    }
    (portSeqMap, "avalon")

  }

  private def referenceAhbLite3(ahbLite3: AhbLite3): (Seq[PortMap], String) = {
    val busChildren=ahbLite3.flatten
    val busDirectionRecord = createDirectionRecord(ahbLite3)
    var portSeqMap: Seq[PortMap] = Seq()
    for (signal <- busChildren) {
      val signalPhysicalName = signal.name
      val physicalPort = PhysicalPort(signalPhysicalName)
      var portMapCanAdd = true
      val lastWord = signalPhysicalName.split("_").last
      if (lastWord == "HREADY" || (lastWord == "HSEL" && busDirectionRecord.value == Master())) {
        portMapCanAdd = false
      }

      val signalLogicalName = if (lastWord == "HREADYOUT") {
        "HREADY"
      } else if (lastWord == "HSEL") {
        "SEL"
      } else {
        lastWord
      }
      val logicalPort = LogicalPort(signalLogicalName)
      val portMap = PortMap(logicalPort, physicalPort)
      if (portMapCanAdd) {
        portSeqMap = portSeqMap :+ portMap
      }
    }
    (portSeqMap, "ahblite")

  }


  private def referenceApb3(apb3: Apb3): (Seq[PortMap], String) = {
    val busChildren=apb3.flatten
    var portSeqMap: Seq[PortMap] = Seq()
    for (signal <- busChildren) {
      val signalPhysicalName = signal.name
      val physicalPort = PhysicalPort(signalPhysicalName)
      val signalLogicalName = if (signalPhysicalName.split("_").last == "PSLVERROR") {
        "PSLVERR"
      } else {
        signalPhysicalName.split("_").last
      }
      val logicalPort = LogicalPort(signalLogicalName)
      val portMap = PortMap(logicalPort, physicalPort)
      portSeqMap = portSeqMap :+ portMap
    }
    (portSeqMap, "apb")

  }

  private def referenceApb4(apb4: Apb4): (Seq[PortMap], String) = {
    val busChildren=apb4.flatten
    var portSeqMap: Seq[PortMap] = Seq()
    for (signal <- busChildren) {
      val signalPhysicalName = signal.name
      val physicalPort = PhysicalPort(signalPhysicalName)
      val signalLogicalName = signalPhysicalName.split("_").last
      val logicalPort = LogicalPort(signalLogicalName)
      val portMap = PortMap(logicalPort, physicalPort)
      portSeqMap = portSeqMap :+ portMap
    }
    (portSeqMap, "apb")

  }

  private def referenceVga(vga: Vga): (Seq[PortMap], String) = {
    val busChildren=vga.flatten
    var portSeqMap: Seq[PortMap] = Seq()
    for (signal <- busChildren) {
      val signalPhysicalName = signal.name
      val physicalPort = PhysicalPort(signalPhysicalName)
      val lastWord = signalPhysicalName.split("_").last
      val signalLogicalName = if (lastWord == "r") {
        "RED"
      } else if (lastWord == "g") {
        "GREEN"
      } else if (lastWord == "b") {
        "BLUE"
      } else if (lastWord == "colorEn") {
        "DE"
      } else {
        lastWord.toUpperCase
      }
      val logicalPort = LogicalPort(signalLogicalName)
      val portMap = PortMap(logicalPort, physicalPort)
      portSeqMap = portSeqMap :+ portMap
    }
    (portSeqMap, "vga")
  }

  private def referenceUART(uart: Uart): (Seq[PortMap], String) = {
    val busChildren=uart.flatten
    var portSeqMap: Seq[PortMap] = Seq()
    for (signal <- busChildren) {
      val signalPhysicalName = signal.name
      val physicalPort = PhysicalPort(signalPhysicalName)
      val lastWord = signalPhysicalName.split("_").last
      val signalLogicalName = if (lastWord == "cts") {
        "CTSn"
      } else if (lastWord == "rts") {
        "RTSn"
      } else if (lastWord == "rxd") {
        "RxD"
      } else if (lastWord == "txd") {
        "TxD"
      } else {
        lastWord.toUpperCase
      }
      val logicalPort = LogicalPort(signalLogicalName)
      val portMap = PortMap(logicalPort, physicalPort)
      portSeqMap = portSeqMap :+ portMap
    }
    (portSeqMap, "uart")
  }

  private def referenceAxi4(axi4: Axi4): (Seq[PortMap], String) = {
    val busChildren=axi4.flatten
    var portSeqMap: Seq[PortMap] = Seq()
    for (signal <- busChildren) {
      val signalPhysicalName = signal.name
      val physicalPort = PhysicalPort(signalPhysicalName)
      val parts = signalPhysicalName.split("_").takeRight(3)
      val thirdLast = parts(0)
      val secondLast = parts(1)
      val last = parts(2)
      val signalLogicalName = if (last == "ready" || last == "valid") {
        (secondLast + last).toUpperCase
      } else {
        (thirdLast + last).toUpperCase
      }
      val logicalPort = LogicalPort(signalLogicalName)
      val portMap = PortMap(logicalPort, physicalPort)
      portSeqMap = portSeqMap :+ portMap
    }
    (portSeqMap, "aximm")
  }

  private def referenceAxiLite4(axiLite4: AxiLite4): (Seq[PortMap], String) = {
    val busChildren=axiLite4.flatten
    var portSeqMap: Seq[PortMap] = Seq()
    for (signal <- busChildren) {
      val signalPhysicalName = signal.name
      val physicalPort = PhysicalPort(signalPhysicalName)
      val parts = signalPhysicalName.split("_").takeRight(3)
      val thirdLast = parts(0)
      val secondLast = parts(1)
      val last = parts(2)
      val signalLogicalName = if (last == "ready" || last == "valid") {
        (secondLast + last).toUpperCase
      } else {
        (thirdLast + last).toUpperCase
      }
      val logicalPort = LogicalPort(signalLogicalName)
      val portMap = PortMap(logicalPort, physicalPort)
      portSeqMap = portSeqMap :+ portMap
    }
    (portSeqMap, "aximm")
  }

  //  def referenceNormalFlow(flowSignal:Flow[_]):BusInterfaceType={
  //    val payloadClassName=flowSignal.payload.getClass.getSimpleName
  //    val busName =s"Stream_${payloadClassName}"+"_"+ flowSignal.name
  //    val busChildren = flowSignal.flatten
  //    val busDirectionRecord =getDirectionRecord(flowSignal)
  //    var portSeqMap: Seq[PortMap] = Seq()
  //    for (signal <- busChildren) {
  //      val signalPhysicalName = signal.name
  //      val physicalPort = PhysicalPort(signalPhysicalName)
  //      val lastWord=signalPhysicalName.split("_").last
  //      val signalLogicalName = if(lastWord=="payload"){
  //        "TDATA"
  //      }else{
  //        "T" + signalPhysicalName.split("_").last.toUpperCase
  //      }
  //      val logicalPort = LogicalPort(signalLogicalName)
  //      val portMap = PortMap(logicalPort, physicalPort)
  //      portSeqMap = portSeqMap :+ portMap
  //    }
  //    val portMaps = PortMaps(portSeqMap)
  //    val (busType, abstractionType) = createBusTypeAndAbstractionType("axis")
  //    val busNameGroupSequence = NameGroupSequence(busName)
  //    BusInterfaceType(busNameGroupSequence, busType, Some(abstractionType), busDirectionRecord, portMaps = Some(portMaps))
  //  }
  //
  //  def referenceNormalStream(streamSignal: Stream[_]): BusInterfaceType = {
  //    val payloadClassName = streamSignal.payload.getClass.getSimpleName
  //    val busName = s"Stream_$payloadClassName" + "_" + streamSignal.name
  //    val busChildren = streamSignal.flatten
  //    val busDirectionRecord = getDirectionRecord(streamSignal)
  //    var portSeqMap: Seq[PortMap] = Seq()
  //    for (signal <- busChildren) {
  //      val signalPhysicalName = signal.name
  //      val physicalPort = PhysicalPort(signalPhysicalName)
  //      val lastWord = signalPhysicalName.split("_").last
  //      val signalLogicalName = if (lastWord == "payload") {
  //        "TDATA"
  //      } else {
  //        "T" + signalPhysicalName.split("_").last.toUpperCase
  //      }
  //      val logicalPort = LogicalPort(signalLogicalName)
  //      val portMap = PortMap(logicalPort, physicalPort)
  //      portSeqMap = portSeqMap :+ portMap
  //    }
  //    val portMaps = PortMaps(portSeqMap)
  //    val busType=createBusDefinition("axis")
  //    val abstractionType=createBusDefinition("axis_rtl")
  //    val busNameGroupSequence = NameGroupSequence(busName)
  //    BusInterfaceType(busNameGroupSequence, busType, Some(abstractionType), busDirectionRecord, portMaps = Some(portMaps))
  //  }

  def referenceAxis4(axi4StreamBundle: Stream[Axi4StreamBundle]): BusInterfaceType = {
    val busName = "Axis4" + "_" + axi4StreamBundle.name
    val busChildren = axi4StreamBundle.flatten
    val busDirectionRecord = createDirectionRecord(axi4StreamBundle)
    var portSeqMap: Seq[PortMap] = Seq()
    for (signal <- busChildren) {
      val signalPhysicalName = signal.name
      val physicalPort = PhysicalPort(signalPhysicalName)
      val signalLogicalName = "T" + signalPhysicalName.split("_").last.toUpperCase
      val logicalPort = LogicalPort(signalLogicalName)
      val portMap = PortMap(logicalPort, physicalPort)
      portSeqMap = portSeqMap :+ portMap
    }
    val portMaps = PortMaps(portSeqMap)
    val busType = createLibraryRefType("axis")
    val abstractionType = createLibraryRefType("axis_rtl")
    val busNameGroupSequence = NameGroupSequence(busName)
    BusInterfaceType(busNameGroupSequence, busType, Some(abstractionType), busDirectionRecord, portMaps = Some(portMaps))
  }

  private def referenceBRAM(bram: BRAM): (Seq[PortMap], String) = {
    val busChildren=bram.flatten
    var portSeqMap: Seq[PortMap] = Seq()
    for (signal <- busChildren) {
      val signalPhysicalName = signal.name
      val physicalPort = PhysicalPort(signalPhysicalName)
      val lastWord = signalPhysicalName.split("_").last
      val signalLogicalName = if (lastWord == "wrdata") {
        "DIN"
      } else if (lastWord == "rddata") {
        "DOUT"
      } else {
        lastWord.toUpperCase
      }
      val logicalPort = LogicalPort(signalLogicalName)
      val portMap = PortMap(logicalPort, physicalPort)
      portSeqMap = portSeqMap :+ portMap
    }
    (portSeqMap, "bram")
  }

}
