package spinal.lib.tools.IPXACTGenerator

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
import IPXACT2009scalaxb._
import IPXACT2009ScalaCases._
object VivadoBusReference {
  private def createBusTypeAndAbstractionType(busInterfaceName: String, libraryName: String = "interface"): (LibraryRefType, LibraryRefType) = {
    val abstractionTypeRecord1 = DataRecord(Some("namespace1"), Some("spirit:vendor"), "xilinx.com")
    val abstractionTypeRecord2 = DataRecord(Some("namespace2"), Some("spirit:library"), libraryName)
    val abstractionTypeRecord3 = DataRecord(Some("namespace3"), Some("spirit:name"), busInterfaceName + "_rtl")
    val busTypeRecord3 = DataRecord(Some("namespace3"), Some("spirit:name"), busInterfaceName)
    val abstractionTypeRecord4 = DataRecord(Some("namespace4"), Some("spirit:version"), "1.0")

    val abstractionTypeAttributes = Map(
      "key1" -> abstractionTypeRecord1,
      "key2" -> abstractionTypeRecord2,
      "key3" -> abstractionTypeRecord3,
      "key4" -> abstractionTypeRecord4
    )
    val abstractionType = LibraryRefType(abstractionTypeAttributes)

    val busTypeAttributes = Map(
      "key1" -> abstractionTypeRecord1,
      "key2" -> abstractionTypeRecord2,
      "key3" -> busTypeRecord3,
      "key4" -> abstractionTypeRecord4
    )
    val thisBusType = LibraryRefType(busTypeAttributes)

    (thisBusType, abstractionType)
  }

  private def getDirectionRecord(iMasterSlaveBus: IMasterSlave): IPXACT2009scalaxb.DataRecord[InterfaceModeOption] = {
    if (iMasterSlaveBus.isMasterInterface) {
      DataRecord(Some("namespace1"), Some("spirit:master"), Master())
    } else {
      DataRecord(Some("namespace1"), Some("spirit:slave"), Slave())
    }
  }

  def referenceReset(resetSignal: BaseType): BusInterfaceType = {
    val thisBusName = resetSignal.name
    val busDirectionRecord = if (resetSignal.isOutput) {
      DataRecord(Some("namespace1"), Some("spirit:master"), Master())
    } else {
      DataRecord(Some("namespace1"), Some("spirit:slave"), Slave())
    }
    var portSeqMap: Seq[PortMap] = Seq()
    val thisSignalPhysicalName = thisBusName
    val thisSignalLogicalName = "RST"
    val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
    val thisLogicalPort = LogicalPort(thisSignalLogicalName)
    val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
    portSeqMap = portSeqMap :+ thisPortMap
    val thisPortMaps = PortMaps(portSeqMap)
    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("reset", libraryName = "signal")
    val busNameGroupSequence = NameGroupSequence(thisBusName)
    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps))
  }

  def referenceClock(clock: ClockDomain, busClockMap: Map[String, String]): BusInterfaceType = {
    val clockSignal = clock.clock
    var thisParametersSeq: Seq[NameValuePairTypable] = Seq()
    if (clock.reset != null) {
      val resetNameGroup = NameGroupStringSequence("ASSOCIATED_RESET")
      val resetValueRecord = DataRecord(Some("namespace4"), Some("spirit:id"), "BUSIFPARAM_VALUE.CLK.ASSOCIATED_RESET")
      val resetAbstractionAttributes = Map("key1" -> resetValueRecord)
      val resetValue = Value(value = clock.reset.name, attributes = resetAbstractionAttributes)
      val resetNameValue = NameValueTypeType(resetNameGroup, resetValue)
      thisParametersSeq = thisParametersSeq :+ resetNameValue
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
      val busValueRecord = DataRecord(Some("namespace4"), Some("spiritid"), "BUSIFPARAM_VALUE.CLK.ASSOCIATED_BUSIF")
      val busAbstractionAttributes = Map("key1" -> busValueRecord)
      val busValue = Value(value = busName, attributes = busAbstractionAttributes)
      val busNameValue = NameValueTypeType(busNameGroup, busValue)
      thisParametersSeq = thisParametersSeq :+ busNameValue
    }
    val thisParameters = Parameters(thisParametersSeq)
    val thisBusName = clockSignal.name
    val busDirectionRecord = if (clockSignal.isOutput) {
      DataRecord(Some("namespace1"), Some("spirit:master"), Master())
    } else {
      DataRecord(Some("namespace1"), Some("spirit:slave"), Slave())
    }
    var portSeqMap: Seq[PortMap] = Seq()
    val thisSignalPhysicalName = thisBusName
    val thisSignalLogicalName = "CLK"
    val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
    val thisLogicalPort = LogicalPort(thisSignalLogicalName)
    val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
    portSeqMap = portSeqMap :+ thisPortMap
    val thisPortMaps = PortMaps(portSeqMap)
    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("clock", libraryName = "signal")
    val busNameGroupSequence = NameGroupSequence(thisBusName)
    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps), parameters = Some(thisParameters))
  }

  def referenceAvalonMM(avalonMM: AvalonMM): BusInterfaceType = {
    val thisBusName = "AvalonMM" + "_" + avalonMM.name
    val busChildren = avalonMM.flatten
    val busDirectionRecord = getDirectionRecord(avalonMM)
    var portSeqMap: Seq[PortMap] = Seq()
    for (thisSignal <- busChildren) {
      val thisSignalPhysicalName = thisSignal.name
      val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
      var portMapCanAdd = true
      if (thisSignalPhysicalName.split("_").last == "debugAccess") {
        portMapCanAdd = false
      }
      val thisSignalLogicalName = if (thisSignalPhysicalName.split("_").last == "waitRequestn") {
        "WAITREQUEST"
      } else {
        thisSignalPhysicalName.split("_").last.toUpperCase
      }
      val thisLogicalPort = LogicalPort(thisSignalLogicalName)
      val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
      if (portMapCanAdd) {
        portSeqMap = portSeqMap :+ thisPortMap
      }
    }
    val thisPortMaps = PortMaps(portSeqMap)
    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("avalon")
    val busNameGroupSequence = NameGroupSequence(thisBusName)
    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps))

  }

  def referenceAhbLite3(ahbLite: AhbLite3): BusInterfaceType = {
    val thisBusName = "AhbLite3" + "_" + ahbLite.name
    val busChildren = ahbLite.flatten
    val busDirectionRecord = getDirectionRecord(ahbLite)
    var portSeqMap: Seq[PortMap] = Seq()
    for (thisSignal <- busChildren) {
      val thisSignalPhysicalName = thisSignal.name
      val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
      var portMapCanAdd = true
      val lastWord = thisSignalPhysicalName.split("_").last
      if (lastWord == "HREADY" || (lastWord == "HSEL" && busDirectionRecord.value == Master())) {
        portMapCanAdd = false
      }

      val thisSignalLogicalName = if (lastWord == "HREADYOUT") {
        "HREADY"
      } else if (lastWord == "HSEL") {
        "SEL"
      } else {
        lastWord
      }
      val thisLogicalPort = LogicalPort(thisSignalLogicalName)
      val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
      if (portMapCanAdd) {
        portSeqMap = portSeqMap :+ thisPortMap
      }
    }
    val thisPortMaps = PortMaps(portSeqMap)
    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("ahblite")
    val busNameGroupSequence = NameGroupSequence(thisBusName)
    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps))

  }


  def referenceApb3(apb3: Apb3): BusInterfaceType = {
    val thisBusName = "Apb3" + "_" + apb3.name
    val busChildren = apb3.flatten
    val busDirectionRecord = getDirectionRecord(apb3)
    var portSeqMap: Seq[PortMap] = Seq()
    for (thisSignal <- busChildren) {
      val thisSignalPhysicalName = thisSignal.name
      val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
      val thisSignalLogicalName = if (thisSignalPhysicalName.split("_").last == "PSLVERROR") {
        "PSLVERR"
      } else {
        thisSignalPhysicalName.split("_").last
      }
      val thisLogicalPort = LogicalPort(thisSignalLogicalName)
      val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
      portSeqMap = portSeqMap :+ thisPortMap
    }
    val thisPortMaps = PortMaps(portSeqMap)
    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("apb")
    val busNameGroupSequence = NameGroupSequence(thisBusName)
    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps))
  }

  def referenceApb4(apb4: Apb4): BusInterfaceType = {
    val thisBusName = "Apb4" + "_" + apb4.name
    val busChildren = apb4.flatten
    val busDirectionRecord = getDirectionRecord(apb4)
    var portSeqMap: Seq[PortMap] = Seq()
    for (thisSignal <- busChildren) {
      val thisSignalPhysicalName = thisSignal.name
      val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
      val thisSignalLogicalName = thisSignalPhysicalName.split("_").last
      val thisLogicalPort = LogicalPort(thisSignalLogicalName)
      val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
      portSeqMap = portSeqMap :+ thisPortMap
    }
    val thisPortMaps = PortMaps(portSeqMap)
    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("apb")
    val busNameGroupSequence = NameGroupSequence(thisBusName)
    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps))
  }

  def referenceVga(vga: Vga): BusInterfaceType = {
    val thisBusName = "Vga" + "_" + vga.name
    val busChildren = vga.flatten
    val busDirectionRecord = getDirectionRecord(vga)
    var portSeqMap: Seq[PortMap] = Seq()
    for (thisSignal <- busChildren) {
      val thisSignalPhysicalName = thisSignal.name
      val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
      val lastWord = thisSignalPhysicalName.split("_").last
      val thisSignalLogicalName = if (lastWord == "r") {
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
      val thisLogicalPort = LogicalPort(thisSignalLogicalName)
      val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
      portSeqMap = portSeqMap :+ thisPortMap
    }
    val thisPortMaps = PortMaps(portSeqMap)
    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("vga")
    val busNameGroupSequence = NameGroupSequence(thisBusName)
    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps))
  }

  def referenceUART(uart: Uart): BusInterfaceType = {
    val thisBusName = "Uart" + "_" + uart.name
    val busChildren = uart.flatten
    val busDirectionRecord = getDirectionRecord(uart)
    var portSeqMap: Seq[PortMap] = Seq()
    for (thisSignal <- busChildren) {
      val thisSignalPhysicalName = thisSignal.name
      val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
      val lastWord = thisSignalPhysicalName.split("_").last
      val thisSignalLogicalName = if (lastWord == "cts") {
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
      val thisLogicalPort = LogicalPort(thisSignalLogicalName)
      val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
      portSeqMap = portSeqMap :+ thisPortMap
    }
    val thisPortMaps = PortMaps(portSeqMap)
    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("uart")
    val busNameGroupSequence = NameGroupSequence(thisBusName)
    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps))
  }

  def referenceAxi4(axi4: Axi4): BusInterfaceType = {
    val thisBusName = "Axi4" + "_" + axi4.name
    val busChildren = axi4.flatten
    val busDirectionRecord = getDirectionRecord(axi4)
    var portSeqMap: Seq[PortMap] = Seq()
    for (thisSignal <- busChildren) {
      val thisSignalPhysicalName = thisSignal.name
      val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
      val parts = thisSignalPhysicalName.split("_").takeRight(3)
      val thirdLast = parts(0)
      val secondLast = parts(1)
      val last = parts(2)
      val thisSignalLogicalName = if (last == "ready" || last == "valid") {
        (secondLast + last).toUpperCase
      } else {
        (thirdLast + last).toUpperCase
      }
      val thisLogicalPort = LogicalPort(thisSignalLogicalName)
      val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
      portSeqMap = portSeqMap :+ thisPortMap
    }
    val thisPortMaps = PortMaps(portSeqMap)

    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("aximm")
    val busNameGroupSequence = NameGroupSequence(thisBusName)
    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps))
  }

  def referenceAxiLite4(axiLite4: AxiLite4): BusInterfaceType = {
    val thisBusName = "AxiLite4" + "_" + axiLite4.name
    val busChildren = axiLite4.flatten
    val busDirectionRecord = getDirectionRecord(axiLite4)
    var portSeqMap: Seq[PortMap] = Seq()
    for (thisSignal <- busChildren) {
      val thisSignalPhysicalName = thisSignal.name
      val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
      val parts = thisSignalPhysicalName.split("_").takeRight(3)
      val thirdLast = parts(0)
      val secondLast = parts(1)
      val last = parts(2)
      val thisSignalLogicalName = if (last == "ready" || last == "valid") {
        (secondLast + last).toUpperCase
      } else {
        (thirdLast + last).toUpperCase
      }
      val thisLogicalPort = LogicalPort(thisSignalLogicalName)
      val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
      portSeqMap = portSeqMap :+ thisPortMap
    }
    val thisPortMaps = PortMaps(portSeqMap)
    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("aximm")
    val busNameGroupSequence = NameGroupSequence(thisBusName)
    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps))
  }

  //  def referenceNormalFlow(flowSignal:Flow[_]):BusInterfaceType={
  //    val payloadClassName=flowSignal.payload.getClass.getSimpleName
  //    val thisBusName =s"Stream_${payloadClassName}"+"_"+ flowSignal.name
  //    val busChildren = flowSignal.flatten
  //    val busDirectionRecord =getDirectionRecord(flowSignal)
  //    var portSeqMap: Seq[PortMap] = Seq()
  //    for (thisSignal <- busChildren) {
  //      val thisSignalPhysicalName = thisSignal.name
  //      val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
  //      val lastWord=thisSignalPhysicalName.split("_").last
  //      val thisSignalLogicalName = if(lastWord=="payload"){
  //        "TDATA"
  //      }else{
  //        "T" + thisSignalPhysicalName.split("_").last.toUpperCase
  //      }
  //      val thisLogicalPort = LogicalPort(thisSignalLogicalName)
  //      val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
  //      portSeqMap = portSeqMap :+ thisPortMap
  //    }
  //    val thisPortMaps = PortMaps(portSeqMap)
  //    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("axis")
  //    val busNameGroupSequence = NameGroupSequence(thisBusName)
  //    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps))
  //  }

  def referenceNormalStream(streamSignal: Stream[_]): BusInterfaceType = {
    val payloadClassName = streamSignal.payload.getClass.getSimpleName
    val thisBusName = s"Stream_$payloadClassName" + "_" + streamSignal.name
    val busChildren = streamSignal.flatten
    val busDirectionRecord = getDirectionRecord(streamSignal)
    var portSeqMap: Seq[PortMap] = Seq()
    for (thisSignal <- busChildren) {
      val thisSignalPhysicalName = thisSignal.name
      val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
      val lastWord = thisSignalPhysicalName.split("_").last
      val thisSignalLogicalName = if (lastWord == "payload") {
        "TDATA"
      } else {
        "T" + thisSignalPhysicalName.split("_").last.toUpperCase
      }
      val thisLogicalPort = LogicalPort(thisSignalLogicalName)
      val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
      portSeqMap = portSeqMap :+ thisPortMap
    }
    val thisPortMaps = PortMaps(portSeqMap)
    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("axis")
    val busNameGroupSequence = NameGroupSequence(thisBusName)
    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps))
  }

  def referenceAxis4(axi4StreamBundle: Stream[Axi4StreamBundle]): BusInterfaceType = {
    val thisBusName = "Axis4" + "_" + axi4StreamBundle.name
    val busChildren = axi4StreamBundle.flatten
    val busDirectionRecord = getDirectionRecord(axi4StreamBundle)
    var portSeqMap: Seq[PortMap] = Seq()
    for (thisSignal <- busChildren) {
      val thisSignalPhysicalName = thisSignal.name
      val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
      val thisSignalLogicalName = "T" + thisSignalPhysicalName.split("_").last.toUpperCase
      val thisLogicalPort = LogicalPort(thisSignalLogicalName)
      val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
      portSeqMap = portSeqMap :+ thisPortMap
    }
    val thisPortMaps = PortMaps(portSeqMap)

    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("axis")
    val busNameGroupSequence = NameGroupSequence(thisBusName)
    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps))
  }

  def referenceBRAM(bram: BRAM): BusInterfaceType = {
    val thisBusName = "BRAM" + "_" + bram.name
    val busChildren = bram.flatten
    val busDirectionRecord = getDirectionRecord(bram)
    var portSeqMap: Seq[PortMap] = Seq()
    for (thisSignal <- busChildren) {
      val thisSignalPhysicalName = thisSignal.name
      val thisPhysicalPort = PhysicalPort(thisSignalPhysicalName)
      val lastWord = thisSignalPhysicalName.split("_").last
      val thisSignalLogicalName = if (lastWord == "wrdata") {
        "DIN"
      } else if (lastWord == "rddata") {
        "DOUT"
      } else {
        lastWord.toUpperCase
      }
      val thisLogicalPort = LogicalPort(thisSignalLogicalName)
      val thisPortMap = PortMap(thisLogicalPort, thisPhysicalPort)
      portSeqMap = portSeqMap :+ thisPortMap
    }
    val thisPortMaps = PortMaps(portSeqMap)

    val (thisBusType, abstractionType) = createBusTypeAndAbstractionType("bram")
    val busNameGroupSequence = NameGroupSequence(thisBusName)
    BusInterfaceType(busNameGroupSequence, thisBusType, Some(abstractionType), busDirectionRecord, portMaps = Some(thisPortMaps))
  }

}
