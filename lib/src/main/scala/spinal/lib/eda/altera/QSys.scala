package spinal.lib.eda.altera

import spinal.core._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.bus.avalon.AvalonMM
import spinal.lib.bus.amba4.axi.Axi4

import scala.collection.mutable
import scala.collection.mutable.StringBuilder
import spinal.lib.bus.amba4.axilite.AxiLite4

object QSysify{
  def apply(that : Component) : Unit = {
    val tool = new QSysify
    tool.interfaceEmiters += new AvalonEmitter()
    tool.interfaceEmiters += new ApbEmitter()
    tool.interfaceEmiters += new Axi4Emitter()
    tool.interfaceEmiters += new AxiLite4Emitter()
    tool.interfaceEmiters += new ClockDomainEmitter()
    tool.interfaceEmiters += new ResetEmitterEmitter()
    tool.interfaceEmiters += new InterruptReceiverEmitter()
    tool.interfaceEmiters += new InterruptSenderEmitter()
    tool.interfaceEmiters += new ConduitEmitter()

    tool.emit(that)
  }
}

trait QSysifyInterfaceEmiter{
  //Check the 'i' interface, if it reconize something, should return true and complette the TCL `builder` with corresponding things.
  def emit(i : Data,builder : StringBuilder) : Boolean
}

class QSysify{
  val interfaceEmiters = mutable.ArrayBuffer[QSysifyInterfaceEmiter]()
  def emit(that : Component) {
    val name = that.definitionName
    var out: java.io.FileWriter = null
    out = new java.io.FileWriter(name + "_hw.tcl")
    val builder = new StringBuilder()

    genHeader()
    genInterfaces()

    out.write(builder.toString())
    out.flush();
    out.close();



    def genHeader(): Unit = {
      builder ++= s"""
  |package require -exact qsys 13.1
  |
  |#
  |# module def
  |#
  |set_module_property DESCRIPTION ""
  |set_module_property NAME ${name}
  |set_module_property VERSION 1.0
  |set_module_property INTERNAL false
  |set_module_property OPAQUE_ADDRESS_MAP true
  |set_module_property AUTHOR ""
  |set_module_property DISPLAY_NAME $name
  |set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
  |set_module_property EDITABLE false
  |set_module_property ANALYZE_HDL false
  |set_module_property REPORT_TO_TALKBACK false
  |set_module_property ALLOW_GREYBOX_GENERATION false
  |
  |#
  |# file sets
  |#
  |add_fileset QUARTUS_SYNTH QUARTUS_SYNTH "" ""
  |set_fileset_property QUARTUS_SYNTH TOP_LEVEL $name
  |set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false
  |#add_fileset_file ${name}.vhd VHDL PATH ${name}.vhd TOP_LEVEL_FILE
  |
  |add_fileset SIM_VHDL SIM_VHDL "" ""
  |set_fileset_property SIM_VHDL TOP_LEVEL $name
  |set_fileset_property SIM_VHDL ENABLE_RELATIVE_INCLUDE_PATHS false
  |#add_fileset_file $name.vhd VHDL PATH $name.vhd
  |
  |""".stripMargin
    }
    def genInterfaces() = {
      for(i <- that.getGroupedIO(true)){
        var find = false
        for(emiter <- interfaceEmiters){
          if(!find && emiter.emit(i,builder)){
            assert(!find)
            find = true
          }
        }
        assert(find, "Can't map interface " + i)
      }
    }
  }
}


class ClockDomainEmitter extends QSysifyInterfaceEmiter {
  override def emit(i: Data, builder: StringBuilder): Boolean = i match {
    case i : Bool =>{
      val driver = ClockDomain.getClockDomainDriver(i)
      val tag = ClockDomain.getClockDomainTag(i)
      if(driver == null) return false
      val name = driver.getName()
      val interfaceName = name
      tag match{
        case tag : ClockTag => {
          builder ++=
            s"""
               |#
               |# connection point $interfaceName
               |#
               |add_interface $interfaceName clock end
               |set_interface_property $interfaceName clockRate 0
               |set_interface_property $interfaceName ENABLED true
               |set_interface_property $interfaceName EXPORT_OF ""
               |set_interface_property $interfaceName PORT_NAME_MAP ""
               |set_interface_property $interfaceName SVD_ADDRESS_GROUP ""
               |
               |add_interface_port $interfaceName $name clk Input 1
             """.stripMargin
          true
        }
        case tag : ResetTag => {
          builder ++=
            s"""
              |#
              |# connection point $interfaceName
              |#
              |add_interface $interfaceName reset end
              |set_interface_property $interfaceName associatedClock ${tag.clockDomain.clock.getName()}
              |set_interface_property $interfaceName synchronousEdges DEASSERT
              |set_interface_property $interfaceName ENABLED true
              |set_interface_property $interfaceName EXPORT_OF ""
              |set_interface_property $interfaceName PORT_NAME_MAP ""
              |set_interface_property $interfaceName SVD_ADDRESS_GROUP ""
              |
              |add_interface_port $interfaceName $name reset Input 1
             """.stripMargin
          true
        }
        case tag : ClockEnableTag => SpinalError("Can't map clock enable in QSys")
        case _ => false
      }
    }
    case _ => false
  }
}

class AvalonEmitter extends QSysifyInterfaceEmiter{
  override def emit(i: Data,builder : StringBuilder): Boolean = i match {
    case e: AvalonMM =>{
      import e.config._
      val isMaster = e.address.isOutput
      val (masterPinDir,slavePinDir,startEnd) = if(isMaster) ("Output", "Input","start") else ("Input","Output","end")
      val name = e.getName()
      val clockDomainTag = e.getTag(classOf[ClockDomainTag])
      if(clockDomainTag.isEmpty) SpinalError(s"Clock domain of ${i} is not defined, You shoud apply the ClockDomainTag to the inferface\nyourBus.addTag(ClockDomainTag(ClockDomain.current))")
      val clockName = clockDomainTag.get.clockDomain.clock.getName()
      val resetName = clockDomainTag.get.clockDomain.reset.getName()
      builder ++= s"""
|#
|# connection point $name
|#
|add_interface $name avalon $startEnd
|set_interface_property $name addressUnits ${addressUnits.getName.toUpperCase}
|set_interface_property $name burstcountUnits ${burstCountUnits.getName.toUpperCase}
|set_interface_property $name burstOnBurstBoundariesOnly ${burstOnBurstBoundariesOnly}
|set_interface_property $name constantBurstBehavior ${constantBurstBehavior}
|set_interface_property $name holdTime ${holdTime}
|set_interface_property $name linewrapBursts ${linewrapBursts}
|set_interface_property $name maximumPendingReadTransactions ${maximumPendingReadTransactions}
|set_interface_property $name maximumPendingWriteTransactions ${maximumPendingWriteTransactions}
|set_interface_property $name readLatency ${readLatency}
|set_interface_property $name readWaitTime ${readWaitTime}
|set_interface_property $name setupTime ${setupTime}
|set_interface_property $name writeWaitTime ${writeWaitTime}
|set_interface_property $name holdTime ${holdTime}
|
|set_interface_property $name associatedClock $clockName
|set_interface_property $name associatedReset $resetName
|set_interface_property $name bitsPerSymbol 8
|
|set_interface_property $name timingUnits Cycles
|set_interface_property $name ENABLED true
|set_interface_property $name EXPORT_OF ""
|set_interface_property $name PORT_NAME_MAP ""
|set_interface_property $name SVD_ADDRESS_GROUP ""
|
""".stripMargin

      if(isMaster){
        builder ++= s"set_interface_property $name doStreamReads false\n"
        builder ++= s"set_interface_property $name doStreamWrites false\n"
      }

      builder ++= s"add_interface_port $name ${e.address.getName()} address ${masterPinDir} ${addressWidth}\n"
      if(useRead) builder ++= s"add_interface_port $name ${e.read.getName()} read ${masterPinDir} 1\n"
      if(useWrite) builder ++= s"add_interface_port $name ${e.write.getName()} write ${masterPinDir} 1\n"
      if(useWaitRequestn) builder ++= s"add_interface_port $name ${e.waitRequestn.getName()} waitrequest_n ${slavePinDir} 1\n"
      if(useLock) builder ++= s"add_interface_port $name ${e.lock.getName()} lock ${masterPinDir} 1\n"
      if(useBurstCount) builder ++= s"add_interface_port $name ${e.burstCount.getName()} burstcount ${masterPinDir} ${burstCountWidth}\n"
      if(useByteEnable) builder ++= s"add_interface_port $name ${e.byteEnable.getName()} byteenable ${masterPinDir} ${dataByteCount}\n"
      if(useWrite) builder ++= s"add_interface_port $name ${e.writeData.getName()} writedata ${masterPinDir} ${dataWidth}\n"
      if(useResponse) builder ++= s"add_interface_port $name ${e.response.getName()} response ${slavePinDir} 2\n"
      if(useReadDataValid) builder ++= s"add_interface_port $name ${e.readDataValid.getName()} readdatavalid ${slavePinDir} 1\n"
    //  if(useEndOfPacket) builder ++= s"add_interface_port $name ${e.endOfPacket.getName()} endofpacket ${slavePinDir} 1\n"
      if(useRead) builder ++= s"add_interface_port $name ${e.readData.getName()} readdata ${slavePinDir} ${dataWidth}\n"
      if(useDebugAccess)
        ???
      true
      }
    case _ => false
  }
}


class ConduitEmitter extends QSysifyInterfaceEmiter{
  override def emit(i: Data, builder: scala.StringBuilder): Boolean = {
    val name = i.getName()
    builder ++=
      s"""
         |#
         |# connection point $name
         |#
         |add_interface $name conduit end
         |set_interface_property $name associatedClock ""
         |set_interface_property $name associatedReset ""
         |set_interface_property $name ENABLED true
         |set_interface_property $name EXPORT_OF ""
         |set_interface_property $name PORT_NAME_MAP ""
         |set_interface_property $name SVD_ADDRESS_GROUP ""
         |""".stripMargin

    for(e <- i.flatten){
      val dirStr = if(e.isOutput) "Output"
      else if(e.isInput) "Input"
      else null;
      val width = e match {
        case e : BitVector => e.getWidth
        case e : Bool => 1
      }
      if(dirStr != null)
        builder ++= s"add_interface_port $name ${e.getName()} export ${dirStr} ${width}\n"
    }
    builder ++= "\n\n"
    true
  }
}

case class InterruptReceiverTag(addressablePoint : Data,clockDomain : ClockDomain) extends SpinalTag

class InterruptReceiverEmitter extends QSysifyInterfaceEmiter{
  override def emit(i: Data, builder: scala.StringBuilder): Boolean = {
    val tag = i.getTag(classOf[InterruptReceiverTag])
    if(tag.isEmpty) return false
    val interfaceName = i.getName()
    val name = i.getName()

    builder ++=
      s"""
|#
|# connection point $interfaceName
|#
|add_interface $interfaceName interrupt start
|set_interface_property $interfaceName associatedAddressablePoint ${tag.get.addressablePoint.getName()}
|set_interface_property $interfaceName associatedClock ${tag.get.clockDomain.clock.getName()}
|set_interface_property $interfaceName associatedReset ${tag.get.clockDomain.reset.getName()}
|set_interface_property $interfaceName irqScheme INDIVIDUAL_REQUESTS
|set_interface_property $interfaceName ENABLED true
|set_interface_property $interfaceName EXPORT_OF ""
|set_interface_property $interfaceName PORT_NAME_MAP ""
|set_interface_property $interfaceName SVD_ADDRESS_GROUP ""
|
|add_interface_port $interfaceName $name irq Input ${i.getBitsWidth}
|""".stripMargin
    true
  }
}

case class ResetEmitterTag(associatedClock : ClockDomain) extends SpinalTag

class ResetEmitterEmitter extends QSysifyInterfaceEmiter{
  override def emit(i: Data, builder: scala.StringBuilder): Boolean = {
    val tag = i.getTag(classOf[ResetEmitterTag])
    if(tag.isEmpty) return false
    val interfaceName = i.getName()
    val name = i.getName()

    builder ++=
      s"""
         |#
         |# connection point $interfaceName
         |#
         |add_interface $interfaceName reset start
         |set_interface_property $interfaceName associatedClock ${tag.get.associatedClock.clock.getName()}
         |set_interface_property $interfaceName associatedDirectReset ""
         |set_interface_property $interfaceName associatedResetSinks ""
         |set_interface_property $interfaceName synchronousEdges DEASSERT
         |set_interface_property $interfaceName ENABLED true
         |set_interface_property $interfaceName EXPORT_OF ""
         |set_interface_property $interfaceName PORT_NAME_MAP ""
         |set_interface_property $interfaceName SVD_ADDRESS_GROUP ""
         |
         |add_interface_port $interfaceName $name reset Output 1
          |""".stripMargin
    true
  }
}



class ApbEmitter extends QSysifyInterfaceEmiter{
  override def emit(i: Data,builder : StringBuilder): Boolean = i match {
    case e: Apb3 =>{
      import e.config._
      val isMaster = e.PADDR.isOutput
      val (masterPinDir,slavePinDir,startEnd) = if(isMaster) ("Output", "Input","start") else ("Input","Output","end")
      val name = e.getName()
      val clockDomainTag = e.getTag(classOf[ClockDomainTag])
      if(clockDomainTag.isEmpty) SpinalError(s"Clock domain of ${i} is not defined, You shoud apply the ClockDomainTag to the inferface\nyourBus.addTag(ClockDomainTag(ClockDomain.current))")
      val clockName = clockDomainTag.get.clockDomain.clock.getName()
      val resetName = clockDomainTag.get.clockDomain.reset.getName()
      builder ++=
s"""
#
# connection point $name
#
add_interface $name apb $startEnd
set_interface_property $name ENABLED true
set_interface_property $name EXPORT_OF ""
set_interface_property $name PORT_NAME_MAP ""
set_interface_property $name CMSIS_SVD_VARIABLES ""
set_interface_property $name SVD_ADDRESS_GROUP ""

set_interface_property $name associatedClock $clockName
set_interface_property $name associatedReset $resetName

add_interface_port $name ${e.PADDR.getName()} paddr ${masterPinDir} ${e.config.addressWidth}
add_interface_port $name ${e.PSEL.getName()} psel ${masterPinDir} ${e.config.selWidth}
add_interface_port $name ${e.PENABLE.getName()} penable ${masterPinDir} 1
add_interface_port $name ${e.PWRITE.getName()} pwrite ${masterPinDir} 1
add_interface_port $name ${e.PWDATA.getName()} pwdata ${masterPinDir} ${e.config.dataWidth}
add_interface_port $name ${e.PRDATA.getName()} prdata ${slavePinDir} ${e.config.dataWidth}
add_interface_port $name ${e.PREADY.getName()} pready ${slavePinDir} 1
"""
      if(useSlaveError) builder ++= s"add_interface_port $name ${e.PSLVERROR.getName()} pslverr ${slavePinDir}  1"
      true
    }
    case _ => false
  }
}

//
//class StreamEmitter extends QSysifyInterfaceEmiter{
//  override def emit(i: Data,builder : StringBuilder): Boolean = i match {
//    case e: Stream =>{
//      import e.c._
//      val isMaster = e.address.isOutput
//      val (masterPinDir,slavePinDir,startEnd) = if(isMaster) ("Output", "Input","start") else ("Input","Output","end")
//      val name = e.getName()
//      val clockDomainTag = e.getTag(classOf[ClockDomainTag])
//      if(clockDomainTag.isEmpty) SpinalError(s"Clock domain of ${i} is not defined, You shoud apply the ClockDomainTag to the inferface\nyourBus.addTag(ClockDomainTag(ClockDomain.current))")
//      val clockName = clockDomainTag.get.clockDomain.clock.getName()
//      val resetName = clockDomainTag.get.clockDomain.reset.getName()
//      builder ++= s"""
//                     |#
//                     |# connection point $name
//          |#
//          |add_interface $name avalon $startEnd
//          |set_interface_property $name addressUnits ${addressUnits.getName.toUpperCase}
//          |set_interface_property $name burstcountUnits ${burstCountUnits.getName.toUpperCase}
//          |set_interface_property $name burstOnBurstBoundariesOnly ${burstOnBurstBoundariesOnly}
//          |set_interface_property $name constantBurstBehavior ${constantBurstBehavior}
//          |set_interface_property $name holdTime ${holdTime}
//          |set_interface_property $name linewrapBursts ${linewrapBursts}
//          |set_interface_property $name maximumPendingReadTransactions ${maximumPendingReadTransactions}
//          |set_interface_property $name maximumPendingWriteTransactions ${maximumPendingWriteTransactions}
//          |set_interface_property $name readLatency ${readLatency}
//          |set_interface_property $name readWaitTime ${readWaitTime}
//          |set_interface_property $name setupTime ${setupTime}
//          |set_interface_property $name writeWaitTime ${writeWaitTime}
//          |set_interface_property $name holdTime ${holdTime}
//          |
//          |set_interface_property $name associatedClock $clockName
//          |set_interface_property $name associatedReset $resetName
//          |set_interface_property $name bitsPerSymbol 8
//                                         |
//                                         |set_interface_property $name timingUnits Cycles
//                                                                        |set_interface_property $name ENABLED true
//                                                                                                       |set_interface_property $name EXPORT_OF ""
//                                                                                                                                      |set_interface_property $name PORT_NAME_MAP ""
//                                                                                                                                                                     |set_interface_property $name SVD_ADDRESS_GROUP ""
//                                                                                                                                                                                                    |
//""".stripMargin
//
//      if(isMaster){
//        builder ++= s"set_interface_property $name doStreamReads false\n"
//        builder ++= s"set_interface_property $name doStreamWrites false\n"
//      }
//
//      builder ++= s"add_interface_port $name ${e.address.getName} address ${masterPinDir} ${addressWidth}\n"
//      if(useRead) builder ++= s"add_interface_port $name ${e.read.getName} read ${masterPinDir} 1\n"
//      if(useWrite) builder ++= s"add_interface_port $name ${e.write.getName} write ${masterPinDir} 1\n"
//      if(useWaitRequestn) builder ++= s"add_interface_port $name ${e.waitRequestn.getName} waitrequest_n ${slavePinDir} 1\n"
//      if(useLock) builder ++= s"add_interface_port $name ${e.lock.getName} lock ${masterPinDir} 1\n"
//      if(useBurstCount) builder ++= s"add_interface_port $name ${e.burstCount.getName} burstcount ${masterPinDir} ${burstCountWidth}\n"
//      if(useByteEnable) builder ++= s"add_interface_port $name ${e.byteEnable.getName} byteenable ${masterPinDir} ${dataByteCount}\n"
//      if(useWrite) builder ++= s"add_interface_port $name ${e.writeData.getName} writedata ${masterPinDir} ${dataWidth}\n"
//      if(useResponse) builder ++= s"add_interface_port $name ${e.response.getName} response ${slavePinDir} 2\n"
//      if(useReadDataValid) builder ++= s"add_interface_port $name ${e.readDataValid.getName} readdatavalid ${slavePinDir} 1\n"
//      if(useRead) builder ++= s"add_interface_port $name ${e.readData.getName} readdata ${slavePinDir} ${dataWidth}\n"
//      if(useDebugAccess)
//        ???
//      true
//    }
//    case _ => false
//  }
//}
//#
//# connection point streamSinkPort
//#
//add_interface streamSinkPort avalon_streaming end
//set_interface_property streamSinkPort associatedClock clock
//set_interface_property streamSinkPort associatedReset reset
//set_interface_property streamSinkPort dataBitsPerSymbol 16
//set_interface_property streamSinkPort errorDescriptor ""
//set_interface_property streamSinkPort firstSymbolInHighOrderBits true
//set_interface_property streamSinkPort maxChannel 0
//set_interface_property streamSinkPort readyLatency 0
//set_interface_property streamSinkPort ENABLED true
//set_interface_property streamSinkPort EXPORT_OF ""
//set_interface_property streamSinkPort PORT_NAME_MAP ""
//set_interface_property streamSinkPort SVD_ADDRESS_GROUP ""
//
//add_interface_port streamSinkPort asi_in0_data data Input 32
//add_interface_port streamSinkPort asi_in0_valid valid Input 1
//add_interface_port streamSinkPort asi_in0_ready ready Output 1

case class InterruptSenderTag(clockDomain : ClockDomain) extends SpinalTag

class InterruptSenderEmitter extends QSysifyInterfaceEmiter{
  override def emit(i: Data, builder: scala.StringBuilder): Boolean = {
    val tag = i.getTag(classOf[InterruptSenderTag])
    if(tag.isEmpty) return false
    val interfaceName = i.getName()
    val name = i.getName()

    builder ++=
      s"""
|#
|# connection point $interfaceName
|#
|add_interface $interfaceName interrupt end
|set_interface_property $interfaceName associatedClock ${tag.get.clockDomain.clock.getName()}
|set_interface_property $interfaceName associatedReset ${tag.get.clockDomain.reset.getName()}
|set_interface_property $interfaceName ENABLED true
|
|add_interface_port $interfaceName $name irq Output ${i.getBitsWidth}
|""".stripMargin
    true
  }
}

class Axi4Emitter extends QSysifyInterfaceEmiter{
  override def emit(i: Data,builder : StringBuilder): Boolean = i match {
    case e: Axi4 =>{
      import e.config._
      val isMaster = e.isMasterInterface
      val (masterPinDir,slavePinDir,startEnd) = if(isMaster) ("Output", "Input","start") else ("Input","Output","end")
      val name = e.getName()
      val clockDomainTag = e.getTag(classOf[ClockDomainTag])
      if(clockDomainTag.isEmpty) SpinalError(s"Clock domain of ${i} is not defined, You shoud apply the ClockDomainTag to the inferface\nyourBus.addTag(ClockDomainTag(ClockDomain.current))")
      val clockName = clockDomainTag.get.clockDomain.clock.getName()
      val resetName = clockDomainTag.get.clockDomain.reset.getName()
      builder ++= s"""
|#
|# connection point $name
|#
|add_interface $name axi4 $startEnd
|
|set_interface_property $name associatedClock $clockName
|set_interface_property $name associatedReset $resetName
|
|set_interface_property $name ENABLED true
|set_interface_property $name EXPORT_OF ""
|set_interface_property $name PORT_NAME_MAP ""
|set_interface_property $name SVD_ADDRESS_GROUP ""
""".stripMargin

      if(isMaster) {
        if(readIssuingCapability > -1)
          builder ++= s"set_interface_property $name readIssuingCapability ${readIssuingCapability}\n"
        if(writeIssuingCapability > -1)
          builder ++= s"set_interface_property $name writeIssuingCapability ${writeIssuingCapability}\n"
        if(combinedIssuingCapability > -1)
          builder ++= s"set_interface_property $name combinedIssuingCapability ${combinedIssuingCapability}\n"
      }
      else {
        if(readIssuingCapability > -1)
          builder ++= s"set_interface_property $name readAcceptanceCapability ${readIssuingCapability}\n"
        if(writeIssuingCapability > -1)
          builder ++= s"set_interface_property $name writeAcceptanceCapability ${writeIssuingCapability}\n"
        if(combinedIssuingCapability > -1)
          builder ++= s"set_interface_property $name combinedAcceptanceCapability ${combinedIssuingCapability}\n"
        if(readDataReorderingDepth > -1)
          builder ++= s"set_interface_property $name readDataReorderingDepth ${readDataReorderingDepth}\n"
      }

      // emit AR
      builder ++= s"""
|add_interface_port $name ${e.ar.valid.getName()} arvalid ${masterPinDir} 1
|add_interface_port $name ${e.ar.ready.getName()} arready ${slavePinDir} 1
|add_interface_port $name ${e.ar.payload.addr.getName()} araddr ${masterPinDir} ${addressWidth}
""".stripMargin

      if(useId) builder ++= s"add_interface_port $name ${e.ar.payload.id.getName()} arid ${masterPinDir} ${idWidth}\n"
      if(useLen) builder ++= s"add_interface_port $name ${e.ar.payload.len.getName()} arlen ${masterPinDir} 8\n"
      if(useSize) builder ++= s"add_interface_port $name ${e.ar.payload.size.getName()} arsize ${masterPinDir} 3\n"
      if(useBurst) builder ++= s"add_interface_port $name ${e.ar.payload.burst.getName()} arburst ${masterPinDir} 2\n"
      if(useLock) builder ++= s"add_interface_port $name ${e.ar.payload.lock.getName()} arlock ${masterPinDir} 2\n"
      if(useCache) builder ++= s"add_interface_port $name ${e.ar.payload.cache.getName()} arcache ${masterPinDir} 4\n"
      if(useProt) builder ++= s"add_interface_port $name ${e.ar.payload.prot.getName()} arprot ${masterPinDir} 3\n"
      if(useArUser) builder ++= s"add_interface_port $name ${e.b.payload.user.getName()} aruser ${slavePinDir} ${arUserWidth}\n"
      
      // emit AW
      builder ++= s"""
|add_interface_port $name ${e.aw.valid.getName()} awvalid ${masterPinDir} 1
|add_interface_port $name ${e.aw.ready.getName()} awready ${slavePinDir} 1
|add_interface_port $name ${e.aw.payload.addr.getName()} awaddr ${masterPinDir} ${addressWidth}
""".stripMargin

      if(useId) builder ++= s"add_interface_port $name ${e.aw.payload.id.getName()} awid ${masterPinDir} ${idWidth}\n"
      if(useLen) builder ++= s"add_interface_port $name ${e.aw.payload.len.getName()} awlen ${masterPinDir} 8\n"
      if(useSize) builder ++= s"add_interface_port $name ${e.aw.payload.size.getName()} awsize ${masterPinDir} 3\n"
      if(useBurst) builder ++= s"add_interface_port $name ${e.aw.payload.burst.getName()} awburst ${masterPinDir} 2\n"
      if(useLock) builder ++= s"add_interface_port $name ${e.aw.payload.lock.getName()} awlock ${masterPinDir} 2\n"
      if(useCache) builder ++= s"add_interface_port $name ${e.aw.payload.cache.getName()} awcache ${masterPinDir} 4\n"
      if(useProt) builder ++= s"add_interface_port $name ${e.aw.payload.prot.getName()} awprot ${masterPinDir} 3\n"
      if(useAwUser) builder ++= s"add_interface_port $name ${e.b.payload.user.getName()} awuser ${slavePinDir} ${awUserWidth}\n"
      
      // emit R
      builder ++= s"""
|add_interface_port $name ${e.r.valid.getName()} rvalid ${slavePinDir} 1
|add_interface_port $name ${e.r.ready.getName()} rready ${masterPinDir} 1
|add_interface_port $name ${e.r.payload.data.getName()} rdata ${slavePinDir} ${dataWidth}
""".stripMargin

      if(useId) builder ++= s"add_interface_port $name ${e.r.payload.id.getName()} rid ${slavePinDir} ${idWidth}\n"
      if(useLast) builder ++= s"add_interface_port $name ${e.r.payload.last.getName()} rlast ${slavePinDir} 1\n"
      if(useResp) builder ++= s"add_interface_port $name ${e.r.payload.resp.getName()} rresp ${slavePinDir} 2\n"
      if(useRUser) builder ++= s"add_interface_port $name ${e.b.payload.user.getName()} ruser ${slavePinDir} ${rUserWidth}\n"

      // emit W
      builder ++= s"""
|add_interface_port $name ${e.w.valid.getName()} wvalid ${masterPinDir} 1
|add_interface_port $name ${e.w.ready.getName()} wready ${slavePinDir} 1
|add_interface_port $name ${e.w.payload.data.getName()} wdata ${masterPinDir} ${dataWidth}
""".stripMargin

      if(useLast) builder ++= s"add_interface_port $name ${e.w.payload.last.getName()} wlast ${masterPinDir} 1\n"
      if(useStrb) builder ++= s"add_interface_port $name ${e.w.payload.strb.getName()} wstrb ${masterPinDir} ${dataWidth/8}\n"
      if(useWUser) builder ++= s"add_interface_port $name ${e.b.payload.user.getName()} wuser ${slavePinDir} ${wUserWidth}\n"

      // emit B
      builder ++= s"""
|add_interface_port $name ${e.b.valid.getName()} bvalid ${slavePinDir} 1
|add_interface_port $name ${e.b.ready.getName()} bready ${masterPinDir} 1
""".stripMargin

      if(useId) builder ++= s"add_interface_port $name ${e.b.payload.id.getName()} bid ${slavePinDir} ${idWidth}\n"
      if(useResp) builder ++= s"add_interface_port $name ${e.b.payload.resp.getName()} bresp ${slavePinDir} 2\n"
      if(useBUser) builder ++= s"add_interface_port $name ${e.b.payload.user.getName()} buser ${slavePinDir} ${bUserWidth}\n"


      true
      }
    case _ => false
  }
}

class AxiLite4Emitter extends QSysifyInterfaceEmiter{
  override def emit(i: Data,builder : StringBuilder): Boolean = i match {
    case e: AxiLite4 =>{
      import e.config._
      val isMaster = e.isMasterInterface
      val (masterPinDir,slavePinDir,startEnd) = if(isMaster) ("Output", "Input","start") else ("Input","Output","end")
      val name = e.getName()
      val clockDomainTag = e.getTag(classOf[ClockDomainTag])
      if(clockDomainTag.isEmpty) SpinalError(s"Clock domain of ${i} is not defined, You shoud apply the ClockDomainTag to the inferface\nyourBus.addTag(ClockDomainTag(ClockDomain.current))")
      val clockName = clockDomainTag.get.clockDomain.clock.getName()
      val resetName = clockDomainTag.get.clockDomain.reset.getName()
      builder ++= s"""
|#
|# connection point $name
|#
|add_interface $name axi4lite $startEnd
|
|set_interface_property $name associatedClock $clockName
|set_interface_property $name associatedReset $resetName
|
|set_interface_property $name ENABLED true
|set_interface_property $name EXPORT_OF ""
|set_interface_property $name PORT_NAME_MAP ""
|set_interface_property $name SVD_ADDRESS_GROUP ""
""".stripMargin

      if(isMaster) {
        if(readIssuingCapability > -1)
          builder ++= s"set_interface_property $name readIssuingCapability ${readIssuingCapability}\n"
        if(writeIssuingCapability > -1)
          builder ++= s"set_interface_property $name writeIssuingCapability ${writeIssuingCapability}\n"
        if(combinedIssuingCapability > -1)
          builder ++= s"set_interface_property $name combinedIssuingCapability ${combinedIssuingCapability}\n"
      }
      else {
        if(readIssuingCapability > -1)
          builder ++= s"set_interface_property $name readAcceptanceCapability ${readIssuingCapability}\n"
        if(writeIssuingCapability > -1)
          builder ++= s"set_interface_property $name writeAcceptanceCapability ${writeIssuingCapability}\n"
        if(combinedIssuingCapability > -1)
          builder ++= s"set_interface_property $name combinedAcceptanceCapability ${combinedIssuingCapability}\n"
        if(readDataReorderingDepth > -1)
          builder ++= s"set_interface_property $name readDataReorderingDepth ${readDataReorderingDepth}\n"
      }

      // emit AR
      builder ++= s"""
|add_interface_port $name ${e.ar.valid.getName()} arvalid ${masterPinDir} 1
|add_interface_port $name ${e.ar.ready.getName()} arready ${slavePinDir} 1
|add_interface_port $name ${e.ar.payload.addr.getName()} araddr ${masterPinDir} ${addressWidth}
|add_interface_port $name ${e.ar.payload.prot.getName()} arprot ${masterPinDir} 3
""".stripMargin

      // emit AW
      builder ++= s"""
|add_interface_port $name ${e.aw.valid.getName()} awvalid ${masterPinDir} 1
|add_interface_port $name ${e.aw.ready.getName()} awready ${slavePinDir} 1
|add_interface_port $name ${e.aw.payload.addr.getName()} awaddr ${masterPinDir} ${addressWidth}
|add_interface_port $name ${e.aw.payload.prot.getName()} awprot ${masterPinDir} 3
""".stripMargin

      // emit R
      builder ++= s"""
|add_interface_port $name ${e.r.valid.getName()} rvalid ${slavePinDir} 1
|add_interface_port $name ${e.r.ready.getName()} rready ${masterPinDir} 1
|add_interface_port $name ${e.r.payload.data.getName()} rdata ${slavePinDir} ${dataWidth}
|add_interface_port $name ${e.r.payload.resp.getName()} rresp ${slavePinDir} 2
""".stripMargin

      // emit W
      builder ++= s"""
|add_interface_port $name ${e.w.valid.getName()} wvalid ${masterPinDir} 1
|add_interface_port $name ${e.w.ready.getName()} wready ${slavePinDir} 1
|add_interface_port $name ${e.w.payload.data.getName()} wdata ${masterPinDir} ${dataWidth}
|add_interface_port $name ${e.w.payload.strb.getName()} wstrb ${masterPinDir} ${dataWidth/8}
""".stripMargin

      // emit B
      builder ++= s"""
|add_interface_port $name ${e.b.valid.getName()} bvalid ${slavePinDir} 1
|add_interface_port $name ${e.b.ready.getName()} bready ${masterPinDir} 1
|add_interface_port $name ${e.b.payload.resp.getName()} bresp ${slavePinDir} 2
""".stripMargin

      true
      }
    case _ => false
  }
}