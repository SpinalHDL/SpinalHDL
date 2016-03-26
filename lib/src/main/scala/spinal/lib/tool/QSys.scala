package spinal.lib.tool

import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon.mm.AvalonMMBus

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, StringBuilder}

object QSysify{
  def apply(that : Component) : Unit = {
    val tool = new QSysify
    tool.interfaceEmiters += new AvalonEmitter()
    tool.interfaceEmiters += new ClockDomainEmitter()
    tool.interfaceEmiters += new ConduitEmitter()

    tool.emit(that)
  }
}

trait QSysifyInterfaceEmiter{
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
    case e: AvalonMMBus =>{
      import e.c._
      val name = e.getName()
      val clockDomainTag = e.getTag[ClockDomainTag]
      if(clockDomainTag.isEmpty) SpinalError(s"Clock domain of ${i} is not defined, You shoud apply the ClockDomainTag to the inferface\nyourBus.addTag(ClockDomainTag(ClockDomain.current))")
      val clockName = clockDomainTag.get.clockDomain.clock.getName()
      val resetName = clockDomainTag.get.clockDomain.reset.getName()
      builder ++= s"""
|#
|# connection point $name
|#
|add_interface $name avalon start
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
|set_interface_property $name doStreamReads false
|set_interface_property $name doStreamWrites false
|
|set_interface_property $name timingUnits Cycles
|set_interface_property $name ENABLED true
|set_interface_property $name EXPORT_OF ""
|set_interface_property $name PORT_NAME_MAP ""
|set_interface_property $name SVD_ADDRESS_GROUP ""
|
""".stripMargin

      val (masterPinDir,slavePinDir) = if(e.address.isOutput) ("Output", "Input") else ("Input","Output")
      builder ++= s"add_interface_port $name ${e.address.getName} address ${masterPinDir} ${addressWidth}\n"
      if(useRead) builder ++= s"add_interface_port $name ${e.read.getName} read ${masterPinDir} 1\n"
      if(useWrite) builder ++= s"add_interface_port $name ${e.write.getName} write ${masterPinDir} 1\n"
      if(useWaitRequestn) builder ++= s"add_interface_port $name ${e.waitRequestn.getName} waitrequest_n ${slavePinDir} 1\n"
      if(useLock) builder ++= s"add_interface_port $name ${e.lock.getName} lock ${masterPinDir} 1\n"
      if(useBurstCount) builder ++= s"add_interface_port $name ${e.burstCount.getName} burstcount ${masterPinDir} ${burstCountWidth}\n"
      if(useByteEnable) builder ++= s"add_interface_port $name ${e.byteEnable.getName} byteenable ${masterPinDir} ${dataByteCount}\n"
      if(useWrite) builder ++= s"add_interface_port $name ${e.writeData.getName} writedata ${masterPinDir} ${dataWidth}\n"
      if(useResponse) builder ++= s"add_interface_port $name ${e.response.getName} response ${slavePinDir} 2\n"
      if(useReadDataValid) builder ++= s"add_interface_port $name ${e.readDataValid.getName} readdatavalid ${slavePinDir} 1\n"
      if(useRead) builder ++= s"add_interface_port $name ${e.readData.getName} readdata ${slavePinDir} ${dataWidth}\n"
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
      if(dirStr != null)
        builder ++= s"add_interface_port $name ${e.getName()} export ${dirStr} ${e.getWidth}\n"
    }
    builder ++= "\n\n"
    true
  }
}