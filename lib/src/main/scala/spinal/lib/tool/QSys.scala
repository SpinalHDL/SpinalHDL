package spinal.lib.tool

import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon.mm.AvalonMMBus

import scala.collection.mutable.{ArrayBuffer, StringBuilder}



case class QSysify[T <: Component](that: T){
  val name = that.definitionName
  var out: java.io.FileWriter = null
  out = new java.io.FileWriter(name + "_hw.tcl")
  val builder = new StringBuilder()




  genHeader()
  genInterfaces()
  genClockDomains


  out.write(builder.toString())
  out.flush();
  out.close();


  def genHeader(): Unit = {
    builder ++= s"""
| package require -exact qsys 13.1
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
//    def walk(that : Data): Unit ={
//
//    }
    val interfaces = ArrayBuffer[Any]()
    for(e <- that.getGroupedIO(true)){
      println(e)
      e match {
        case e : AvalonMMBus => interfaces += e
        case _ => SpinalWarning("???")
      }
    }

    for(i <- interfaces) i match{
      case e : AvalonMMBus =>{
        import e.c._
        val name = e.getName()
        builder ++=s"""
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
|set_interface_property $name associatedClock clock
|set_interface_property $name associatedReset reset
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

        val (masterPinDir,slavePinDir) = if(e.address.isOutput) ("Output","Input") else ("Input","Output")
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
        if(useDebugAccess) ???
      }
    }
  }
  def genClockDomains = {
    builder ++= s"""
|#
|# connection point clock
|#
|add_interface clock clock end
|set_interface_property clock clockRate 0
|set_interface_property clock ENABLED true
|set_interface_property clock EXPORT_OF ""
|set_interface_property clock PORT_NAME_MAP ""
|
|set_interface_property clock SVD_ADDRESS_GROUP ""
|
|add_interface_port clock clk clk Input 1
|
|#
|# connection point reset
|#
|add_interface reset reset end
|set_interface_property reset associatedClock clock
|set_interface_property reset synchronousEdges DEASSERT
|set_interface_property reset ENABLED true
|set_interface_property reset EXPORT_OF ""
|set_interface_property reset PORT_NAME_MAP ""
|
|set_interface_property reset SVD_ADDRESS_GROUP ""
|
|add_interface_port reset reset reset Input 1
|
|""".stripMargin
  }

}




