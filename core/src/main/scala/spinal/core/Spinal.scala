/*                                                                           *\
**        _____ ____  _____   _____    __                                    **
**       / ___// __ \/  _/ | / /   |  / /   HDL Core                         **
**       \__ \/ /_/ // //  |/ / /| | / /    (c) Dolu, All rights reserved    **
**      ___/ / ____// // /|  / ___ |/ /___                                   **
**     /____/_/   /___/_/ |_/_/  |_/_____/                                   **
**                                                                           **
**      This library is free software; you can redistribute it and/or        **
**    modify it under the terms of the GNU Lesser General Public             **
**    License as published by the Free Software Foundation; either           **
**    version 3.0 of the License, or (at your option) any later version.     **
**                                                                           **
**      This library is distributed in the hope that it will be useful,      **
**    but WITHOUT ANY WARRANTY; without even the implied warranty of         **
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      **
**    Lesser General Public License for more details.                        **
**                                                                           **
**      You should have received a copy of the GNU Lesser General Public     **
**    License along with this library.                                       **
\*                                                                           */
package spinal.core


import org.apache.commons.io.FileUtils

import java.io.{BufferedWriter, File, FileWriter}
import spinal.core.internals._

import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source
import scala.util.matching.Regex


//TODO
// undefined

trait SpinalMode
object VHDL    extends SpinalMode
object Verilog extends SpinalMode
object SystemVerilog extends SpinalMode


case class DumpWaveConfig(depth: Int = 0, vcdPath: String = "wave.vcd")



/**
 * target device
 */
case class Device(vendor: String = "?",
                  family: String = "?",
                  name: String = "?",
                  supportBootResetKind : Boolean = true){
  def isVendorDefault = vendor == "?"
}
object Device{
  val ALTERA = Device(vendor = "altera")
  val XILINX = Device(vendor = "xilinx")
  val LATTICE = Device(vendor = "lattice")
  val ACTEL = Device(vendor = "actel")
  val ASIC = Device(vendor = "asic", supportBootResetKind = false)
  val NONE = Device(vendor = "none")
}


trait MemBlackboxingPolicy {
  def translationInterest(topology: MemTopology): Boolean

  def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit

  def generateUnblackboxableError(topology: MemTopology, who: Any, message: String): Unit = {
    PendingError(s"${this.getClass} is not able to blackbox ${topology.mem}\n  write ports : ${topology.writes.size} \n  readAsync ports : ${topology.readsAsync.size} \n  readSync ports : ${topology.readsSync.size} \n  readWrite ports : ${topology.readWriteSync.size}\n  -> $message")
  }
}


object blackboxAllWhatsYouCan extends MemBlackboxingPolicy {
  override def translationInterest(topology: MemTopology): Boolean = true

  override def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit = {}
}


object blackboxAll extends MemBlackboxingPolicy {
  override def translationInterest(topology: MemTopology): Boolean = true

  override def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit = generateUnblackboxableError(topology, who, message)
}


object blackboxRequestedAndUninferable extends MemBlackboxingPolicy {

  override def translationInterest(topology: MemTopology): Boolean = {
    if(blackboxOnlyIfRequested.translationInterest(topology))                      return true
    if(topology.readsAsync.exists(_.readUnderWrite != writeFirst))                 return true
    if(topology.readsSync.exists(_.readUnderWrite != readFirst))                   return true
    if(topology.writeReadSameAddressSync.exists(_._2.readUnderWrite != readFirst)) return true
//    if(topology.readWriteSync.exists(_._2.readUnderWrite != readFirst)) return true
    return false
  }

  override def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit = generateUnblackboxableError(topology, who, message)
}


object blackboxOnlyIfRequested extends MemBlackboxingPolicy{
  override def translationInterest(topology: MemTopology): Boolean = {
    topology.mem.forceMemToBlackboxTranslation
  }

  override def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit = generateUnblackboxableError(topology, who, message)
}

object blackboxByteEnables extends MemBlackboxingPolicy{
  override def translationInterest(topology: MemTopology): Boolean = {
    if(topology.writes.exists(_.mask != null) && topology.mem.initialContent == null) return true
    if(topology.readWriteSync.exists(_.mask != null) && topology.mem.initialContent == null) return true
    false
  }

  override def onUnblackboxable(topology: MemTopology, who: Any, message: String): Unit = generateUnblackboxableError(topology, who, message)
}


/**
 * Spinal configuration for the generation of the RTL 
 */
case class SpinalConfig(mode                           : SpinalMode = null,
                        flags                          : mutable.HashSet[Any] = mutable.HashSet[Any](),
                        debugComponents                : mutable.HashSet[Class[_]] = mutable.HashSet[Class[_]](),
                        keepAll                        : Boolean = false,
                        defaultConfigForClockDomains   : ClockDomainConfig = ClockDomainConfig(),
                        onlyStdLogicVectorAtTopLevelIo : Boolean = false,
                        defaultClockDomainFrequency    : IClockDomainFrequency = UnknownFrequency(),
                        targetDirectory                : String = SpinalConfig.defaultTargetDirectory,
                        oneFilePerComponent            : Boolean = false,
                        netlistFileName                : String = null,
                        dumpWave                       : DumpWaveConfig = null,
                        globalPrefix                   : String = "",
                        var privateNamespace           : Boolean = false,
                        var formalAsserts              : Boolean = false,
                        anonymSignalPrefix             : String = null,
                        device                         : Device = Device(),
                        inlineRom                      : Boolean = false,
                        romReuse                       : Boolean = false,
                        genVhdlPkg                     : Boolean = true,
                        verbose                        : Boolean = false,
                        mergeAsyncProcess              : Boolean = false,
                        asyncResetCombSensitivity      : Boolean = false,
                        anonymSignalUniqueness         : Boolean = false,
                        inlineConditionalExpression    : Boolean = false,
                        nameWhenByFile                 : Boolean = true,
                        var genLineComments            : Boolean = false,
                        noRandBoot                     : Boolean = false,
                        randBootFixValue               : Boolean = true,
                        noAssert                       : Boolean = false,
                        fixToWithWrap                  : Boolean = true,
                        headerWithDate                 : Boolean = false,
                        headerWithRepoHash             : Boolean = true,
                        removePruned                   : Boolean = false,
                        allowOutOfRangeLiterals        : Boolean = false,
                        dontCareGenAsZero              : Boolean = false,
                        phasesInserters                : ArrayBuffer[(ArrayBuffer[Phase]) => Unit] = ArrayBuffer[(ArrayBuffer[Phase]) => Unit](),
                        transformationPhases           : ArrayBuffer[Phase] = ArrayBuffer[Phase](),
                        memBlackBoxers                 : ArrayBuffer[Phase] = ArrayBuffer[Phase] (/*new PhaseMemBlackBoxerDefault(blackboxNothing)*/),
                        rtlHeader                      : String = null,
                        scopeProperties                : mutable.LinkedHashMap[ScopeProperty[_], Any] = mutable.LinkedHashMap[ScopeProperty[_], Any](),
                        private [core] var _withEnumString : Boolean = true,
                        var enumPrefixEnable           : Boolean = true,
                        var enumGlobalEnable           : Boolean = false,
                        bitVectorWidthMax              : Int = 4096,
                        var singleTopLevel             : Boolean = true
){
  def generate       [T <: Component](gen: => T): SpinalReport[T] = Spinal(this)(gen)
  def generateVhdl   [T <: Component](gen: => T): SpinalReport[T] = Spinal(this.copy(mode = VHDL))(gen)
  def generateVerilog[T <: Component](gen: => T): SpinalReport[T] = Spinal(this.copy(mode = Verilog))(gen)
  def generateSystemVerilog[T <: Component](gen: => T): SpinalReport[T] = Spinal(this.copy(mode = SystemVerilog))(gen)

  def apply[T <: Component](gen : => T): SpinalReport[T] = {
    Spinal(this)(gen)
  }

  def applyToGlobalData(globalData: GlobalData): Unit = {
    globalData.scalaLocatedEnable = debugComponents.nonEmpty
    globalData.scalaLocatedComponents ++= debugComponents
    globalData.commonClockConfig  = defaultConfigForClockDomains
    for((p, v) <- scopeProperties){
      p.asInstanceOf[ScopeProperty[Any]].set(v)
    }
  }

  def dumpWave(depth: Int = 0, vcdPath: String = "wave.vcd"): SpinalConfig = this.copy(dumpWave=DumpWaveConfig(depth,vcdPath))

  def addTransformationPhase(phase: Phase): SpinalConfig = {
    transformationPhases += phase
    this
  }

  def isSystemVerilog = mode == SystemVerilog

  def withPrivateNamespace : this.type = { privateNamespace = true; this }

  def addStandardMemBlackboxing(policy: MemBlackboxingPolicy): this.type = {
    memBlackBoxers += new PhaseMemBlackBoxingDefault(policy)
    this
  }

  def withoutAssert : SpinalConfig = this.copy(noAssert = true)
  def withoutEnumString() : this.type = {
    _withEnumString = false
    this
  }
  def includeSynthesis : this.type = {flags += GenerationFlags.synthesis; this}
  def includeFormal : this.type = {flags += GenerationFlags.formal; formalAsserts = true; this}
  def includeSimulation : this.type = {flags += GenerationFlags.simulation; this}


  def setScopeProperty[T](scopeProperty: ScopeProperty[T], value : T): this.type ={
    scopeProperties(scopeProperty) = value
    this
  }

  def setScopeProperty[T](value: ScopePropertyValue): this.type ={
    scopeProperties(value.dady) = value
    this
  }

  def withGlobalEnum: this.type ={
    enumGlobalEnable = true
    this
  }

  def withoutLineComment: this.type = {
    genLineComments = false
    this
  }
  def withLineComment: this.type = {
    genLineComments = true
    this
  }
}
class GenerationFlags {
  def isEnabled = GlobalData.get.config.flags.contains(this)
  def apply[T](block : => T) : T = if(isEnabled) block else null.asInstanceOf[T]
}

object GenerationFlags{
  object synthesis extends GenerationFlags
  object formal extends GenerationFlags
  object simulation extends GenerationFlags

  implicit def generationFlagsToBoolean(flag : GenerationFlags) : Boolean = flag.isEnabled
}

object SpinalConfig{
  def shell[T <: Component](args: Seq[String]): SpinalConfig = {
    val parser = new scopt.OptionParser[SpinalConfig]("SpinalCore") {
      opt[Unit]("vhdl")                   action { (_, c) => c.copy(mode = VHDL)         } text("Select the VHDL mode")
      opt[Unit]("verilog")                action { (_, c) => c.copy(mode = Verilog)      } text("Select the Verilog mode")
      opt[String]('o', "targetDirectory") action { (v, c) => c.copy(targetDirectory = v) } text("Set the target directory")
    }

    parser.parse(args, SpinalConfig()) match {
      case Some(config) => config
      case None         => ???
    }
  }

  var defaultTargetDirectory: String = System.getenv().getOrDefault("SPINAL_TARGET_DIR", ".")
}


/**
 * Spinal report give after the generation of the RTL
 */
class SpinalReport[T <: Component]() {
  var toplevel: T     = null.asInstanceOf[T]
  val prunedSignals   = mutable.Set[BaseType]()
  val unusedSignals   = mutable.Set[BaseType]()
  var counterRegister = 0
  var toplevelName: String = null
  var globalData : GlobalData = null


  val generatedSourcesPaths  = mutable.LinkedHashSet[String]()
  val blackboxesSourcesPaths = mutable.LinkedHashSet[String]()
  val blackboxesIncludeDir = mutable.LinkedHashSet[String]()
  def rtlSourcesPaths        = generatedSourcesPaths ++ blackboxesSourcesPaths
  def rtlIncludeDirs        = blackboxesIncludeDir


  def printUnused() : this.type = {
    unusedSignals.foreach(bt => SpinalWarning(s"Unused wire detected : $bt"))
    this
  }

  def printPruned() : this.type = {
    prunedSignals.foreach(bt => SpinalWarning(s"Pruned wire detected : $bt"))
    this
  }

  def printPrunedIo() : this.type = {
    prunedSignals.filter(_.dir != null).foreach(bt => SpinalWarning(s"Pruned wire detected : $bt"))
    this
  }

  def printRtl() : this.type = {
    for(f <- generatedSourcesPaths){
      println(scala.io.Source.fromFile(f).mkString)
    }
    this
  }

  def printZeroWidth() : this.type = {
    if(globalData.zeroWidths.isEmpty) return this
    globalData.zeroWidths.foreach{case (c, n) =>
      SpinalWarning(s"${c}/${n.toString}")
    }
    this
  }


  def mergeRTLSource(fileName: String = null): Unit = {

    val bb_vhdl    = new mutable.LinkedHashSet[String]()
    val bb_verilog = new mutable.LinkedHashSet[String]()

    /** Split verilog/vhdl path */
    blackboxesSourcesPaths.foreach{ path =>
      val vhdl_regex    = """.*\.(vhdl|vhd)""".r
      val verilog_regex = """.*\.(v)""".r
      val systemVerilog_regex = """.*\.(sv)""".r

      path.toLowerCase match {
        case vhdl_regex(f)    => bb_vhdl    += path
        case verilog_regex(f) => bb_verilog += path
        case systemVerilog_regex(f) => bb_verilog += path
        case _                => SpinalWarning(s"Merging blackbox sources : Extension file not supported (${path})")
      }
    }


    /** Merge a list of path into one file */
    def mergeFile(listPath: mutable.LinkedHashSet[String], fileName: String) {
      val str = new StringBuilder() //We use a temporary string to build the result, allowing overriding on input file as output

      listPath.foreach{ path =>
        if( new File(path).exists ) {
          val buffer = Source.fromFile(path)
          buffer.getLines.foreach{ line => str. ++= (line + "\n") }
          buffer.close()
        }else{
          SpinalWarning(s"Merging blackbox sources : Path (${new File(path).getAbsolutePath}) not found ")
        }
      }
      val fw = new FileWriter(new File(s"${globalData.config.targetDirectory}/$fileName"))
      val bw = new BufferedWriter(fw)
      bw.write(str.toString())
      bw.close()
      fw.close()
    }

    // Merge vhdl/verilog file
    val nameFile = if(fileName == null) s"${toplevel.definitionName}_bb" else fileName
    if(bb_vhdl.size > 0)   { mergeFile(bb_vhdl,    s"${nameFile}.vhd") }
    if (globalData.config.mode == Verilog) {
      if(bb_verilog.size > 0){ mergeFile(bb_verilog, s"${nameFile}.v") }
    } else if (globalData.config.mode == SystemVerilog) {
      if(bb_verilog.size > 0){ mergeFile(bb_verilog, s"${nameFile}.sv") }
    }

  }

  def getRtlString(): String = {
    assert(generatedSourcesPaths.size == 1)
    scala.io.Source.fromFile(generatedSourcesPaths.head).mkString
  }
}


object Spinal{
  val version = (if(Character.isDigit(spinal.core.Info.version(0))) "v" else "") + spinal.core.Info.version

  def apply[T <: Component](config: SpinalConfig)(gen: => T): SpinalReport[T] = {

    if(config.memBlackBoxers.isEmpty)
      config.addStandardMemBlackboxing(blackboxOnlyIfRequested)
    val configPatched = config.copy(targetDirectory = if(config.targetDirectory.startsWith("~")) System.getProperty( "user.home" ) + config.targetDirectory.drop(1) else config.targetDirectory)

    println({
      SpinalLog.tag("Runtime", Console.YELLOW)
    } + s" SpinalHDL $version    git head : ${spinal.core.Info.gitHash}")


    val runtime = Runtime.getRuntime
    println({
      SpinalLog.tag("Runtime", Console.YELLOW)
    } + s" JVM max memory : ${f"${(runtime.maxMemory()).toFloat / 1048576f}%1.1f"}MiB")

    val curDate: Date = new Date()
    val dateFmt: SimpleDateFormat = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss")
    println({
      SpinalLog.tag("Runtime", Console.YELLOW)
    } + s" Current date : ${dateFmt.format(curDate)}")

    FileUtils.forceMkdir(new File(config.targetDirectory))

    val report = configPatched.mode match {
      case `VHDL`    => SpinalVhdlBoot(configPatched)(gen)
      case `Verilog` => SpinalVerilogBoot(configPatched)(gen)
      case `SystemVerilog` => SpinalVerilogBoot(configPatched)(gen)
      case null => throw new Exception("Please specify mode in SpinalConfig (mode=[Verilog, SystemVerilog, VHDL])")
    }

    println({SpinalLog.tag("Done", Console.GREEN)} + s" at ${f"${Driver.executionTime}%1.3f"}")
    report
  }
}


object SpinalVhdl {
  def apply[T <: Component](config: SpinalConfig)(gen: => T): SpinalReport[T] = Spinal(config.copy(mode = VHDL))(gen)
  def apply[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig(mode = VHDL).generate(gen)
}


object SpinalVerilog {
  def apply[T <: Component](config: SpinalConfig)(gen: => T): SpinalReport[T] = Spinal(config.copy(mode = Verilog))(gen)
  def apply[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig(mode = Verilog).generate(gen)
}

object SpinalSystemVerilog {
  def apply[T <: Component](config: SpinalConfig)(gen: => T): SpinalReport[T] = Spinal(config.copy(mode = SystemVerilog))(gen)
  def apply[T <: Component](gen: => T): SpinalReport[T] = SpinalConfig(mode = SystemVerilog).generate(gen)
}
