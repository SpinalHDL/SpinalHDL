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
package spinal.core.sim

import java.io.{File, PrintWriter}
import org.apache.commons.io.FileUtils
import spinal.core.internals.{BaseNode, DeclarationStatement, GraphUtils, PhaseCheck, PhaseContext, PhaseNetlist}
import spinal.core.{BaseType, Bits, BlackBox, Bool, Component, GlobalData, InComponent, Mem, MemSymbolesMapping, MemSymbolesTag, SInt, ScopeProperty, SpinalConfig, SpinalEnumCraft, SpinalReport, SpinalTag, SpinalTagReady, TimeNumber, UInt, Verilator, noLatchCheck}
import spinal.sim._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random
import sys.process._


case class SpinalVerilatorBackendConfig[T <: Component](
                                                         rtl               : SpinalReport[T],
                                                         waveFormat        : WaveFormat = WaveFormat.NONE,
                                                         maxCacheEntries   : Int = 100,
                                                         cachePath         : String = null,
                                                         workspacePath     : String = "./",
                                                         workspaceName     : String = null,
                                                         vcdPath           : String = null,
                                                         vcdPrefix         : String = null,
                                                         waveDepth         : Int = 0,
                                                         optimisationLevel : Int = 2,
                                                         simulatorFlags    : ArrayBuffer[String] = ArrayBuffer[String](),
                                                         withCoverage      : Boolean,
                                                         timePrecision     : TimeNumber = null,
                                                         testPath          : String
)


object SpinalVerilatorBackend {

  def apply[T <: Component](config: SpinalVerilatorBackendConfig[T]) = {

    import config._

    val vconfig = new VerilatorBackendConfig()
    vconfig.rtlIncludeDirs ++= rtl.rtlIncludeDirs
    vconfig.rtlSourcesPaths ++= rtl.rtlSourcesPaths
    vconfig.toplevelName      = rtl.toplevelName
    vconfig.vcdPath           = vcdPath
    vconfig.vcdPrefix         = vcdPrefix
    vconfig.maxCacheEntries   = maxCacheEntries
    vconfig.cachePath         = cachePath
    vconfig.workspaceName     = workspaceName
    vconfig.workspacePath     = workspacePath
    vconfig.waveFormat        = waveFormat match {
      case WaveFormat.DEFAULT => WaveFormat.VCD
      case _ => waveFormat
    }
    vconfig.waveDepth         = waveDepth
    vconfig.optimisationLevel = optimisationLevel
    vconfig.simulatorFlags        = simulatorFlags
    vconfig.withCoverage  = withCoverage
    vconfig.timePrecision = config.timePrecision match {
      case null => null
      case v => v.decomposeString
    }

    var signalId = 0

    def addSignal(bt: DeclarationStatement with InComponent): Unit ={
      val signal = new Signal(config.rtl.toplevelName +: bt.getComponents().tail.map(_.getName()) :+ bt.getName(), bt match{
        case bt: Bool               => new BoolDataType
        case bt: Bits               => new BitsDataType(bt.getBitsWidth)
        case bt: UInt               => new UIntDataType(bt.getBitsWidth)
        case bt: SInt               => new SIntDataType(bt.getBitsWidth)
        case bt: SpinalEnumCraft[_] => new BitsDataType(bt.getBitsWidth)
        case mem: Mem[_] => new BitsDataType(mem.width).setMem()
      })

      bt.algoInt = signalId
      bt.algoIncrementale = -1
      signal.id = signalId
      vconfig.signals += signal
      signalId += 1
    }

    GraphUtils.walkAllComponents(rtl.toplevel, c => c.dslBody.walkStatements(s => {
      s match {
        case bt: BaseType if bt.hasTag(Verilator.public) && !(!bt.isDirectionLess && bt.component.parent == null) => {
          addSignal(bt)
        }
        case mem : Mem[_] if mem.hasTag(Verilator.public) => {
          val tag = mem.getTag(classOf[MemSymbolesTag])
          mem.algoInt = signalId
          mem.algoIncrementale = -1
          tag match {
            case None => addSignal(mem)
            case Some(tag) => {
              for(mapping <- tag.mapping){
                val signal =  new Signal(config.rtl.toplevelName +: mem.getComponents().tail.map(_.getName()) :+ mapping.name, new BitsDataType(mapping.width).setMem())
                signal.id = signalId
                vconfig.signals += signal
                signalId += 1
              }
            }
          }
        }
        case _ =>{
          s.algoInt = -1
        }
      }
    }))

    for(io <- rtl.toplevel.getAllIo){
      val bt = io
      val signal = new Signal(bt.getComponents().tail.map(_.getName()) :+ bt.getName(), bt match{
        case bt: Bool               => new BoolDataType
        case bt: Bits               => new BitsDataType(bt.getBitsWidth)
        case bt: UInt               => new UIntDataType(bt.getBitsWidth)
        case bt: SInt               => new SIntDataType(bt.getBitsWidth)
        case bt: SpinalEnumCraft[_] => new BitsDataType(bt.getBitsWidth)
      })

      bt.algoInt = signalId
      bt.algoIncrementale = -1
      signal.id = signalId
      vconfig.signals += signal
      signalId += 1
    }
    new VerilatorBackend(vconfig)
  }
}


object SpinalVerilatorSim {
  def apply[T <: Component](config: SpinalVerilatorBackendConfig[T], seed: Int) : SimVerilator = {
    val backend = SpinalVerilatorBackend(config)
    SpinalVerilatorSim(backend, seed)
  }

  def apply[T <: Component](backend : VerilatorBackend, seed : Int) : SimVerilator = {
    val sim = new SimVerilator(backend, backend.instanciate("test1", seed))
    sim.userData = backend.config.signals
    sim
  }
}

class SpinalVpiBackendConfig[T <: Component](val rtl               : SpinalReport[T],
                                             val waveFormat       : WaveFormat,
                                             val workspacePath    : String,
                                             val workspaceName    : String,
                                             val wavePath         : String,
                                             val wavePrefix       : String,
                                             val waveDepth        : Int,
                                             val optimisationLevel: Int,
                                             val simulatorFlags   : ArrayBuffer[String],
                                             val runFlags         : ArrayBuffer[String],
                                             val usePluginsCache  : Boolean,
                                             val pluginsCachePath : String,
                                             val enableLogging    : Boolean,
                                             val timePrecision    : TimeNumber)


case class SpinalIVerilogBackendConfig[T <: Component](override val rtl : SpinalReport[T],
                                                   override val waveFormat        : WaveFormat = WaveFormat.NONE,
                                                   override val workspacePath     : String = "./",
                                                   override val workspaceName     : String = null,
                                                   override val wavePath           : String = null,
                                                   override val wavePrefix         : String = null,
                                                   override val waveDepth         : Int = 0,
                                                   override val optimisationLevel : Int = 2,
                                                   override val simulatorFlags    : ArrayBuffer[String] = ArrayBuffer[String](),
                                                   override val runFlags          : ArrayBuffer[String] = ArrayBuffer[String](),
                                                   override val usePluginsCache   : Boolean = true,
                                                   override val pluginsCachePath  : String = "./simWorkspace/.pluginsCachePath",
                                                   override val enableLogging     : Boolean = false,
                                                   override val timePrecision     : TimeNumber = null) extends
                                              SpinalVpiBackendConfig[T](rtl,
                                                                        waveFormat,
                                                                        workspacePath,
                                                                        workspaceName,
                                                                        wavePath,
                                                                        wavePrefix,
                                                                        waveDepth,
                                                                        optimisationLevel,
                                                                        simulatorFlags,
                                                                        runFlags,
                                                                        usePluginsCache,
                                                                        pluginsCachePath,
                                                                        enableLogging,
                                                                        timePrecision)


case class SpinalVCSBackendConfig[T <: Component](override val rtl : SpinalReport[T],
                                                  override val waveFormat        : WaveFormat = WaveFormat.NONE,
                                                  override val workspacePath     : String = "./",
                                                  override val workspaceName     : String = null,
                                                  override val wavePath          : String = null,
                                                  override val wavePrefix        : String = null,
                                                  override val waveDepth         : Int = 0,
                                                  override val optimisationLevel : Int = 2,
                                                  override val simulatorFlags    : ArrayBuffer[String] = ArrayBuffer[String](),
                                                  override val runFlags          : ArrayBuffer[String] = ArrayBuffer[String](),
                                                  override val usePluginsCache   : Boolean = true,
                                                  override val pluginsCachePath  : String = "./simWorkspace/.pluginsCachePath",
                                                  override val enableLogging     : Boolean = false,
                                                  override val timePrecision     : TimeNumber = null,
                                                  val simSetupFile               : String = null,
                                                  val envSetup                   : () => Unit = null,
                                                  val vcsFlags                   : VCSFlags = null,
                                                  val compileFlags               : ArrayBuffer[String] = ArrayBuffer[String](),
                                                  val elaborateFlags             : ArrayBuffer[String] = ArrayBuffer[String](),
                                                  val vcsCC                      : Option[String] = None,
                                                  val vcsLd                      : Option[String] = None) extends
  SpinalVpiBackendConfig[T](rtl,
    waveFormat,
    workspacePath,
    workspaceName,
    wavePath,
    wavePrefix,
    waveDepth,
    optimisationLevel,
    simulatorFlags,
    runFlags,
    usePluginsCache,
    pluginsCachePath,
    enableLogging,
    timePrecision)

case class SpinalGhdlBackendConfig[T <: Component](override val rtl : SpinalReport[T],
                                                   override val waveFormat        : WaveFormat = WaveFormat.NONE,
                                                   override val workspacePath     : String = "./",
                                                   override val workspaceName     : String = null,
                                                   override val wavePath           : String = null,
                                                   override val wavePrefix         : String = null,
                                                   override val waveDepth         : Int = 0,
                                                   override val optimisationLevel : Int = 2,
                                                   override val simulatorFlags    : ArrayBuffer[String] = ArrayBuffer[String](),
                                                   override val runFlags          : ArrayBuffer[String] = ArrayBuffer[String](),
                                                   override val usePluginsCache   : Boolean = true,
                                                   override val pluginsCachePath  : String = "./simWorkspace/.pluginsCachePath",
                                                   override val enableLogging     : Boolean = false,
                                                   override val timePrecision     : TimeNumber = null) extends
                                              SpinalVpiBackendConfig[T](rtl,
                                                                        waveFormat,
                                                                        workspacePath,
                                                                        workspaceName,
                                                                        wavePath,
                                                                        wavePrefix,
                                                                        waveDepth,
                                                                        optimisationLevel,
                                                                        simulatorFlags,
                                                                        runFlags,
                                                                        usePluginsCache,
                                                                        pluginsCachePath,
                                                                        enableLogging,
                                                                        timePrecision)


object SpinalGhdlBackend {
  class Backend(val signals : ArrayBuffer[Signal], vconfig : GhdlBackendConfig) extends GhdlBackend(vconfig)

  def apply[T <: Component](config: SpinalGhdlBackendConfig[T]) : Backend = {
    val vconfig = new GhdlBackendConfig()
    vconfig.analyzeFlags = config.simulatorFlags.mkString(" ")
    if (config.timePrecision != null) {
      vconfig.elaborationFlags = s"--time-resolution=${config.timePrecision.decompose._2}"
    }
    vconfig.runFlags = config.runFlags.mkString(" ")
    vconfig.logSimProcess = config.enableLogging

    val signalsCollector = SpinalVpiBackend(config, vconfig)

    new Backend(signalsCollector, vconfig)
  }
}

object SpinalIVerilogBackend {
  class Backend(val signals : ArrayBuffer[Signal], vconfig : IVerilogBackendConfig) extends IVerilogBackend(vconfig)

  def apply[T <: Component](config: SpinalIVerilogBackendConfig[T]) = {
    val vconfig = new IVerilogBackendConfig()
    vconfig.analyzeFlags = config.simulatorFlags.mkString(" ")
    vconfig.runFlags = config.simulatorFlags.mkString(" ")
    vconfig.logSimProcess = config.enableLogging
    vconfig.timePrecision = config.timePrecision match {
      case null => null
      case t => t.decomposeString
    }

    val signalsCollector = SpinalVpiBackend(config, vconfig)

    new Backend(signalsCollector, vconfig)
  }
}

object SpinalVCSBackend {
  class Backend(val signals : ArrayBuffer[Signal], vconfig : VCSBackendConfig) extends VCSBackend(vconfig)

  def apply[T <: Component](config: SpinalVCSBackendConfig[T]) = {
    val vconfig = new VCSBackendConfig()
    vconfig.flags = config.vcsFlags
    vconfig.logSimProcess = config.enableLogging
    vconfig.vcsLd = config.vcsLd
    vconfig.vcsCC = config.vcsCC
    vconfig.waveDepth = config.waveDepth
    vconfig.wavePath = config.wavePath
    vconfig.simSetupFile = config.simSetupFile
    vconfig.envSetup = config.envSetup
    vconfig.timePrecision = config.timePrecision match {
      case null => null
      case t => t.decomposeString
    }

    val signalsCollector = SpinalVpiBackend(config, vconfig)

    new Backend(signalsCollector, vconfig)
  }
}

object SpinalVpiBackend {

  def apply[T <: Component](config: SpinalVpiBackendConfig[T], vconfig: VpiBackendConfig) = {

    import config._

    vconfig.rtlIncludeDirs  ++= rtl.rtlIncludeDirs
    vconfig.rtlSourcesPaths ++= rtl.rtlSourcesPaths.map(new File(_).getAbsolutePath)
    vconfig.toplevelName      = rtl.toplevelName
    vconfig.wavePath          = "test.vcd"
    vconfig.waveFormat        = waveFormat match {
      case WaveFormat.DEFAULT => WaveFormat.VCD
      case _ => waveFormat
    }
    vconfig.workspaceName     = workspaceName
    vconfig.workspacePath     = workspacePath
    vconfig.useCache = usePluginsCache
    vconfig.timePrecision = config.timePrecision match {
      case null => null
      case t => t.decomposeString
    }
    vconfig.pluginsPath = if(usePluginsCache) {

    val pluginsCachePathFile = new File(pluginsCachePath)
      if(!pluginsCachePathFile.exists()) {
        pluginsCachePathFile.mkdirs
      }
      pluginsCachePath
    } else workspacePath

    var signalId = 0

    val signalsCollector = ArrayBuffer[Signal]()

    def addSignal(bt: DeclarationStatement with InComponent): Unit ={
      val signal = new Signal(config.rtl.toplevelName +: bt.getComponents().tail.map(_.getName()) :+ bt.getName(), bt match{
        case bt: Bool               => new BoolDataType
        case bt: Bits               => new BitsDataType(bt.getBitsWidth)
        case bt: UInt               => new UIntDataType(bt.getBitsWidth)
        case bt: SInt               => new SIntDataType(bt.getBitsWidth)
        case bt: SpinalEnumCraft[_] => new BitsDataType(bt.getBitsWidth)
        case mem: Mem[_] => new BitsDataType(mem.width)
      })

      bt.algoInt = signalId
      bt.algoIncrementale = -1
      signal.id = signalId
      signalsCollector += signal
      signalId += 1
    }

    GraphUtils.walkAllComponents(rtl.toplevel, c => c.dslBody.walkStatements(s => {
      s match {
        case bt: BaseType if bt.hasTag(SimPublic) && !(!bt.isDirectionLess && bt.component.parent == null) => {
          addSignal(bt)
        }
        case mem : Mem[_] if mem.hasTag(SimPublic) => {
          val tag = mem.getTag(classOf[MemSymbolesTag])
          mem.algoInt = signalId
          mem.algoIncrementale = -1
          tag match {
            case None => addSignal(mem)
            case Some(tag) => {
              for(mapping <- tag.mapping){
                val signal =  new Signal(config.rtl.toplevelName +: mem.getComponents().tail.map(_.getName()) :+ mapping.name, new BitsDataType(mapping.width))
                signal.id = signalId
                signalsCollector += signal
                signalId += 1
              }
            }
          }
        }
        case _ =>{
          s.algoInt = -1
        }
      }
    }))

    for(io <- rtl.toplevel.getAllIo){
      val bt = io
      val signal = new Signal(config.rtl.toplevelName +: bt.getComponents().tail.map(_.getName()) :+ bt.getName(), bt match{
        case bt: Bool               => new BoolDataType
        case bt: Bits               => new BitsDataType(bt.getBitsWidth)
        case bt: UInt               => new UIntDataType(bt.getBitsWidth)
        case bt: SInt               => new SIntDataType(bt.getBitsWidth)
        case bt: SpinalEnumCraft[_] => new BitsDataType(bt.getBitsWidth)
      })

      bt.algoInt = signalId
      bt.algoIncrementale = -1
      signal.id = signalId
      signalsCollector += signal
      signalId += 1
    }
    signalsCollector
  }
}

case class SpinalXSimBackendConfig[T <: Component](val rtl               : SpinalReport[T],
                                               val xciSourcesPaths  : ArrayBuffer[String] = ArrayBuffer[String](),
                                               val bdSourcesPaths   : ArrayBuffer[String] = ArrayBuffer[String](),
                                               val waveFormat       : WaveFormat,
                                               val workspacePath    : String,
                                               val workspaceName    : String,
                                               val wavePath         : String,
                                               val xilinxDevice     : String,
                                               val simScript        : String,
                                               val simulatorFlags   : ArrayBuffer[String] = ArrayBuffer[String](),
                                               val timePrecision    : TimeNumber = null)

object SpinalXSimBackend {
  class Backend(val signals : ArrayBuffer[Signal], vconfig : XSimBackendConfig) extends XSimBackend(vconfig)
  def apply[T <: Component](config: SpinalXSimBackendConfig[T]) = {
    import config._

    val vconfig = new XSimBackendConfig()
    vconfig.rtlIncludeDirs  ++= rtl.rtlIncludeDirs
    vconfig.rtlSourcesPaths ++= rtl.rtlSourcesPaths.map(new File(_).getAbsolutePath)
    vconfig.xciSourcesPaths   =  xciSourcesPaths
    vconfig.bdSourcesPaths    = bdSourcesPaths
    vconfig.toplevelName      = rtl.toplevelName
    vconfig.wavePath          = "test.wdb"
    vconfig.waveFormat        = waveFormat match {
      case WaveFormat.DEFAULT => WaveFormat.WDB
      case _ => waveFormat
    }
    vconfig.workspaceName     = workspaceName
    vconfig.workspacePath     = workspacePath
    vconfig.xilinxDevice      = xilinxDevice
    vconfig.userSimulationScript = simScript
    vconfig.xelabFlags        = simulatorFlags.toArray
    vconfig.timePrecision     = if (timePrecision != null) timePrecision.decomposeString else null

    var signalId = 0

    val signalsCollector = ArrayBuffer[Signal]()

    for(io <- rtl.toplevel.getAllIo){
      val bt = io
      val signal = new Signal(config.rtl.toplevelName +: bt.getComponents().tail.map(_.getName()) :+ bt.getName(), bt match{
        case bt: Bool               => new BoolDataType
        case bt: Bits               => new BitsDataType(bt.getBitsWidth)
        case bt: UInt               => new UIntDataType(bt.getBitsWidth)
        case bt: SInt               => new SIntDataType(bt.getBitsWidth)
        case bt: SpinalEnumCraft[_] => new BitsDataType(bt.getBitsWidth)
      })

      bt.algoInt = signalId
      bt.algoIncrementale = -1
      signal.id = signalId
      signalsCollector += signal
      signalId += 1
    }
    new Backend(signalsCollector, vconfig)
  }
}

/** Tag SimPublic  */
object SimPublic extends SpinalTag

object TracingOff extends SpinalTag

/**
  * Swap all oldTag with newTag
  */
class SwapTagPhase(oldOne: SpinalTag, newOne: SpinalTag) extends PhaseNetlist {

  override def impl(pc: PhaseContext): Unit = {
    pc.walkDeclarations{
      case x: SpinalTagReady if(x.hasTag(oldOne)) =>  {
        x.removeTag(oldOne)
        x.addTag(newOne)
      }
      case _ =>
    }
  }
}


class SimVerilatorPhase extends PhaseNetlist {

  override def impl(pc: PhaseContext): Unit = {
    val latchesIn = mutable.LinkedHashSet[Component]()
    pc.walkDeclarations { d =>
      d match {
        case x: SpinalTagReady if (x.hasTag(SimPublic)) => {
          x.addTag(Verilator.public)
        }
        case _ =>
      }
      d match {
        case bt: BaseType if bt.hasTag(noLatchCheck) => latchesIn += bt.component
        case _ =>
      }
    }
    latchesIn.foreach(_.definition.addComment("verilator lint_off LATCH"))
  }
}

class CoreSimManager(sim : SimRaw, random : Random, allocatedName : String, val compiled : SimCompiled[_ <: Component]) extends SimManager(sim, random, allocatedName){
  val spinalGlobalData =  GlobalData.get
  override def setupJvmThread(thread: Thread): Unit = {
    super.setupJvmThread(thread)
    GlobalData.it.set(spinalGlobalData)
  }

  override def newSpawnTask() = new SimThreadSpawnTask {
    val initialContext = ScopeProperty.capture()
    override def setup() = initialContext.restore()
  }
}

/**
  * Run simulation
  */
abstract class SimCompiled[T <: Component](val report: SpinalReport[T], val compiledPath : File, val simConfig : SpinalSimConfig){
  def dut = report.toplevel

  val testNameMap = mutable.HashMap[String, Int]()

  def allocateTestName(name: String): String = {
    testNameMap.synchronized{
      val value = testNameMap.getOrElseUpdate(name, 0)
      testNameMap(name) = value + 1
      if(value == 0){
        return name
      }else{
        val ret = name + "_" + value
        println(s"[Info] Test '$name' was reallocated as '$ret' to avoid collision")
        return ret
      }
    }
  }

  def doSim(body: T => Unit): Unit =  doSimApi(joinAll = false)(body)
  def doSim(name: String)(body: T => Unit): Unit = doSimApi(name = name, joinAll = false)(body)
  def doSim(seed: Int)(body: T => Unit): Unit = doSimApi(seed = seed, joinAll = false)(body)
  def doSim(name: String, seed: Int)(body : T => Unit): Unit = {
    doSimApi(name, seed, false)(body)
  }

  def doSimUntilVoid(body: T => Unit): Unit =  doSimApi(joinAll = true)(body)
  def doSimUntilVoid(name: String)(body: T => Unit): Unit = doSimApi(name = name, joinAll = true)(body)
  def doSimUntilVoid(seed: Int)(body: T => Unit): Unit = doSimApi(seed = seed, joinAll = true)(body)
  def doSimUntilVoid(name: String, seed: Int)(body : T => Unit): Unit = {
    doSimApi(name, seed, true)(body)
  }

  def newSimRaw(name: String, seed: Int) : SimRaw

  def newSeed(): Int = {
    sys.env.get("SPINAL_SIM_SEED") match {
      case Some(v) => v.toInt
      case None => Random.nextInt(Integer.MAX_VALUE)
    }
  }

  def doSimApi(name: String = "test", seed: Int = newSeed(), joinAll: Boolean)(body: T => Unit): Unit = {
    val random = new Random(seed)
    GlobalData.set(report.globalData)

    val allocatedName = allocateTestName(name)
    val backendSeed   = if(seed == 0) 1 else seed

    val sim = newSimRaw(allocatedName, backendSeed)

    val manager = new CoreSimManager(sim, random, allocatedName, this)
    manager.userData = dut

    println(f"[Progress] Start ${dut.definitionName} $allocatedName simulation with seed $seed")

    if(joinAll) {
      manager.runAll(body(dut))
    }else {
      manager.run(body(dut))
    }
  }
}


/**
  * Simulation Workspace
  */
object SimWorkspace {
  private var uniqueId = 0

  def allocateUniqueId(): Int = {
    this.synchronized {
      uniqueId = uniqueId + 1
      uniqueId
    }
  }

  val workspaceMap = mutable.HashMap[(String, String), Int]()

  def allocateWorkspace(path: String, name: String): String = {
    workspaceMap.synchronized{
      val value = workspaceMap.getOrElseUpdate((path,name), 0)
      workspaceMap((path, name)) = value + 1
      if(value == 0){
        return name
      }else{
        val ret = name + "_" + value
        println(s"[Info] Workspace '$name' was reallocated as '$ret' to avoid collision")
        return ret
      }
    }
  }
}

class SpinalSimBackendSel
object SpinalSimBackendSel{
  val VERILATOR = new SpinalSimBackendSel
  val GHDL = new SpinalSimBackendSel
  val IVERILOG = new SpinalSimBackendSel
  val VCS = new SpinalSimBackendSel
  val XSIM = new SpinalSimBackendSel
}

/**
  * SpinalSim configuration
  */
case class SpinalSimConfig(
                            var _workspacePath     : String = System.getenv().getOrDefault("SPINALSIM_WORKSPACE","./simWorkspace"),
                            var _workspaceName     : String = null,
                            var _waveDepth         : Int = 0, //0 => all
                            var _spinalConfig      : SpinalConfig = SpinalConfig(),
                            var _optimisationLevel : Int = 0,
                            var _simulatorFlags    : ArrayBuffer[String] = ArrayBuffer[String](),
                            var _runFlags          : ArrayBuffer[String] = ArrayBuffer[String](),
                            var _additionalRtlPath : ArrayBuffer[String] = ArrayBuffer[String](),
                            var _additionalIncludeDir : ArrayBuffer[String] = ArrayBuffer[String](),
                            var _waveFormat        : WaveFormat = WaveFormat.NONE,
                            var _backend           : SpinalSimBackendSel = SpinalSimBackendSel.VERILATOR,
                            var _withCoverage      : Boolean = false,
                            var _maxCacheEntries   : Int = 100,
                            var _cachePath         : String = null, // null => workspacePath + "/.cache"
                            var _disableCache      : Boolean = false,
                            var _withLogging       : Boolean = false,
                            var _vcsCC             : Option[String] = None,
                            var _vcsLd             : Option[String] = None,
                            var _vcsUserFlags      : VCSFlags = VCSFlags(),
                            var _vcsSimSetupFile   : String = null,
                            var _vcsEnvSetup       : () => Unit = null,
                            var _xciSourcesPaths   : ArrayBuffer[String] = ArrayBuffer[String](),
                            var _bdSourcesPaths    : ArrayBuffer[String] = ArrayBuffer[String](),
                            var _xilinxDevice:String = "xc7vx485tffg1157-1",
                            var _simScript         : String = null,
                            var _timePrecision     : TimeNumber = null,
                            var _timeScale         : TimeNumber = null,
                            var _testPath          : String = "$WORKSPACE/$COMPILED"
  ){


  def  withVerilator : this.type = {
    _backend = SpinalSimBackendSel.VERILATOR
    this
  }
  def  withGhdl : this.type = {
    _backend = SpinalSimBackendSel.GHDL
    this
  }
  def  withIVerilog : this.type = {
    _backend = SpinalSimBackendSel.IVERILOG
    this
  }

  def withVcs : this.type = withVCS
  def withVCS : this.type = {
    _backend = SpinalSimBackendSel.VCS
    this
  }

  def withVCS(vcsFlags: VCSFlags = VCSFlags()) : this.type = {
    _backend = SpinalSimBackendSel.VCS
    _vcsUserFlags = vcsFlags
    this
  }

  def withVCSSimSetup(setupFile: String, beforeAnalysis: () => Unit): this.type = {
    _vcsSimSetupFile = setupFile
    _vcsEnvSetup = beforeAnalysis
    this
  }

  def withXSim: this.type = {
    _backend = SpinalSimBackendSel.XSIM
    this
  }

  def withXSimSourcesPaths(xciSourcesPaths: ArrayBuffer[String], bdSourcesPaths: ArrayBuffer[String]): this.type = {
    _xciSourcesPaths = xciSourcesPaths
    _bdSourcesPaths = bdSourcesPaths
    this
  }

  def withSimScript(script: String): this.type = {
    _simScript = script
    this
  }

  def withVPDWave: this.type = {
    _waveFormat = WaveFormat.VPD
    this
  }
  def withFSDBWave: this.type = {
    _waveFormat = WaveFormat.FSDB
    this
  }

  def withVCSCc(cc: String) : this.type = {
    _vcsCC = Some(cc)
    this
  }
  def withVCSLd(ld: String) : this.type = {
    _vcsLd = Some(ld)
    this
  }

  def withVcdWave : this.type = {
    _waveFormat = WaveFormat.VCD
    this
  }

  def withFstWave : this.type = {
    _waveFormat = WaveFormat.FST
    this
  }

  def withFsdbWave : this.type = {
    _waveFormat = WaveFormat.FSDB
    this
  }

  def withVpdWave : this.type = {
    _waveFormat = WaveFormat.VPD
    this
  }

  def withWave: this.type = {
    _waveFormat = WaveFormat.DEFAULT
    this
  }

  def withWaveDepth(depth: Int): this.type = {
    _waveDepth = depth
    this
  }

  def withWave(depth: Int): this.type = {
    _waveFormat = WaveFormat.DEFAULT
    _waveDepth = depth
    this
  }

  def withCoverage: this.type = {
    _withCoverage = true
    this
  }

  def withLogging: this.type = {
    _withLogging = true
    this
  }

  def withXilinxDevice(xilinxDevice:String):this.type ={
    _xilinxDevice = xilinxDevice
    this
  }

  def workspacePath(path: String): this.type = {
    _workspacePath = path
    this
  }

  def workspaceName(name: String): this.type = {
    _workspaceName = name
    this
  }

  def withConfig(config: SpinalConfig): this.type = {
    _spinalConfig = config
    this
  }

  def noOptimisation: this.type = {
    _optimisationLevel = 0
    this
  }
  def fewOptimisation: this.type = {
    _optimisationLevel = 1
    this
  }
  def normalOptimisation: this.type = {
    _optimisationLevel = 2
    this
  }
  def allOptimisation: this.type = {
    _optimisationLevel = 3
    this
  }

  def addSimulatorFlag(flag: String): this.type = {
    _simulatorFlags += flag
    this
  }

  def addRunFlag(flag: String): this.type = {
    _runFlags += flag
    this
  }

  def addRtl(that : String) : this.type = {
    _additionalRtlPath += that
    this
  }

  def addIncludeDir(that : String) : this.type = {
    _additionalIncludeDir += that
    this
  }

  def maxCacheEntries(count: Int): this.type = {
    _maxCacheEntries = count
    this
  }

  def cachePath(path: String): this.type = {
    _cachePath = path
    this
  }

  def disableCache: this.type = {
    _disableCache = true
    this
  }

  def withTimeScale(timeScale: TimeNumber): this.type = {
    _timeScale = timeScale
    this
  }

  def withTimePrecision(timePrecision: TimeNumber): this.type = {
    _timePrecision = timePrecision
    this
  }

  def setTestPath(path : String) : this.type = {
    _testPath = path
    this
  }

  def getTestPath(test : String) = _testPath.replace("$TEST", test)

  def withTestFolder : this.type = {
    this.setTestPath("$WORKSPACE/$COMPILED/$TEST")
    this
  }

  def doSim[T <: Component](report: SpinalReport[T])(body: T => Unit): Unit = compile(report).doSim(body)
  def doSim[T <: Component](report: SpinalReport[T], name: String)(body: T => Unit): Unit = compile(report).doSim(name)(body)
  def doSim[T <: Component](report: SpinalReport[T], name: String, seed: Int)(body: T => Unit): Unit = compile(report).doSim(name, seed)(body)

  def doSimUntilVoid[T <: Component](report: SpinalReport[T])(body: T => Unit): Unit = compile(report).doSimUntilVoid(body)
  def doSimUntilVoid[T <: Component](report: SpinalReport[T], name: String)(body: T => Unit): Unit = compile(report).doSimUntilVoid(name)(body)
  def doSimUntilVoid[T <: Component](report: SpinalReport[T], name: String, seed: Int)(body: T => Unit): Unit = compile(report).doSimUntilVoid(name, seed)(body)

  def doSim[T <: Component](rtl: => T)(body: T => Unit): Unit = compile(rtl).doSim(body)
  def doSim[T <: Component](rtl: => T, name: String)(body: T => Unit): Unit = compile(rtl).doSim(name)(body)
  def doSim[T <: Component](rtl: => T, name: String, seed: Int)(body: T => Unit): Unit = compile(rtl).doSim(name, seed)(body)

  def doSimUntilVoid[T <: Component](rtl: => T)(body: T => Unit): Unit = compile(rtl).doSimUntilVoid(body)
  def doSimUntilVoid[T <: Component](rtl: => T, name: String)(body: T => Unit): Unit = compile(rtl).doSimUntilVoid(name)(body)
  def doSimUntilVoid[T <: Component](rtl: => T, name: String, seed: Int)(body: T => Unit): Unit = compile(rtl).doSimUntilVoid(name,seed)(body)

  def compile[T <: Component](rtl: => T) : SimCompiled[T] = {
    this.copy().compileCloned(rtl)
  }

  def compileCloned[T <: Component](rtl: => T) : SimCompiled[T] = {
    val uniqueId = SimWorkspace.allocateUniqueId()
    new File(s"tmp").mkdirs()
    new File(s"tmp/job_$uniqueId").mkdirs()
    val config = _spinalConfig.copy(targetDirectory = s"tmp/job_$uniqueId").addTransformationPhase(new PhaseNetlist {
      override def impl(pc: PhaseContext): Unit = {
        //Ensure the toplevel pull its clock domain
        pc.topLevel.rework{
          val cd = pc.topLevel.clockDomain
          if(cd != null){
            if (cd.clock != null) cd.readClockWire
            if (cd.reset != null) cd.readResetWire
            if (cd.softReset != null) cd.readSoftResetWire
            if (cd.clockEnable != null) cd.readClockEnableWire
          }
        }

        //Implement isSpinalSimWb
        pc.walkComponents{
          case b : BlackBox if b.isBlackBox && b.isSpinalSimWb => b.clearBlackBox()
          case _ =>
        }
      }
    })
    val report = _backend match {
      case SpinalSimBackendSel.VERILATOR => {
        config.addTransformationPhase(new SimVerilatorPhase)
        config.generateVerilog(rtl)
      }
      case SpinalSimBackendSel.GHDL => config.generateVhdl(rtl)
      case SpinalSimBackendSel.IVERILOG | SpinalSimBackendSel.VCS | SpinalSimBackendSel.XSIM => config.generateVerilog(rtl)
    }
    report.blackboxesSourcesPaths ++= _additionalRtlPath
    report.blackboxesIncludeDir ++= _additionalIncludeDir
    compile[T](report)
  }

  def compile[T <: Component](report: SpinalReport[T]): SimCompiled[T] = {
    if (_workspacePath.startsWith("~"))
      _workspacePath = System.getProperty("user.home") + _workspacePath.drop(1)

    if (_workspaceName == null)
      _workspaceName = s"${report.toplevelName}"

    _workspaceName = SimWorkspace.allocateWorkspace(_workspacePath, _workspaceName)

    println(f"[Progress] Simulation workspace in ${new File(s"${_workspacePath}/${_workspaceName}").getAbsolutePath}")
    new File(s"${_workspacePath}").mkdirs()
    FileUtils.deleteQuietly(new File(s"${_workspacePath}/${_workspaceName}"))
    new File(s"${_workspacePath}/${_workspaceName}").mkdirs()
    new File(s"${_workspacePath}/${_workspaceName}/rtl").mkdirs()

    val compiledPath = new File(s"${_workspacePath}/${_workspaceName}")

    val rtlDir = new File(s"${_workspacePath}/${_workspaceName}/rtl")
    _testPath = _testPath.replace("$WORKSPACE", _workspacePath).replace("$COMPILED", _workspaceName)
    val wavePath = _testPath

    //    val rtlPath = rtlDir.getAbsolutePath
    report.generatedSourcesPaths.foreach { srcPath =>
      val src = new File(srcPath)
      val lines = Source.fromFile(src).getLines.toArray
      val w = new PrintWriter(src)
      for(line <- lines){
          val str = if(line.contains("readmem")){
            val exprPattern = """.*\$readmem.*\(\"(.+)\".+\).*""".r
            val absline = line match {
              case exprPattern(relpath) => {
                val windowsfix = relpath.replace(".\\", "")
                val abspath = new File(src.getParent + "/" + windowsfix).getAbsolutePath
                val ret = line.replace(relpath, abspath)
                ret.replace("\\", "\\\\") //windows escape "\"
              }
              case _ => new Exception("readmem abspath replace failed")
            }
            absline
          } else {
            line
          }
          w.println(str)
        }
      w.close()

      val dst = new File(rtlDir.getAbsolutePath + "/" + src.getName)
      FileUtils.copyFileToDirectory(src, rtlDir)
    }

    _backend match {
      case SpinalSimBackendSel.VERILATOR =>
        println(f"[Progress] Verilator compilation started")
        val startAt = System.nanoTime()
        val vConfig = SpinalVerilatorBackendConfig[T](
          rtl = report,
          waveFormat = _waveFormat,
          maxCacheEntries = _maxCacheEntries,
          cachePath = if (!_disableCache) (if (_cachePath != null) _cachePath else s"${_workspacePath}/.cache") else null,
          workspacePath = s"${_workspacePath}/${_workspaceName}",
          vcdPath = wavePath,
          vcdPrefix = null,
          workspaceName = "verilator",
          waveDepth = _waveDepth,
          optimisationLevel = _optimisationLevel,
          simulatorFlags = _simulatorFlags,
          withCoverage = _withCoverage,
          timePrecision = _timePrecision,
          testPath = _testPath
        )
        val backend = SpinalVerilatorBackend(vConfig)
        val deltaTime = (System.nanoTime() - startAt) * 1e-6
        println(f"[Progress] Verilator compilation done in $deltaTime%1.3f ms")
        new SimCompiled(report, compiledPath, this){
          override def newSimRaw(name: String, seed: Int): SimRaw = {
            val raw = new SimVerilator(backend, backend.instanciate(name, seed))
            raw.userData = backend.config.signals
            raw
          }
        }

      case SpinalSimBackendSel.GHDL =>
        println(f"[Progress] GHDL compilation started")
        val startAt = System.nanoTime()
        val vConfig = SpinalGhdlBackendConfig[T](
          rtl = report,
          waveFormat = _waveFormat,
          workspacePath = s"${_workspacePath}/${_workspaceName}",
          wavePath = wavePath,
          wavePrefix = null,
          workspaceName = "ghdl",
          waveDepth = _waveDepth,
          optimisationLevel = _optimisationLevel,
          simulatorFlags = _simulatorFlags,
          runFlags = _runFlags,
          enableLogging = _withLogging,
          usePluginsCache = !_disableCache,
          timePrecision = _timePrecision
        )
        val backend = SpinalGhdlBackend(vConfig)
        val deltaTime = (System.nanoTime() - startAt) * 1e-6
        println(f"[Progress] GHDL compilation done in $deltaTime%1.3f ms")
        new SimCompiled(report, compiledPath, this){
          override def newSimRaw(name: String, seed: Int): SimRaw = {
            val raw = new SimVpi(backend)
            raw.userData = backend.signals
            raw
          }
        }

      case SpinalSimBackendSel.IVERILOG =>
        println(f"[Progress] IVerilog compilation started")
        val startAt = System.nanoTime()
        val vConfig = SpinalIVerilogBackendConfig[T](
          rtl = report,
          waveFormat = _waveFormat,
          workspacePath = s"${_workspacePath}/${_workspaceName}",
          wavePath = s"${_workspacePath}/${_workspaceName}",
          wavePrefix = null,
          workspaceName = "iverilog",
          waveDepth = _waveDepth,
          optimisationLevel = _optimisationLevel,
          simulatorFlags = _simulatorFlags,
          enableLogging = _withLogging,
          usePluginsCache = !_disableCache,
          timePrecision = _timePrecision
        )
        val backend = SpinalIVerilogBackend(vConfig)
        val deltaTime = (System.nanoTime() - startAt) * 1e-6
        println(f"[Progress] IVerilog compilation done in $deltaTime%1.3f ms")
        new SimCompiled(report, compiledPath, this){
          override def newSimRaw(name: String, seed: Int): SimRaw = {
            val raw = new SimVpi(backend)
            raw.userData = backend.signals
            raw
          }
        }

      case SpinalSimBackendSel.VCS =>
        val vConfig = SpinalVCSBackendConfig[T](
          rtl = report,
          waveFormat = _waveFormat,
          workspacePath = s"${_workspacePath}/${_workspaceName}",
          wavePath = s"${_workspacePath}/${_workspaceName}",
          wavePrefix = null,
          workspaceName = "vcs",
          waveDepth = _waveDepth,
          optimisationLevel = _optimisationLevel,
          simulatorFlags = _simulatorFlags,
          enableLogging = _withLogging,
          usePluginsCache = !_disableCache,
          vcsCC = _vcsCC,
          vcsLd = _vcsLd,
          vcsFlags = _vcsUserFlags,
          simSetupFile = _vcsSimSetupFile,
          envSetup = _vcsEnvSetup,
          timePrecision = _timePrecision
        )
        val backend = SpinalVCSBackend(vConfig)
        new SimCompiled(report, compiledPath, this) {
          override def newSimRaw(name: String, seed: Int): SimRaw = {
            val raw = new SimVpi(backend)
            raw.userData = backend.signals
            raw
          }
        }

      case SpinalSimBackendSel.XSIM =>
        println(f"[Progress] XSIM compilation started")
        val vConfig = SpinalXSimBackendConfig[T](
          rtl = report,
          waveFormat = _waveFormat,
          workspacePath = s"${_workspacePath}/${_workspaceName}",
          wavePath = s"${_workspacePath}/${_workspaceName}",
          workspaceName = "xsim",
          xciSourcesPaths = _xciSourcesPaths,
          bdSourcesPaths = _bdSourcesPaths,
          xilinxDevice = _xilinxDevice,
          simScript = _simScript,
          simulatorFlags = _simulatorFlags,
          timePrecision = _timePrecision
        )
        val backend = SpinalXSimBackend(vConfig)
        new SimCompiled(report, compiledPath, this) {
          override def newSimRaw(name: String, seed: Int): SimRaw = {
            val raw = new SimXSim(backend)
            raw.userData = backend.signals
            raw
          }
        }
    }
  }
}


/**
  * Legacy simulation configuration
  */
case class SimConfigLegacy[T <: Component](
  var _rtlGen       : Option[() => T] = None,
  var _spinalConfig : SpinalConfig = SpinalConfig(),
  var _spinalReport : Option[SpinalReport[T]] = None
){

  private val _simConfig = SpinalSimConfig()

  def withWave: this.type = { _simConfig.withWave; this }
  def withWave(depth: Int): this.type = { _simConfig.withWave(depth); this }

  def workspacePath(path: String): this.type = { _simConfig.workspacePath(path); this }
  def workspaceName(name: String): this.type = { _simConfig.workspaceName(name); this }

  def withConfig(config: SpinalConfig): this.type =  { _simConfig.withConfig(config); this }

  def noOptimisation: this.type     = { _simConfig.noOptimisation ; this }
  def fewOptimisation: this.type    = { _simConfig.fewOptimisation ; this }
  def normalOptimisation: this.type = { _simConfig.normalOptimisation ; this }
  def allOptimisation: this.type    = { _simConfig.allOptimisation ; this }

  def doSim(body: T => Unit): Unit = compile().doSim(body)
  def doSim(name: String)(body: T => Unit): Unit = compile().doSim(name)(body)
  def doSim(name: String, seed: Int)(body: T => Unit): Unit = compile().doSim(name, seed)(body)

  def doManagedSim(body: T => Unit): Unit = compile().doSim(body)
  def doManagedSim(name: String)(body: T => Unit): Unit = compile().doSim(name)(body)
  def doManagedSim(name: String, seed: Int)(body: T => Unit): Unit = compile().doSim(name, seed)(body)

  def doSimUntilVoid(body: T => Unit): Unit = compile().doSimUntilVoid(body)
  def doSimUntilVoid(name: String)(body: T => Unit): Unit = compile().doSimUntilVoid(name)(body)
  def doSimUntilVoid(name: String, seed: Int)(body: T => Unit): Unit = compile().doSimUntilVoid(name, seed)(body)

  def compile(): SimCompiled[T] = {
    (_rtlGen, _spinalReport)  match {
      case (None, Some(report)) => _simConfig.compile(report)
      case (Some(gen), None)    => _simConfig.compile(gen())
      case _ => ???
    }
  }
}
