package spinal.core.sim

import java.io.File

import org.apache.commons.io.FileUtils
import spinal.core.internals.{GraphUtils, PhaseCheck, PhaseContext, PhaseNetlist}
import spinal.core.{BaseType, Bits, Bool, Component, SInt, SpinalConfig, SpinalEnumCraft, SpinalReport, SpinalTag, SpinalTagReady, UInt, Verilator}
import spinal.sim._

import scala.collection.mutable
import scala.util.Random
import sys.process._

case class SpinalVerilatorBackendConfig[T <: Component]( rtl : SpinalReport[T],
                                                         withWave : Boolean = false,
                                                         workspacePath : String = "./",
                                                         workspaceName : String = null,
                                                         vcdPath : String = null,
                                                         vcdPrefix : String = null,
                                                         waveDepth : Int = 0,
                                                         optimisationLevel : Int = 2)

object SpinalVerilatorBackend{
  def apply[T <: Component](config : SpinalVerilatorBackendConfig[T]) = {
    import config._
    val vconfig = new VerilatorBackendConfig()
    vconfig.rtlSourcesPaths ++= rtl.rtlSourcesPaths
    vconfig.toplevelName = rtl.toplevelName
    vconfig.vcdPath = vcdPath
    vconfig.vcdPrefix = vcdPrefix
    vconfig.workspaceName = workspaceName
    vconfig.workspacePath = workspacePath
    vconfig.withWave = withWave
    vconfig.waveDepth = waveDepth
    vconfig.optimisationLevel = optimisationLevel

    var signalId = 0
    def addSignal(bt : BaseType): Unit ={
      val signal = new Signal(config.rtl.toplevelName +: bt.getComponents().tail.map(_.getName()) :+ bt.getName()  , bt match {
        case bt : Bool => new BoolDataType
        case bt : Bits => new BitsDataType(bt.getBitsWidth)
        case bt : UInt => new UIntDataType(bt.getBitsWidth)
        case bt : SInt => new SIntDataType(bt.getBitsWidth)
        case bt : SpinalEnumCraft[_] => new BitsDataType(bt.getBitsWidth)
      })
      bt.algoInt = signalId
      bt.algoIncrementale = -1
      signal.id = signalId
      vconfig.signals += signal
      signalId += 1
    }
    GraphUtils.walkAllComponents(rtl.toplevel, c => c.dslBody.walkStatements(s => {
      s match {
        case bt : BaseType if bt.hasTag(Verilator.public) && !(!bt.isDirectionLess && bt.component.parent == null) => {
          addSignal(bt)
        }
        case _ =>{
          s.algoInt = -1
        }
      }
    }))

    for(io <- rtl.toplevel.getAllIo){
      val bt = io
      val signal = new Signal(bt.getComponents().tail.map(_.getName()) :+ bt.getName()  , bt match {
        case bt : Bool => new BoolDataType
        case bt : Bits => new BitsDataType(bt.getBitsWidth)
        case bt : UInt => new UIntDataType(bt.getBitsWidth)
        case bt : SInt => new SIntDataType(bt.getBitsWidth)
        case bt : SpinalEnumCraft[_] => new BitsDataType(bt.getBitsWidth)
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


object SpinalVerilatorSim{
  def apply[T <: Component](config : SpinalVerilatorBackendConfig[T], seed : Int) : SimVerilator = {
    val backend = SpinalVerilatorBackend(config)
    SpinalVerilatorSim(backend, seed)
  }

  def apply[T <: Component](backend : VerilatorBackend, seed : Int) : SimVerilator = {
    val sim = new SimVerilator(backend, backend.instanciate("test1", seed))
    sim.userData = backend.config.signals
    sim
  }
}

object SimPublic extends SpinalTag

class SwapTagPhase(oldOne : SpinalTag, newOne : SpinalTag) extends PhaseNetlist{
  override def impl(pc: PhaseContext): Unit = {
    pc.walkDeclarations{
      case x : SpinalTagReady if(x.hasTag(oldOne)) =>  {
        x.removeTag(oldOne)
        x.addTag(newOne)
      }
      case _ =>
    }
  }
}

class SimCompiled[T <: Component](backend : VerilatorBackend, dut : T){
  val testNameMap = mutable.HashMap[String, Int]()
  def allocateTestName(name : String) : String = {
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
  def doSim(body : T => Unit@suspendable) : Unit = doSim("test")(body)
  def doSim(name : String)(body : T => Unit@suspendable) : Unit = doSim(name, Random.nextLong())(body)
  def doSim(name : String, seed : Long)(body : T => Unit@suspendable) : Unit = {
    Random.setSeed(seed)
    doSimPostSeed(name, Random.nextLong(), false)(body)
  }

  @deprecated("Use doSim instead")
  def doManagedSim(body : T => Unit@suspendable) : Unit = doSim("test")(body)
  @deprecated("Use doSim instead")
  def doManagedSim(name : String)(body : T => Unit@suspendable) : Unit = doSim(name, Random.nextLong())(body)
  @deprecated("Use doSim instead")
  def doManagedSim(name : String, seed : Long)(body : T => Unit@suspendable) : Unit = {
    Random.setSeed(seed)
    doSimPostSeed(name, Random.nextLong(), false)(body)
  }

  def doSimUntilVoid(body : T => Unit@suspendable) : Unit = doSimUntilVoid("test")(body)
  def doSimUntilVoid(name : String)(body : T => Unit@suspendable) : Unit = doSimUntilVoid(name, Random.nextLong())(body)
  def doSimUntilVoid(name : String, seed : Long)(body : T => Unit@suspendable) : Unit = {
    Random.setSeed(seed)
    doSimPostSeed(name, Random.nextLong(), true)(body)
  }


  def doSimPostSeed(name : String, seed : Long, joinAll : Boolean)(body : T => Unit@suspendable) : Unit = {
    val allocatedName = allocateTestName(name)
    val seedInt = seed.toInt
    val backendSeed = if(seedInt == 0) 1 else seedInt
    val sim = new SimVerilator(backend, backend.instanciate(allocatedName, backendSeed))
    sim.userData = backend.config.signals
    val manager = new SimManager(sim)
    manager.userData = dut
    println(f"[Progress] Start ${dut.definitionName} $allocatedName simulation with seed $seed${if(backend.config.withWave) s", wave in ${new File(backend.config.vcdPath).getAbsolutePath}/${allocatedName}.vcd" else ", without wave"}")
    if(joinAll)
      manager.runAll(body(dut))
    else
      manager.run(body(dut))
  }


}

object SimWorkspace{
  private var uniqueId = 0
  def allocateUniqueId() : Int = {
    this.synchronized {
      uniqueId = uniqueId + 1
      uniqueId
    }
  }

  val workspaceMap = mutable.HashMap[(String, String), Int]()
  def allocateWorkspace(path : String, name : String) : String = {
    workspaceMap.synchronized{
      val value = workspaceMap.getOrElseUpdate((path,name), 0)
      workspaceMap((path,name)) = value + 1
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



case class SpinalSimConfig(var _withWave: Boolean = false,
                           var _workspacePath : String = System.getenv().getOrDefault("SPINALSIM_WORKSPACE","./simWorkspace"),
                           var _workspaceName: String = null,
                           var _waveDepth : Int = 0, //0 => all
                           var _spinalConfig: SpinalConfig = SpinalConfig(),
                           var _optimisationLevel : Int = 0){
  def withWave : this.type = {
    _withWave = true
    this
  }

  def withWave(depth : Int) : this.type = {
    _withWave = true
    _waveDepth = depth
    this
  }

  def workspacePath(path : String) : this.type = {
    _workspacePath = path
    this
  }

  def workspaceName(name : String) : this.type = {
    _workspaceName = name
    this
  }

  def withConfig(config : SpinalConfig) : this.type = {
    _spinalConfig = config
    this
  }

  def noOptimisation : this.type = {
    _optimisationLevel = 0
    this
  }
  def fewOptimisation : this.type = {
    _optimisationLevel = 1
    this
  }
  def normalOptimisation : this.type = {
    _optimisationLevel = 2
    this
  }
  def allOptimisation : this.type = {
    _optimisationLevel = 3
    this
  }

  def doSim[T <: Component](report : SpinalReport[T])(body : T => Unit@suspendable): Unit = compile(report).doSim(body)
  def doSim[T <: Component](report : SpinalReport[T], name : String)(body : T => Unit@suspendable) : Unit = compile(report).doSim(name)(body)
  def doSim[T <: Component](report : SpinalReport[T], name : String, seed : Long)(body : T => Unit@suspendable) : Unit = compile(report).doSim(name,seed)(body)

  def doSimUntilVoid[T <: Component](report : SpinalReport[T])(body : T => Unit@suspendable): Unit = compile(report).doSimUntilVoid(body)
  def doSimUntilVoid[T <: Component](report : SpinalReport[T], name : String)(body : T => Unit@suspendable) : Unit = compile(report).doSimUntilVoid(name)(body)
  def doSimUntilVoid[T <: Component](report : SpinalReport[T], name : String, seed : Long)(body : T => Unit@suspendable) : Unit = compile(report).doSimUntilVoid(name,seed)(body)

  def doSim[T <: Component](rtl : => T)(body : T => Unit@suspendable): Unit = compile(rtl).doSim(body)
  def doSim[T <: Component](rtl : => T, name : String)(body : T => Unit@suspendable) : Unit = compile(rtl).doSim(name)(body)
  def doSim[T <: Component](rtl : => T, name : String, seed : Long)(body : T => Unit@suspendable) : Unit = compile(rtl).doSim(name,seed)(body)

  def doSimUntilVoid[T <: Component](rtl : => T)(body : T => Unit@suspendable): Unit = compile(rtl).doSimUntilVoid(body)
  def doSimUntilVoid[T <: Component](rtl : => T, name : String)(body : T => Unit@suspendable) : Unit = compile(rtl).doSimUntilVoid(name)(body)
  def doSimUntilVoid[T <: Component](rtl : => T, name : String, seed : Long)(body : T => Unit@suspendable) : Unit = compile(rtl).doSimUntilVoid(name,seed)(body)


  def compile[T <: Component](rtl: => T) : SimCompiled[T] = {
    val uniqueId = SimWorkspace.allocateUniqueId()
    new File(s"tmp").mkdirs()
    new File(s"tmp/job_$uniqueId").mkdirs()
    val report = _spinalConfig.copy(targetDirectory = s"tmp/job_$uniqueId").addTransformationPhase(new SwapTagPhase(SimPublic, Verilator.public)).generateVerilog(rtl)
    compile[T](report)
  }

  def compile[T <: Component](report : SpinalReport[T]) : SimCompiled[T] = {
    if(_workspacePath.startsWith("~"))
      _workspacePath = System.getProperty( "user.home" ) + _workspacePath.drop(1)

    if(_workspaceName == null)
      _workspaceName = s"${report.toplevelName}"

    _workspaceName = SimWorkspace.allocateWorkspace(_workspacePath, _workspaceName)

    println(f"[Progress] Simulation workspace in ${new File(s"${_workspacePath}/${_workspaceName}").getAbsolutePath}")
    new File(s"${_workspacePath}").mkdirs()
    FileUtils.deleteQuietly(new File(s"${_workspacePath}/${_workspaceName}"))
    new File(s"${_workspacePath}/${_workspaceName}").mkdirs()
    new File(s"${_workspacePath}/${_workspaceName}/rtl").mkdirs()
    report.generatedSourcesPaths.foreach{srcPath =>
      FileUtils.copyFileToDirectory(new File(srcPath), new File(s"${_workspacePath}/${_workspaceName}/rtl"))
    }

    println(f"[Progress] Verilator compilation started")
    val startAt = System.nanoTime()
    val vConfig = SpinalVerilatorBackendConfig[T](
      rtl = report,
      withWave = _withWave,
      workspacePath = s"${_workspacePath}/${_workspaceName}",
      vcdPath = s"${_workspacePath}/${_workspaceName}",
      vcdPrefix = null,
      workspaceName = "verilator",
      //      workspacePath = s"${_workspacePath}",
      //      workspaceName = s"${_workspaceName}",
      waveDepth = _waveDepth,
      optimisationLevel = _optimisationLevel
    )
    val backend = SpinalVerilatorBackend(vConfig)
    val deltaTime = (System.nanoTime() - startAt)*1e-6
    println(f"[Progress] Verilator compilation done in $deltaTime%1.3f ms")
    new SimCompiled(backend, report.toplevel)
  }
}



case class SimConfigLegacy[T <: Component]( var _rtlGen : Option[() => T] = None,
                                            var _spinalConfig: SpinalConfig = SpinalConfig(),
                                            var _spinalReport : Option[SpinalReport[T]] = None){
  private val _simConfig = SpinalSimConfig()
  def withWave : this.type = { _simConfig.withWave; this }
  def withWave(depth : Int) : this.type =  { _simConfig.withWave(depth); this }

  def workspacePath(path : String) : this.type =  { _simConfig.workspacePath(path); this }
  def workspaceName(name : String) : this.type =  { _simConfig.workspaceName(name); this }
  def withConfig(config : SpinalConfig) : this.type =  { _simConfig.withConfig(config); this }

  def noOptimisation : this.type = { _simConfig.noOptimisation ; this }
  def fewOptimisation : this.type =  { _simConfig.fewOptimisation ; this }
  def normalOptimisation : this.type =  { _simConfig.normalOptimisation ; this }
  def allOptimisation : this.type =  { _simConfig.allOptimisation ; this }

  def doSim(body : T => Unit@suspendable): Unit = compile.doSim(body)
  def doSim(name : String)(body : T => Unit@suspendable) : Unit = compile.doSim(name)(body)
  def doSim(name : String, seed : Long)(body : T => Unit@suspendable) : Unit = compile.doSim(name,seed)(body)

  def doManagedSim(body : T => Unit@suspendable): Unit = compile.doSim(body)
  def doManagedSim(name : String)(body : T => Unit@suspendable) : Unit = compile.doSim(name)(body)
  def doManagedSim(name : String, seed : Long)(body : T => Unit@suspendable) : Unit = compile.doSim(name,seed)(body)

  def doSimUntilVoid(body : T => Unit@suspendable): Unit = compile.doSimUntilVoid(body)
  def doSimUntilVoid(name : String)(body : T => Unit@suspendable) : Unit = compile.doSimUntilVoid(name)(body)
  def doSimUntilVoid(name : String, seed : Long)(body : T => Unit@suspendable) : Unit = compile.doSimUntilVoid(name,seed)(body)

  def compile() : SimCompiled[T] = {
    (_rtlGen, _spinalReport) match {
      case (None, Some(report)) => _simConfig.compile(report)
      case (Some(gen), None) => _simConfig.compile(gen())
    }
  }
}
