package spinal.core.sim

import spinal.core.internals.GraphUtils
import spinal.core.{Bits, Bool, Component, SInt, SpinalConfig, SpinalEnumCraft, SpinalReport, UInt}
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

    GraphUtils.walkAllComponents(rtl.toplevel, c => c.dslBody.walkStatements(_.algoInt = -1))
    var signalId = 0
    for(io <- rtl.toplevel.getAllIo){
      val signal = new Signal(List(io.getName()), io match {
        case io : Bool => new BoolDataType
        case io : Bits => new BitsDataType(io.getBitsWidth)
        case io : UInt => new UIntDataType(io.getBitsWidth)
        case io : SInt => new SIntDataType(io.getBitsWidth)
        case io : SpinalEnumCraft[_] => new BitsDataType(io.getBitsWidth)
      })
      io.algoInt = signalId
      io.algoIncrementale = -1
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

object SimConfig{
  def apply[T <: Component](rtl:  => T) : SimConfig[T] ={
    SimConfig[T](_rtlGen = Some(() => rtl))
  }
  def apply[T <: Component](rtl: SpinalReport[T]) : SimConfig[T] ={
    SimConfig[T](_spinalReport = Some(rtl))
  }
}

class SimCompiled[T <: Component](backend : VerilatorBackend, dut : T){
  var anonymRunId = 0
  def allocateRunId : Int = this.synchronized{
    anonymRunId += 1
    anonymRunId
  }
  def doManagedSim(body : T => Unit@suspendable) : Unit = doManagedSim("test" + allocateRunId)(body)
  def doManagedSim(name : String)(body : T => Unit@suspendable) : Unit = doManagedSim(name, Random.nextLong())(body)
  def doManagedSim(name : String, seed : Long)(body : T => Unit@suspendable) : Unit = {
    Random.setSeed(seed)
    doManagedSimPostSeed(name, Random.nextLong())(body)
  }
  def doManagedSimPostSeed(name : String, seed : Long)(body : T => Unit@suspendable) : Unit = {
    val seedInt = seed.toInt
    val backendSeed = if(seedInt == 0) 1 else seedInt
    val sim = new SimVerilator(backend, backend.instanciate(name, backendSeed))
    sim.userData = backend.config.signals
    val manager = new SimManager(sim)
    manager.userData = dut
    println(f"[Progress] Start ${dut.definitionName} $name simulation with seed $seed")
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

  val workspaceMap = mutable.HashMap[String, Int]()
  def allocateWorkspace(name : String) : String = {
    workspaceMap.synchronized{
      val value = workspaceMap.getOrElseUpdate(name, 0)
      workspaceMap(name) = value + 1
      if(value == 0){
        return name
      }else{
        val ret = name + "_1"
        println(s"[Info] Workspace $name was reallocated as $ret to avoid collision")
        return ret
      }
    }
  }
}
case class SimConfig[T <: Component](var _withWave: Boolean = false,
                                     var _workspacePath : String = System.getenv().getOrDefault("SPINALSIM_WORKSPACE","./simWorkspace"),
                                     var _workspaceName: String = null,
                                     var _waveDepth : Int = 0, //0 => all
                                     var _rtlGen : Option[() => T] = None,
                                     var _spinalConfig: SpinalConfig = SpinalConfig(),
                                     var _spinalReport : Option[SpinalReport[T]] = None,
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

  def doManagedSim(body : T => Unit@suspendable): Unit = compile.doManagedSim(body)
  def doManagedSim(name : String)(body : T => Unit@suspendable) : Unit = compile.doManagedSim(name)(body)
  def doManagedSim(name : String, seed : Long)(body : T => Unit@suspendable) : Unit = compile.doManagedSim(name,seed)(body)

  def compile() : SimCompiled[T] = {
    if(_workspacePath.startsWith("~"))
      _workspacePath = System.getProperty( "user.home" ) + _workspacePath.drop(1)

    val report = (_rtlGen, _spinalReport) match {
      case (None, Some(report)) => report
      case (Some(gen), None) => {
        val uniqueId = SimWorkspace.allocateUniqueId()
        s"mkdir -p tmp".!
        s"mkdir -p tmp/job_$uniqueId".!
        _spinalConfig.copy(targetDirectory = s"tmp/job_$uniqueId").generateVerilog(gen())
      }
    }
    if(_workspaceName == null)
      _workspaceName = s"${report.toplevelName}"
    s"mkdir -p ${_workspacePath}".!
    s"rm -rf ${_workspacePath}/${_workspaceName}".!
    s"mkdir -p ${_workspacePath}/${_workspaceName}".!
    s"mkdir -p ${_workspacePath}/${_workspaceName}/rtl".!
    report.generatedSourcesPaths.foreach{srcPath =>
      s"cp ${srcPath} ${_workspacePath}/${_workspaceName}/rtl/".!
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
