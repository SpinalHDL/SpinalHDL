package spinal.core.sim

import spinal.core.internals.GraphUtils
import spinal.core.{Bits, Bool, Component, SInt, SpinalConfig, SpinalEnumCraft, SpinalReport, UInt}
import spinal.sim._

import scala.util.Random


case class SpinalVerilatorBackendConfig[T <: Component]( rtl : SpinalReport[T],
                                                         withWave : Boolean = false,
                                                         waveDepth : Int = 0,
                                                         optimisationLevel : Int = 2)

object SpinalVerilatorBackend{
  def apply[T <: Component](config : SpinalVerilatorBackendConfig[T]) = {
    import config._
    val vconfig = new VerilatorBackendConfig()
    vconfig.rtlSourcesPaths ++= rtl.rtlSourcesPaths
    vconfig.toplevelName = rtl.toplevelName
    vconfig.workspacePath = s"${rtl.toplevelName}_verilatorSim"
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

case class SimConfig[T <: Component]( var _withWave: Boolean = false,
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
    val report = (_rtlGen, _spinalReport) match {
      case (None, Some(report)) => report
      case (Some(gen), None) => _spinalConfig.generateVerilog(gen())
    }
    val startAt = System.nanoTime()
    val vConfig = SpinalVerilatorBackendConfig[T](
      rtl = report,
      withWave = _withWave,
      waveDepth = _waveDepth,
      optimisationLevel = _optimisationLevel
    )
    val backend = SpinalVerilatorBackend(vConfig)
    val deltaTime = (System.nanoTime() - startAt)*1e-6
    println(f"[Progress] Verilator compilation done in $deltaTime%1.3f ms")
    new SimCompiled(backend, report.toplevel)
  }
}
