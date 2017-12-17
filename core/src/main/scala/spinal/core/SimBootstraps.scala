package spinal.core

import spinal.core.internals.GraphUtils
import spinal.sim.{VerilatorBackend, _}

import scala.util.continuations.suspendable


object SpinalVerilatorBackend{
  def apply[T <: Component](rtl : SpinalReport[T], withWave : Boolean = false, optimisationLevel : Int = 2) = {
    val config = new VerilatorBackendConfig()
    config.rtlSourcesPaths ++= rtl.rtlSourcesPaths
    config.toplevelName = rtl.toplevelName
    config.workspacePath = s"${rtl.toplevelName}_verilatorSim"
    config.withWave = withWave
    config.optimisationLevel = optimisationLevel

    GraphUtils.walkAllComponents(rtl.toplevel, c => c.dslBody.walkStatements(_.algoInt = -1))
    var signalId = 0
    for(io <- rtl.toplevel.getAllIo){
      val signal = new Signal(List(io.getName()), io match {
        case io : Bool => new BoolDataType
        case io : Bits => new BitsDataType(io.getBitsWidth)
        case io : UInt => new UIntDataType(io.getBitsWidth)
        case io : SInt => new SIntDataType(io.getBitsWidth)
      })
      io.algoInt = signalId
      signal.id = signalId
      config.signals += signal
      signalId += 1
    }
    new VerilatorBackend(config)
  }
}


object SpinalVerilatorSim{
  def apply[T <: Component](rtl : SpinalReport[T], withWave : Boolean = false, optimisationLevel : Int = 2) = {
    val backend = SpinalVerilatorBackend(rtl, withWave, optimisationLevel)
    val sim = new SimVerilator(backend, backend.instanciate("test1"))
    sim.userData = backend.config.signals
    sim
  }

  def apply[T <: Component](backend : VerilatorBackend) = {
    val sim = new SimVerilator(backend, backend.instanciate("test1"))
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
  def doManagedSim(name : String)(body : T => Unit@suspendable) : Unit = {
    val sim = new SimVerilator(backend, backend.instanciate(name))
    sim.userData = backend.config.signals
    val manager = new SimManager(sim)
    manager.userData = dut
    println(s"[Progress] Start ${dut.definitionName} $name simulation")
    manager.run(body(dut))
  }
}

case class SimConfig[T <: Component]( var _withWave: Boolean = false,
                                      var _rtlGen : Option[() => T] = None,
                                      var _spinalConfig: SpinalConfig = SpinalConfig(),
                                      var _spinalReport : Option[SpinalReport[T]] = None,
                                      var _optimisationLevel : Int = 0){
  def withWave : this.type = {
    _withWave = true
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

  def compile() : SimCompiled[T] = {
    val report = (_rtlGen, _spinalReport) match {
      case (None, Some(report)) => report
      case (Some(gen), None) => _spinalConfig.generateVerilog(gen())
    }
    val startAt = System.nanoTime()
    val backend = SpinalVerilatorBackend(report, _withWave, _optimisationLevel)
    val deltaTime = (System.nanoTime() - startAt)*1e-6
    println(f"[Progress] Verilator compilation done in $deltaTime%1.3f ms")
    new SimCompiled(backend, report.toplevel)
  }
}
