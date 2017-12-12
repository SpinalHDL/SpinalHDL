package spinal.core

import spinal.core.internals.GraphUtils
import spinal.sim._

import scala.util.continuations.suspendable

object SimVerilator{
//  def apply[T <: Component](rtl : => T) = {
//    this.apply(SpinalVerilog(rtl))
//  }

  def apply[T <: Component](rtl : SpinalReport[T], withWave : Boolean = false) = {
    val config = new BackendConfig()
    config.rtlSourcesPaths ++= rtl.rtlSourcesPaths
    config.toplevelName = rtl.toplevelName
    config.workspacePath = s"${rtl.toplevelName}_verilatorSim"
    config.withWave = withWave

    val vConfig = new VerilatorBackendConfig
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
      vConfig.signals += signal
      signalId += 1
    }
    val backend = new VerilatorBackend(config,vConfig)
    val sim = new SimVerilator(backend, backend.instanciate())
    sim.userData = vConfig.signals
    (sim, rtl.toplevel)
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

case class SimConfig[T <: Component](var _withWave: Boolean = false,
                        var _rtlGen : Option[() => T] = None,
                        var _spinalConfig: SpinalConfig = SpinalConfig(),
                        var _spinalReport : Option[SpinalReport[T]] = None){
  def withWave : this.type = {
    _withWave = true
    this
  }

  def doManagedSim(body : T => Unit@suspendable): Unit ={
    (_rtlGen, _spinalReport) match {
      case (None, Some(report)) => SimManagedVerilator(report, _withWave)(body)
      case (Some(gen), None) => SimManagedVerilator(gen(), _withWave)(body)
    }
  }
}

object SimManagedVerilator {
  def apply[T <: Component](gen: => T)(body: T => Unit@suspendable): Unit = {
    SimManagedVerilator(SpinalVerilog(gen), false)(body)
  }

  def apply[T <: Component](gen: => T, withWave : Boolean)(body: T => Unit@suspendable): Unit = {
    SimManagedVerilator(SpinalVerilog(gen), withWave)(body)
  }

  def apply[T <: Component](rtl : SpinalReport[T], withWave : Boolean)(body: T => Unit@suspendable): Unit = {
    val (sim, dut) = SimVerilator(rtl, withWave)
    val manager = new SimManager(sim)
    manager.userData = dut
    manager.run(body(dut.asInstanceOf[T]))
  }

}
