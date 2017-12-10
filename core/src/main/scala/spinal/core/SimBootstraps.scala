package spinal.core

import spinal.core.internals.GraphUtils
import spinal.sim._

import scala.util.continuations.suspendable

object SimVerilator{
  def apply[T <: Component](rtl : => T) = {
    val report = SpinalVerilog(rtl)
    val config = new BackendConfig()
    config.rtlSourcesPaths ++= report.rtlSourcesPaths
    config.toplevelName = report.toplevelName
    config.workspacePath = s"${report.toplevelName}_verilatorSim"

    val vConfig = new VerilatorBackendConfig
    GraphUtils.walkAllComponents(report.toplevel, c => c.dslBody.walkStatements(_.algoInt = -1))
    var signalId = 0
    for(io <- report.toplevel.getAllIo){
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
    (sim, report.toplevel)
  }
}


object SimManagedVerilator {
  def apply[T <: Component](gen: => T)(body: T => Unit@suspendable): Unit = {
    val (sim, dut) = SimVerilator(gen)
    val manager = new SimManager(sim)
    manager.userData = dut
    manager.run(body(dut.asInstanceOf[T]))
  }
}
