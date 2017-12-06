package spinal.sim

import spinal.core._

class VerilatorSim[T <: Component](val dut : T, backend : VerilatorBackend, handle : Long) {
  def peak(bt : BaseType) : Byte = backend.native.wrapperGetCData(handle, bt.algoInt)
  def poke(bt : BaseType, value : Byte) : Unit = backend.native.wrapperSetCData(handle, bt.algoInt, value)
  def step() = backend.native.wrapperEval(handle)
}

object VerilatorSim{
  def apply[T <: Component](rtl : => T) = {
    val report = SpinalVerilog(rtl)
    val config = new BackendConfig()
    config.rtlSourcesPaths ++= report.rtlSourcesPaths
    config.toplevelName = report.toplevelName
    config.workspacePath = s"${report.toplevelName}_verilatorSim"

    val vConfig = new VerilatorBackendConfig
    var signalId = 0
    for(io <- report.toplevel.getAllIo){
      val dataType = if(io.getBitsWidth <= 8) "CData"
      else SpinalError("???")
      vConfig.signals += VerilatorSignal(List(io.getName()),dataType)

      io.algoInt = signalId
      signalId += 1
    }
    val backend = new VerilatorBackend(config,vConfig)
    new VerilatorSim(report.toplevel, backend, backend.instanciate())
  }
}