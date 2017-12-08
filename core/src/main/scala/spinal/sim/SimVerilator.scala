package spinal.sim

import spinal.core._
import spinal.core.internals.GraphUtils




class SimVerilator[T <: Component](dut : T, backend : VerilatorBackend, handle : Long) extends SimRaw(dut){
  override def peak(bt : BaseType) : Long = {
    assert(bt.algoInt != -1, "You can't access this signal in the simulation, as it isn't public")
    backend.native.wrapperGetU64(handle, bt.algoInt)
  }
  override def poke(bt : BaseType, value : Long) : Unit = {
    assert(bt.algoInt != -1, "You can't access this signal in the simulation, as it isn't public")
    backend.native.wrapperSetU64(handle, bt.algoInt, value)
  }
  override def eval() = backend.native.wrapperEval(handle)
  override def sleep(cycles : Long) = backend.native.wrapperSleep(handle, cycles)
  override def end() = backend.native.wrapperDeleteHandle(handle)
  override protected def getClock(cd: ClockDomain): Bool = dut.pulledDataCache(cd.clock).asInstanceOf[Bool]
}

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
      val dataType = if(io.getBitsWidth <= 8) "CData"
      else SpinalError("???")
      vConfig.signals += VerilatorSignal(List(io.getName()),dataType)

      io.algoInt = signalId
      signalId += 1
    }
    val backend = new VerilatorBackend(config,vConfig)
    val sim = new SimVerilator(report.toplevel, backend, backend.instanciate())
    SimRaw.threadLocal.set(sim)
    sim
  }
}