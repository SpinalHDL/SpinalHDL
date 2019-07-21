package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._

case class CoreParameterAggregate(cp : CoreParameter, pl : PhyLayout, cpp : Seq[CorePortParameter]){
  def ml = pl.ml
  def backendContextWidth = cpp.map(_.contextWidth).max
  def portCount = cpp.size
}

case class Core(cpa : CoreParameterAggregate) extends Component {
  import cpa._
  
  val io = new Bundle {
    val config = in(CoreConfig(cpa))
    val soft = slave(SoftBus(cpa))
    val ports = Vec(cpp.map(cpp => slave(CorePort(cpp, cpa))))
    val phy = master(SdramXdrPhyCtrl(pl))
  }

  val refresher = Refresher(cpa)
  refresher.io.config <> io.config

  val tasker = Tasker(cpa)
  tasker.io.inputs <> Vec(io.ports.map(_.cmd))
  tasker.io.refresh <> refresher.io.refresh

  val timingEnforcer = TimingEnforcer(cpa)
  timingEnforcer.io.config <> io.config
  timingEnforcer.io.input << tasker.io.output.stage()

  val backend = Backend(cpa)
  backend.io.config <> io.config
  backend.io.input << timingEnforcer.io.output.stage()
  backend.io.phy <> io.phy
  backend.io.full <> tasker.io.backendFull
  backend.io.init <> io.soft
  (backend.io.outputs, io.ports).zipped.foreach(_ <> _.rsp)
}

object CoreMain extends App{
  val ml = MemoryLayout(bankWidth = 2,
                        columnWidth = 10,
                        rowWidth = 13,
                        dataWidth = 16,
                        withDqs = false,
                        burstLength = 1)
  val pl = SdrInferedPhy.memoryLayoutToPhyLayout(ml)
//  val cp = CoreParameter(
//    pl = pl,
//    portCount = 3,
//    contextWidth = 6,
//    timingWidth = 4,
//    refWidth = 16,
//    writeLatencies = List(0),
//    readLatencies = List(2)
//  )
//  SpinalVerilog(Core(cp))
}