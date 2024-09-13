package spinal.lib.memory.sdram.Dfi.CtrlWithBmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.memory.sdram.Dfi.Interface._


case class BmbPortParameter(bmb : BmbParameter,
                            clockDomain : ClockDomain,
                            cmdBufferSize : Int,
                            dataBufferSize : Int,
                            rspBufferSize : Int){
}

case class CtrlParameter(task : TaskParameter,
                         port : BmbPortParameter)

case class Bmb2Dfi(val ctp : CtrlParameter, pl : PhyConfig, config: DfiConfig) extends Component {
  val io = new Bundle {
    val bmb = slave(Bmb(ctp.port.bmb))
    val dfi = master(Dfi(config))
  }

  val tpa = TaskParameterAggregate(ctp.task, pl, BmbAdapter.corePortParameter(ctp.port, pl), config)

  val bmbBridge = BmbBridge(ctp.port, tpa)
  bmbBridge.io.bmb <>  io.bmb

  val control = Control(tpa)
  control.io.inport <> bmbBridge.io.taskPort

  val alignment = Alignment(tpa.config)
  alignment.io.inIdfiport <> control.io.outport
  alignment.io.outDfiport <> io.dfi
}
