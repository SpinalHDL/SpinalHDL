package spinal.lib.memory.sdram.xdr.Dfi.CtrlWithBmb
import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.memory.sdram.xdr.Dfi.Interface._
import spinal.lib.memory.sdram.xdr.Dfi._


case class BmbPortParameter(bmb : BmbParameter,
                            clockDomain : ClockDomain,
                            cmdBufferSize : Int,
                            dataBufferSize : Int,
                            rspBufferSize : Int){
}

case class CtrlParameter( core : CoreParameter,
                          port : BmbPortParameter)

case class Bmb2Dfi(val ctp : CtrlParameter, pl : PhyLayout, config: DfiConfig/*, initp : BmbParameter*/) extends Component {
  val io = new Bundle {
    val bmb = slave(Bmb(ctp.port.bmb))
    val dfi = master(Dfi(config))
  }

  val cpa = CoreParameterAggregate(ctp.core, pl, BmbAdapter.corePortParameter(ctp.port, pl), config)

  val bmbBridge = BmbBridge(ctp.port, cpa)
  bmbBridge.io.bmb <>  io.bmb

  val control = Control(cpa)
  control.io.inport <> bmbBridge.io.taskPort

  val alignment = Alignment(cpa.config)
  alignment.io.inIdfiport <> control.io.outport
  alignment.io.outDfiport <> io.dfi
}
