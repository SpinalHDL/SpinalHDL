package spinal.lib.memory.sdram.xdr.Dfi.CtrlWithBmb
import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.memory.sdram.xdr.Dfi.Interface._
import spinal.lib.memory.sdram.xdr.Dfi._
//import spinal.lib.memory.sdram.xdr.PhyLayout

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
    val initDone = out Bool()
  }

  val cpa = CoreParameterAggregate(ctp.core, pl, BmbAdapter.corePortParameter(ctp.port, pl), config)

  val bmbAdapter =  BmbAdapter(ctp.port, cpa)
  bmbAdapter.io.input <>  io.bmb

//  val dfiAlignment = DfiAlignment(cpa.cpp, cpa, initp)
  val dfiAlignment = DfiAlignment(cpa.cpp, cpa)
  dfiAlignment.io.inCoreport <> bmbAdapter.io.output
  dfiAlignment.io.outDfiport <> io.dfi
  dfiAlignment.io.refresh <> bmbAdapter.io.refresh
//  dfiAlignment.io.ctrl <> io.ctrl
  dfiAlignment.io.initDone <> io.initDone
}
