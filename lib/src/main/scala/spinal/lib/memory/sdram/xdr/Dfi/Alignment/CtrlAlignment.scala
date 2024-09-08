package spinal.lib.memory.sdram.xdr.Dfi.Alignment

import spinal.lib.memory.sdram.xdr.Dfi.Tools.{CmdTxd, MakeTask, RdDataRxd, WrDataTxd}
import spinal.lib.memory.sdram.xdr.Dfi.CtrlWithBmb._
import spinal.core._
import spinal.lib.memory.sdram.xdr.Dfi.Interface.{CoreConfig, CoreParameterAggregate, CorePort, CorePortParameter, DfiConfig, DfiTimeConfig, IDFI}
import spinal.lib._
//import spinal.lib.memory.sdram.xdr
case class CtrlAlignment(cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Component{
  import cpa._
  import cpa.config._
  val io = new Bundle{
    val inport = slave(CorePort(cpp,cpa))
    val outport = master(IDFI(config))
    val refresh = out Bool()
  }

//  val coreconfig = CoreConfig(cpa)
//  coreconfig.RAS := 4
//  coreconfig.RP := 4
//  coreconfig.WR := 4
//  coreconfig.RCD := 4
//  coreconfig.WTR := 4
//  coreconfig.RTP := 4
//  coreconfig.RRD := 4
//  coreconfig.RTW := 4
//  coreconfig.REF := 50
//  coreconfig.RFC := 4
//  coreconfig.autoRefresh := True

  val maketask = MakeTask(cpp,cpa)
//  maketask.io.config <> coreconfig
  maketask.io.cmd << io.inport.cmd
  maketask.io.writeDataTockens <> io.inport.writeDataTocken

  val refresher = Refresher(cpa)
//  refresher.io.config <> coreconfig
  refresher.io.refresh.valid <> io.refresh
  refresher.io.refresh <> maketask.io.refresh


  val cmd = new Area {
    val cmdtxd = CmdTxd(cpa)
    cmdtxd.io.task <> maketask.io.output.init()
  }

  val wrdata = new Area {
    val wrdatatxd = WrDataTxd(cpa)
    wrdatatxd.io.write <> RegNext(maketask.io.output.task.write).init(False)
    wrdatatxd.io.coreWrdata << io.inport.writeData
  }

  val radata = new Area {
    val rddatarxd = RdDataRxd(cpa)
    rddatarxd.io.task <> maketask.io.output.init()
    io.inport.rsp << rddatarxd.io.coreRddata.toStream
  }

  val idfiout = new Area {
    io.outport.cmd <> cmd.cmdtxd.io.cmd
    io.outport.address <> cmd.cmdtxd.io.address
    io.outport.wrdata <> wrdata.wrdatatxd.io.idfiWrdata
    io.outport.rddata <> radata.rddatarxd.io.idfiRddata
    io.outport.rden <> radata.rddatarxd.io.rden
  }

}
