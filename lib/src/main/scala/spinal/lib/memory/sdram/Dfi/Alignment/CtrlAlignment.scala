package spinal.lib.memory.sdram.Dfi.Alignment

import spinal.lib.memory.sdram.xdr.Dfi.CtrlWithBmb._
import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.Dfi.CtrlWithBmb.Refresher
import spinal.lib.memory.sdram.Dfi.Interface.{CoreConfig, CoreParameterAggregate, CorePort, CorePortParameter, DfiConfig, DfiTimeConfig, IDFI}
import spinal.lib.memory.sdram.Dfi.Tools
import spinal.lib.memory.sdram.Dfi.Tools.{CmdTxd, MakeTask, RdDataRxd, WrDataTxd}

//import spinal.lib.memory.sdram.xdr
case class CtrlAlignment(cpp : CorePortParameter, cpa : CoreParameterAggregate) extends Component{
  import cpa._
  import cpa.config._
  val io = new Bundle{
    val inport = slave(CorePort(cpp,cpa))
    val outport = master(IDFI(config))
    val refresh = out Bool()
  }


  val maketask = MakeTask(cpp,cpa)
  maketask.io.cmd << io.inport.cmd
  maketask.io.writeDataTockens <> io.inport.writeDataTocken

  val refresher = Refresher(cpa)
  refresher.io.refresh.valid <> io.refresh
  refresher.io.refresh <> maketask.io.refresh


  val cmd = new Area {
    val cmdtxd = CmdTxd(cpa)
    cmdtxd.io.task <> maketask.io.output.init()
  }

  val wrdata = new Area {
    val wrdatatxd = Tools.WrDataTxd(cpa)
    wrdatatxd.io.write <> RegNext(maketask.io.output.task.write).init(False)
    wrdatatxd.io.coreWrdata << io.inport.writeData
  }

  val radata = new Area {
    val rddatarxd = Tools.RdDataRxd(cpa)
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
