package spinal.lib.memory.sdram.Dfi.CtrlWithBmb

import spinal.lib._
import spinal.core._
import spinal.lib.memory.sdram.Dfi.Interface.{CoreParameterAggregate, IDFI, TaskPort}
import spinal.lib.memory.sdram.Dfi.Tools
import spinal.lib.memory.sdram.Dfi.Tools.{CmdTxd, RdDataRxd, WrDataTxd}

case class Control(cpa : CoreParameterAggregate) extends Component{
  import cpa._
  val io = new Bundle{
    val inport  = slave(TaskPort(cpp, cpa))
    val outport = master(IDFI(config))
  }

  val cmd = new Area {
    val cmdtxd = CmdTxd(cpa)
    cmdtxd.io.task <> io.inport.tasks.init()
  }

  val wrdata = new Area {
    val wrdatatxd = Tools.WrDataTxd(cpa)
    wrdatatxd.io.write <> RegNext(io.inport.tasks.task.write).init(False)
    wrdatatxd.io.coreWrdata << io.inport.writeData
  }

  val radata = new Area {
    val rddatarxd = Tools.RdDataRxd(cpa)
    rddatarxd.io.task <> io.inport.tasks.init()
    rddatarxd.io.coreRddata.toStream >> io.inport.rsp
  }

  val idfiout = new Area {
    io.outport.cmd <> cmd.cmdtxd.io.cmd
    io.outport.address <> cmd.cmdtxd.io.address
    io.outport.wrdata <> wrdata.wrdatatxd.io.idfiWrdata
    io.outport.rddata <> radata.rddatarxd.io.idfiRddata
    io.outport.rden <> radata.rddatarxd.io.rden
  }


}
