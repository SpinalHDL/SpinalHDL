package spinal.lib.memory.sdram.Dfi.CtrlWithBmb

import spinal.lib._
import spinal.core._
import spinal.lib.bus.bmb.Bmb
import spinal.lib.memory.sdram.Dfi.Interface.{CoreParameterAggregate, TaskPort}
import spinal.lib.memory.sdram.Dfi.Tools.MakeTask
import spinal.lib.memory.sdram.xdr.Dfi.Interface._

case class BmbBridge(port : BmbPortParameter, cpa : CoreParameterAggregate) extends Component{
  import cpa._
  val io = new Bundle{
    val bmb      = slave(Bmb(port.bmb))
    val taskPort = master(TaskPort(cpp, cpa))
  }

  val bmbAdapter =  BmbAdapter(port, cpa)
  bmbAdapter.io.input <>  io.bmb
  bmbAdapter.io.output.writeData <> io.taskPort.writeData
  bmbAdapter.io.output.rsp <> io.taskPort.rsp

  val maketask = MakeTask(cpp,cpa)
  maketask.io.cmd << bmbAdapter.io.output.cmd
  maketask.io.writeDataTockens <> bmbAdapter.io.output.writeDataTocken
  maketask.io.output <> io.taskPort.tasks

  val refresher = Refresher(cpa)
  refresher.io.refresh.valid <> bmbAdapter.io.refresh
  refresher.io.refresh <> maketask.io.refresh
}
