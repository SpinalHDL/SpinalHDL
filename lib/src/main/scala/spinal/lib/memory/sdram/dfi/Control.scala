package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi.foundation._
import spinal.lib.memory.sdram.dfi.interface.{IDFI, TaskParameterAggregate, TaskPort}

case class Control(tpa: TaskParameterAggregate) extends Component {
  import tpa._
  val io = new Bundle {
    val inport = slave(TaskPort(tpp, tpa))
    val outport = master(IDFI(config))
  }

  val cmd = new Area {
    val cmdtxd = CmdTxd(tpa)
    cmdtxd.io.task <> io.inport.tasks.init()
  }

  val wrdata = new Area {
    val wrdatatxd = WrDataTxd(tpa)
    wrdatatxd.io.write <> RegNext(io.inport.tasks.write).init(False)
    wrdatatxd.io.taskWrData << io.inport.writeData
  }

  val radata = new Area {
    val rddatarxd = RdDataRxd(tpa)
    rddatarxd.io.task <> io.inport.tasks.init()
    rddatarxd.io.taskRdData.toStream >> io.inport.rsp
  }

  val idfiout = new Area {
    io.outport.cmd <> cmd.cmdtxd.io.cmd
    io.outport.address <> cmd.cmdtxd.io.address
    io.outport.wrData <> wrdata.wrdatatxd.io.idfiWrData
    io.outport.rdData <> radata.rddatarxd.io.idfiRdData
    io.outport.rdEn <> radata.rddatarxd.io.rden
  }

}
