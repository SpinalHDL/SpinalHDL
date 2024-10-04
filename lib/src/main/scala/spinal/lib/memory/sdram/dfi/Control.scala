package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi.function._
import spinal.lib.memory.sdram.dfi.interface.{DfiConfig, IDFI, TaskConfig, TaskPort}

case class Control(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Component {
  val io = new Bundle {
    val inport = slave(TaskPort(taskConfig, dfiConfig))
    val outport = master(IDFI(dfiConfig))
  }

  val cmd = new Area {
    val cmdtxd = CmdTxd(taskConfig, dfiConfig)
    cmdtxd.io.task <> io.inport.tasks.init()
  }

  val wrdata = new Area {
    val wrdatatxd = WrDataTxd(taskConfig, dfiConfig)
    wrdatatxd.io.write <> RegNext(io.inport.tasks.write).init(False)
    wrdatatxd.io.taskWrData << io.inport.writeData
  }

  val radata = new Area {
    val rddatarxd = RdDataRxd(taskConfig, dfiConfig)
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
