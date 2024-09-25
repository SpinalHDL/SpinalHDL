package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.dfi.foundation._
import spinal.lib.memory.sdram.dfi.interface.{DfiConfig, IDFI, TaskConfig, TaskPort}

case class Control(tc: TaskConfig, dc: DfiConfig) extends Component {
  val io = new Bundle {
    val inport = slave(TaskPort(tc, dc))
    val outport = master(IDFI(dc))
  }

  val cmd = new Area {
    val cmdtxd = CmdTxd(tc, dc)
    cmdtxd.io.task <> io.inport.tasks.init()
  }

  val wrdata = new Area {
    val wrdatatxd = WrDataTxd(tc, dc)
    wrdatatxd.io.write <> RegNext(io.inport.tasks.write).init(False)
    wrdatatxd.io.taskWrData << io.inport.writeData
  }

  val radata = new Area {
    val rddatarxd = RdDataRxd(tc, dc)
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
