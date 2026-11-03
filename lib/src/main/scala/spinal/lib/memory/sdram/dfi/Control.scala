package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

case class Control(taskConfig: TaskConfig, dfiConfig: DfiConfig) extends Component {
  val io = new Bundle {
    val input = slave(TaskPort(taskConfig, dfiConfig))
    val output = master(IDFI(dfiConfig))
  }

  // Single registered copy of the task port, shared by the command and read-data datapaths
  // (calling init() twice would duplicate the whole OpTasks register bank).
  val tasksInit = io.input.tasks.init()

  val cmd = new Area {
    val cmdtxd = CmdTxd(taskConfig, dfiConfig)
    cmdtxd.io.task <> tasksInit
  }

  val wrdata = new Area {
    val wrdatatxd = WrDataTxd(taskConfig, dfiConfig)
    // write 标志与 rank 选择均延迟一拍,与 writeData 流(BmbAdapter 队列)的到达节奏对齐
    wrdatatxd.io.write <> RegNext(io.input.tasks.write).init(False)
    // 写命令的 rank 选择(低有效);无写命令时全 1(不选中任何 rank)
    wrdatatxd.io.cs <> RegNext(
      Mux(
        io.input.tasks.write,
        ~(B(1) << io.input.tasks.address.cs).resize(dfiConfig.chipSelectNumber),
        Bits(dfiConfig.chipSelectNumber bits).setAll()
      )
    ).init(Bits(dfiConfig.chipSelectNumber bits).setAll())
    wrdatatxd.io.input << io.input.writeData
  }

  val radata = new Area {
    val rddatarxd = RdDataRxd(taskConfig, dfiConfig)
    rddatarxd.io.task <> tasksInit
    rddatarxd.io.output.toStream >> io.input.rsp
  }

  val idfiout = new Area {
    io.output.cmd <> cmd.cmdtxd.io.cmd
    io.output.address <> cmd.cmdtxd.io.address
    io.output.wrData <> wrdata.wrdatatxd.io.output
    io.output.rdData <> radata.rddatarxd.io.input
    io.output.rdEn <> radata.rddatarxd.io.rden
    if (dfiConfig.useWrdataCsN) io.output.wrCs := wrdata.wrdatatxd.io.outputCs
    if (dfiConfig.useRddataCsN) io.output.rdCs := radata.rddatarxd.io.outputCs
  }

}
