package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbParameter}

case class BmbBridge(bmbp: BmbParameter, taskConfig: TaskConfig, dfiConfig: DfiConfig, addrMap: AddrMap)
    extends Component {

  val io = new Bundle {
    val bmb = slave(Bmb(bmbp))
    val taskPort = master(TaskPort(taskConfig, dfiConfig))
    // Pass-through of the training pause (ctrlmsg channel or legacy write-leveling
    // handshake) to MakeTask (present only when either channel is enabled).
    val trainBusy =
      (dfiConfig.signalConfig.useCtrlMsg || dfiConfig.signalConfig.useLegacyWrLvl) generate in(Bool())
    // MakeTask's initDone, exported only for the boot-training orchestrator (DfiController's
    // BootTrainer consumes it). Absent in every other config. (Braced block: an unbraced
    // `generate out Bool ()` parses as `(generate out) Bool ()` and NPEs when the gate is off.)
    val initDone = (dfiConfig.signalConfig.useCtrlMsg && dfiConfig.bootTraining) generate out(Bool())
  }

  val bmbAdapter = BmbAdapter(bmbp, taskConfig, dfiConfig)
  bmbAdapter.io.input <> io.bmb
  bmbAdapter.io.output.writeData >-> io.taskPort.writeData
  bmbAdapter.io.output.rsp <-< io.taskPort.rsp

  val maketask = MakeTask(taskConfig, dfiConfig, addrMap)
  if (dfiConfig.signalConfig.useCtrlMsg || dfiConfig.signalConfig.useLegacyWrLvl) maketask.io.trainBusy := io.trainBusy
  if (dfiConfig.signalConfig.useCtrlMsg && dfiConfig.bootTraining) io.initDone := maketask.io.initDoneOut
  maketask.io.cmd <-< bmbAdapter.io.output.cmd
  maketask.io.writeDataToken <-< bmbAdapter.io.output.writeDataToken
  maketask.io.output <> io.taskPort.tasks
  RegNext(maketask.io.halt) <> bmbAdapter.io.halt
}
