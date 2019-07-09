package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._

case class Backend(cp : CoreParameter) extends Component{
  def pl = cp.pl
  def ml = pl.ml


  val io = new Bundle {
    val config = in(CoreConfig(cp))
    val input = slave(Flow(Fragment(FrontendCmdOutput(cp))))
    val phy = master(SdramXdrPhyCtrl(pl))
    val outputs = Vec(master(Stream(Fragment(CoreRsp(cp)))), cp.portCount)
    val full = out Bool()
    val init = slave(InitBus(cp))
  }


  val command = new Area{
    val CSn  = False
    val RASn = True
    val CASn = True
    val WEn  = True
    val CKE  = True
  }

  io.phy.ADDR.assignDontCare()
  io.phy.BA.assignDontCare()
  io.phy.DQe := False
  for((phase, id) <- io.phy.phases.zipWithIndex){
    when(io.config.commandPhase =/= id) {
      phase.CSn := False
      phase.RASn := True
      phase.CASn := True
      phase.WEn := True
      phase.CKE := True
    } otherwise {
      phase.CSn := command.CSn
      phase.RASn := command.RASn
      phase.CASn := command.CASn
      phase.WEn := command.WEn
      phase.CKE := command.CKE
    }
    phase.DQw.assignDontCare()
    phase.DM.assignDontCare()
  }

  val writePipeline = new Area{
    case class Cmd() extends Bundle {
      val data = Bits(pl.beatWidth bits)
      val mask = Bits(pl.beatWidth/8 bits)
    }
    val input = Flow(Cmd())
    val histories = History(input, 0 to cp.writeLatencies.max)
    histories.tail.foreach(_.valid init(False))

    if(ml.withDqs) ???
    switch(io.config.writeLatency) {
      for (i <- 0 until cp.writeLatencies.size) {
        is(i){
          when(histories(cp.writeLatencies(i)).valid){
            io.phy.DQe := True
            for((phase, dq, dm) <- (io.phy.phases, histories(i).data.subdivideIn(pl.phaseCount slices), histories(i).mask.subdivideIn(pl.phaseCount slices)).zipped){
              phase.DQw := dq
              phase.DM := ~dm
            }
          }
        }
      }
    }
  }

  case class PipelineCmd() extends Bundle{
    val write = Bool
    val context = Bits(cp.contextWidth bits)
    val source = UInt(log2Up(cp.portCount) bits)
  }
  case class PipelineRsp() extends Bundle{
    val data = Bits(pl.beatWidth bits)
    val source = UInt(log2Up(cp.portCount) bits)
    val context = Bits(cp.contextWidth bits)
  }

  val rspPipeline = new Area{
    val input = Flow(PipelineCmd())

    val histories = History(input, 0 to cp.readLatencies.max + pl.outputLatency + pl.inputLatency - 1)
    histories.tail.foreach(_.valid init(False))

    val buffer = Reg(Flow(PipelineCmd()))
    val bufferCounter = Reg(UInt(log2Up(pl.beatCount) bits)) init(0)
    val bufferLast = bufferCounter === pl.beatCount || buffer.write
    bufferCounter := bufferCounter + U(buffer.valid).resized
    buffer.valid init(False)
    buffer.valid clearWhen(bufferLast)

    switch(io.config.readLatency) {
      for (i <- 0 until cp.readLatencies.size) {
        is(i){
          val history = histories(cp.readLatencies(i))
          when(history.valid){
            buffer.valid := True
            buffer.payload := history.payload
          }
        }
      }
    }


    val output = Flow(Fragment(PipelineRsp()))
    output.valid := buffer.valid
    output.context := buffer.context
    output.source := buffer.source
    output.last := bufferLast
    for((outputData, phase) <- (output.data.subdivideIn(pl.phaseCount slices), io.phy.phases).zipped){
      outputData := phase.DQw
    }
  }

  val rspBuffer = StreamFifoLowLatency(Fragment(PipelineRsp()), 1 + pl.outputLatency + cp.readLatencies.max + pl.inputLatency + 1)
  rspBuffer.io.push << rspPipeline.output.toStream
  io.full := !rspBuffer.empty.pull()
  val rspPop = rspBuffer.io.pop.stage()
  for((output, outputId) <- io.outputs.zipWithIndex){
    val hit = rspPop.source === outputId
    output.valid := rspPop.valid && hit
    output.last := rspPop.last
    output.data := rspPop.data
    output.context := rspPop.context
  }
  rspPop.ready := io.outputs(rspPop.source).ready


  writePipeline.input.valid := io.input.valid && io.input.kind === FrontendCmdOutputKind.WRITE
  writePipeline.input.data := io.input.data
  writePipeline.input.mask := io.input.mask

  rspPipeline.input.valid := False
  rspPipeline.input.context := io.input.context
  rspPipeline.input.source := io.input.source
  rspPipeline.input.write := io.input.kind === FrontendCmdOutputKind.WRITE

  when(io.input.valid) {
    when(io.input.first) {
      switch(io.input.kind) {
        is(FrontendCmdOutputKind.PRECHARGE) {
          io.phy.ADDR := io.input.address.row.asBits.resized
          io.phy.ADDR(10) := io.input.all
          io.phy.BA := io.input.address.bank.asBits
          command.CSn := False
          command.RASn := False
          command.CASn := True
          command.WEn := False
        }
        is(FrontendCmdOutputKind.REFRESH) {
          command.CSn := False
          command.RASn := False
          command.CASn := False
          command.WEn := True
        }
        is(FrontendCmdOutputKind.ACTIVE) {
          io.phy.ADDR := io.input.address.row.asBits.resized
          io.phy.BA := io.input.address.bank.asBits
          command.CSn := False
          command.RASn := False
          command.CASn := True
          command.WEn := True
        }
        is(FrontendCmdOutputKind.WRITE) {
          io.phy.ADDR := io.input.address.column.asBits.resized
          io.phy.ADDR(10) := False
          io.phy.BA := io.input.address.bank.asBits
          command.CSn := False
          command.RASn := True
          command.CASn := False
          command.WEn := False
          rspPipeline.input.valid := True
        }
        is(FrontendCmdOutputKind.READ) {
          io.phy.ADDR := io.input.address.column.asBits.resized
          io.phy.ADDR(10) := False
          io.phy.BA := io.input.address.bank.asBits
          command.CSn := False
          command.RASn := True
          command.CASn := False
          command.WEn := True
          rspPipeline.input.valid := True
        }
      }
    }
  }

  when(io.init.cmd.valid){
    io.phy.ADDR := io.init.cmd.ADDR
    io.phy.BA := io.init.cmd.BA
    command.CASn := io.init.cmd.CASn
    command.CKE := io.init.cmd.CKE
    command.CSn := io.init.cmd.CSn
    command.RASn := io.init.cmd.RASn
    command.WEn := io.init.cmd.WEn
  }
}
