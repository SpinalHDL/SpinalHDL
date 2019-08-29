package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._

case class Backend(cpa : CoreParameterAggregate) extends Component{
  import cpa._


  val io = new Bundle {
    val config = in(CoreConfig(cpa))
    val input = slave(Flow(Fragment(CoreTask(cpa))))
    val phy = master(SdramXdrPhyCtrl(pl))
    val outputs = Vec(cpa.cpp.map(cpp => master(Stream(Fragment(CoreRsp(cpp, cpa))))))
    val full = out Bool()
    val soft = slave(SoftBus(cpa))
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
    if(generation.RESETn) phase.RESETn := io.soft.RESETn

    phase.CSn := False
    phase.RASn := True
    phase.CASn := True
    phase.WEn := True
    phase.CKE := io.soft.CKE

    when(io.config.commandPhase === id) {
      phase.CSn := command.CSn
      phase.RASn := command.RASn
      phase.CASn := command.CASn
      phase.WEn := command.WEn
    }

    phase.DQw.assignDontCare()
    phase.DM.assignDontCare()
  }

  val odt = new Area{
    val start = False
    val counter = Reg(UInt(cpa.cp.timingWidth bits)) init(0)
    when(counter =/= 0){
      counter := counter - 1
    }
    when(start){
      counter := io.config.ODT
    }

    val end = counter === 1
    for((phase, id) <- io.phy.phases.zipWithIndex){
      phase.ODT := False
      when(start && id >= io.config.commandPhase) {
        phase.ODT := True
      }
      when(end && io.config.ODTend(id)){
        phase.ODT := True
      }
    }
  }

  val writePipeline = new Area{
    case class Cmd() extends Bundle {
      val data = Bits(pl.beatWidth bits)
      val mask = Bits(pl.beatWidth/8 bits)
    }
    val input = Flow(Cmd())
    val histories = History(input, 0 to cp.writeLatencies.max)
    histories.tail.foreach(_.valid init(False))

//    if(pl.withDqs) ???
    switch(io.config.writeLatency) {
      for (i <- 0 until cp.writeLatencies.size) {
        is(i){
          when(histories(cp.writeLatencies(i)).valid){
            io.phy.DQe := True
          }
          for((phase, dq, dm) <- (io.phy.phases, histories(i).data.subdivideIn(pl.phaseCount slices), histories(i).mask.subdivideIn(pl.phaseCount slices)).zipped){
            phase.DQw := Vec(dq.subdivideIn(pl.dataRatio slices))
            phase.DM := Vec((~dm).subdivideIn(pl.dataRatio slices))
          }
        }
      }
    }
  }

  case class PipelineCmd() extends Bundle{
    val write = Bool
    val context = Bits(backendContextWidth bits)
    val source = UInt(log2Up(portCount) bits)
  }
  case class PipelineRsp() extends Bundle{
    val data = Bits(pl.beatWidth bits)
    val source = UInt(log2Up(portCount) bits)
    val context = Bits(backendContextWidth bits)
  }

  val rspPipeline = new Area{
    val input = Flow(PipelineCmd())

    val histories = History(input, 0 to cp.readLatencies.max + pl.outputLatency + pl.inputLatency-1)
    histories.tail.foreach(_.valid init(False))

    val buffer = Reg(Flow(PipelineCmd()))
    val bufferCounter = Reg(UInt(log2Up(pl.beatCount) bits)) init(0)
    val bufferLast = bufferCounter === pl.beatCount-1 || buffer.write
    bufferCounter := bufferCounter + U(buffer.valid).resized
    buffer.valid init(False)
    buffer.valid clearWhen(bufferLast)

    switch(io.config.readLatency) {
      for (i <- 0 until cp.readLatencies.size) {
        is(i){
          val history = histories(cp.readLatencies(i) + pl.outputLatency + pl.inputLatency-1)
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
      outputData := B(phase.DQr)
    }
  }

  val rspBufferMinSize = 1 + pl.outputLatency + cp.readLatencies.max + pl.inputLatency + 1 + 1
  val rspBuffer = StreamFifoLowLatency(Fragment(PipelineRsp()), 1 << log2Up(rspBufferMinSize + cp.rspFifoSize))
  rspBuffer.io.push << rspPipeline.output.toStream
  io.full := RegNext(rspBuffer.io.occupancy >= rspBuffer.depth - rspBufferMinSize)
  val rspPop = rspBuffer.io.pop.stage()
  for((output, outputId) <- io.outputs.zipWithIndex){
    val hit = rspPop.source === outputId
    output.valid := rspPop.valid && hit
    output.last := rspPop.last
    output.data := rspPop.data
    output.context := rspPop.context.resized
  }
  rspPop.ready := io.outputs(rspPop.source).ready


  writePipeline.input.valid := io.input.valid && io.input.kind === FrontendCmdOutputKind.WRITE
  writePipeline.input.data := io.input.data
  writePipeline.input.mask := 0

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
          writePipeline.input.mask := io.input.mask
          command.CSn := False
          command.RASn := True
          command.CASn := False
          command.WEn := False
          rspPipeline.input.valid := True
          odt.start := True
        }
        is(FrontendCmdOutputKind.READ) {
          io.phy.ADDR := io.input.address.column.asBits.resized
          io.phy.ADDR(10) := False
          io.phy.BA := io.input.address.bank.asBits
          writePipeline.input.mask.setAll()
          command.CSn := False
          command.RASn := True
          command.CASn := False
          command.WEn := True
          rspPipeline.input.valid := True
        }
      }
    }
  }

  when(io.soft.cmd.valid){
    io.phy.ADDR := io.soft.cmd.ADDR
    io.phy.BA := io.soft.cmd.BA
    command.CASn := io.soft.cmd.CASn
    command.CSn := io.soft.cmd.CSn
    command.RASn := io.soft.cmd.RASn
    command.WEn := io.soft.cmd.WEn
  }
}
