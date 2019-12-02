package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.SdramGeneration

case class Backend(cpa: CoreParameterAggregate) extends Component {

  import cpa._


  val io = new Bundle {
    val config = in(CoreConfig(cpa))
    val input = slave(CoreTasks(cpa))
    val writeDatas = Vec(cpa.cpp.map(cpp => slave(Stream(CoreWriteData(cpp, cpa)))))
    val phy = master(SdramXdrPhyCtrl(pl))
    val outputs = Vec(cpa.cpp.map(cpp => master(Flow(Fragment(CoreRsp(cpp, cpa))))))
    val soft = slave(SoftBus(cpa))
  }


  val command = new Area {
    val CSn = False
    val RASn = True
    val CASn = True
    val WEn = True
    val CKE = True
  }

  io.phy.ADDR.assignDontCare()
  io.phy.BA.assignDontCare()
  for ((phase, id) <- io.phy.phases.zipWithIndex) {
    if (generation.RESETn) phase.RESETn := io.soft.RESETn

    phase.CSn := False
    phase.RASn := True
    phase.CASn := True
    phase.WEn := True
    phase.CKE := True

    when(io.config.commandPhase === id) {
      phase.CSn := command.CSn
      phase.RASn := command.RASn
      phase.CASn := command.CASn
      phase.WEn := command.WEn
    }

    phase.DQw.assignDontCare()
    phase.DM.assignDontCare()
  }

  val odt = generation.ODT generate new Area {
    val start = False
    val counter = Reg(UInt(cpa.cp.timingWidth bits)) init (0)
    when(counter =/= 0) {
      counter := counter - 1
    }
    when(start) {
      counter := io.config.ODT
    }

    for ((phase, id) <- io.phy.phases.zipWithIndex) {
      phase.ODT := False
      phase.ODT setWhen(start && id >= io.config.commandPhase)
      phase.ODT setWhen(counter > 1)
      phase.ODT setWhen(counter === 1 && io.config.ODTend(id))
    }
  }

  val writePipeline = new Area {
    case class Cmd() extends Bundle {
      val sel = UInt(log2Up(cpp.length) bits)
    }

    val input = Flow(Cmd())
    val inputRepeated = CombInit(input)
    val writeHistory = History(inputRepeated, 0 to Math.max(1, cp.writeLatencies.max + pl.writeDelay))
    writeHistory.tail.foreach(_.valid init (False))

    val repeat = (pl.beatCount != 1) generate new Area {
      val counter = Reg(UInt(log2Up(pl.beatCount) bits)) init (0)
      when(inputRepeated.valid) {
        counter := counter + 1
      }
      when(counter =/= 0) {
        inputRepeated.valid := True
        inputRepeated.payload := writeHistory(1).payload
      }
    }

    val history = Flow(Cmd()).allowOverride
    history.valid := False
    history.sel.assignDontCare()
    switch(io.config.writeLatency) {
      for (i <- 0 until cp.writeLatencies.size) {
        is(i) {
          history.valid := writeHistory(cp.writeLatencies(i)).valid
          history.sel := writeHistory(cp.writeLatencies(i) + pl.writeDelay).sel
        }
      }
    }
    io.phy.writeEnable := history.valid
    io.writeDatas.foreach(_.ready := False)
    io.writeDatas(history.sel).ready := history.valid
    assert(io.writeDatas.map(p => !(!p.valid && p.ready)).andR, "SDRAM write data stream starved !")

    val payload = io.writeDatas.map(_.payload).read(history.sel)
    for ((phase, dq, dm) <- (io.phy.phases, payload.data.subdivideIn(pl.phaseCount slices), payload.mask.subdivideIn(pl.phaseCount slices)).zipped) {
      phase.DQw := Vec(dq.subdivideIn(pl.dataRate slices))
      phase.DM := Vec((~dm).subdivideIn(pl.dataRate slices))
    }
  }

  case class PipelineCmd() extends Bundle {
    val write = Bool
    val context = Bits(backendContextWidth bits)
    val source = UInt(log2Up(portCount) bits)
  }

  case class PipelineRsp() extends Bundle {
    val data = Bits(pl.beatWidth bits)
    val source = UInt(log2Up(portCount) bits)
    val context = Bits(backendContextWidth bits)
  }

  val rspPipeline = new Area {
    val input = Flow(PipelineCmd())
    assert(cp.readLatencies.min + pl.readDelay >= 1)
    val cmd = input.toStream.queueLowLatency(1 << log2Up((cp.readLatencies.max + pl.readDelay + pl.transferPerBurst-1)/pl.transferPerBurst + 1), latency = 1) //TODO

    val readHistory = History(input.valid && !input.write, 0 until cp.readLatencies.max + pl.beatCount)
    readHistory.tail.foreach(_ init (False))

    io.phy.readEnable.allowOverride
    io.phy.readEnable := False
    switch(io.config.readLatency) {
      for (i <- 0 until cp.readLatencies.size) {
        is(i) {
          io.phy.readEnable := readHistory.drop(cp.readLatencies(i)).take(pl.beatCount).orR
        }
      }
    }

    val beatCounter = Counter(pl.beatCount, io.phy.readValid)

    val output = Flow(Fragment(PipelineRsp()))
    output.valid := cmd.valid && cmd.write || io.phy.readValid
    output.context := cmd.context
    output.source := cmd.source
    output.last := cmd.write || beatCounter.willOverflowIfInc
    cmd.ready := cmd.write || beatCounter.willOverflow

    for ((outputData, phase) <- (output.data.subdivideIn(pl.phaseCount slices), io.phy.phases).zipped) {
      outputData := B(phase.DQr)
    }

    val debugData = RegNextWhen(output.data, output.valid)
  }

  val rspPop = rspPipeline.output.stage()
  for ((output, outputId) <- io.outputs.zipWithIndex) {
    val hit = rspPop.source === outputId
    output.valid := rspPop.valid && hit
    output.last := rspPop.last
    output.data := rspPop.data
    output.context := rspPop.context.resized
  }


  def portEvent(f: CoreTask => Bool) = io.input.ports.map(f).orR

  val muxedCmd = MuxOH(io.input.ports.map(p => p.read || p.write || p.precharge || p.active), io.input.ports)
  writePipeline.input.valid := portEvent(p => p.write)
  writePipeline.input.sel := OHToUInt(io.input.ports.map(p => p.write))

  rspPipeline.input.valid := False
  rspPipeline.input.context := muxedCmd.context
  rspPipeline.input.source := muxedCmd.source
  rspPipeline.input.write.assignDontCare()


  when(io.input.prechargeAll) {
    io.phy.ADDR(10) := True
    command.CSn := False
    command.RASn := False
    command.WEn := False
  }
  when(portEvent(p => p.precharge)) {
    io.phy.ADDR := muxedCmd.address.row.asBits.resized
    io.phy.ADDR(10) := False
    io.phy.BA := muxedCmd.address.bank.asBits
    command.CSn := False
    command.RASn := False
    command.WEn := False
  }
  when(portEvent(p => p.active)) {
    io.phy.ADDR := muxedCmd.address.row.asBits.resized
    io.phy.BA := muxedCmd.address.bank.asBits
    command.CSn := False
    command.RASn := io.config.noActive
  }
  when(portEvent(p => p.write)) {
    io.phy.ADDR := muxedCmd.address.column.asBits.resized
    io.phy.ADDR(10) := False
    io.phy.BA := muxedCmd.address.bank.asBits
    command.CSn := False
    command.CASn := False
    command.WEn := False
    rspPipeline.input.valid := True
    rspPipeline.input.write := True
    if(generation.ODT) odt.start := True
  }
  when(portEvent(p => p.read)) {
    io.phy.ADDR := muxedCmd.address.column.asBits.resized
    io.phy.ADDR(10) := False
    io.phy.BA := muxedCmd.address.bank.asBits
    if(pl.sdram.generation == SdramGeneration.SDR) io.phy.phases.foreach(_.DM.foreach(_.setAll()))
    command.CSn := False
    command.CASn := False
    rspPipeline.input.valid := True
    rspPipeline.input.write := False
  }
  when(io.soft.cmd.valid) {
    io.phy.ADDR := io.soft.cmd.ADDR
    io.phy.BA := io.soft.cmd.BA
    command.CASn := io.soft.cmd.CASn
    command.CSn := io.soft.cmd.CSn
    command.RASn := io.soft.cmd.RASn
    command.WEn := io.soft.cmd.WEn
  }
}
