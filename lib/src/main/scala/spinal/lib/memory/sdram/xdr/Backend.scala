package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.SdramGeneration

case class Backend(cpa: CoreParameterAggregate) extends Component {
  import cpa._


  val io = new Bundle {
    val config = in(CoreConfig(cpa))
    val input = slave(CoreTasks(cpa))
    val writeDatas = Vec(cpa.cpp.filter(_.canWrite).map(cpp => slave(Stream(CoreWriteData(cpp, cpa)))))
    val phy = master(SdramXdrPhyCtrl(pl))
    val outputs = Vec(cpa.cpp.map(cpp => master(Flow(Fragment(CoreRsp(cpp, cpa))))))
    val soft = slave(SoftBus(cpa))
  }

  io.phy.ADDR.assignDontCare()
  io.phy.BA.assignDontCare()
  for ((phase, id) <- io.phy.phases.zipWithIndex) {
    if (generation.RESETn) phase.RESETn := io.soft.RESETn

    phase.CSn := False
    phase.RASn := True
    phase.CASn := True
    phase.WEn := True
    phase.CKE := io.soft.CKE
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
      phase.ODT setWhen(start && id >= io.config.phase.write)
      phase.ODT setWhen(counter > 1)
      phase.ODT setWhen(counter === 1 && io.config.ODTend(id))
    }
  }

  val writePipeline = new Area {
    case class Cmd() extends Bundle {
      val sel = UInt(log2Up(cpp.filter(_.canWrite).length) bits)
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
    assert(io.writeDatas.map(p => !(!p.valid && p.ready)).andR, "SDRAM write data stream starved !", ERROR)

    val payload = io.writeDatas.map(_.payload).read(history.sel)
    for ((phase, dq, dm) <- (io.phy.phases, payload.data.subdivideIn(pl.phaseCount slices), payload.mask.subdivideIn(pl.phaseCount slices)).zipped) {
      phase.DQw := Vec(dq.subdivideIn(pl.dataRate slices))
      when(history.valid) {
        phase.DM := Vec((~dm).subdivideIn(pl.dataRate slices))
      } otherwise {
        phase.DM.foreach(_.clearAll())
      }
    }
  }

  case class PipelineCmd() extends Bundle {
    val write = Bool
    val last = Bool
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
    val cmd = input.toStream.queueLowLatency(1 << log2Up((cp.readLatencies.max + pl.readDelay + pl.beatCount-1)/pl.beatCount + 1), latency = 1) //TODO

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
    output.valid := cmd.valid && cmd.write && cmd.last || io.phy.readValid
    output.context := cmd.context
    output.source := cmd.source
    output.last := cmd.write || beatCounter.willOverflowIfInc && cmd.last
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
    if(output.cpp.canRead) output.data := rspPop.data
    output.context := rspPop.context.resized
  }


  def portEvent(f: CoreTask => Bool) = io.input.ports.map(f).orR

  val muxedCmd = MuxOH(io.input.ports.map(p => p.read || p.write || p.precharge || p.active), io.input.ports)
  writePipeline.input.valid := portEvent(p => p.write)
  val portIdToWriteId = cpp.zipWithIndex.filter(_._1.canWrite).map(_._2).zipWithIndex
  writePipeline.input.sel := muxedCmd.portId.muxListDc(portIdToWriteId.map{case (portId, writeId) => portId -> U(writeId, widthOf(writePipeline.input.sel) bits)})

  rspPipeline.input.valid := False
  rspPipeline.input.last := muxedCmd.last
  rspPipeline.input.context := muxedCmd.context
  rspPipeline.input.source := muxedCmd.portId
  rspPipeline.input.write.assignDontCare()

  val phase = new {
    def precharge = io.phy.phases(io.config.phase.precharge)
    def active = io.phy.phases(io.config.phase.active)
    def read = io.phy.phases(io.config.phase.read)
    def write = io.phy.phases(io.config.phase.write)
  }

  when(io.input.prechargeAll) {
    io.phy.ADDR(10) := True
    phase.precharge.CSn := False
    phase.precharge.RASn := False
    phase.precharge.WEn := False
  }
  when(io.input.refresh){
    phase.active.CSn := False
    phase.active.RASn := False
    phase.active.CASn := False
  }
  when(portEvent(p => p.precharge)) {
    io.phy.ADDR := muxedCmd.address.row.asBits.resized
    io.phy.ADDR(10) := False
    io.phy.BA := muxedCmd.address.bank.asBits
    phase.precharge.CSn := False
    phase.precharge.RASn := False
    phase.precharge.WEn := False
  }
  when(portEvent(p => p.active)) {
    io.phy.ADDR := muxedCmd.address.row.asBits.resized
    io.phy.BA := muxedCmd.address.bank.asBits
    phase.active.CSn := False
    phase.active.RASn := io.config.noActive
  }
  when(portEvent(p => p.write)) {
    io.phy.ADDR := muxedCmd.address.column.asBits.resized
    io.phy.ADDR(10) := False
    io.phy.BA := muxedCmd.address.bank.asBits
    phase.write.CSn := False
    phase.write.CASn := False
    phase.write.WEn := False
    rspPipeline.input.valid := True
    rspPipeline.input.write := True
    if(generation.ODT) odt.start := True
  }
  when(portEvent(p => p.read)) {
    io.phy.ADDR := muxedCmd.address.column.asBits.resized
    io.phy.ADDR(10) := False
    io.phy.BA := muxedCmd.address.bank.asBits
    phase.read.CSn := False
    phase.read.CASn := False
    rspPipeline.input.valid := True
    rspPipeline.input.write := False
  }
  when(io.soft.cmd.valid) {
    io.phy.ADDR := io.soft.cmd.ADDR
    io.phy.BA := io.soft.cmd.BA
    io.phy.phases(0).CASn := io.soft.cmd.CASn
    io.phy.phases(0).CSn := io.soft.cmd.CSn
    io.phy.phases(0).RASn := io.soft.cmd.RASn
    io.phy.phases(0).WEn := io.soft.cmd.WEn
  }
}
