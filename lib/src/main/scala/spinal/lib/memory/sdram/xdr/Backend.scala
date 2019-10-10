package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._

case class Backend(cpa: CoreParameterAggregate) extends Component {

  import cpa._


  val io = new Bundle {
    val config = in(CoreConfig(cpa))
    val input = slave(CoreTasks(cpa))
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
  io.phy.DQe := False
  for ((phase, id) <- io.phy.phases.zipWithIndex) {
    if (generation.RESETn) phase.RESETn := io.soft.RESETn

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

  val odt = new Area {
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
      val data = Bits(pl.beatWidth bits)
      val mask = Bits(pl.beatWidth / 8 bits)
    }

    val input = Flow(Cmd())
    val histories = History(input, 0 to cp.writeLatencies.max)
    histories.tail.foreach(_.valid init (False))

    //    if(pl.withDqs) ???
    switch(io.config.writeLatency) {
      for (i <- 0 until cp.writeLatencies.size) {
        is(i) {
          val latency = cp.writeLatencies(i)
          val history = histories(latency)
          when(history.valid) {
            io.phy.DQe := True
          }
          for ((phase, dq, dm) <- (io.phy.phases, history.data.subdivideIn(pl.phaseCount slices), history.mask.subdivideIn(pl.phaseCount slices)).zipped) {
            phase.DQw := Vec(dq.subdivideIn(pl.dataRatio slices))
            phase.DM := Vec((~dm).subdivideIn(pl.dataRatio slices))
          }
        }
      }
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
    val input = Flow(Fragment(PipelineCmd()))

    val histories = History(input, 0 to cp.readLatencies.max + pl.outputLatency + pl.inputLatency - 1)
    histories.tail.foreach(_.valid init (False))

    val output = Flow(Fragment(PipelineRsp()))
    output.valid := False
    output.payload.assignDontCare()
    switch(io.config.readLatency) {
      for (i <- 0 until cp.readLatencies.size) {
        is(i) {
          val history = histories(cp.readLatencies(i) + pl.outputLatency + pl.inputLatency - 1)
          when(history.valid) {
            output.valid := True
            output.context := history.context
            output.source := history.source
            output.last := history.last
          }
        }
      }
    }


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

  val muxedPort = MuxOH(io.input.ports.map(p => p.read || p.write || p.precharge || p.active), io.input.ports)
  writePipeline.input.valid := portEvent(p => p.write)
  writePipeline.input.data := muxedPort.data
  writePipeline.input.mask := muxedPort.mask

  rspPipeline.input.valid := False
  rspPipeline.input.context := muxedPort.context
  rspPipeline.input.source := muxedPort.source
  rspPipeline.input.last.assignDontCare()
  rspPipeline.input.write.assignDontCare()

  when(io.input.prechargeAll) {
    command.CSn := False
    command.RASn := False
    command.CASn := False
  }
  when(io.input.prechargeAll) {
    io.phy.ADDR(10) := True
    command.CSn := False
    command.RASn := False
    command.WEn := False
  }
  when(portEvent(p => p.precharge)) {
    io.phy.ADDR := muxedPort.address.row.asBits.resized
    io.phy.ADDR(10) := False
    io.phy.BA := muxedPort.address.bank.asBits
    command.CSn := False
    command.RASn := False
    command.WEn := False
  }

  when(portEvent(p => p.active)) {
    io.phy.ADDR := muxedPort.address.row.asBits.resized
    io.phy.BA := muxedPort.address.bank.asBits
    command.CSn := False
    command.RASn := io.config.noActive
  }

  val portFirst = RegInit(True)
  when(portEvent(p => p.write)) {
    io.phy.ADDR := muxedPort.address.column.asBits.resized
    io.phy.ADDR(10) := False
    io.phy.BA := muxedPort.address.bank.asBits
    writePipeline.input.mask := muxedPort.mask
    when(portFirst) {
      command.CSn := False
      command.CASn := False
      command.WEn := False
      rspPipeline.input.valid := True
      rspPipeline.input.last := True
      rspPipeline.input.write := True
      odt.start := True
    }
    portFirst := muxedPort.last
  }

  when(portEvent(p => p.read)) {
    io.phy.ADDR := muxedPort.address.column.asBits.resized
    io.phy.ADDR(10) := False
    io.phy.BA := muxedPort.address.bank.asBits
    writePipeline.input.mask.setAll()
    when(portFirst) {
      command.CSn := False
      command.CASn := False
    }
    rspPipeline.input.valid := True
    rspPipeline.input.last := muxedPort.last
    rspPipeline.input.write := False
    portFirst := muxedPort.last
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
