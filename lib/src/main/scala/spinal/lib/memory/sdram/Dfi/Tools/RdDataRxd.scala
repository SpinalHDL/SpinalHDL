package spinal.lib.memory.sdram.Dfi.Tools

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.Dfi.Interface._
case class RdDataRxd(cpa:CoreParameterAggregate) extends Component {
  import cpa._
  import cpa.config._
  val io = new Bundle{
    val task = slave(CoreTasks(cpa))
    val idfiRddata = Vec(slave(Stream(Fragment(Dfirddata(config)))), config.frequencyRatio)
    val rden = out Vec(Bool(),frequencyRatio)
    val coreRddata =  master(Flow(Fragment(CoreRsp(cpp, cpa))))
  }

  case class PipelineCmd() extends Bundle {
    val context = Bits(backendContextWidth bits)
  }

  case class PipelineRsp() extends Bundle {
    val data = Bits(pl.beatWidth bits)
    val context = Bits(backendContextWidth bits)
  }

  val rspPipeline = new Area {
    val input = Flow(Fragment(PipelineCmd()))
    assert(timeConfig.tPhyRdlat + timeConfig.tRddataEn >= 1)
    val cmd = input.toStream.queueLowLatency(1 << log2Up((timeConfig.tPhyRdlat + timeConfig.tRddataEn + pl.beatCount-1)/pl.beatCount + 1), latency = 1) //TODO

    val rdensHistory = Vec(Vec(Bool(),(cmdPhase+timeConfig.tRddataEn)/frequencyRatio+2),frequencyRatio)
    rdensHistory.foreach(_ := History(input.valid,0 to (cmdPhase+timeConfig.tRddataEn)/frequencyRatio+1))
    rdensHistory.foreach(_.tail.foreach(_ init (False)))

    val beatCounter = Counter(pl.beatCount, io.idfiRddata.map(_.valid).orR)

    val delay = DelayCyc(config,timeConfig)
    val delaycyc  = delay.mcdelaycyc(cmdPhase,timeConfig.tRddataEn)
    val nextphase = delay.sp2np(cmdPhase,timeConfig.tRddataEn)

    for(i <- 0 until(frequencyRatio)){
      if(i >= nextphase){
        io.rden(i) := History(rdensHistory(nextphase)(delaycyc),0 until(timeConfig.dfiRWLength)).orR
      }else{
        io.rden(i) := History(rdensHistory(nextphase)(delaycyc + 1),0 until(timeConfig.dfiRWLength)).orR
      }
    }

    val output = Flow(Fragment(PipelineRsp()))
    output.valid.clear()
    output.valid.setWhen(io.idfiRddata.map(_.valid).orR)
    output.context := cmd.context
    output.last := beatCounter.willOverflowIfInc && cmd.last
    cmd.ready := beatCounter.willOverflow

    for ((outputData, phase) <- (output.data.subdivideIn(frequencyRatio slices).reverse, io.idfiRddata).zipped) {
      outputData := B(phase.rddata)
    }
  }

  rspPipeline.input.valid := False
  rspPipeline.input.last := io.task.task.last
  rspPipeline.input.context := io.task.task.context

  val rspPop = rspPipeline.output.stage()
  io.coreRddata.valid := rspPop.valid
  io.coreRddata.last := rspPop.last
  if(io.coreRddata.cpp.canRead) io.coreRddata.data := rspPop.data
  io.coreRddata.context := rspPop.context.resized

  rspPipeline.input.valid.setWhen(io.task.task.read)

  val ready = Vec(Reg(Bool()),frequencyRatio)
  ready.foreach(_.init(False))
    for(i <- 0 until(frequencyRatio)){
      ready(i).setWhen(io.task.task.read).clearWhen(io.task.task.write)
    }
  for((outport,iready) <- (io.idfiRddata,ready).zipped){
    outport.ready := iready
  }
}
