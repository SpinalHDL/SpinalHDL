package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib.memory.sdram.dfi.Interface.DDR.DDRConfig
import spinal.lib._
import spinal.lib.memory.sdram.dfi.Interface.{TaskParameterAggregate, TaskRsp, OpTasks, TaskWriteData, DfiAddr, DfiCmd, DfiConfig, DfiWrData, DfiRdData}

case class CmdTxd(tpa:TaskParameterAggregate) extends Component{
  import tpa._
  val io = new Bundle{
    val task    = slave(OpTasks(tpa))
    val cmd     = Vec(master(Flow(DfiCmd(config))),config.frequencyRatio)
    val address = Vec(master(Flow(DfiAddr(config))),config.frequencyRatio)
  }
  def cmdphase(i:Int) = io.cmd(i)
  def addrphase(i:Int) = io.address(i)

  def ACTIVE:Bits    = (~(B(1) << io.task.task.address.cs).resize(config.chipSelectNumber) ## B"b011").setName("ACTIVE")
  def WRITE:Bits     = (~(B(1) << io.task.task.address.cs).resize(config.chipSelectNumber) ## B"b100").setName("WRITE")
  def READ:Bits      = (~(B(1) << io.task.task.address.cs).resize(config.chipSelectNumber) ## B"b101").setName("READ")
  def PRECHARGE:Bits = (~(B(1) << io.task.task.address.cs).resize(config.chipSelectNumber) ## B"b010")
  def REFRESH:Bits   = (~(B(1) << io.task.task.address.cs).resize(config.chipSelectNumber) ## B"b001").setName("REFRESH")

  def AUTO_PRECHARGE_BIT = 10  // Disable auto precharge (auto close of row)
  def ALL_BANKS_BIT =10        // Precharge all banks
  def COULMNRang = 0 until(tpa.pl.sdram.columnWidth)
  def BANK:Bits   = io.task.task.address.bank.asBits
  def ROW:Bits    =  io.task.task.address.row.asBits
  def COLUMN:Bits    =  io.task.task.address.column.asBits

  def active = io.task.task.active
  def write = io.task.task.write
  def read  = io.task.task.read
  def precharge  = io.task.task.precharge
  def prechargeAll = io.task.prechargeAll
  def refresh = io.task.refresh

  io.cmd.foreach(_.valid.clear())
  io.cmd.foreach(_.payload.setAll())
  io.address.foreach(_.valid.clear())
  io.address.foreach(_.address.clearAll())
  io.address.foreach(_.bank.clearAll())

  when(active){
    cmdphase(config.cmdPhase).valid.set()
    cmdphase(config.cmdPhase).payload.assignFromBits(ACTIVE)
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).bank := BANK.resized
    addrphase(config.cmdPhase).address := ROW.resized
  }
  when(write){
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(WRITE)
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).bank := BANK.resized
    addrphase(config.cmdPhase).address(COULMNRang) := COLUMN
    addrphase(config.cmdPhase).address(AUTO_PRECHARGE_BIT).clear()
  }
  when(read){
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(READ)
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).bank := BANK.resized
    addrphase(config.cmdPhase).address(COULMNRang) := COLUMN
    addrphase(config.cmdPhase).address(AUTO_PRECHARGE_BIT).clear()
  }
  when(precharge){
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(PRECHARGE.setName("PRECHARGE"))
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).bank := BANK.resized
    addrphase(config.cmdPhase).address(ALL_BANKS_BIT).clear()
  }
  when(prechargeAll){
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(PRECHARGE.setName("PRECHARGEALL"))
    addrphase(config.cmdPhase).valid.set()
    addrphase(config.cmdPhase).address(ALL_BANKS_BIT).set()

  }
  when(refresh){
    cmdphase(config.cmdPhase).valid := True
    cmdphase(config.cmdPhase).payload.assignFromBits(REFRESH)
    addrphase(config.cmdPhase).valid.set()
  }
}






case class RdDataRxd(tpa:TaskParameterAggregate) extends Component {
  import tpa._
  import tpa.config._
  val io = new Bundle{
    val task = slave(OpTasks(tpa))
    val idfiRddata = Vec(slave(Stream(Fragment(DfiRdData(config)))), config.frequencyRatio)
    val rden = out Vec(Bool(),frequencyRatio)
    val coreRddata =  master(Flow(Fragment(TaskRsp(tpp, tpa))))
  }


  case class PipelineRsp() extends Bundle {
    val data = Bits(pl.beatWidth bits)
    val context = Bits(backendContextWidth bits)
  }

  case class Context() extends Bundle{
    val context = Bits(backendContextWidth bits)
  }

  val rspPipeline = new Area {
    val input = Flow(Fragment(Context()))
    assert(timeConfig.tPhyRdlat + timeConfig.tRddataEn >= 1)
    val cmd = input.toStream.queueLowLatency(1 << log2Up((timeConfig.tPhyRdlat + timeConfig.tRddataEn + pl.beatCount-1)/pl.beatCount + 1), latency = 1)

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
  if(io.coreRddata.tpp.canRead) io.coreRddata.data := rspPop.data
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






case class WrDataTxd(tpa:TaskParameterAggregate) extends Component{
  import tpa._
  import tpa.config._
  val io = new Bundle{
    val write = in Bool()
    val coreWrdata = slave(Stream(TaskWriteData(tpp, tpa)))
    val idfiWrdata = Vec(master(Flow(DfiWrData(config))),config.frequencyRatio)
  }
  def wrdataPhase(i:Int) = io.idfiWrdata(i)


  val delay = DelayCyc(config,timeConfig)

  val delaycyc  = delay.mcdelaycyc(cmdPhase,timeConfig.tPhyWrLat)
  val nextphase = delay.sp2np(cmdPhase,timeConfig.tPhyWrLat)

  val writeHistory = History(io.write,0 until  timeConfig.dfiRWLength)
  val write = writeHistory.orR

  val wrens = Vec(Bool(),frequencyRatio)
  val wrensHistory = Vec(Vec(Bool(),(cmdPhase+timeConfig.tPhyWrLat)/frequencyRatio+2),frequencyRatio)
  for(i <- 0 until(frequencyRatio)){
    wrensHistory(i) := History(wrens(i),0 to (cmdPhase+timeConfig.tPhyWrLat)/frequencyRatio+1)
    wrensHistory(i).tail.foreach(_.init(False))
  }
  wrens.foreach(_.clear())
  wrens.foreach(_.setWhen(write))
  io.coreWrdata.ready.clear()
  io.coreWrdata.ready.setWhen(wrensHistory(nextphase)(delaycyc))
  assert(!(!io.coreWrdata.valid && io.coreWrdata.ready), "SDRAM write data stream starved !", ERROR)
  for(i <- 0 until(frequencyRatio)){
    if(i>= nextphase){
      wrdataPhase(i).valid := wrensHistory(nextphase)(delaycyc)
      wrdataPhase(i).wrData := Vec(io.coreWrdata.payload.data.subdivideIn(frequencyRatio slices).reverse).shuffle(t=>(t+nextphase)%frequencyRatio)(i)
      wrdataPhase(i).wrDataMask := Vec(io.coreWrdata.payload.mask.subdivideIn(frequencyRatio slices).reverse).shuffle(t=>(t+nextphase)%frequencyRatio)(i)
    }else{
      wrdataPhase(i).valid := wrensHistory(nextphase)(delaycyc+1)
      wrdataPhase(i).wrData := RegNext(Vec(io.coreWrdata.payload.data.subdivideIn(frequencyRatio slices).reverse).shuffle(t=>(t+nextphase)%frequencyRatio)(i))
      wrdataPhase(i).wrDataMask := RegNext(Vec(io.coreWrdata.payload.mask.subdivideIn(frequencyRatio slices).reverse).shuffle(t=>(t+nextphase)%frequencyRatio)(i))
    }
  }
}
