package spinal.lib.memory.sdram.xdr.Dfi.Tools

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.xdr.Dfi.Interface.{CoreWriteData, CoreParameterAggregate, CoreTasks, DfiAddr, DfiCmd, DfiConfig, DfiTimeConfig, DfiWrdata}
//import spinal.lib.memory.sdram.xdr.CoreWriteData
case class WrDataTxd(cpa:CoreParameterAggregate) extends Component{
  import cpa._
  import cpa.config._
  val io = new Bundle{
    val write = in Bool()
    val coreWrdata = slave(Stream(CoreWriteData(cpp, cpa)))
    val idfiWrdata = Vec(master(Flow(DfiWrdata(config))),config.frequencyRatio)
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
      wrdataPhase(i).wrdata := Vec(io.coreWrdata.payload.data.subdivideIn(frequencyRatio slices).reverse).shuffle(t=>(t+nextphase)%frequencyRatio)(i)
      wrdataPhase(i).wrdataMask := Vec(io.coreWrdata.payload.mask.subdivideIn(frequencyRatio slices).reverse).shuffle(t=>(t+nextphase)%frequencyRatio)(i)
    }else{
      wrdataPhase(i).valid := wrensHistory(nextphase)(delaycyc+1)
      wrdataPhase(i).wrdata := RegNext(Vec(io.coreWrdata.payload.data.subdivideIn(frequencyRatio slices).reverse).shuffle(t=>(t+nextphase)%frequencyRatio)(i))
      wrdataPhase(i).wrdataMask := RegNext(Vec(io.coreWrdata.payload.mask.subdivideIn(frequencyRatio slices).reverse).shuffle(t=>(t+nextphase)%frequencyRatio)(i))
    }
  }
}
