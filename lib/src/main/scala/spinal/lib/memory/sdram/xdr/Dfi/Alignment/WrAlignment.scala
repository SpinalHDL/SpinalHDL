package spinal.lib.memory.sdram.xdr.Dfi.Alignment

import spinal.lib.memory.sdram.xdr.Dfi.Tools.DelayCyc
import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.xdr.Dfi.Interface.{DfiConfig, DfiTimeConfig, DfiWrcs, DfiWrdata, DfiWriteInterface}

case class WrAlignment(config: DfiConfig) extends Component{
  import config._
  val io = new Bundle{
    val wrcs   = useWrdataCsN generate Vec(slave(Flow(DfiWrcs(config))),config.frequencyRatio)
    val wrdata = Vec(slave(Flow(DfiWrdata(config))),config.frequencyRatio)
    val output = master(DfiWriteInterface(config))
  }
  for(i <- 0 until(frequencyRatio)){
    io.output.wr(i).wrdataen := io.wrdata(i).valid
  }
  val delay = DelayCyc(config,timeConfig)
  val delaycyc  = Reg(delay.mcdelaycyc(delay.findsp(io.wrdata.map(_.valid)),timeConfig.tPhyWrData)<<1)

  when(io.wrdata.map(_.valid).orR.rise()){
    delaycyc := delay.mcdelaycyc(delay.findsp(io.wrdata.map(_.valid)),timeConfig.tPhyWrData)
  }

val wrdatahistary = Vec(Vec(DfiWrdata(config),timeConfig.tPhyWrData/frequencyRatio+2),frequencyRatio)
  for(i <- 0 until(frequencyRatio)){
    wrdatahistary(i) := History(io.wrdata(i).payload,0 to timeConfig.tPhyWrData/frequencyRatio+1)
    io.output.wr(i).wrdata.assignDontCare()
    io.output.wr(i).wrdataMask.assignDontCare()
  }

  for(k <- 0 until(frequencyRatio)){
    if(k < timeConfig.tPhyWrData%frequencyRatio){
      io.output.wr(k).wrdata := (delaycyc + 1).muxListDc(wrdatahistary.shuffle(t => (t+timeConfig.tPhyWrData%frequencyRatio)%frequencyRatio)(k).zipWithIndex.map(t =>(t._2,t._1))).wrdata
      io.output.wr(k).wrdataMask := (delaycyc + 1).muxListDc(wrdatahistary.shuffle(t => (t+timeConfig.tPhyWrData%frequencyRatio)%frequencyRatio)(k).zipWithIndex.map(t =>(t._2,t._1))).wrdataMask
    }else{
      io.output.wr(k).wrdata := (delaycyc).muxListDc(wrdatahistary.shuffle(t => (t+timeConfig.tPhyWrData%frequencyRatio)%frequencyRatio)(k).zipWithIndex.map(t =>(t._2,t._1))).wrdata
      io.output.wr(k).wrdataMask := (delaycyc).muxListDc(wrdatahistary.shuffle(t => (t+timeConfig.tPhyWrData%frequencyRatio)%frequencyRatio)(k).zipWithIndex.map(t =>(t._2,t._1))).wrdataMask
    }
  }

  if(useWrdataCsN){
    for(i <- 0 until(frequencyRatio)){
      io.output.wr(i).wrdataCsN.setAll()
      when(io.wrcs(i).valid){
        io.output.wr(i).wrdataCsN := io.wrcs(i).wrcs
      }
    }
  }
}
