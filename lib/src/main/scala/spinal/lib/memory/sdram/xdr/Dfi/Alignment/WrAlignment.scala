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
//  val startphase = Reg(U(0,log2Up(frequencyRatio) bits))
//  val nextphase = Reg(U(0,log2Up(frequencyRatio)+1 bits))
  val delaycyc  = Reg(delay.mcdelaycyc(delay.findsp(io.wrdata.map(_.valid)),timeConfig.tPhyWrData)<<1)
//  val delaycyc  = Reg(UInt(log2Up(timeConfig.tPhyWrData/frequencyRatio) + 2 bits))
//  when(io.wrdata.map(_.valid).orR.rise()){
//    val startphase = delay.findsp(Vec(io.wrdata.map(_.valid)))
//    val lastphase = delay.sp2lp(startphase,timeConfig.tPhyWrData)
//    val mcdelaycyc = delay.mcdelaycyc(startphase,timeConfig.tPhyWrData)
//  }
  when(io.wrdata.map(_.valid).orR.rise()){
//    startphase := delay.findsp(io.wrdata.map(_.valid))
//    nextphase := delay.sp2np(delay.findsp(io.wrdata.map(_.valid)),timeConfig.tPhyWrData)
    delaycyc := delay.mcdelaycyc(delay.findsp(io.wrdata.map(_.valid)),timeConfig.tPhyWrData)
//    nextphase := (OHToUInt(OHMasking.last(B(io.wrdata.map(_.valid))).asBools) + timeConfig.tPhyWrData)%frequencyRatio
//    delaycyc := (OHToUInt(OHMasking.last(B(io.wrdata.map(_.valid))).asBools) + timeConfig.tPhyWrData)/frequencyRatio
  }
//  val wrdatahistary = Vec(Vec(cloneOf(io.wrdata(0).payload),timeConfig.tPhyWrData/frequencyRatio+1),frequencyRatio)
//  for(i <- 0 until(frequencyRatio)){
//    wrdatahistary(i) := History(io.wrdata(i).payload,0 to timeConfig.tPhyWrData/frequencyRatio+1)
//  }
val wrdatahistary = Vec(Vec(DfiWrdata(config),timeConfig.tPhyWrData/frequencyRatio+2),frequencyRatio)
  for(i <- 0 until(frequencyRatio)){
    wrdatahistary(i) := History(io.wrdata(i).payload,0 to timeConfig.tPhyWrData/frequencyRatio+1)
    io.output.wr(i).wrdata.assignDontCare()
    io.output.wr(i).wrdataMask.assignDontCare()
  }
//  for(i <- 0 until(frequencyRatio)){
//    when(i<nextphase){
////      io.output.wr(i).wrdata := wrdatahistary((i+frequencyRatio-nextphase)%frequencyRatio)(delaycyc+1).wrdata
//      io.output.wr(i).wrdata := wrdatahistary.shuffle(t => (t+frequencyRatio-nextphase)%frequencyRatio))(i)(delaycyc+1).wrdata
//      io.output.wr(i).wrdataMask := wrdatahistary((i+frequencyRatio-nextphase)%frequencyRatio)(delaycyc+1).wrdataMask
//    }otherwise{
//      io.output.wr(i).wrdata := wrdatahistary((i+frequencyRatio-nextphase)%frequencyRatio)(delaycyc).wrdata
//      io.output.wr(i).wrdataMask := wrdatahistary((i+frequencyRatio-nextphase)%frequencyRatio)(delaycyc).wrdataMask
//    }

//  }
//  for(i <- 0 until(frequencyRatio)){
//    io.output.wr(i).wrdata.assignDontCare()
//    io.output.wr(i).wrdataMask.assignDontCare()
//  }
//
//  switch(delaycyc){
//    for(j <- 0 to timeConfig.tPhyWrData/frequencyRatio){
//      is(j){
////        switch(nextphase){
////          for(i <- 0 until(frequencyRatio)){
////            is(i){
////              switch(startphase){
////                for(s <- 0 until(frequencyRatio)){
////                  is(s){
////                    for(k <- 0 until(frequencyRatio)){
//////                      if(k < i){
//////                        if(k < s){
//////                          io.output.wr(k).wrdata := wrdatahistary.shuffle(t => (t+frequencyRatio-i)%frequencyRatio)(k)(j).wrdata
//////                          io.output.wr(k).wrdataMask := wrdatahistary.shuffle(t => (t+frequencyRatio-i)%frequencyRatio)(k)(j).wrdataMask
//////                        }else{
//////                          io.output.wr(k).wrdata := wrdatahistary.shuffle(t => (t+frequencyRatio-i)%frequencyRatio)(k)(j+1).wrdata
//////                          io.output.wr(k).wrdataMask := wrdatahistary.shuffle(t => (t+frequencyRatio-i)%frequencyRatio)(k)(j+1).wrdataMask
//////                        }
//////                      }else{
//////                        io.output.wr(k).wrdata := wrdatahistary.shuffle(t => (t+frequencyRatio-i)%frequencyRatio)(k)(j).wrdata
//////                        io.output.wr(k).wrdataMask := wrdatahistary.shuffle(t => (t+frequencyRatio-i)%frequencyRatio)(k)(j).wrdataMask
//////                      }
////                      if(k < (i + frequencyRatio - s)%frequencyRatio){
////                        io.output.wr(k).wrdata := wrdatahistary.shuffle(t => (t+(i + frequencyRatio - s)%frequencyRatio)%frequencyRatio)(k)(j+1).wrdata
////                        io.output.wr(k).wrdataMask := wrdatahistary.shuffle(t => (t+(i + frequencyRatio - s)%frequencyRatio)%frequencyRatio)(k)(j+1).wrdataMask
////                      }else{
////                        io.output.wr(k).wrdata := wrdatahistary.shuffle(t => (t+(i + frequencyRatio - s)%frequencyRatio)%frequencyRatio)(k)(j).wrdata
////                        io.output.wr(k).wrdataMask := wrdatahistary.shuffle(t => (t+(i + frequencyRatio - s)%frequencyRatio)%frequencyRatio)(k)(j).wrdataMask
////                      }
////                    }
////                  }
////                }
////              }
////            }
////          }
////        }
//        for(k <- 0 until(frequencyRatio)){
//          if(k < timeConfig.tPhyWrData%frequencyRatio){
//            io.output.wr(k).wrdata := wrdatahistary.shuffle(t => (t+timeConfig.tPhyWrData%frequencyRatio)%frequencyRatio)(k)(j+1).wrdata
//            io.output.wr(k).wrdataMask := wrdatahistary.shuffle(t => (t+timeConfig.tPhyWrData%frequencyRatio)%frequencyRatio)(k)(j+1).wrdataMask
//          }else{
//            io.output.wr(k).wrdata := wrdatahistary.shuffle(t => (t+timeConfig.tPhyWrData%frequencyRatio)%frequencyRatio)(k)(j).wrdata
//            io.output.wr(k).wrdataMask := wrdatahistary.shuffle(t => (t+timeConfig.tPhyWrData%frequencyRatio)%frequencyRatio)(k)(j).wrdataMask
//          }
//        }
//      }
//    }
//  }

  for(k <- 0 until(frequencyRatio)){
    if(k < timeConfig.tPhyWrData%frequencyRatio){
      io.output.wr(k).wrdata := (delaycyc + 1).muxListDc(wrdatahistary.shuffle(t => (t+timeConfig.tPhyWrData%frequencyRatio)%frequencyRatio)(k).zipWithIndex.map(t =>(t._2,t._1))).wrdata
      io.output.wr(k).wrdataMask := (delaycyc + 1).muxListDc(wrdatahistary.shuffle(t => (t+timeConfig.tPhyWrData%frequencyRatio)%frequencyRatio)(k).zipWithIndex.map(t =>(t._2,t._1))).wrdataMask
    }else{
      io.output.wr(k).wrdata := (delaycyc).muxListDc(wrdatahistary.shuffle(t => (t+timeConfig.tPhyWrData%frequencyRatio)%frequencyRatio)(k).zipWithIndex.map(t =>(t._2,t._1))).wrdata
      io.output.wr(k).wrdataMask := (delaycyc).muxListDc(wrdatahistary.shuffle(t => (t+timeConfig.tPhyWrData%frequencyRatio)%frequencyRatio)(k).zipWithIndex.map(t =>(t._2,t._1))).wrdataMask
    }
  }
//  switch(nextphase){
//    for(i <- 0 until(frequencyRatio)){
//      is(i){
////        for(j <- 0 until(frequencyRatio) if delay == j){
//            for(k <- 0 until(frequencyRatio)){
//              if(k < i){
//                io.output.wr(k).wrdata := wrdatahistary.shuffle(t => (t+frequencyRatio-i)%frequencyRatio)(k)(1).wrdata
//                io.output.wr(k).wrdataMask := wrdatahistary.shuffle(t => (t+frequencyRatio-i)%frequencyRatio)(k)(1).wrdataMask
//              }else{
//                io.output.wr(k).wrdata := wrdatahistary.shuffle(t => (t+frequencyRatio-i)%frequencyRatio)(k)(0).wrdata
//                io.output.wr(k).wrdataMask := wrdatahistary.shuffle(t => (t+frequencyRatio-i)%frequencyRatio)(k)(0).wrdataMask
//              }
//            }
////        }
//      }
//    }
//  }
  if(useWrdataCsN){
    for(i <- 0 until(frequencyRatio)){
      io.output.wr(i).wrdataCsN.setAll()
      when(io.wrcs(i).valid){
        io.output.wr(i).wrdataCsN := io.wrcs(i).wrcs
      }
    }
  }
}
