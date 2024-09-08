package spinal.lib.memory.sdram.xdr.Dfi.Alignment

import spinal.lib.memory.sdram.xdr.Dfi.Tools.DelayCyc
import spinal.core._
import spinal.lib.memory.sdram.xdr.Dfi.Interface.{DfiConfig, DfiRd, DfiRdCs, DfiReadInterface, DfiTimeConfig, Dfirddata, Dfireadcs}
import spinal.lib._

case class RdAlignment(config: DfiConfig) extends Component {
  import config._
  val io = new Bundle {
    val phaseclear = in Bool()
    val rdcs = useRddataCsN generate Vec(slave(Flow(Dfireadcs(config))), config.frequencyRatio)
    val rd = in Vec(DfiRd(config), config.frequencyRatio)
    val rdCs = useRddataCsN generate Vec(out(DfiRdCs(config)), config.frequencyRatio)
    val rddata = Vec(master(Stream(Fragment(Dfirddata(config)))), config.frequencyRatio)
    //    val phyrd  = slave(DfiReadInterface(config))
  }


  val validCnt = Vec(Reg(UInt(widthOf(U(timeConfig.dfiRWLength)) bits)), frequencyRatio)
  val rddataTemp = Vec(Stream(Fragment(Dfirddata(config))), config.frequencyRatio)
  validCnt.foreach(_.init(0))
  rddataTemp.foreach(_.last.clear())
  for (i <- 0 until (frequencyRatio)) {
    rddataTemp(i).valid := io.rd(i).rddatavalid
    rddataTemp(i).rddata.assignDontCare()
    rddataTemp(i).last.setWhen(validCnt(i) === timeConfig.dfiRWLength - 1 & io.rd(i).rddatavalid)
    //    rddataTemp(i).last.noCombLoopCheck()

    when(rddataTemp(i).last) {
      validCnt(i) := 0
    }
    when(rddataTemp(i).fire) {
      validCnt(i) := validCnt(i) + 1
    }
  }

  val delay = DelayCyc(config, timeConfig)
  //  val lastphase  = Reg(UInt(log2Up(frequencyRatio) bits))init((1<<log2Up(frequencyRatio)) - 1)
  val curphase = Reg(UInt(log2Up(frequencyRatio) bits)) init (0)
  val rddataphase = Reg(UInt(log2Up(frequencyRatio)+1 bits)) init (0)
  //  val rddataphase = UInt(log2Up(frequencyRatio) bits)
  rddataphase.clearAll()
  when(io.phaseclear) {
    curphase := U(0)
  }

  //  when(io.rd.map(_.rddatavalid).orR.rise()) {
  //    lastphase := delay.findlp(io.rd.map(_.rddatavalid))
  //  }


  //  switch(curphase){
  //    for(c <- 0 until(frequencyRatio)){
  //      is(c){
  //        switch(rddataphase){
  //          for(r <- 0 until(frequencyRatio)){
  //            is(r){
  //              for(i <- 0 until(frequencyRatio) if c <= i){
  //                when(rddataTemp(i).fire){
  //                  rddataTemp((r + i - c)%frequencyRatio).rddata := io.rd(i).rddata
  //                  if(useRddataDbiN)rddataTemp((r + i - c)%frequencyRatio).rddataDbiN := io.rd(i).rddataDbiN
  //                  if(useRddataDnv)rddataTemp((r + i - c)%frequencyRatio).rddataDnv := io.rd(i).rddataDnv
  //                  curphase := (i + 1)%frequencyRatio
  //                  rddataphase := (r + i + 1 - c)%frequencyRatio
  //                }
  //              }
  //              for(i <- 0 until(frequencyRatio) if c > i){
  //                when(rddataTemp(i).fire){
  //                  rddataTemp((r + frequencyRatio - c + i)%frequencyRatio).rddata := io.rd(i).rddata
  //                  if(useRddataDbiN)rddataTemp((r + frequencyRatio - c + i)%frequencyRatio).rddataDbiN := io.rd(i).rddataDbiN
  //                  if(useRddataDnv)rddataTemp((r + frequencyRatio - c + i)%frequencyRatio).rddataDnv := io.rd(i).rddataDnv
  //                  curphase := (i + 1)%frequencyRatio
  //                  rddataphase := (r + i + 1 + frequencyRatio - c)%frequencyRatio
  //                }
  //              }
  //            }
  //          }
  //        }
  //      }
  //    }
  //  }

  for (i <- 0 until (frequencyRatio)) {
    when(rddataTemp(i).fire) {
//      ((rddataphase + frequencyRatio - curphase + i)%frequencyRatio).muxListDc(rddataTemp.zipWithIndex.map(t => (t._2,t._1))).rddata := io.rd(i).rddata
      rddataTemp(i).rddata := ((i + curphase + frequencyRatio - rddataphase)%frequencyRatio).muxListDc(io.rd.zipWithIndex.map(t => (t._2,t._1))).rddata
      curphase := (i + 1) % frequencyRatio
      rddataphase := (rddataphase + i + 1 + frequencyRatio - curphase) % frequencyRatio
//      rddataTemp((r + frequencyRatio - c + i) % frequencyRatio).rddata := io.rd(i).rddata
//      if (useRddataDbiN) rddataTemp((r + frequencyRatio - c + i) % frequencyRatio).rddataDbiN := io.rd(i).rddataDbiN
//      if (useRddataDnv) rddataTemp((r + frequencyRatio - c + i) % frequencyRatio).rddataDnv := io.rd(i).rddataDnv
//      curphase := (i + 1) % frequencyRatio
//      rddataphase := (r + i + 1 + frequencyRatio - c) % frequencyRatio
    }
  }


  val rddatadelay = Vec(Reg(UInt(log2Up(timeConfig.dfiRWLength)+1 bits)),frequencyRatio)
  val rddateHistory = History(rddataTemp,0 to timeConfig.dfiRWLength)
  val rddatadelayRst = Vec(Vec(Bool(),timeConfig.dfiRWLength),frequencyRatio)
  val rddatadelayCnt = Vec(Vec(Bool(),timeConfig.dfiRWLength),frequencyRatio)

  rddatadelay.map(_.init(timeConfig.dfiRWLength))
  for(i <- 0 until(frequencyRatio)){
//    when(rddataTemp(i).fire){
//      rddatadelay(i) := rddatadelay(i)
//    }otherwise{
//    rddatadelay(i).setAll()
    rddatadelayRst(i) := History(rddataTemp(i).last.fall() & !rddataTemp(i).valid,0 until timeConfig.dfiRWLength)
    when(rddatadelayRst(i)(timeConfig.dfiRWLength-1)){
//      switch(lastphase){
//        for(j <- 0 until(frequencyRatio)){
//          is(j){
//            if(i <= j){
//              rddatadelay(i).setAll()
//            }else{
              rddatadelay(i) := timeConfig.dfiRWLength
//            }
//          }
//        }
//      }
    }
    rddatadelayCnt(i) := History(if(frequencyRatio == 1)rddataTemp(i).fire.fall() & RegNext(!rddataTemp(i).last) else !rddataTemp(i).fire & rddataTemp.map(_.valid).orR & RegNext(!rddataTemp(i).last),0 until timeConfig.dfiRWLength)
//    rddatadelayCnt(i) := History(rddataTemp(i).fire.fall() & RegNext(!rddataTemp(i).last),0 until timeConfig.dfiRWLength)
    when(rddatadelayCnt(i)(timeConfig.dfiRWLength-1) & rddataTemp(i).ready){
      when(rddatadelay(i) =/= 0){
        rddatadelay(i) := rddatadelay(i) - 1
      }otherwise{
        rddatadelay(i) := rddatadelay(i)
      }
    }
//    }
  }
  for(i <- 0 until(frequencyRatio)){
//    switch(rddatadelay(i)){
//      for(j <- 0 until(timeConfig.dfiRWLength)){
//        is(j){
//          when(rddateHistory(j)(i).valid){
            rddataTemp(i).ready := io.rddata(i).ready
            io.rddata(i) << rddatadelay(i).muxListDc(rddateHistory.zipWithIndex.map(t => (t._2,t._1)))(i)
//          }
//          io.rddata(i).valid := rddatadelay(i).muxListDc(rddateHistory.zipWithIndex.map(t => (t._2,t._1)))(i).valid
//          io.rddata(i).last := rddateHistory(j)(i).last
//          io.rddata(i).payload := rddateHistory(j)(i).payload
//          rddateHistory(j)(i).ready := io.rddata(i).ready
//        }
//      }
//    }
  }
  if(useRddataCsN){
    for(i <- 0 until(frequencyRatio)){
      io.rdCs(i).rddataCsN.setAll()
      when(io.rdcs(i).valid){
        io.rdCs(i).rddataCsN := io.rdcs(i).rdcs
      }
    }
  }

}
