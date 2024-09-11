package spinal.lib.memory.sdram.Dfi

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.Dfi.Interface._

case class CAAlignment(config: DfiConfig) extends Component{
  import config._
  val io = new Bundle{
    val cmd     = Vec(slave(Flow(DfiCmd(config))),config.frequencyRatio)
    val address = Vec(slave(Flow(DfiAddr(config))),config.frequencyRatio)
    val ckeN    = in Vec(Bits(config.chipSelectNumber bits),config.frequencyRatio)
    val output  = master(DfiControlInterface(config))
  }
  //cke,reserN,odt
  //Most DRAMs define CKE as low at reset; some devices, such as LPDDR1, LPDDR2 and LPDDR3, define CKE as high at
  // reset. The default value should adhere to the DRAM definition.
  io.output.cke := io.ckeN.asBits
  //In general, the dfi_reset_n signal is defined as low at reset; however, in some cases it may be necessary
  // to hold dfi_reset_n high during initialization.
  if(useResetN)io.output.resetN.setAll()
  //The MC generates dfi_odt (dfi_odt_pN in frequency ratio systems) based on the DRAM burst length including
  //CRC data. With CRC enabled, the MC may need to extend ODT.
  if(useOdt)io.output.odt.clearAll()

  //cmd
  if(useAckN)io.output.actN := Bits(widthOf(io.output.actN) bits).setAll()
  io.output.csN  := Bits(widthOf(io.output.csN) bits).setAll()
  io.output.rasN := Bits(widthOf(io.output.rasN) bits).setAll()
  io.output.casN := Bits(widthOf(io.output.casN) bits).setAll()
  io.output.weN  := Bits(widthOf(io.output.weN) bits).setAll()
  if(useCid)io.output.cid  := Bits(widthOf(io.output.cid) bits).clearAll()

  //address
  if(config.useBg)io.output.bg      := Bits(widthOf(io.output.bg) bits).assignDontCare()
  io.output.bank    := Bits(widthOf(io.output.bank) bits).clearAll()
  io.output.address := Bits(widthOf(io.output.address) bits).clearAll()

  for(i <- 0 until(frequencyRatio)){
    when(io.cmd(i).valid){
      if(useAckN)io.output.actN(i) := io.cmd(i).actN
      io.output.csN(i*chipSelectNumber,chipSelectNumber bits)  := io.cmd(i).csN
      io.output.rasN(i) := io.cmd(i).rasN
      io.output.casN(i) := io.cmd(i).casN
      io.output.weN(i)  := io.cmd(i).weN
      if(useCid)io.output.cid(i*chipIdWidth,chipIdWidth bits) := io.cmd(i).cid
    }
  }

  for(i <- 0 until(frequencyRatio)){
    when(io.address(i).valid){
      if(config.useBg)io.output.bg(i*bankGroupWidth,bankGroupWidth bits) := io.address(i).bg
      io.output.bank(i*bankWidth,bankWidth bits)         := io.address(i).bank
      io.output.address(i*addressWidth,addressWidth bits) := io.address(i).address
    }
  }
}








case class RdAlignment(config: DfiConfig) extends Component {
  import config._
  val io = new Bundle {
    val phaseclear = in Bool()
    val rdcs = useRddataCsN generate Vec(slave(Flow(Dfireadcs(config))), config.frequencyRatio)
    val rd = in Vec(DfiRd(config), config.frequencyRatio)
    val rdCs = useRddataCsN generate Vec(out(DfiRdCs(config)), config.frequencyRatio)
    val rddata = Vec(master(Stream(Fragment(Dfirddata(config)))), config.frequencyRatio)
  }


  val validCnt = Vec(Reg(UInt(widthOf(U(timeConfig.dfiRWLength)) bits)), frequencyRatio)
  val rddataTemp = Vec(Stream(Fragment(Dfirddata(config))), config.frequencyRatio)
  validCnt.foreach(_.init(0))
  rddataTemp.foreach(_.last.clear())
  for (i <- 0 until (frequencyRatio)) {
    rddataTemp(i).valid := io.rd(i).rddatavalid
    rddataTemp(i).rddata.assignDontCare()
    rddataTemp(i).last.setWhen(validCnt(i) === timeConfig.dfiRWLength - 1 & io.rd(i).rddatavalid)

    when(rddataTemp(i).last) {
      validCnt(i) := 0
    }
    when(rddataTemp(i).fire) {
      validCnt(i) := validCnt(i) + 1
    }
  }

  val curphase = Reg(UInt(log2Up(frequencyRatio) bits)) init (0)
  val rddataphase = Reg(UInt(log2Up(frequencyRatio)+1 bits)) init (0)
  rddataphase.clearAll()
  when(io.phaseclear) {
    curphase := U(0)
  }


  for (i <- 0 until (frequencyRatio)) {
    when(rddataTemp(i).fire) {
      rddataTemp(i).rddata := ((i + curphase + frequencyRatio - rddataphase)%frequencyRatio).muxListDc(io.rd.zipWithIndex.map(t => (t._2,t._1))).rddata
      curphase := (i + 1) % frequencyRatio
      rddataphase := (rddataphase + i + 1 + frequencyRatio - curphase) % frequencyRatio
    }
  }


  val rddatadelay = Vec(Reg(UInt(log2Up(timeConfig.dfiRWLength)+1 bits)),frequencyRatio)
  val rddateHistory = History(rddataTemp,0 to timeConfig.dfiRWLength)
  val rddatadelayRst = Vec(Vec(Bool(),timeConfig.dfiRWLength),frequencyRatio)
  val rddatadelayCnt = Vec(Vec(Bool(),timeConfig.dfiRWLength),frequencyRatio)

  rddatadelay.map(_.init(timeConfig.dfiRWLength))
  for(i <- 0 until(frequencyRatio)){
    rddatadelayRst(i) := History(rddataTemp(i).last.fall() & !rddataTemp(i).valid,0 until timeConfig.dfiRWLength)
    when(rddatadelayRst(i)(timeConfig.dfiRWLength-1)){
      rddatadelay(i) := timeConfig.dfiRWLength
    }
    rddatadelayCnt(i) := History(if(frequencyRatio == 1)rddataTemp(i).fire.fall() & RegNext(!rddataTemp(i).last) else !rddataTemp(i).fire & rddataTemp.map(_.valid).orR & RegNext(!rddataTemp(i).last),0 until timeConfig.dfiRWLength)

    when(rddatadelayCnt(i)(timeConfig.dfiRWLength-1) & rddataTemp(i).ready){
      when(rddatadelay(i) =/= 0){
        rddatadelay(i) := rddatadelay(i) - 1
      }otherwise{
        rddatadelay(i) := rddatadelay(i)
      }
    }
  }

  for(i <- 0 until(frequencyRatio)){
    rddataTemp(i).ready := io.rddata(i).ready
    io.rddata(i) << rddatadelay(i).muxListDc(rddateHistory.zipWithIndex.map(t => (t._2,t._1)))(i)
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

  val delaycyc  = Reg(U(timeConfig.tPhyWrData/frequencyRatio)<<1).init(0)

  when(io.wrdata.map(_.valid).orR.rise()){
    delaycyc := timeConfig.tPhyWrData/frequencyRatio
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

