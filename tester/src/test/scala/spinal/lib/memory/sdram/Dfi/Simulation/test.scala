package spinal.lib.memory.sdram.Dfi.Simulation

import spinal.core._
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.memory.sdram.Dfi.CtrlWithBmb.{Bmb2Dfi, BmbAdapter, BmbPortParameter, CtrlParameter}
import spinal.lib.memory.sdram.Dfi.Interface.{TaskParameter, TaskParameterAggregate, DDR, DfiConfig, DfiTimeConfig, PhyConfig, SdramGeneration, SdramConfig, SdramTiming}



case class test() extends Component{
//  import cpa.config._
//  val wralignment = WrAlignment(config: DfiConfig, timeConfig: DfiTimeConfig)
//  val rdalignment = RdAlignment(config: DfiConfig, timeConfig: DfiTimeConfig)
//  val cpp = CorePortParameter(contextWidth = 8, writeTockenInterfaceWidth = 1, writeTockenBufferSize = 8, canRead = true, canWrite = true)
//  val sdram = SdramLayout(SdramGeneration.DDR3,2,10,13,16,sdramtime=SdramTiming(3))
//  val cpa = CoreParameterAggregate(CoreParameter(64,5,5,List(1),List(1)),PhyLayout(sdram,4,2,0,0,0,0,8),cpp)
//  val cpa = CoreParameterAggregate(CoreParameter(64,5,5),PhyLayout(sdram,4,2,0,0,0,0,8),cpp)
//  val maketask = MakeTask(cpp,cpa,config)
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//val maketask = MakeTask(
//  CorePortParameter(contextWidth = 8, writeTockenInterfaceWidth = 1, writeTockenBufferSize = 8, canRead = true, canWrite = true)
//  ,(CoreParameterAggregate(CoreParameter(64,5,5,List(1),List(1)),PhyLayout(SdramLayout(SdramGeneration.DDR3,2,9,12,16),4,2,0,0,0,0,8),CorePortParameter(contextWidth = 8, writeTockenInterfaceWidth = 1, writeTockenBufferSize = 8, canRead = true, canWrite = true)))
//  ,config)
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  val cmdtxd = CmdTxd(CoreParameterAggregate(CoreParameter(64,5,5,List(1),List(1)),PhyLayout(SdramLayout(SdramGeneration.DDR3,2,9,12,16),4,2,0,0,0,0,8),CorePortParameter(contextWidth = 8, writeTockenInterfaceWidth = 1, writeTockenBufferSize = 8, canRead = true, canWrite = true)),config)
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  val wrdatatxd = WrDataTxd(cpa,config, timeConfig)
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  val rddatarxd = RdDataRxd(cpa)
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
val bmbclockDomain = ClockDomain(ClockDomain.current.clock,ClockDomain.current.reset,config=ClockDomainConfig(resetActiveLevel = HIGH))
  val core:TaskParameter = TaskParameter(timingWidth=3,refWidth=9)
  val sdramtime = SdramTiming(3, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35)
  val sdram = SdramConfig(SdramGeneration.MYDDR,bankWidth=3,columnWidth=10,rowWidth=15,dataWidth=16,ddrMHZ=100,ddrWrLat=4,ddrRdLat=4,sdramtime=sdramtime)
  val pl:PhyConfig = PhyConfig(sdram = sdram, phaseCount=4,dataRate=SdramGeneration.MYDDR.dataRate,0,0,0,0,transferPerBurst=8)
  val timeConfig = DfiTimeConfig(tPhyWrLat=4,tPhyWrData=0,tPhyWrCsGap=3,dramBurst=pl.transferPerBurst,frequencyRatio=pl.phaseCount,tRddataEn=4,tPhyRdlat=4,tPhyRdCsGap=3,tPhyRdCslat = 0,tPhyWrCsLat = 0)
  val config:DfiConfig = DfiConfig(frequencyRatio=pl.phaseCount,dramAddrWidth=Math.max(pl.sdram.columnWidth,pl.sdram.rowWidth),dramDataWidth=pl.phyIoWidth,
    dramChipselectNumber=2,dramBankWidth=pl.sdram.bankWidth,0,0,1,cmdPhase=0,ddr=new DDR(),timeConfig=timeConfig)
  val bmbp:BmbParameter = BmbParameter(addressWidth=pl.sdram.byteAddressWidth+log2Up(config.chipSelectNumber),dataWidth=pl.beatWidth,
    sourceWidth=1,contextWidth=2,lengthWidth=6,alignment= BmbParameter.BurstAlignement.WORD)
//  val ctrlbmbp:BmbParameter = BmbParameter(addressWidth = 5, dataWidth = pl.sdram.chipAddressWidth,
//    sourceWidth=1,contextWidth=2,lengthWidth=6,alignment= BmbParameter.BurstAlignement.WORD)
  val bmbpp:BmbPortParameter = BmbPortParameter(bmbp,bmbclockDomain,cmdBufferSize=16,dataBufferSize=16,rspBufferSize=36)
  val ctp : CtrlParameter = CtrlParameter(core, bmbpp)
  val cpa = TaskParameterAggregate(ctp.core, pl, BmbAdapter.corePortParameter(ctp.port, pl), config)
//  val bmb2dfi = Bmb2Dfi(ctp,pl,config,ctrlbmbp)
  val bmb2dfi = Bmb2Dfi(ctp,pl,config)
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//val phy = ddr3_dfi_phy()
//  phy.io.clk.assignDontCare()
//  phy.io.rst_i.assignDontCare()
//  phy.io.cfg.assignDontCare()
//  phy.io.dfi.address_i.assignDontCare()
//  phy.io.dfi.bank_i.assignDontCare()
//  phy.io.dfi.cas_n_i.assignDontCare()
//  phy.io.dfi.cke_i.assignDontCare()
//  phy.io.dfi.cs_n_i.assignDontCare()
//  phy.io.dfi.odt_i.assignDontCare()
//  phy.io.dfi.ras_n_i.assignDontCare()
//  phy.io.dfi.reset_n_i.assignDontCare()
//  phy.io.dfi.we_n_i.assignDontCare()
//  phy.io.dfi.wrdata_i.assignDontCare()
//  phy.io.dfi.wrdata_en_i.assignDontCare()
//  phy.io.dfi.wrdata_mask_i.assignDontCare()
//  phy.io.dfi.rddata_en_i.assignDontCare()


}



object test {
  def main(args: Array[String]): Unit = {
//    SpinalConfig().generateVerilog(test(CoreParameterAggregate(CoreParameter(64,5,5),PhyLayout(SdramLayout(SdramGeneration.DDR3,2,9,12,12),4,2,0,0,0,0,8),
//      CorePortParameter(contextWidth = 8, writeTockenInterfaceWidth = 1, writeTockenBufferSize = 8, canRead = true, canWrite = true),DfiConfig(4,12,24,4,2,2,1,1,0,DDR(),
//        DfiTimeConfig(tPhyWrData = 4,dramBurst = 8,frequencyRatio = 4,tRddataEn = 5)))))
    SpinalConfig().generateVerilog(test())
  }
}
