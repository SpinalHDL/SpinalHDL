package spinal.lib.memory.sdram.xdr.Simulation

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.memory.sdram.Dfi.Tools._
import spinal.lib.memory.sdram.Dfi.CtrlWithBmb._
import spinal.lib.memory.sdram.Dfi.Interface._
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.memory.sdram.xdr.PHY.Initialize
case class InitializeSim() extends Component{
  val bmbclockDomain = ClockDomain(ClockDomain.current.clock,ClockDomain.current.reset,config=ClockDomainConfig(resetActiveLevel = HIGH))
  val core:CoreParameter = CoreParameter(timingWidth=3,refWidth=14)
  val sdramtime = SdramTiming(3, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35)
  val sdram = SdramLayout(SdramGeneration.MYDDR,bankWidth=2,columnWidth=9,rowWidth=12,dataWidth=8,ddrMHZ=100,ddrWrLat=4,ddrRdLat=4,sdramtime=sdramtime)
  val pl:PhyLayout = PhyLayout(sdram = sdram, phaseCount=4,dataRate=SdramGeneration.MYDDR.dataRate,0,0,0,0,transferPerBurst=8)
  val timeConfig = DfiTimeConfig(tPhyWrLat=1,tPhyWrData=2,tPhyWrCsGap=3,dramBurst=pl.transferPerBurst,frequencyRatio=pl.phaseCount,tRddataEn=1,tPhyRdlat=4,tPhyRdCsGap=3)
  val config:DfiConfig = DfiConfig(frequencyRatio=pl.phaseCount,dramAddrWidth=Math.max(pl.sdram.columnWidth,pl.sdram.rowWidth),dramDataWidth=pl.phyIoWidth,
    dramChipselectNumber=2,dramBankWidth=pl.sdram.bankWidth,0,0,1,cmdPhase=0,ddr=DDR(),timeConfig=timeConfig)
  val bmbp:BmbParameter = BmbParameter(addressWidth=pl.sdram.byteAddressWidth+log2Up(config.chipSelectNumber),dataWidth=pl.beatWidth,
    sourceWidth=1,contextWidth=2,lengthWidth=6,alignment= BmbParameter.BurstAlignement.WORD)
  val bmbpp:BmbPortParameter = BmbPortParameter(bmbp,bmbclockDomain,cmdBufferSize=64,dataBufferSize=64,rspBufferSize=64)
  val ctp : CtrlParameter = CtrlParameter(core, bmbpp)
  val cpa = CoreParameterAggregate(core, pl, BmbAdapter.corePortParameter(ctp.port, pl), config)
  val io = new Bundle{
    val cmd      = Vec(master(Flow(DfiCmd(config))),config.frequencyRatio)
    val address  = Vec(master(Flow(DfiAddr(config))),config.frequencyRatio)
    val ckeN     = out Bits(config.chipSelectNumber * config.frequencyRatio bits)
    val initDone = out Bool()
  }
  val init = Initialize(cpa)
  io.cmd := init.io.cmd
  io.address := init.io.address
  io.ckeN := init.io.ckeN
  io.initDone := init.io.initDone
}

object InitializeSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(InitializeSim()).doSimUntilVoid{dut=>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(100000)
      simSuccess()
    }
  }
}
