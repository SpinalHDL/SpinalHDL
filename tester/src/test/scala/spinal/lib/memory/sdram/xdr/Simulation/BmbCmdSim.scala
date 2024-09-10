package spinal.lib.memory.sdram.xdr.Simulation

import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.memory.sdram.Dfi.Interface._
import spinal.lib.memory.sdram.Dfi.CtrlWithBmb._
import spinal.lib.memory.sdram.Dfi.Tools._
import spinal.core._
import spinal.core.sim._
import spinal.lib.memory.sdram.xdr.PHY.BmbCmd

object BmbCmdSim {
  def main(args: Array[String]): Unit = {
    val sdramtime = SdramTiming(3, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8, WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35)
    val sdram = SdramLayout(SdramGeneration.MYDDR,bankWidth=2,columnWidth=9,rowWidth=12,dataWidth=8,ddrMHZ=100,ddrWrLat=4,ddrRdLat=4,sdramtime=sdramtime)
    val pl:PhyLayout = PhyLayout(sdram = sdram, phaseCount=4,dataRate=SdramGeneration.MYDDR.dataRate,0,0,0,0,transferPerBurst=8)
    val timeConfig = DfiTimeConfig(tPhyWrLat = pl.sdram.tPhyWrlat, tPhyWrData = 0, tPhyWrCsGap = 3, dramBurst = pl.transferPerBurst, frequencyRatio = pl.phaseCount, tRddataEn = pl.sdram.tRddataEn, tPhyRdlat = 4, tPhyRdCsGap = 3)
    val config: DfiConfig = DfiConfig(frequencyRatio = pl.phaseCount, dramAddrWidth = Math.max(pl.sdram.columnWidth, pl.sdram.rowWidth), dramDataWidth = pl.phyIoWidth,
      dramChipselectNumber = 1, dramBankWidth = pl.sdram.bankWidth, 0, 0, 1, cmdPhase = 0, ddr = DDR(), timeConfig = timeConfig)
    val bmbp: BmbParameter = BmbParameter(addressWidth = pl.sdram.byteAddressWidth + log2Up(config.chipSelectNumber), dataWidth = pl.beatWidth,
      sourceWidth = 1, contextWidth = 2, lengthWidth = 6, alignment = BmbParameter.BurstAlignement.WORD)
    SimConfig.withWave.compile {
      val dut = BmbCmd(bmbp, pl)
      //      dut.bmb2dfi.bmbAdapter.io.output.rsp.payload.last.simPublic()
      //      dut.bmb2dfi.dfiAlignment.initialize.io.initDone.simPublic()
      dut
    }.doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.initDone #= false
      dut.clockDomain.waitSampling(10)
      dut.io.initDone #= true
      dut.clockDomain.waitSampling(100)
      simSuccess()

    }
  }
}
