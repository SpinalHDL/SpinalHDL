package spinal.lib.memory.sdram.dfi.simulation

import spinal.core._
import spinal.core.sim._
import spinal.demo.phy.BmbCmdOp
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.memory.sdram.dfi._

object BmbCmdOpSim {
  def main(args: Array[String]): Unit = {
    val sdramtime = SdramTiming(
      generation = 3,
      RFC = 260,
      RAS = 38,
      RP = 15,
      RCD = 15,
      WTR = 8,
      WTP = 0,
      RTP = 8,
      RRD = 6,
      REF = 64000,
      FAW = 35
    )
    val sdram = SdramConfig(
      SdramGeneration.DDR3,
      bgWidth = 0,
      cidWidth = 0,
      bankWidth = 3,
      columnWidth = 10,
      rowWidth = 15,
      dataWidth = 16,
      ddrMHZ = 100,
      ddrWrLat = 4,
      ddrRdLat = 4,
      sdramtime = sdramtime
    )
    val timeConfig = DfiTimeConfig(
      frequencyRatio = 1,
      cmdPhase = 0,
      tPhyWrLat = sdram.tPhyWrlat,
      tPhyWrData = 0,
      tPhyWrCsGap = 3,
      tRddataEn = sdram.tRddataEn,
      tPhyRdlat = 4,
      tPhyRdCsGap = 3,
      tPhyRdCslat = 0,
      tPhyWrCsLat = 0
    )
    val dfiConfig: DfiConfig = DfiConfig(
      chipSelectNumber = 1,
      dataSlice = 1,
      signalConfig = new DDR3SignalConfig(DfiFunctionConfig(), false) {
        override val useWrdataCsN = false
        override val useRddataCsN = false
      },
      timeConfig = timeConfig,
      sdram = sdram
    )
    val bmbp: BmbParameter = BmbParameter(
      addressWidth = sdram.byteAddressWidth + log2Up(dfiConfig.chipSelectNumber),
      dataWidth = dfiConfig.beatWidth,
      sourceWidth = 1,
      contextWidth = 2,
      lengthWidth = 6,
      alignment = BmbParameter.BurstAlignement.WORD
    )
    SimConfig.withWave
      .compile {
        val dut = BmbCmdOp(bmbp, dfiConfig)
        dut
      }
      .doSimUntilVoid { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.io.bmb.rsp.valid #= false
        dut.io.bmb.rsp.last #= false
        dut.io.initDone #= false
        dut.clockDomain.waitSampling(10)
        dut.io.initDone #= true
        dut.io.bmb.rsp.valid #= true
        dut.clockDomain.waitSampling(31)
        dut.io.bmb.rsp.last #= true
        dut.clockDomain.waitSampling()
        dut.io.bmb.rsp.valid #= false
        dut.io.bmb.rsp.last #= false
        dut.clockDomain.waitSampling(100)
        simSuccess()

      }
  }
}
