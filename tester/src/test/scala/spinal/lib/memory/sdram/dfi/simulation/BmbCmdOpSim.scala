package spinal.lib.memory.sdram.dfi.simulation

import spinal.core._
import spinal.core.sim._
import spinal.demo.phy.BmbCmdOp
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.memory.sdram.dfi.interface._

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
      SdramGeneration.MYDDR,
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
      tPhyWrLat = sdram.tPhyWrlat,
      tPhyWrData = 0,
      tPhyWrCsGap = 3,
      tRddataEn = sdram.tRddataEn,
      tPhyRdlat = 4,
      tPhyRdCsGap = 3,
      tPhyRdCslat = 0,
      tPhyWrCsLat = 0
    )
    val config: DfiConfig = DfiConfig(
      frequencyRatio = 2,
      transferPerBurst = 8,
      addressWidth = Math.max(sdram.columnWidth, sdram.rowWidth),
      chipSelectNumber = 2,
      bankWidth = sdram.bankWidth,
      bgWidth = 0,
      cidWidth = 0,
      dataSlice = 1,
      cmdPhase = 0,
      signalConfig = new DDRSignalConfig(),
      timeConfig = timeConfig,
      sdram = sdram
    )
    val bmbp: BmbParameter = BmbParameter(
      addressWidth = sdram.byteAddressWidth + log2Up(config.chipSelectNumber),
      dataWidth = config.beatWidth,
      sourceWidth = 1,
      contextWidth = 2,
      lengthWidth = 6,
      alignment = BmbParameter.BurstAlignement.WORD
    )
    SimConfig.withWave
      .compile {
        val dut = BmbCmdOp(bmbp, config)
        dut
      }
      .doSimUntilVoid { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.io.initDone #= false
        dut.clockDomain.waitSampling(10)
        dut.io.initDone #= true
        dut.clockDomain.waitSampling(100)
        simSuccess()

      }
  }
}
