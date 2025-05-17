package spinal.lib.memory.sdram.dfi.simulation

import spinal.core._
import spinal.core.sim._
import spinal.demo.phy.Initialize
import spinal.lib._
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.memory.sdram.dfi._

case class InitializeSim() extends Component {
  val task: TaskParameter =
    TaskParameter(timingWidth = 3, refWidth = 23, cmdBufferSize = 64, dataBufferSize = 64, rspBufferSize = 64)
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
    tPhyWrLat = 1,
    tPhyWrData = 2,
    tPhyWrCsGap = 3,
    tRddataEn = 1,
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
      override val useOdt: Boolean = true
      override val useResetN: Boolean = true
      override val useRddataDnv = true
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
  val io = new Bundle {
    val control = master(DfiControlInterface(dfiConfig))
    val initDone = out Bool ()
  }
  val taskConfig = BmbAdapter.taskConfig(bmbp, dfiConfig, task)
  val init = Initialize(taskConfig, dfiConfig)
  io.assignUnassignedByName(init.io)
}

object InitializeSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(InitializeSim()).doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(100000)
      simSuccess()
    }
  }
}
