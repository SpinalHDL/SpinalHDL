package spinal.lib.memory.sdram.dfi.simulation

import spinal.core._
import spinal.core.sim._
import spinal.demo.phy.Initialize
import spinal.lib._
import spinal.lib.bus.bmb.BmbParameter
import spinal.lib.memory.sdram.dfi.foundation.BmbAdapter
import spinal.lib.memory.sdram.dfi.interface._

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
    tPhyWrLat = 1,
    tPhyWrData = 2,
    tPhyWrCsGap = 3,
    tRddataEn = 1,
    tPhyRdlat = 4,
    tPhyRdCsGap = 3,
    tPhyRdCslat = 0,
    tPhyWrCsLat = 0
  )
  val config: DfiConfig = DfiConfig(
    frequencyRatio = 1,
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
  val ctp: CtrlParameter = CtrlParameter(task, bmbp)
  val tpa = TaskParameterAggregate(task, BmbAdapter.taskPortParameter(ctp.bmbp, config, task), config)
  val io = new Bundle {
    val cmd = master(Flow {
      new Bundle {
        val weN = out Bool ()
        val casN = out Bool ()
        val rasN = out Bool ()
        val csN = out Bool ()
      }
    })
    val address = master(Flow(new Bundle {
      val address = Bits(sdram.rowWidth bits)
      val bank = Bits(sdram.bankWidth bits)
    }))
    val cke = out Bool ()
    val initDone = out Bool ()
  }
  val init = Initialize(tpa)
  io.cmd := init.io.cmd
  io.address := init.io.address
  io.cke := init.io.cke
  io.initDone := init.io.initDone
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
