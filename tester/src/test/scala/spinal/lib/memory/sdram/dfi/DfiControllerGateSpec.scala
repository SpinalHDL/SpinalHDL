package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.bmb.BmbParameter
import spinal.tester.code.SpinalAnyFunSuite

/** Negative elaboration gates for the ADR-0002 command-bus consistency asserts in DfiController.
  *
  * The positive (consistent) configs are covered by the full DfiControllerTester regression;
  * these pin down that INCONSISTENT configs are still rejected at elaboration after the
  * generation-identity gate (`isDdr4`) became the capability-flag gate (`hasModernCmdBus`).
  * Without them, a silent relaxation of the gate (e.g. collapsing the legacy side to a mere
  * conjunction) would pass the whole regression unnoticed. */
class DfiControllerGateSpec extends SpinalAnyFunSuite {

  // Assert that elaborating the component body throws (precedent: CounterTester).
  private def expectElaborationFailure(body: => Component): Unit = {
    intercept[Throwable] { SimConfig.compile(body) }
    ()
  }

  private def buildDut(generation: SdramGeneration, signalConfig: DfiSignalConfig): DfiController = {
    val task: TaskParameter = TaskParameter(
      timingWidth = 5,
      refWidth = 23,
      cmdBufferSize = 1024,
      dataBufferSize = 1024,
      rspBufferSize = 1024
    )
    val sdram = SdramConfig(
      generation,
      bgWidth = if (generation == SdramGeneration.DDR4) 2 else 0,
      cidWidth = 0,
      bankWidth = if (generation == SdramGeneration.DDR4) 4 else 3,
      columnWidth = 10,
      rowWidth = 16,
      dataWidth = 16,
      ddrMHZ = 200,
      ddrWrLat = 4,
      ddrRdLat = 4,
      sdramtime = SdramTiming(
        generation = 3, RFC = 260, RAS = 38, RP = 15, RCD = 15, WTR = 8,
        WTP = 0, RTP = 8, RRD = 6, REF = 64000, FAW = 35
      )
    )
    val timeConfig = DfiTimeConfig(
      frequencyRatio = 2,
      cmdPhase = 0,
      tPhyWrLat = sdram.tPhyWrlat,
      tPhyWrData = 0,
      tPhyWrCsGap = 3,
      tRddataEn = sdram.tRddataEn,
      tPhyRdlat = 4,
      tPhyRdCsGap = 3,
      tPhyRdCslat = 0,
      tPhyWrCsLat = 0,
      tWrlvlResp = 16
    )
    val dfiConfig = DfiConfig(
      chipSelectNumber = 1,
      dataSlice = 1,
      signalConfig = signalConfig,
      timeConfig = timeConfig,
      sdram = sdram
    )
    val bmbp: BmbParameter = BmbParameter(
      addressWidth = sdram.byteAddressWidth + log2Up(dfiConfig.chipSelectNumber),
      dataWidth = dfiConfig.beatWidth,
      sourceWidth = 0,
      contextWidth = 2,
      lengthWidth = 10,
      alignment = BmbParameter.BurstAlignement.WORD
    )
    DfiController(bmbp, task, dfiConfig, RowBankColumn)
  }

  test("DDR4 config missing useBg is rejected at elaboration") {
    val signalConfig = new DDR4SignalConfig(DfiFunctionConfig(), false) {
      override val useBg = false
    }
    expectElaborationFailure(buildDut(SdramGeneration.DDR4, signalConfig))
  }

  test("legacy (DDR3) config wiring useBg is rejected at elaboration") {
    val signalConfig = new DDR3SignalConfig(DfiFunctionConfig(), false) {
      override val useBg = true
    }
    expectElaborationFailure(buildDut(SdramGeneration.DDR3, signalConfig))
  }

  // Positive (consistent) configs are elaborated by every DfiControllerTester regression case;
  // if the gate over-fired, those 28 tests would all fail at elaboration.
}
