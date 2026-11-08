package spinal.lib.memory.sdram.dfi

import spinal.core._

case class DfiTimeConfig(
    frequencyRatio: Int, // PHY:MC
    cmdPhase: Int,
    tPhyWrLat: Int,
    //  the number of cycles between when the write command is driven on the DFI to
    // assertion of the dfi_wrdata_en signal
    tPhyWrData: Int,
    // The tphy_wrdata parameter works to define the number of cycles from the
    // assertion of a write command on the DFI control interface
    // to when write data is driven on the DFI bus
    tPhyWrCsLat: Int,
    // specify the desired alignment of the
    // command to the dfi_wrdata_cs_n signal
    tPhyWrCsGap: Int,
    // specify the additional delay it requires between two consecutive commands
    // that are targeting different chip selects.
    tRddataEn: Int,
    // defines the timing requirements between the read command on the DFI interface and the
    // assertion of the dfi_rddata_en signal to maintain synchronicity between the MC and the PHY for the start of
    // contiguous read data expected on the DFI interface.
    tPhyRdlat: Int,
    // dfi_rddata_en -> dfi_rddata/dfi_rddata_valid
    tPhyRdCslat: Int,
    // specify the desired alignment of the command to the
    // dfi_rddata_cs_n signal
    tPhyRdCsGap: Int
    // specify the additional delay it
    // requires between two consecutive commands that are targeting different chip selects.
) {}

case class DfiFunctionConfig(
    useCtrlSignals: Boolean = true,
    useWrDataSignals: Boolean = true,
    useRdDataSignals: Boolean = true,
    useUpdateSignals: Boolean = false,
    useStatusSignals: Boolean = false,
    useTrainingSignals: Boolean = false,
    useLowPowerSignals: Boolean = false,
    useErrorSignals: Boolean = false
)

object DfiSignalConfig {
  val DDR1: DDR1SignalConfig = DDR1()
  val DDR2: DDR2SignalConfig = DDR2()
  val DDR3: DDR3SignalConfig = DDR3()
  val DDR4: DDR4SignalConfig = DDR4()

  def DDR1(groupConfig: DfiFunctionConfig = DfiFunctionConfig(), useCrc: Boolean = false): DDR1SignalConfig =
    new DDR1SignalConfig(groupConfig, useCrc)

  def DDR2(groupConfig: DfiFunctionConfig = DfiFunctionConfig(), useCrc: Boolean = false): DDR2SignalConfig =
    new DDR2SignalConfig(groupConfig, useCrc)

  def DDR3(groupConfig: DfiFunctionConfig = DfiFunctionConfig(), useCrc: Boolean = false): DDR3SignalConfig =
    new DDR3SignalConfig(groupConfig, useCrc)

  def DDR4(groupConfig: DfiFunctionConfig = DfiFunctionConfig(), useCrc: Boolean = false): DDR4SignalConfig =
    new DDR4SignalConfig(groupConfig, useCrc)
}

class DfiSignalConfig(groupConfig: DfiFunctionConfig = DfiFunctionConfig(), useCrc: Boolean = false) {
  val useBank: Boolean = false
  val useAckN: Boolean = false
  val useRasN: Boolean = false
  val useCasN: Boolean = false
  val useWeN: Boolean = false
  val useBg: Boolean = false
  val useCid: Boolean = false
  val useOdt: Boolean = false
  val useResetN: Boolean = false

  val useWrdataCsN: Boolean = false

  val useRddataDbiN: Boolean = false
  val useRddataCsN: Boolean = false
  val useRddataDnv: Boolean = false

  val useCtrlupdReq: Boolean = false
  val useCtrlupdAck: Boolean = false

  val usePhyupdReq: Boolean = false
  val usePhyupdAck: Boolean = false
  val usePhyupdType: Boolean = false

  val useDataByteDisable: Boolean = false
  val useFreqRatio: Boolean = false
  val useInitStart: Boolean = false
  val useParityIn: Boolean = false
  val useAlertN: Boolean = false

  val useRdlvlReq: Boolean = false
  val usePhyRdlvlCsN: Boolean = false
  val useRdlvlEn: Boolean = false
  val useRdlvlResp: Boolean = false
  val useRdlvlGateReq: Boolean = false
  val usePhyRdlvlGateCsN: Boolean = false
  val useRdlvlGateEn: Boolean = false

  val useWrlvlReq: Boolean = false
  val usePhyWrlvlCsN: Boolean = false
  val useWrlvlEn: Boolean = false
  val useWrlvlStrobe: Boolean = false
  val useWrlvlResp: Boolean = false

  val useCalvlReq: Boolean = false
  val usePhyCalvlCsN: Boolean = false
  val useCalvlEn: Boolean = false
  val useCalvlCapture: Boolean = false
  val useCalvlResp: Boolean = false

  val useLvlPattern: Boolean = false
  val useLvlPeriodic: Boolean = false
  val usePhylvlReqCsN: Boolean = false
  val usePhylvlAckCsN: Boolean = false

  val useLpCtrlReq: Boolean = false
  val useLpDataReq: Boolean = false
  val useLpWakeUp: Boolean = false
  val useLpAck: Boolean = false

  val useError: Boolean = false
  val useErrorInfo: Boolean = false

  val useStatusSignals = groupConfig.useStatusSignals

  def useCtrlupd = useCtrlupdReq & useCtrlupdAck
  def usePhyupd = usePhyupdReq & usePhyupdAck
  def useParity = useParityIn & useAlertN
  def usePhylvl = usePhylvlReqCsN & usePhylvlAckCsN
  def useLpData = useLpDataReq & useLpAck
  def useCrcMode = useCrc & useAlertN
}

class DDR1SignalConfig(groupConfig: DfiFunctionConfig, useCrc: Boolean) extends DfiSignalConfig(groupConfig, useCrc) {
  override val useBank = groupConfig.useCtrlSignals
  override val useRasN = groupConfig.useCtrlSignals
  override val useCasN = groupConfig.useCtrlSignals
  override val useWeN = groupConfig.useCtrlSignals

  override val useWrdataCsN = groupConfig.useWrDataSignals

  override val useRddataCsN = groupConfig.useRdDataSignals

  override val useCtrlupdReq = groupConfig.useUpdateSignals
  override val useCtrlupdAck = groupConfig.useUpdateSignals
  override val usePhyupdReq = groupConfig.useUpdateSignals
  override val usePhyupdAck = groupConfig.useUpdateSignals
  override val usePhyupdType = groupConfig.useUpdateSignals

  override val useDataByteDisable = groupConfig.useStatusSignals
  override val useFreqRatio = groupConfig.useStatusSignals
  override val useInitStart = groupConfig.useStatusSignals
  override val useParityIn = groupConfig.useStatusSignals
  override val useAlertN = groupConfig.useStatusSignals

  override val useLvlPeriodic = groupConfig.useTrainingSignals

  override val useLpCtrlReq = groupConfig.useLowPowerSignals
  override val useLpDataReq = groupConfig.useLowPowerSignals
  override val useLpWakeUp = groupConfig.useLowPowerSignals
  override val useLpAck = groupConfig.useLowPowerSignals

  override val useError = groupConfig.useErrorSignals
  override val useErrorInfo = groupConfig.useErrorSignals
}

class DDR2SignalConfig(groupConfig: DfiFunctionConfig, useCrc: Boolean) extends DDR1SignalConfig(groupConfig, useCrc) {
  override val useOdt = groupConfig.useCtrlSignals
}

class DDR3SignalConfig(groupConfig: DfiFunctionConfig, useCrc: Boolean) extends DDR2SignalConfig(groupConfig, useCrc) {
  override val useResetN = groupConfig.useCtrlSignals

  override val useRdlvlReq = groupConfig.useTrainingSignals
  override val usePhyRdlvlCsN = groupConfig.useTrainingSignals
  override val useRdlvlEn = groupConfig.useTrainingSignals
  override val useRdlvlResp = groupConfig.useTrainingSignals
  override val useRdlvlGateReq = groupConfig.useTrainingSignals
  override val usePhyRdlvlGateCsN = groupConfig.useTrainingSignals
  override val useRdlvlGateEn = groupConfig.useTrainingSignals
  override val useWrlvlReq = groupConfig.useTrainingSignals
  override val usePhyWrlvlCsN = groupConfig.useTrainingSignals
  override val useWrlvlEn = groupConfig.useTrainingSignals
  override val useWrlvlStrobe = groupConfig.useTrainingSignals
  override val useWrlvlResp = groupConfig.useTrainingSignals
}

class DDR4SignalConfig(groupConfig: DfiFunctionConfig, useCrc: Boolean) extends DDR3SignalConfig(groupConfig, useCrc) {
  override val useAckN = groupConfig.useCtrlSignals
  override val useBg = groupConfig.useCtrlSignals
  override val useCid = groupConfig.useCtrlSignals

  override val useRddataDbiN = groupConfig.useRdDataSignals

  override val useLvlPattern = groupConfig.useTrainingSignals
  override val usePhylvlReqCsN = groupConfig.useTrainingSignals
  override val usePhylvlAckCsN = groupConfig.useTrainingSignals
}

class AddrMap {}
object RowBankColumn extends AddrMap
object BankRowColumn extends AddrMap
object RowColumnBank extends AddrMap

object DfiConfig {
  implicit def toSignalConfig(that: DfiConfig): DfiSignalConfig = that.signalConfig
}

case class DfiConfig(
    chipSelectNumber: Int,
    dataSlice: Int,
    signalConfig: DfiSignalConfig,
    timeConfig: DfiTimeConfig,
    sdram: SdramConfig,
    // DDR4 mode register file programmed at init; MR5 A12=Read DBI, A11=Write DBI,
    // A10=Data Mask (JESD79-4B §4.11). This controller never drives DM (wrDataMask is a
    // pass-through of the BMB mask, all-ones when unmapped), so no DM/DBI mutual-exclusion
    // is enforced here: the integrator must not enable both at the SDRAM level.
    ddr4ModeRegs: Ddr4ModeRegs = Ddr4ModeRegs(),
    // Optional init micro-program (step table) for the generic SdramInitSequencer in MakeTask.
    // None: DDR4 keeps its built-in Ddr4InitSequencer preset, every other generation runs
    // init-less exactly as before (no engine, no new bus drivers). Some(List(...)): the engine
    // plays those steps before any user traffic, for any generation whose CmdTxd can encode
    // MRS/ZQ (DDR3/DDR2 legacy path included).
    initSteps: Option[List[SdramInitStep]] = None
) {
  val frequencyRatio = timeConfig.frequencyRatio
  val dataRate = sdram.generation.dataRate
  val transferPerBurst = sdram.burstLength
  val phyIoWidth = dataRate * sdram.dataWidth
  val beatCount = transferPerBurst / frequencyRatio / dataRate
  val bytePerDq = sdram.dataWidth / 8
  val burstWidth = sdram.dataWidth * transferPerBurst
  val bytePerBurst = burstWidth / 8
  val beatWidth = frequencyRatio * dataRate * sdram.dataWidth
  val bytePerBeat = beatWidth / 8
  val dataWidth = phyIoWidth
  val addressWidth = Math.max(sdram.columnWidth, sdram.rowWidth)
  val bankWidth = sdram.bankWidth
  // DFI-side bank (BA) width: the flat task-level bank index minus the bank-group bits.
  // Legacy generations have bgWidth == 0, so dfiBankWidth == bankWidth (behavior identical).
  val dfiBankWidth = sdram.bankWidth - sdram.bgWidth

  val controlWidth = 1
  val bankGroupWidth = sdram.bgWidth
  val chipIdWidth = sdram.cidWidth
  val dataEnableWidth = dataWidth / dataSlice
  val chipSelectWidth = log2Up(chipSelectNumber)
  val taskAddressWidth = sdram.byteAddressWidth + chipSelectWidth
  // For PHYs with an 8-bit slice, this is generally 1/16th of the DFI Data Width to provide a single enable bit per
  // memory data slice, but may be 1/4, 1/8, 1/32, or any other ratio.
  val dbiWidth = dataWidth / 8
  val readDataValidWidth = dataEnableWidth
  val alertWidth = 1
  val readLevelingPhyIFWidth = dataSlice
  val readLevelingMCIFWidth = readLevelingPhyIFWidth
  val readTrainingPhyIFWidth = readLevelingPhyIFWidth
  val readLevelingResponseWidth = dataSlice
  val writeLevelingPhyIFWidth = dataSlice
  val writeLevelingMCIFWidth = dataSlice
  val writeLevelingResponseWidth = dataSlice
  val caTrainingPhyIFWidth = dataSlice
  val caTrainingMCIFWidth = dataSlice
  val caTrainingResponseWidth = 2
  val levelingPhyIFWidth = dataSlice
  val rankWidth = chipSelectNumber
  val errorNumber = dataSlice + 1

  // NOTE ON GENERATION GATES: DBI_n and CA parity (PAR) are DDR4-era physical pins; DDR1/DDR2/DDR3
  // SDRAMs have no such pins, so enabling these features on older generations would invent protocol
  // behavior the silicon cannot honor. The generation checks below are protocol truth, NOT
  // conservatism — do not relax them to signal-presence-only (legacy signalConfigs map
  // useStatusSignals -> parityIn/alertN, which would silently turn the features on for DDR3).
  // The generically reusable knob is the refresh scheduler (SdramTiming REF_NS/REF_RATE),
  // which carries no generation gate.
  // DDR4 DBI enable, decoded from MR5 (JESD79-4B §4.11): A12=Read DBI, A11=Write DBI.
  // Read DBI additionally requires the rddata_dbi_n signal to exist in the signal set.
  val useRdDbi =
    sdram.generation == SdramGeneration.DDR4 && signalConfig.useRddataDbiN && ((ddr4ModeRegs.mr5 >> 12) & 1) == 1
  val useWrDbi = sdram.generation == SdramGeneration.DDR4 && ((ddr4ModeRegs.mr5 >> 11) & 1) == 1

  // DDR4 CA parity (JESD79-4B §4.17 / DFI 6.0.1 §4.3.1): the MC drives dfi_parity (even parity
  // over ACT_n, RAS_n/A16, CAS_n/A15, WE_n/A14, A13..A0, BA, BG — NOT CKE/ODT/CS_n) in the same
  // cycle as the command (tcapar_lat = 0, allowed by the spec). Any non-zero MR5.A2:A0 parity
  // latency (PL4/5/6) means CA parity is enabled at the DRAM; the PL value itself only affects
  // DRAM-internal timing and is not modeled on the MC side, so the signal-level useParity
  // (dfi_parity_in + dfi_alert_n present) gates the feature here.
  val useCaParity = sdram.generation == SdramGeneration.DDR4 && signalConfig.useParity

}
