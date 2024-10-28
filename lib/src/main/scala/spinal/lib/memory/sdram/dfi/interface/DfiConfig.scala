package spinal.lib.memory.sdram.dfi.interface

import spinal.core._

case class DfiTimeConfig(
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
case class DDRInterfaceSignals(
    useCtrlSignals: Boolean = true,
    useWrDataSignals: Boolean = true,
    useRdDataSignals: Boolean = true,
    useUpdateSignals: Boolean = false,
    useStatusSignals: Boolean = false,
    useTrainingSignals: Boolean = false,
    useLowPowerSignals: Boolean = false,
    useErrorSignals: Boolean = false
)
object DDRSignalConfig {
  abstract class SignalConfig {
    def useBank: Boolean
    def useAckN: Boolean
    def useRasN: Boolean
    def useCasN: Boolean
    def useWeN: Boolean
    def useBg: Boolean
    def useCid: Boolean
    def useOdt: Boolean
    def useResetN: Boolean

    def useWrdataCsN: Boolean

    def useRddataDbiN: Boolean
    def useRddataCsN: Boolean
    def useRddataDnv: Boolean

    def useCtrlupdReq: Boolean
    def useCtrlupdAck: Boolean
    def usePhyupdReq: Boolean
    def usePhyupdAck: Boolean
    def usePhyupdType: Boolean

    def useDataByteDisable: Boolean
    def useFreqRatio: Boolean
    def useInitStart: Boolean
    def useParityIn: Boolean
    def useAlertN: Boolean

    def useRdlvlReq: Boolean
    def usePhyRdlvlCsN: Boolean
    def useRdlvlEn: Boolean
    def useRdlvlResp: Boolean
    def useRdlvlGateReq: Boolean
    def usePhyRdlvlGateCsN: Boolean
    def useRdlvlGateEn: Boolean
    def useWrlvlReq: Boolean
    def usePhyWrlvlCsN: Boolean
    def useWrlvlEn: Boolean
    def useWrlvlStrobe: Boolean
    def useWrlvlResp: Boolean
    def useCalvlReq: Boolean
    def usePhyCalvlCsN: Boolean
    def useCalvlEn: Boolean
    def useCalvlCapture: Boolean
    def useCalvlResp: Boolean
    def useLvlPattern: Boolean
    def useLvlPeriodic: Boolean
    def usePhylvlReqCsN: Boolean
    def usePhylvlAckCsN: Boolean

    def useLpCtrlReq: Boolean
    def useLpDataReq: Boolean
    def useLpWakeUp: Boolean
    def useLpAck: Boolean

    def useError: Boolean
    def useErrorInfo: Boolean
  }
  abstract class DDR1SignalConfig extends SignalConfig {
    override def useBank = true
    override def useAckN = false
    override def useRasN = true
    override def useCasN = true
    override def useWeN = true
    override def useBg = false
    override def useCid = false
    override def useOdt = false
    override def useResetN = false

    override def useWrdataCsN = true

    override def useRddataDbiN = false
    override def useRddataCsN = true
    override def useRddataDnv = false

    override def useCtrlupdReq = true
    override def useCtrlupdAck = true
    override def usePhyupdReq = true
    override def usePhyupdAck = true
    override def usePhyupdType = true

    override def useDataByteDisable = true
    override def useFreqRatio = true
    override def useInitStart = true
    override def useParityIn = true
    override def useAlertN = true

    override def useRdlvlReq = false
    override def usePhyRdlvlCsN = false
    override def useRdlvlEn = false
    override def useRdlvlResp = false
    override def useRdlvlGateReq = false
    override def usePhyRdlvlGateCsN = false
    override def useRdlvlGateEn = false
    override def useWrlvlReq = false
    override def usePhyWrlvlCsN = false
    override def useWrlvlEn = false
    override def useWrlvlStrobe = false
    override def useWrlvlResp = false
    override def useCalvlReq = false
    override def usePhyCalvlCsN = false
    override def useCalvlEn = false
    override def useCalvlCapture = false
    override def useCalvlResp = false
    override def useLvlPattern = false
    override def useLvlPeriodic = true
    override def usePhylvlReqCsN = false
    override def usePhylvlAckCsN = false

    override def useLpCtrlReq = true
    override def useLpDataReq = true
    override def useLpWakeUp = true
    override def useLpAck = true

    override def useError = true
    override def useErrorInfo = true
  }

  abstract class DDR2SignalConfig extends DDR1SignalConfig {
    override def useOdt = true
  }

  abstract class DDR3SignalConfig extends DDR2SignalConfig {
    override def useResetN = true
    override def useRdlvlReq = true
    override def usePhyRdlvlCsN = true
    override def useRdlvlEn = true
    override def useRdlvlResp = true
    override def useRdlvlGateReq = true
    override def usePhyRdlvlGateCsN = true
    override def useRdlvlGateEn = true
    override def useWrlvlReq = true
    override def usePhyWrlvlCsN = true
    override def useWrlvlEn = true
    override def useWrlvlStrobe = true
    override def useWrlvlResp = true
  }

  abstract class DDR4SignalConfig extends DDR3SignalConfig {
    override def useAckN = true
    override def useBg = true
    override def useCid = true

    override def useRddataDbiN = true
    override def useLvlPattern = true
    override def usePhylvlReqCsN = true
    override def usePhylvlAckCsN = true
  }
  object DDR1SC extends DDR1SignalConfig
  object DDR2SC extends DDR2SignalConfig
  object DDR3SC extends DDR3SignalConfig
  object DDR4SC extends DDR4SignalConfig
  object MYDDRSC extends DDR3SignalConfig {
    override def useOdt: Boolean = false
    override def useResetN: Boolean = false
    override def useWrdataCsN: Boolean = false
    override def useRddataCsN: Boolean = false
  }
}

class DDRSignalConfig(
    generation: Int = 0,
    val isPLDDR: Boolean = false,
    val ddris: DDRInterfaceSignals = DDRInterfaceSignals(),
    val useCrcMode: Boolean = false
) {
  import ddris._
  val kind = !isPLDDR generate
    generation match {
    case 1 => DDRSignalConfig.DDR1SC
    case 2 => DDRSignalConfig.DDR2SC
    case 3 => DDRSignalConfig.DDR3SC
    case 4 => DDRSignalConfig.DDR4SC
    case _ => DDRSignalConfig.MYDDRSC
  }
  assert(kind != null)
  def useBank = kind.useBank & useCtrlSignals
  def useAckN = kind.useAckN & useCtrlSignals
  def useRasN = kind.useRasN & useCtrlSignals
  def useCasN = kind.useCasN & useCtrlSignals
  def useWeN = kind.useWeN & useCtrlSignals
  def useBg = kind.useBg & useCtrlSignals
  def useCid = kind.useCid & useCtrlSignals
  def useOdt = kind.useOdt & useCtrlSignals
  def useResetN = kind.useResetN & useCtrlSignals

  def useWrdataCsN = kind.useWrdataCsN & useWrDataSignals

  def useRddataDbiN = kind.useRddataDbiN & useRdDataSignals
  def useRddataCsN = kind.useRddataCsN & useRdDataSignals
  def useRddataDnv = kind.useRddataDnv & useRdDataSignals

  def useCtrlupdReq = kind.useCtrlupdReq & useUpdateSignals
  def useCtrlupdAck = kind.useCtrlupdAck & useUpdateSignals
  def usePhyupdReq = kind.usePhyupdReq & useUpdateSignals
  def usePhyupdAck = kind.usePhyupdAck & useUpdateSignals
  def usePhyupdType = kind.usePhyupdType & useUpdateSignals

  def useDataByteDisable = kind.useDataByteDisable & useStatusSignals
  def useFreqRatio = kind.useFreqRatio & useStatusSignals
  def useInitStart = kind.useInitStart & useStatusSignals
  def useParityIn = kind.useParityIn & useStatusSignals
  def useAlertN = kind.useAlertN & useStatusSignals

  def useRdlvlReq = kind.useRdlvlReq & useTrainingSignals
  def usePhyRdlvlCsN = kind.usePhyRdlvlCsN & useTrainingSignals
  def useRdlvlEn = kind.useRdlvlEn & useTrainingSignals
  def useRdlvlResp = kind.useRdlvlResp & useTrainingSignals
  def useRdlvlGateReq = kind.useRdlvlGateReq & useTrainingSignals
  def usePhyRdlvlGateCsN = kind.usePhyRdlvlGateCsN & useTrainingSignals
  def useRdlvlGateEn = kind.useRdlvlGateEn & useTrainingSignals
  def useWrlvlReq = kind.useWrlvlReq & useTrainingSignals
  def usePhyWrlvlCsN = kind.usePhyWrlvlCsN & useTrainingSignals
  def useWrlvlEn = kind.useWrlvlEn & useTrainingSignals
  def useWrlvlStrobe = kind.useWrlvlStrobe & useTrainingSignals
  def useWrlvlResp = kind.useWrlvlResp & useTrainingSignals
  def useCalvlReq = kind.useCalvlReq & useTrainingSignals
  def usePhyCalvlCsN = kind.usePhyCalvlCsN & useTrainingSignals
  def useCalvlEn = kind.useCalvlEn & useTrainingSignals
  def useCalvlCapture = kind.useCalvlCapture & useTrainingSignals
  def useCalvlResp = kind.useCalvlResp & useTrainingSignals
  def useLvlPattern = kind.useLvlPattern & useTrainingSignals
  def useLvlPeriodic = kind.useLvlPeriodic & useTrainingSignals
  def usePhylvlReqCsN = kind.usePhylvlReqCsN & useTrainingSignals
  def usePhylvlAckCsN = kind.usePhylvlAckCsN & useTrainingSignals

  def useLpCtrlReq = kind.useLpCtrlReq & useLowPowerSignals
  def useLpDataReq = kind.useLpDataReq & useLowPowerSignals
  def useLpWakeUp = kind.useLpWakeUp & useLowPowerSignals
  def useLpAck = kind.useLpAck & useLowPowerSignals

  def useError = kind.useError & useErrorSignals
  def useErrorInfo = kind.useErrorInfo & useErrorSignals
}


class AddrMap {}
object RowBankColumn extends AddrMap
object BankRowColumn extends AddrMap
object RowColumnBank extends AddrMap

case class DfiConfig(
    addrMap: AddrMap = RowBankColumn,
    frequencyRatio: Int, // PHY:MC
    chipSelectNumber: Int,
    bgWidth: Int,
    cidWidth: Int,
    dataSlice: Int,
    cmdPhase: Int,
    signalConfig: DDRSignalConfig,
    timeConfig: DfiTimeConfig,
    sdram: SdramConfig
) {
  assert(signalConfig.isPLDDR == false)
  val useBank = signalConfig.useBank
  val useAckN = signalConfig.useAckN
  val useRasN = signalConfig.useRasN
  val useCasN = signalConfig.useCasN
  val useWeN = signalConfig.useWeN
  val useBg = signalConfig.useBg
  val useCid = signalConfig.useCid
  val useOdt = signalConfig.useOdt
  val useResetN = signalConfig.useResetN
  val useWrdataCsN = signalConfig.useWrdataCsN
  val useRddataDbiN = signalConfig.useRddataDbiN
  val useRddataCsN = signalConfig.useRddataCsN
  val useRddataDnv = signalConfig.useRddataDnv
  val useCtrlupdReq = signalConfig.useCtrlupdReq
  val useCtrlupdAck = signalConfig.useCtrlupdAck
  val usePhyupdReq = signalConfig.usePhyupdReq
  val usePhyupdAck = signalConfig.usePhyupdAck
  val usePhyupdType = signalConfig.usePhyupdType
  val useDataByteDisable = signalConfig.useDataByteDisable
  val useFreqRatio = signalConfig.useFreqRatio
  val useInitStart = signalConfig.useInitStart
  val useParityIn = signalConfig.useParityIn
  val useAlertN = signalConfig.useAlertN
  val useRdlvlReq = signalConfig.useRdlvlGateReq
  val usePhyRdlvlCsN = signalConfig.usePhyRdlvlCsN
  val useRdlvlEn = signalConfig.useRdlvlEn
  val useRdlvlResp = signalConfig.useRdlvlResp
  val useRdlvlGateReq = signalConfig.useRdlvlGateReq
  val usePhyRdlvlGateCsN = signalConfig.usePhyRdlvlGateCsN
  val useRdlvlGateEn = signalConfig.useRdlvlGateEn
  val useWrlvlReq = signalConfig.useWrlvlReq
  val usePhyWrlvlCsN = signalConfig.usePhyWrlvlCsN
  val useWrlvlEn = signalConfig.useWrlvlEn
  val useWrlvlStrobe = signalConfig.useWrlvlStrobe
  val useWrlvlResp = signalConfig.useWrlvlResp
  val useCalvlReq = signalConfig.useCalvlReq
  val usePhyCalvlCsN = signalConfig.usePhyCalvlCsN
  val useCalvlEn = signalConfig.useCalvlEn
  val useCalvlCapture = signalConfig.useCalvlCapture
  val useCalvlResp = signalConfig.useCalvlResp
  val useLvlPattern = signalConfig.useLvlPattern
  val useLvlPeriodic = signalConfig.useLvlPeriodic
  val usePhylvlReqCsN = signalConfig.usePhylvlReqCsN
  val usePhylvlAckCsN = signalConfig.usePhylvlAckCsN
  val useLpCtrlReq = signalConfig.useLpCtrlReq
  val useLpDataReq = signalConfig.useLpDataReq
  val useLpWakeUp = signalConfig.useLpWakeUp
  val useLpAck = signalConfig.useLpAck
  val useError = signalConfig.useError
  val useErrorInfo = signalConfig.useErrorInfo

  val useCtrlupd = useCtrlupdReq & useCtrlupdAck
  val usePhyupd = usePhyupdReq & usePhyupdAck
  val useParity = useParityIn & useAlertN
  val usePhylvl = usePhylvlReqCsN & usePhylvlAckCsN
  val useLpData = useLpDataReq & useLpAck
  val useCrcMode = signalConfig.useCrcMode & useAlertN

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

  val controlWidth = 1
  val bankGroupWidth = bgWidth
  val chipIdWidth = cidWidth
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

}
