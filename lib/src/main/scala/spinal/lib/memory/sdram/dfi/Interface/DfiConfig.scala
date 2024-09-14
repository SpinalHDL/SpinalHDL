package spinal.lib.memory.sdram.dfi.Interface

import spinal.core.{Bundle, assert}
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
                          dramBurst: Int,
                          frequencyRatio: Int, // PHY:MC
                          tRddataEn: Int,
                          // defines the timing requirements between the read command on the DFI interface and the
                          // assertion of the dfi_rddata_en signal to maintain synchronicity between the MC and the PHY for the start of
                          // contiguous read data expected on the DFI interface.
                          tPhyRdlat: Int,
                          // dfi_rddata_en -> dfi_rddata/dfi_rddata_valid
                          tPhyRdCslat: Int,
                          // specify the desired alignment of the command to the
                          // dfi_rddata_cs_n signal
                          tPhyRdCsGap: Int,
                          //specify the additional delay it
                          //requires between two consecutive commands that are targeting different chip selects.
                          phyCrcMode: Boolean=false,
                          phyDbiMode: Boolean=false
                        ) {
  val dfiRWLength = dramBurst / 2 / frequencyRatio
}



object DDR {
  abstract class DDRInterface(
                               val useCtrlSignals:Boolean=true,
                               val useWrDataSignals:Boolean=true,
                               val useRdDataSignals:Boolean=true,
                               val useUpdateSignals:Boolean=false,
                               val useStatusSignals:Boolean=false,
                               val useTrainingSignals:Boolean=false,
                               val useLowPowerSignals:Boolean=false,
                               val useErrorSignals:Boolean=false
                             )
  abstract class DDRConfig extends DDRInterface {
    def useBank = true & useCtrlSignals
    def useAckN = false & useCtrlSignals
    def useRasN = true & useCtrlSignals
    def useCasN = true & useCtrlSignals
    def useWeN = true & useCtrlSignals
    def useBg = false & useCtrlSignals
    def useCid = false & useCtrlSignals
    def useOdt = false & useCtrlSignals
    def useResetN = false & useCtrlSignals

    def useWrdataCsN = true & useWrDataSignals

    def useRddataDbiN = false & useRdDataSignals
    def useRddataCsN = true & useRdDataSignals
    def useRddataDnv = false & useRdDataSignals

    def useCtrlupdReq = true & useUpdateSignals
    def useCtrlupdAck = true & useUpdateSignals
    def usePhyupdReq = true & useUpdateSignals
    def usePhyupdAck = true & useUpdateSignals
    def usePhyupdType = true & useUpdateSignals

    def useDataByteDisable = true & useStatusSignals
    def useFreqRatio = true & useStatusSignals
    def useInitStart = true & useStatusSignals
    def useParityIn =  true & useStatusSignals
    def useAlertN =  true & useStatusSignals

    def useRdlvlReq =  false & useTrainingSignals
    def usePhyRdlvlCsN = false & useTrainingSignals
    def useRdlvlEn = false & useTrainingSignals
    def useRdlvlResp = false & useTrainingSignals
    def useRdlvlGateReq = false & useTrainingSignals
    def usePhyRdlvlGateCsN = false & useTrainingSignals
    def useRdlvlGateEn = false & useTrainingSignals
    def useWrlvlReq = false & useTrainingSignals
    def usePhyWrlvlCsN = false & useTrainingSignals
    def useWrlvlEn = false & useTrainingSignals
    def useWrlvlStrobe = false & useTrainingSignals
    def useWrlvlResp = false & useTrainingSignals
    def useCalvlReq = false & useTrainingSignals
    def usePhyCalvlCsN = false & useTrainingSignals
    def useCalvlEn = false & useTrainingSignals
    def useCalvlCapture = false & useTrainingSignals
    def useCalvlResp = false & useTrainingSignals
    def useLvlPattern = false & useTrainingSignals
    def useLvlPeriodic = true & useTrainingSignals
    def usePhylvlReqCsN = false & useTrainingSignals
    def usePhylvlAckCsN = false & useTrainingSignals

    def useLpCtrlReq = true & useLowPowerSignals
    def useLpDataReq = true & useLowPowerSignals
    def useLpWakeUp = true & useLowPowerSignals
    def useLpAck = true & useLowPowerSignals

    def useError = true & useErrorSignals
    def useErrorInfo = true & useErrorSignals
  }
  abstract class DDR1Config extends DDRConfig
  abstract class DDR2Config extends DDR1Config{
    override def useOdt = true & useCtrlSignals
  }
  abstract class DDR3Config extends DDR2Config{
    override def useResetN = true & useCtrlSignals

    override def useRdlvlReq =  true & useTrainingSignals
    override def usePhyRdlvlCsN = true & useTrainingSignals
    override def useRdlvlEn = true & useTrainingSignals
    override def useRdlvlResp = true & useTrainingSignals
    override def useRdlvlGateReq = true & useTrainingSignals
    override def usePhyRdlvlGateCsN = true & useTrainingSignals
    override def useRdlvlGateEn = true & useTrainingSignals
    override def useWrlvlReq = true & useTrainingSignals
    override def usePhyWrlvlCsN = true & useTrainingSignals
    override def useWrlvlEn = true & useTrainingSignals
    override def useWrlvlStrobe = true & useTrainingSignals
    override def useWrlvlResp = true & useTrainingSignals
  }
  abstract class DDR4Config extends DDR3Config{
    override def useAckN = true & useCtrlSignals
    override def useBg = true & useCtrlSignals
    override def useCid = true & useCtrlSignals

    override def useRddataDbiN = true & useRdDataSignals
    override def useLvlPattern = true & useTrainingSignals
    override def usePhylvlReqCsN = true & useTrainingSignals
    override def usePhylvlAckCsN = true & useTrainingSignals
  }
  object DDR1 extends DDR1Config
  object DDR2 extends DDR2Config
  object DDR3 extends DDR3Config
  object DDR4 extends DDR4Config

  object MYDDR extends DDR3Config{
    override def useOdt: Boolean = false
    override def useResetN: Boolean = false
    override def useWrdataCsN: Boolean = false
    override def useRddataCsN: Boolean = false
  }
}
class DDR(generation:Int = 0,isPLDDR:Boolean = false){
  val kind = if(!isPLDDR){
    generation match {
      case 1 => DDR.DDR1
      case 2 => DDR.DDR2
      case 3 => DDR.DDR3
      case 4 => DDR.DDR4

      case _ => DDR.MYDDR
    }
  }else{
    null
  }
}




case class DfiConfig(
                      frequencyRatio: Int, // PHY:MC
                      dramAddrWidth: Int,
                      dramDataWidth: Int,
                      dramChipselectNumber: Int,
                      dramBankWidth: Int,
                      dramBgWidth: Int,
                      dramCidWidth: Int,
                      dramDataSlice: Int,
                      cmdPhase:Int,
                      //                      ddrGC: DDRGenerationConfig
                      ddr:DDR,
                      timeConfig:DfiTimeConfig
                    ) {
  assert(ddr.kind != null)
  val useBank = ddr.kind.useBank
  val useAckN = ddr.kind.useAckN
  val useRasN = ddr.kind.useRasN
  val useCasN = ddr.kind.useCasN
  val useWeN = ddr.kind.useWeN
  val useBg = ddr.kind.useBg
  val useCid = ddr.kind.useCid
  val useOdt = ddr.kind.useOdt
  val useResetN = ddr.kind.useResetN
  val useWrdataCsN = ddr.kind.useWrdataCsN
  val useRddataDbiN = ddr.kind.useRddataDbiN
  val useRddataCsN = ddr.kind.useRddataCsN
  val useRddataDnv = ddr.kind.useRddataDnv
  val useCtrlupdReq = ddr.kind.useCtrlupdReq
  val useCtrlupdAck = ddr.kind.useCtrlupdAck
  val usePhyupdReq = ddr.kind.usePhyupdReq
  val usePhyupdAck = ddr.kind.usePhyupdAck
  val usePhyupdType = ddr.kind.usePhyupdType
  val useDataByteDisable = ddr.kind.useDataByteDisable
  val useFreqRatio = ddr.kind.useFreqRatio
  val useInitStart = ddr.kind.useInitStart
  val useParityIn = ddr.kind.useParityIn
  val useAlertN = ddr.kind.useAlertN
  val useRdlvlReq = ddr.kind.useRdlvlGateReq
  val usePhyRdlvlCsN =ddr.kind.usePhyRdlvlCsN
  val useRdlvlEn = ddr.kind.useRdlvlEn
  val useRdlvlResp = ddr.kind.useRdlvlResp
  val useRdlvlGateReq =ddr.kind.useRdlvlGateReq
  val usePhyRdlvlGateCsN = ddr.kind.usePhyRdlvlGateCsN
  val useRdlvlGateEn =ddr.kind.useRdlvlGateEn
  val useWrlvlReq = ddr.kind.useWrlvlReq
  val usePhyWrlvlCsN =ddr.kind.usePhyWrlvlCsN
  val useWrlvlEn = ddr.kind.useWrlvlEn
  val useWrlvlStrobe =ddr.kind.useWrlvlStrobe
  val useWrlvlResp = ddr.kind.useWrlvlResp
  val useCalvlReq = ddr.kind.useCalvlReq
  val usePhyCalvlCsN =ddr.kind.usePhyCalvlCsN
  val useCalvlEn = ddr.kind.useCalvlEn
  val useCalvlCapture =ddr.kind.useCalvlCapture
  val useCalvlResp = ddr.kind.useCalvlResp
  val useLvlPattern = ddr.kind.useLvlPattern
  val useLvlPeriodic =ddr.kind.useLvlPeriodic
  val usePhylvlReqCsN =ddr.kind.usePhylvlReqCsN
  val usePhylvlAckCsN =ddr.kind.usePhylvlAckCsN
  val useLpCtrlReq = ddr.kind.useLpCtrlReq
  val useLpDataReq =ddr.kind.useLpDataReq
  val useLpWakeUp =ddr.kind.useLpWakeUp
  val useLpAck = ddr.kind.useLpAck
  val useError = ddr.kind.useError
  val useErrorInfo =ddr.kind.useErrorInfo

  val useCtrlupd = useCtrlupdReq & useCtrlupdAck
  val usePhyupd = usePhyupdReq & usePhyupdAck
  val useParity = useParityIn & useAlertN
  val usePhylvl = usePhylvlReqCsN & usePhylvlAckCsN
  val useLpData = useLpDataReq & useLpAck

  val addressWidth = dramAddrWidth
  val bankWidth = dramBankWidth
  val controlWidth = 1
  val chipSelectNumber = dramChipselectNumber
  val bankGroupWidth = dramBgWidth
  val chipIdWidth = dramCidWidth
  val dataWidth = dramDataWidth
  val dataEnableWidth = dataWidth / dramDataSlice
  // For PHYs with an 8-bit slice, this is generally 1/16th of the DFI Data Width to provide a single enable bit per
  // memory data slice, but may be 1/4, 1/8, 1/32, or any other ratio.
  val dbiWidth = dataWidth / 8
  val readDataValidWidth = dataEnableWidth
  val alertWidth = 1
  val readLevelingPhyIFWidth = dramDataSlice
  val readLevelingMCIFWidth = readLevelingPhyIFWidth
  val readTrainingPhyIFWidth = readLevelingPhyIFWidth
  val readLevelingResponseWidth = dramDataSlice
  val writeLevelingPhyIFWidth = dramDataSlice
  val writeLevelingMCIFWidth = dramDataSlice
  val writeLevelingResponseWidth = dramDataSlice
  val caTrainingPhyIFWidth = dramDataSlice
  val caTrainingMCIFWidth = dramDataSlice
  val caTrainingResponseWidth = 2
  val levelingPhyIFWidth = dramDataSlice
  val rankWidth = chipSelectNumber
  val errorNumber = dramDataSlice + 1
}
