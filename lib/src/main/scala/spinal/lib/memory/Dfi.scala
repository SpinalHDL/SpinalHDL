package spinal.lib.memory

import spinal.core._
import spinal.lib._

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
    // dfi_rddata_valid -> dfi_rddata
    tPhyRdCslat: Int,
    // specify the desired alignment of the command to the
    // dfi_rddata_cs_n signal
    tPhyRdCsGap: Int,
    //specify the additional delay it
    //requires between two consecutive commands that are targeting different chip selects.
    phyCrcMode: Boolean,
    phyDbiMode: Boolean
) {
    val dfiRWLength = dramBurst / 2 / frequencyRatio
}

case class DfiConfig(
    frequencyRatio: Int, // PHY:MC
    dramBusWidth: Int,
    dramChipselectWidth: Int,
    dramBankWidth: Int,
    dramBgWidth: Int,
    dramCidWidth: Int,
    dramDataSlice: Int,

    useBank: Boolean,
    useAckN: Boolean,
    useRasN: Boolean,
    useCasN: Boolean,
    useWeN: Boolean,
    useBg: Boolean,
    useCid: Boolean,
    useOdt: Boolean,
    useResetN: Boolean,

    useWrdataCsN: Boolean,
    
    useRddataDbiN: Boolean,
    useRddataCsN: Boolean,
    useRddataDnv: Boolean,

    useDataByteDisable: Boolean,
    useFreqRatio: Boolean,
    useInitStart: Boolean,
    useParityIn: Boolean,
    useAlertN: Boolean,

    useRdlvlReq: Boolean,
    usePhyRdlvlCsN: Boolean,
    useRdlvlEn: Boolean,
    useRdlvlResp: Boolean,
    useRdlvlGateReq: Boolean,
    usePhyRdlvlGateCsN: Boolean,
    useRdlvlGateEn: Boolean,

    useWrlvlReq: Boolean,
    usePhyWrlvlCsN: Boolean,
    useWrlvlEn: Boolean,
    useWrlvlStrobe: Boolean,
    useWrlvlResp: Boolean,

    useCalvlReq: Boolean,
    usePhyCalvlCsN: Boolean,
    useCalvlEn: Boolean,
    useCalvlCapture: Boolean,
    useCalvlResp: Boolean,

    useLvlPattern: Boolean,
    useLvlPeriodic: Boolean,

    usePhylvlReqCsN: Boolean,
    usePhylvlAckCsN: Boolean,

    useLpCtrlReq: Boolean,
    useLpDataReq: Boolean,
    useLpWakeUp: Boolean,
    useLpAck: Boolean,

    useError: Boolean,
    useErrorInfo: Boolean
) {
    val addressWidth = dramBusWidth
    val bankWidth = dramBankWidth
    val controlWidth = 1
    val chipSelectWidth = dramChipselectWidth
    val bankGroupWidth = dramBgWidth
    val chipIdWidth = dramCidWidth
    val dataWidth = 2 * dramBusWidth
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
    val rankWidth = chipSelectWidth
    val errorWidth = dramDataSlice + 1
}

case class DfiControlInterface(config: DfiConfig) extends Bundle with IMasterSlave {
    val address = Bits(config.addressWidth * config.frequencyRatio bits)
    val bank = config.useBank generate Bits(config.bankWidth * config.frequencyRatio bits)
    val rasN = config.useRasN generate Bits(config.controlWidth * config.frequencyRatio bits)
    val casN = config.useCasN generate Bits(config.controlWidth * config.frequencyRatio bits)
    val weN = config.useWeN generate Bits(config.controlWidth * config.frequencyRatio bits)
    val csN = Bits(config.chipSelectWidth * config.frequencyRatio bits)
    val actN = config.useAckN generate Bits(config.frequencyRatio bits)
    val bg = config.useBg generate Bits(config.bankGroupWidth * config.frequencyRatio bits)
    val cid = config.useCid generate Bits(config.chipIdWidth * config.frequencyRatio bits)
    val cke = Bits(config.chipSelectWidth * config.frequencyRatio bits)
    val odt = config.useOdt generate Bits(config.chipSelectWidth * config.frequencyRatio bits)
    val resetN = config.useResetN generate Bits(config.chipSelectWidth * config.frequencyRatio bits)

    override def asMaster(): Unit = {
        out(address, bank, rasN, casN, weN, csN, actN, bg, cid, cke, odt, resetN)
    }
}

case class DfiWr(config: DfiConfig) extends Bundle {
    val wrdata = Bits(config.dataWidth / config.dataEnableWidth bits)
    val wrdataMask = Bits(config.dataWidth / 8 / config.dataEnableWidth bits)
    // config.dramDataSlice must >= 8, or else config.dataWidth / 8 / config.dataEnableWidth < 1
    val wrdataCsN = config.useWrdataCsN generate Bits(config.chipSelectWidth bits)
}

case class DfiWriteInterface(config: DfiConfig) extends Bundle with  IMasterSlave {
    val wr = Vec(Flow(DfiWr(config)), config.dataEnableWidth * config.frequencyRatio)
    override def asMaster(): Unit = {
        out(wr)
    }
}

case class DfiRd(config: DfiConfig) extends Bundle {
    val rddata = Bits(config.dataWidth / config.readDataValidWidth bits)
    val rddataDbiN = config.useRddataDbiN generate Bits(config.dbiWidth / config.readDataValidWidth bits)
    val rddataDnv = config.useRddataDnv generate Bits(config.dataWidth / 8 / config.readDataValidWidth bits)
}

case class DfiRdCs(config:DfiConfig) extends Bundle {
    val rddataCsN = config.useRddataCsN generate Bits(config.chipSelectWidth bits)
}

case class DfiReadInterface(config: DfiConfig) extends Bundle with IMasterSlave {
    val rdCs = Vec(Flow(DfiRdCs(config)), config.dataEnableWidth * config.frequencyRatio)
    val rd = Vec(Flow(DfiRd(config)), config.readDataValidWidth * config.frequencyRatio)
    override def asMaster(): Unit = {
        out(rdCs)
        in(rd)
    }
}

case class DfiUpdateInterface(config: DfiConfig) extends Bundle with IMasterSlave {
    val ctrlupdAck = Bool()
    val ctrlupdReq = Bool()
    val phyupdAck = Bool()
    val phyupdReq = Bool()
    val phyupdType = Bits(2 bits)
    override def asMaster(): Unit = {
        out(ctrlupdReq, phyupdAck)
        in(ctrlupdAck, phyupdReq, phyupdType)
    }
}

case class DfiStatusInterface(config: DfiConfig) extends Bundle with IMasterSlave {
    val alertN = config.useAlertN generate Bits(config.alertWidth * config.frequencyRatio bits)
    val dataByteDisable = config.useDataByteDisable generate Bits(config.dataWidth / 8 bits)
    val dramClkDisable = Bits(config.chipSelectWidth bits)
    val freqRatio = config.useFreqRatio generate Bits(2 bits)
    val initComplete = Bool()
    val initStart = config.useInitStart generate Bool()
    val parityIn = config.useParityIn generate Bits(config.frequencyRatio bits)
    override def asMaster(): Unit = {
        out(dataByteDisable, dramClkDisable, freqRatio, initStart, parityIn)
        in(initComplete, alertN)
    }
}

case class DfiReadTrainingInterface(config: DfiConfig) extends Bundle with IMasterSlave {
    val rdlvlReq = config.useRdlvlReq generate Bits(config.readLevelingPhyIFWidth bits)
    val phyRdlvlCsN = config.usePhyRdlvlCsN generate Bits(config.chipSelectWidth * config.readTrainingPhyIFWidth bits)
    val rdlvlEn = config.useRdlvlEn generate Bits(config.readLevelingMCIFWidth bits)
    val rdlvlResp = config.useRdlvlResp generate Bits(config.readLevelingResponseWidth bits)
    val rdlvlGateReq = config.useRdlvlGateReq generate Bits(config.readLevelingPhyIFWidth bits)
    val phyRdlvlGateCsN = config.usePhyRdlvlCsN generate Bits(config.readTrainingPhyIFWidth * config.chipSelectWidth bits)
    val rdlvlGateEn = config.useRdlvlGateEn generate Bits(config.readLevelingMCIFWidth bits)
    override def asMaster(): Unit = {
        out(rdlvlEn, rdlvlGateEn)
        in(rdlvlReq, phyRdlvlCsN, rdlvlResp, rdlvlGateReq, phyRdlvlGateCsN)
    }
}

case class DfiWriteTrainingInterface(config: DfiConfig) extends Bundle with IMasterSlave {
    val wrlvlReq = config.useWrlvlReq generate Bits(config.writeLevelingPhyIFWidth bits)
    val phyWrlvlCsN = config.usePhyWrlvlCsN generate Bits(config.chipSelectWidth * config.writeLevelingPhyIFWidth bits)
    val wrlvlEn = config.useWrlvlEn generate Bits(config.writeLevelingMCIFWidth bits)
    val wrlvlStrobe = config.useWrlvlStrobe generate Bits(config.writeLevelingMCIFWidth bits)
    val wrlvlResp = config.useWrlvlResp generate Bits(config.writeLevelingResponseWidth bits)
    override def asMaster(): Unit = {
        out(wrlvlEn, wrlvlStrobe)
        in(wrlvlReq, wrlvlResp, phyWrlvlCsN)
    }
}

case class DfiCATrainingInterface(config: DfiConfig) extends Bundle with IMasterSlave {
    val calvlReq = config.useCalvlReq generate Bits(config.caTrainingPhyIFWidth bits)
    val phyCalvlCsN = config.usePhyCalvlCsN generate Bits(config.chipSelectWidth bits)
    val calvlEn = config.useCalvlEn generate Bits(config.caTrainingMCIFWidth bits)
    val calvlCapture = config.useCalvlCapture generate Bits(config.caTrainingMCIFWidth bits)
    val calvlResp = config.useCalvlResp generate Bits(config.caTrainingResponseWidth bits)
    override def asMaster(): Unit = {
        out(calvlCapture, calvlEn)
        in(phyCalvlCsN, calvlReq, calvlResp)
    }
}

case class DfiLevelingTraingInterface(config: DfiConfig) extends Bundle with IMasterSlave {
    val lvlPattern = config.useLvlPattern generate Bits(4 * config.readTrainingPhyIFWidth bits)
    val lvlPeriodic = config.useLvlPeriodic generate Bits(config.levelingPhyIFWidth bits)
    override def asMaster(): Unit = {
        out(lvlPattern, lvlPeriodic)
    }
}

case class DfiPhyRequesetedTrainingInterface(config: DfiConfig) extends Bundle with IMasterSlave {
    val phylvlReqCsN = config.usePhylvlReqCsN generate Bits(config.rankWidth bits)
    val phylvlAckCsN = config.usePhylvlAckCsN generate Bits(config.chipSelectWidth bits)

    override def asMaster(): Unit = {
        out(phylvlReqCsN)
        in(phylvlAckCsN)
    }
}

case class DfiLowPowerControlInterface(config: DfiConfig) extends Bundle with IMasterSlave {
    val lpCtrlReq = config.useLpCtrlReq generate Bool()
    val lpDataReq = config.useLpDataReq generate Bool()
    val lpWakeUp  = config.useLpWakeUp generate  Bits(4 bits)
    val lpAck     = config.useLpAck generate Bool()
    override def asMaster(): Unit = {
        out(lpCtrlReq, lpDataReq, lpWakeUp)
        in(lpAck)
    }
}

case class DfiErrorInterface(config: DfiConfig) extends Bundle with IMasterSlave {
    val error = config.useError generate Bits(config.errorWidth bits)
    val error_info = config.useErrorInfo generate Bits(config.errorWidth * 4 bits)
    override def asMaster(): Unit = {
        in(error, error_info)
    }
}

case class Dfi(config: DfiConfig, timeConfig: DfiTimeConfig) extends Bundle with IMasterSlave {
    val control = DfiControlInterface(config)
    val write = DfiWriteInterface(config)
    val read = DfiReadInterface(config)
    val updata = DfiUpdateInterface(config)
    val status = DfiStatusInterface(config)
    val rdTraining = DfiReadTrainingInterface(config)
    val wrTraining = DfiWriteTrainingInterface(config)
    val caTraining = DfiCATrainingInterface(config)
    val levelingTraing = DfiLevelingTraingInterface(config)
    val phyRequesetedTraining = DfiPhyRequesetedTrainingInterface(config)
    val lowPowerControl = DfiLowPowerControlInterface(config)
    val error = DfiErrorInterface(config)
    override def asMaster(): Unit = {
        master(control, read, status, write,
        rdTraining, wrTraining, caTraining, levelingTraing,
        phyRequesetedTraining, lowPowerControl,
        error)
    }
}
