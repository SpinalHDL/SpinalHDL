//package spinal.lib.memory
package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

case class DfiControlInterface(config: DfiConfig) extends Bundle with IMasterSlave {
  val address = Bits(config.addressWidth * config.frequencyRatio bits)
  val bank = config.useBank generate Bits(config.bankWidth * config.frequencyRatio bits)
  val rasN = config.useRasN generate Bits(config.controlWidth * config.frequencyRatio bits)
  val casN = config.useCasN generate Bits(config.controlWidth * config.frequencyRatio bits)
  val weN = config.useWeN generate Bits(config.controlWidth * config.frequencyRatio bits)
  val csN = Bits(config.chipSelectNumber * config.frequencyRatio bits)
  val actN = config.useAckN generate Bits(config.frequencyRatio bits)
  val bg = config.useBg generate Bits(config.bankGroupWidth * config.frequencyRatio bits)
  val cid = config.useCid generate Bits(config.chipIdWidth * config.frequencyRatio bits)
  val cke = Bits(config.chipSelectNumber * config.frequencyRatio bits)
  val odt = config.useOdt generate Bits(config.chipSelectNumber * config.frequencyRatio bits)
  val resetN = config.useResetN generate Bits(config.chipSelectNumber * config.frequencyRatio bits)

  override def asMaster(): Unit = {
    out(address, bank, rasN, casN, weN, csN, actN, bg, cid, cke, odt, resetN)
  }
}

case class DfiWr(config: DfiConfig) extends Bundle {
  val wrdataEn = Bool()
  val wrdata = Bits(config.dataWidth bits)
  val wrdataMask = Bits(config.dataWidth / 8 bits)
  // config.dramDataSlice must >= 8, or else config.dataWidth / 8 / config.dataEnableWidth < 1
  val wrdataCsN = config.useWrdataCsN generate Bits(config.chipSelectNumber bits)
}

case class DfiWriteInterface(config: DfiConfig) extends Bundle with IMasterSlave {
  val wr = Vec(DfiWr(config), config.frequencyRatio)
  override def asMaster(): Unit = {
    out(wr)
  }
}

case class DfiRd(config: DfiConfig) extends Bundle {
  val rddataValid = Bool()
  val rddata = Bits(config.dataWidth bits)
  val rddataDbiN = config.useRddataDbiN generate Bits(config.dbiWidth bits)
  val rddataDnv = config.useRddataDnv generate Bits(config.dataWidth / 8 bits)
}

case class DfiRdCs(config: DfiConfig) extends Bundle {
  val rddataCsN = config.useRddataCsN generate Bits(config.chipSelectNumber bits)
}

case class DfiReadInterface(config: DfiConfig) extends Bundle with IMasterSlave {
  val rden = Vec(Bool(), config.frequencyRatio)
  val rdCs = Vec(DfiRdCs(config), config.frequencyRatio)
  val rd = Vec(DfiRd(config), config.frequencyRatio)
  override def asMaster(): Unit = {
    out(rdCs, rden)
    in(rd)
  }
}

case class DfiUpdateInterface(config: DfiConfig) extends Bundle with IMasterSlave {
  val ctrlupdAck = config.useCtrlupdReq generate Bool()
  val ctrlupdReq = config.useCtrlupdAck generate Bool()
  val phyupdAck = config.usePhyupdAck generate Bool()
  val phyupdReq = config.usePhyupdReq generate Bool()
  val phyupdType = config.usePhyupdType generate Bits(2 bits)
  override def asMaster(): Unit = {
    out(ctrlupdReq, phyupdAck)
    in(ctrlupdAck, phyupdReq, phyupdType)
  }
}

case class DfiStatusInterface(config: DfiConfig) extends Bundle with IMasterSlave {
  val alertN = config.useAlertN generate Bits(config.alertWidth * config.frequencyRatio bits)
  val dataByteDisable = config.useDataByteDisable generate Bits(config.dataWidth / 8 bits)
  val dramClkDisable = config.useStatusSignals generate Bits(config.chipSelectNumber bits)
  val freqRatio = config.useFreqRatio generate Bits(2 bits)
  val initComplete = config.useInitStart generate Bool()
  val initStart = config.useInitStart generate Bool()
  val parityIn = config.useParityIn generate Bits(config.frequencyRatio bits)
  override def asMaster(): Unit = {
    out(dataByteDisable, dramClkDisable, freqRatio, initStart, parityIn)
    in(initComplete, alertN)
  }
}

case class DfiReadTrainingInterface(config: DfiConfig) extends Bundle with IMasterSlave {
  val rdlvlReq = config.useRdlvlReq generate Bits(config.readLevelingPhyIFWidth bits)
  val phyRdlvlCsN = config.usePhyRdlvlCsN generate Bits(config.chipSelectNumber * config.readTrainingPhyIFWidth bits)
  val rdlvlEn = config.useRdlvlEn generate Bits(config.readLevelingMCIFWidth bits)
  val rdlvlResp = config.useRdlvlResp generate Bits(config.readLevelingResponseWidth bits)
  val rdlvlGateReq = config.useRdlvlGateReq generate Bits(config.readLevelingPhyIFWidth bits)
  val phyRdlvlGateCsN =
    config.usePhyRdlvlGateCsN generate Bits(config.readTrainingPhyIFWidth * config.chipSelectNumber bits)
  val rdlvlGateEn = config.useRdlvlGateEn generate Bits(config.readLevelingMCIFWidth bits)
  override def asMaster(): Unit = {
    out(rdlvlEn, rdlvlGateEn)
    in(rdlvlReq, phyRdlvlCsN, rdlvlResp, rdlvlGateReq, phyRdlvlGateCsN)
  }
}

case class DfiWriteTrainingInterface(config: DfiConfig) extends Bundle with IMasterSlave {
  val wrlvlReq = config.useWrlvlReq generate Bits(config.writeLevelingPhyIFWidth bits)
  val phyWrlvlCsN = config.usePhyWrlvlCsN generate Bits(config.chipSelectNumber * config.writeLevelingPhyIFWidth bits)
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
  val phyCalvlCsN = config.usePhyCalvlCsN generate Bits(config.chipSelectNumber bits)
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
  val phylvlAckCsN = config.usePhylvlAckCsN generate Bits(config.chipSelectNumber bits)

  override def asMaster(): Unit = {
    out(phylvlReqCsN)
    in(phylvlAckCsN)
  }
}

case class DfiLowPowerControlInterface(config: DfiConfig) extends Bundle with IMasterSlave {
  val lpCtrlReq = config.useLpCtrlReq generate Bool()
  val lpDataReq = config.useLpDataReq generate Bool()
  val lpWakeUp = config.useLpWakeUp generate Bits(4 bits)
  val lpAck = config.useLpAck generate Bool()
  override def asMaster(): Unit = {
    out(lpCtrlReq, lpDataReq, lpWakeUp)
    in(lpAck)
  }
}

case class DfiErrorInterface(config: DfiConfig) extends Bundle with IMasterSlave {
  val error = config.useError generate Bits(config.errorNumber bits)
  val error_info = config.useErrorInfo generate Bits(config.errorNumber * 4 bits)
  override def asMaster(): Unit = {
    in(error, error_info)
  }
}

case class Dfi(config: DfiConfig) extends Bundle with IMasterSlave {
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
    master(
      control,
      read,
      status,
      write,
      updata,
      rdTraining,
      wrTraining,
      caTraining,
      levelingTraing,
      phyRequesetedTraining,
      lowPowerControl,
      error
    )
  }
}
