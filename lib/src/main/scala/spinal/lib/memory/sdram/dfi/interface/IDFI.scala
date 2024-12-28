package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._

case class DfiCmd(config: DfiConfig) extends Bundle {
  val cid = config.useCid generate Bits(config.chipIdWidth bits)
  val actN = config.useAckN generate Bool()
  val weN = config.useWeN generate Bool()
  val casN = config.useCasN generate Bool()
  val rasN = config.useRasN generate Bool()
  val csN = Bits(config.chipSelectNumber bits)
}

case class DfiOdt(config: DfiConfig) extends Bundle {
  val odt = config.useOdt generate Bits(config.chipSelectNumber bits)
}

case class DfiAddr(config: DfiConfig) extends Bundle {
  val bg = config.useBg generate Bits(config.bankGroupWidth bits)
  val bank = config.useBank generate Bits(config.bankWidth bits)
  val address = Bits(config.addressWidth bits)
}

case class DfiWrData(config: DfiConfig) extends Bundle {
  val wrData = Bits(config.dataWidth bits)
  val wrDataMask = Bits(config.dataWidth / 8 bits)
}

case class DfiWrCs(config: DfiConfig) extends Bundle {
  val wrCs = config.useWrdataCsN generate Bits(config.chipSelectNumber bits)
}

case class DfiRdData(config: DfiConfig) extends Bundle {
  val rdData = Bits(config.dataWidth bits)
  val rdDataDbiN = config.useRddataDbiN generate Bits(config.dbiWidth bits)
  val rdDataDnv = config.useRddataDnv generate Bits(config.dataWidth / 8 bits)
}

case class DfiReadCs(config: DfiConfig) extends Bundle {
  val rdCs = config.useRddataCsN generate Bits(config.chipSelectNumber bits)
}

case class DfiCtrlUp(config: DfiConfig) {}

case class DfiPhyUp(config: DfiConfig) extends Bundle {
  val phyupType = config.usePhyupdType generate Bits(2 bits)
}

case class DfiInit(config: DfiConfig) extends Bundle {
  val dataByteDisable = config.useDataByteDisable generate Bits(config.dataWidth / 8 bits)
  val freqRatio = config.useFreqRatio generate Bits(2 bits)
}

case class DfiRdLvlCs(config: DfiConfig) extends Bundle {
  val rdLvlCs = config.usePhyRdlvlCsN generate Bits(config.chipSelectNumber bits)
}

case class DfiRdLvl(config: DfiConfig) extends Bundle {
  val lvlPeriodic = config.useLvlPeriodic generate Bool()
  val lvlPattern = config.useLvlPattern generate Bits(4 bits)
}

case class DfiRdGateCs(config: DfiConfig) extends Bundle {
  val rdGateCs = config.usePhyRdlvlGateCsN generate Bits(config.chipSelectNumber bits)
}

case class DfiRdGate(config: DfiConfig) extends Bundle {
  val lvlPeriodic = config.useLvlPeriodic generate Bool()
  val lvlPattern = config.useLvlPattern generate Bits(4 bits)
}

case class DfiWrLvlCs(config: DfiConfig) extends Bundle {
  val wrLvlCs = config.usePhyWrlvlCsN generate Bits(config.chipSelectNumber bits)
}

case class DfiWrLvl(config: DfiConfig) extends Bundle {
  val lvlPeriodic = config.useLvlPeriodic generate Bool()
  val strobe = config.useWrlvlStrobe generate Bool()
}

case class DfiPhyLvlCs(config: DfiConfig) extends Bundle {}

case class DfiLpCtrl(config: DfiConfig) extends Bundle {
  val lpCtrlReq = config.useLpCtrlReq generate Bool()
}

case class DfiLp(config: DfiConfig) extends Bundle {
  val lpWakeUp = config.useLpWakeUp generate Bits(4 bits)
}

case class DfiError(config: DfiConfig) extends Bundle {
  val info = config.useErrorInfo generate Bits(4 bits)
}

case class IDFI(config: DfiConfig) extends Bundle with IMasterSlave {
  val frequencyRatio = config.frequencyRatio
  val chipSelectNumber = config.chipSelectNumber
  val dataSlice = config.dataSlice

  val cke = Vec(Bits(chipSelectNumber bits), frequencyRatio)
  val rstN = config.useResetN generate Vec(Bits(chipSelectNumber bits), frequencyRatio)
  val cmd = Vec(master(Flow(DfiCmd(config))), frequencyRatio)
  val odt = Vec(master(Flow(DfiOdt(config))), frequencyRatio)
  val address = Vec(master(Flow(DfiAddr(config))), frequencyRatio)
  val wrData = Vec(master(Flow(DfiWrData(config))), frequencyRatio)
  val wrCs = config.useWrdataCsN generate Vec(master(Flow(DfiWrCs(config))), frequencyRatio)
  val rdEn = Vec(Bool(), frequencyRatio)
  val rdData = Vec(slave(Stream(Fragment(DfiRdData(config)))), frequencyRatio)
  val rdCs = config.useRddataCsN generate Vec(master(Flow(DfiReadCs(config))), frequencyRatio)
  val ctrlUp = config.useCtrlupd generate Stream(Event)
  val phyUp = config.usePhyupd generate Stream(DfiPhyUp(config))
  val clkDisable = master(Flow(Bits(chipSelectNumber bits)))
  val init = config.useInitStart generate Stream(DfiInit(config))
  val parity = config.useParity generate Vec(master(Stream(Event)), frequencyRatio)
  val crc = config.useCrcMode generate Vec(master(Stream(Event)), frequencyRatio)
  val rdLvlCs = config.useRdlvlReq & config.useRdlvlEn generate Vec(slave(Stream(DfiRdLvlCs(config))), dataSlice)
  val rdLvl = config.useRdlvlResp & config.useRdlvlEn generate Vec(master(Stream(DfiRdLvl(config))), dataSlice)
  val rdGataCs =
    config.useRdlvlGateReq & config.useRdlvlGateEn generate Vec(slave(Stream(DfiRdGateCs(config))), dataSlice)
  val rdGate = config.useRdlvlResp & config.useRdlvlGateEn generate Vec(master(Stream(DfiRdGate(config))), dataSlice)
  val wrLvlCs = config.useWrlvlReq & config.useWrlvlEn generate Vec(slave(Stream(DfiWrLvlCs(config))), dataSlice)
  val wrLvl = config.useWrlvlResp & config.useWrlvlEn generate Vec(master(Stream(DfiWrLvl(config))), dataSlice)
  val phyLvl = config.usePhylvl generate Vec(slave(Stream(Event)), chipSelectNumber)
  val lpCtrlReq = Flow(DfiLpCtrl(config))
  val lowPower = config.useLpData generate Stream(DfiLp(config))
  val error = config.useError generate Vec(master(Flow(DfiError(config))), config.errorNumber)

  override def asMaster(): Unit = {
    master(ctrlUp, clkDisable, init, lpCtrlReq, lowPower)
    slave(phyUp)
    out(cke, rstN, rdEn)
  }

}
