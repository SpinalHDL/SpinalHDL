package spinal.lib.memory.sdram.dfi.interface

import spinal.core._
import spinal.lib._

case class DfiCmd(config: DfiConfig) extends Bundle {
  import config._
  val cid = useCid generate Bits(chipIdWidth bits)
  val actN = useAckN generate Bool()
  val weN = useWeN generate Bool()
  val casN = useCasN generate Bool()
  val rasN = useRasN generate Bool()
  val csN = Bits(chipSelectNumber bits)
}

case class DfiOdt(config: DfiConfig) extends Bundle {
  val odt = config.useOdt generate Bits(config.chipSelectNumber bits)
}

case class DfiAddr(config: DfiConfig) extends Bundle {
  import config._
  val bg = useBg generate Bits(bankGroupWidth bits)
  val bank = useBank generate Bits(bankWidth bits)
  val address = Bits(addressWidth bits)
}

case class DfiWrData(config: DfiConfig) extends Bundle {
  val wrData = Bits(config.dataWidth bits)
  val wrDataMask = Bits(config.dataWidth / 8 bits)
}

case class DfiWrCs(config: DfiConfig) extends Bundle {
  val wrCs = config.useWrdataCsN generate Bits(config.chipSelectNumber bits)
}

case class DfiRdData(config: DfiConfig) extends Bundle {
  import config._
  val rdData = Bits(dataWidth bits)
  val rdDataDbiN = useRddataDbiN generate Bits(dbiWidth bits)
  val rdDataDnv = useRddataDnv generate Bits(dataWidth / 8 bits)
}

case class DfiReadCs(config: DfiConfig) extends Bundle {
  val rdCs = config.useRddataCsN generate Bits(config.chipSelectNumber bits)
}

case class DfiCtrlUp(config: DfiConfig) {}

case class DfiPhyUp(config: DfiConfig) extends Bundle {
  val phyupType = config.usePhyupdType generate Bits(2 bits)
}

case class DfiInit(config: DfiConfig) extends Bundle {
  import config._
  val dataByteDisable = useDataByteDisable generate Bits(dataWidth / 8 bits)
  val freqRatio = useFreqRatio generate Bits(2 bits)
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
  import config._
  val cke = Vec(Bits(chipSelectNumber bits), frequencyRatio)
  val rstN = useResetN generate Vec(Bits(chipSelectNumber bits), frequencyRatio)
  val cmd = Vec(master(Flow(DfiCmd(config))), frequencyRatio)
  val odt = Vec(master(Flow(DfiOdt(config))), frequencyRatio)
  val address = Vec(master(Flow(DfiAddr(config))), frequencyRatio)
  val wrData = Vec(master(Flow(DfiWrData(config))), frequencyRatio)
  val wrCs = Vec(master(Flow(DfiWrCs(config))), frequencyRatio)
  val rdEn = Vec(Bool(), frequencyRatio)
  val rdData = Vec(slave(Stream(Fragment(DfiRdData(config)))), frequencyRatio)
  val rdCs = Vec(master(Flow(DfiReadCs(config))), frequencyRatio)
  val ctrlUp = useCtrlupd generate Stream(Event)
  val phyUp = usePhyupd generate Stream(DfiPhyUp(config))
  val clkDisable = master(Flow(Bits(chipSelectNumber bits)))
  val init = useInitStart generate Stream(DfiInit(config))
  val parity = useParity generate Vec(master(Stream(Event)), frequencyRatio)
  val crc = useCrcMode generate Vec(master(Stream(Event)), frequencyRatio)
  val rdLvlCs = useRdlvlReq & useRdlvlEn generate Vec(slave(Stream(DfiRdLvlCs(config))), dataSlice)
  val rdLvl = useRdlvlResp & useRdlvlEn generate Vec(master(Stream(DfiRdLvl(config))), dataSlice)
  val rdGataCs = useRdlvlGateReq & useRdlvlGateEn generate Vec(slave(Stream(DfiRdGateCs(config))), dataSlice)
  val rdGate = useRdlvlResp & useRdlvlGateEn generate Vec(master(Stream(DfiRdGate(config))), dataSlice)
  val wrLvlCs = useWrlvlReq & useWrlvlEn generate Vec(slave(Stream(DfiWrLvlCs(config))), dataSlice)
  val wrLvl = useWrlvlResp & useWrlvlEn generate Vec(master(Stream(DfiWrLvl(config))), dataSlice)
  val phyLvl = usePhylvl generate Vec(slave(Stream(Event)), chipSelectNumber)
  val lpCtrlReq = Flow(DfiLpCtrl(config))
  val lowPower = useLpData generate Stream(DfiLp(config))
  val error = useError generate Vec(master(Flow(DfiError(config))), errorNumber)

  override def asMaster(): Unit = {
    master(ctrlUp, clkDisable, init, lpCtrlReq, lowPower)
    slave(phyUp)
    out(cke, rstN, rdEn)
  }

}
