package spinal.lib.memory.sdram.Dfi.Interface

import spinal.core._
import spinal.lib._

case class DfiCmd(config: DfiConfig) extends Bundle{
  import config._
  val cid =  useCid generate Bits( chipIdWidth bits)
  val actN =  useAckN generate Bool()
  val weN =  useWeN generate Bool()
  val casN =  useCasN generate Bool()
  val rasN =  useRasN generate Bool()
  val csN = Bits( chipSelectNumber bits)
}

case class Dfiodt(config: DfiConfig) extends Bundle{
  val odt = config.useOdt generate Bits(config.chipSelectNumber bits)
}

case class DfiAddr(config: DfiConfig) extends Bundle{
  import config._
  val bg =  useBg generate Bits( bankGroupWidth bits)
  val bank =  useBank generate Bits( bankWidth bits)
  val address = Bits( addressWidth bits)
}

case class DfiWrdata(config: DfiConfig) extends Bundle{
  val wrdata = Bits(config.dataWidth bits)
  val wrdataMask = Bits(config.dataWidth / 8 bits)
}

case class DfiWrcs(config: DfiConfig) extends Bundle{
  val wrcs = config.useWrdataCsN generate Bits(config.chipSelectNumber bits)
}

case class Dfirddata(config: DfiConfig) extends Bundle{
  import config._
  val rddata = Bits( dataWidth bits)
  val rddataDbiN =  useRddataDbiN generate Bits( dbiWidth bits)
  val rddataDnv =  useRddataDnv generate Bits( dataWidth / 8 bits)
}

case class Dfireadcs(config: DfiConfig) extends Bundle{
  val rdcs =  config.useRddataCsN generate Bits( config.chipSelectNumber bits)
}

case class Dfictrlup(config: DfiConfig){}

case class Dfiphyup(config: DfiConfig) extends Bundle{
  val phyuptype =  config.usePhyupdType generate Bits(2 bits)
}

case class Dfiinit(config: DfiConfig) extends  Bundle{
  import config._
  val dataByteDisable =  useDataByteDisable generate Bits( dataWidth / 8 bits)
  val freqRatio =  useFreqRatio generate Bits(2 bits)
}

case class Dfirdlvlcs(config: DfiConfig) extends  Bundle{
  val rdlvlcs = config.usePhyRdlvlCsN generate Bits(config.chipSelectNumber bits)
}

case class Dfirdlvl(config: DfiConfig) extends  Bundle{
  val lvlperiodic = config.useLvlPeriodic generate Bool()
  val lvlPattern = config.useLvlPattern generate Bits(4 bits)
}

case class Dfirdgatecs(config: DfiConfig) extends  Bundle{
  val rdgatecs = config.usePhyRdlvlGateCsN generate Bits(config.chipSelectNumber bits)
}

case class Dfirdgate(config: DfiConfig) extends  Bundle{
  val lvlperiodic = config.useLvlPeriodic generate Bool()
  val lvlPattern = config.useLvlPattern generate Bits(4 bits)
}

case class Dfiwrlvlcs(config: DfiConfig) extends  Bundle{
  val wrlvlcs = config.usePhyWrlvlCsN generate Bits(config.chipSelectNumber bits)
}

case class Dfiwrlvl(config: DfiConfig) extends  Bundle{
  val lvlperiodic = config.useLvlPeriodic generate Bool()
  val strobe = config.useWrlvlStrobe generate Bool()
}

case  class Dfiphylvlcs(config: DfiConfig) extends Bundle{}

case  class Dfilpctrl(config: DfiConfig) extends Bundle{
  val lpctrlreq = config.useLpCtrlReq generate Bool()
}

case  class Dfilp(config: DfiConfig) extends Bundle{
  val lpwakeup = config.useLpWakeUp generate  Bits(4 bits)
}

case class Dfierror(config: DfiConfig) extends Bundle{
  val info = config.useErrorInfo generate Bits( 4 bits)
}

case class IDFI(config: DfiConfig) extends Bundle with IMasterSlave{
  import config._
  val cke         = Vec(Bits( chipSelectNumber bits), frequencyRatio)
  val rstn        =  useResetN generate Vec(Bits( chipSelectNumber bits), frequencyRatio)
  val cmd         = Vec(master(Flow(DfiCmd(config))), frequencyRatio)
  val odt         = Vec(master(Flow(Dfiodt(config))), frequencyRatio)
  val address     = Vec(master(Flow(DfiAddr(config))), frequencyRatio)
  val wrdata      = Vec(master(Flow(DfiWrdata(config))), frequencyRatio)
  val wrcs        = Vec(master(Flow(DfiWrcs(config))), frequencyRatio)
  val rden        = Vec(Bool(), frequencyRatio)
  val rddata      = Vec(slave(Stream(Fragment(Dfirddata(config)))), frequencyRatio)
  val rdcs        = Vec(master(Flow(Dfireadcs(config))), frequencyRatio)
  val ctrlup      =  useCtrlupd generate Stream(Event)
  val phyup       =  usePhyupd generate Stream(Dfiphyup(config))
  val clkdisable  = master(Flow(Bits( chipSelectNumber bits)))
  val init        =  useInitStart generate Stream(Dfiinit(config))
  val parity      =  useParity generate Vec(master(Stream(Event)), frequencyRatio)
  val crc         = timeConfig.phyCrcMode & useAlertN generate Vec(master(Stream(Event)), frequencyRatio)
  val rdlvlcs     =  useRdlvlReq &  useRdlvlEn generate Vec(slave(Stream(Dfirdlvlcs(config))), dramDataSlice)
  val rdlvl       =  useRdlvlResp &  useRdlvlEn generate Vec(master(Stream(Dfirdlvl(config))), dramDataSlice)
  val rdgatacs    =  useRdlvlGateReq &  useRdlvlGateEn generate Vec(slave(Stream(Dfirdgatecs(config))), dramDataSlice)
  val rdgate      =  useRdlvlResp &  useRdlvlGateEn generate Vec(master(Stream(Dfirdgate(config))), dramDataSlice)
  val wrlvlcs     =  useWrlvlReq &  useWrlvlEn generate Vec(slave(Stream(Dfiwrlvlcs(config))), dramDataSlice)
  val wrlvl       =  useWrlvlResp &  useWrlvlEn generate Vec(master(Stream(Dfiwrlvl(config))), dramDataSlice)
  val phylvl      =  usePhylvl generate Vec(slave(Stream(Event)), dramChipselectNumber)
  val lpctrlreq   = Flow(Dfilpctrl(config))
  val lowpower    =  useLpData generate Stream(Dfilp(config))
  val error       =  useError generate Vec(master(Flow(Dfierror(config))), errorNumber)

  override def asMaster(): Unit = {
    master(ctrlup,clkdisable,init,lpctrlreq,lowpower)
    slave(phyup)
    out(cke,rstn,rden)
  }

}
