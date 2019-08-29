package spinal.lib.memory.sdram.xdr.phy

import spinal.core.RegNext
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.memory.sdram.SdramLayout
import spinal.lib.memory.sdram.sdr.SdramInterface
import spinal.lib.memory.sdram.xdr.{Phy, PhyParameter}


object SdrInferedPhy{
  def memoryLayoutToPhyLayout(sl : SdramLayout) = PhyParameter(
    sdram = sl,
    phaseCount = 1,
    dataRatio = 1,
    outputLatency = 1,
    inputLatency = 1,
    burstLength = 1,
    CCD = 1
  )
}

case class SdrInferedPhy(sl : SdramLayout) extends Phy[SdramInterface](SdrInferedPhy.memoryLayoutToPhyLayout(sl)){
  override def MemoryBus(): SdramInterface = SdramInterface(sl)
  override def driveFrom(mapper: BusSlaveFactory): Unit = {}

  io.memory.ADDR  := RegNext(io.ctrl.ADDR)
  io.memory.BA    := RegNext(io.ctrl.BA  )
  io.memory.DQM   := RegNext(io.ctrl.phases(0).DM(0)  )
  io.memory.CASn  := RegNext(io.ctrl.phases(0).CASn)
  io.memory.CKE   := RegNext(io.ctrl.phases(0).CKE )
  io.memory.CSn   := RegNext(io.ctrl.phases(0).CSn )
  io.memory.RASn  := RegNext(io.ctrl.phases(0).RASn)
  io.memory.WEn   := RegNext(io.ctrl.phases(0).WEn )

  io.memory.DQ.writeEnable  := RegNext(io.ctrl.DQe)
  io.memory.DQ.write        := RegNext(io.ctrl.phases(0).DQw(0))
  io.ctrl.phases(0).DQr(0)     := RegNext(io.memory.DQ.read )
}