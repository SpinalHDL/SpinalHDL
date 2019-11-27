package spinal.lib.memory.sdram.xdr.phy

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.memory.sdram.SdramLayout
import spinal.lib.memory.sdram.sdr.SdramInterface
import spinal.lib.memory.sdram.xdr.{PhyLayout, SdramXdrPhyCtrl}


object SdrInferedPhy{
  def phyLayout(sl : SdramLayout) = PhyLayout(
    sdram = sl,
    phaseCount = 1,
    dataRate = 1,
    outputLatency = 1,
    readDelay = 0,
    writeDelay = 0,
    transferPerBurst = 1
  )
}

case class SdrInferedPhy(sl : SdramLayout) extends Component{
  val pl = SdrInferedPhy.phyLayout(sl)

  val io = new Bundle {
    val ctrl = slave(SdramXdrPhyCtrl(pl))
    val sdram = master(SdramInterface(sl))
  }

  io.sdram.ADDR  := RegNext(io.ctrl.ADDR)
  io.sdram.BA    := RegNext(io.ctrl.BA  )
  io.sdram.DQM   := RegNext(io.ctrl.phases(0).DM(0)  )
  io.sdram.CASn  := RegNext(io.ctrl.phases(0).CASn)
  io.sdram.CKE   := RegNext(io.ctrl.phases(0).CKE )
  io.sdram.CSn   := RegNext(io.ctrl.phases(0).CSn )
  io.sdram.RASn  := RegNext(io.ctrl.phases(0).RASn)
  io.sdram.WEn   := RegNext(io.ctrl.phases(0).WEn )

  io.sdram.DQ.writeEnable  := RegNext(io.ctrl.writeEnable)
  io.sdram.DQ.write        := RegNext(io.ctrl.phases(0).DQw(0))
  io.ctrl.phases(0).DQr(0)  := RegNext(io.sdram.DQ.read )

  io.ctrl.readValid := io.ctrl.readEnable
}