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
    cmdToDqDelayDelta = 0,
    transferPerBurst = 1
  )
}

case class SdrInferedPhy(sl : SdramLayout) extends Component{
  val pl = SdrInferedPhy.phyLayout(sl)

  val io = new Bundle {
    val ctrl = slave(SdramXdrPhyCtrl(pl))
    val sdram = master(SdramInterface(sl))
  }

  val regs = Reg(SdramInterface(sl))
  regs.ADDR  := io.ctrl.ADDR
  regs.BA    := io.ctrl.BA
  regs.DQM   := io.ctrl.phases(0).DM(0)
  regs.CASn  := io.ctrl.phases(0).CASn
  regs.CKE   := io.ctrl.phases(0).CKE
  regs.CSn   := io.ctrl.phases(0).CSn
  regs.RASn  := io.ctrl.phases(0).RASn
  regs.WEn   := io.ctrl.phases(0).WEn

  regs.DQ.writeEnable := KeepAttribute(Cat(List.fill(sl.dataWidth)(io.ctrl.writeEnable)))
  regs.DQ.write        := io.ctrl.phases(0).DQw(0)
  io.ctrl.phases(0).DQr(0)  := regs.DQ.read

  io.ctrl.readValid := io.ctrl.readEnable

  io.sdram <> regs
}