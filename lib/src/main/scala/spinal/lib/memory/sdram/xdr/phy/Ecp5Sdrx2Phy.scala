package spinal.lib.memory.sdram.xdr.phy

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.lattice.ecp5.{IDDRX1F, ODDRX1F}
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.memory.sdram.SdramLayout
import spinal.lib.memory.sdram.sdr.SdramInterface
import spinal.lib.memory.sdram.xdr.{PhyLayout, SdramXdrPhyCtrl}


object Ecp5Sdrx2Phy{
  def phyLayout(sl : SdramLayout) = PhyLayout(
    sdram = sl,
    phaseCount = 2,
    dataRate = 1,
    outputLatency = 3,
    readDelay = 0,
    writeDelay = 0,
    cmdToDqDelayDelta = 0,
    transferPerBurst = 2
  )
}



case class Ecp5Sdrx2Phy(sl : SdramLayout) extends Component{
  val pl = Ecp5Sdrx2Phy.phyLayout(sl)

  val io = new Bundle {
    val ctrl = slave(SdramXdrPhyCtrl(pl))
    val sdram = master(SdramInterface(sl))
  }

  def ddrOutputBool(i : Seq[Bool], o : Bool) = {
    val bb = ODDRX1F()
    bb.D0 <> i(0)
    bb.D1 <> i(1)
    bb.Q <> o
    bb
  }

  def ddrInputBool(i : Bool, o : Seq[Bool]) = {
    val bb = IDDRX1F()
    bb.D <> i
    bb.Q0 <> o(0)
    bb.Q1 <> o(1)
    bb
  }

  def sdrOutput[T <: Data](i : T, o : T) = new Area {
    val iBits = i.asBits
    val oBits = Bits(widthOf(o) bits)
    val gears = for(bitId <- 0 until widthOf(o)) yield ddrOutputBool(List.fill(2)(iBits(bitId)), oBits(bitId))
    o.assignFromBits(oBits)
  }

  def ddrOutput[T <: Data](i : Seq[T], o : T) = new Area {
    val iBits = i.map(_.asBits)
    val oBits = Bits(widthOf(o) bits)
    val gears = for(bitId <- 0 until widthOf(o)) yield ddrOutputBool(iBits.map(_(bitId)), oBits(bitId))
    o.assignFromBits(oBits)
  }

  def ddrInput[T <: Data](i : T, o :  Seq[T]) = new Area {
    val iBits = i.asBits
    val oBits = o.map(e => Bits(widthOf(e) bits))
    val gears = for(bitId <- 0 until widthOf(i)) yield ddrInputBool(iBits(bitId), oBits.map(_(bitId)))
    (o, oBits).zipped.foreach(_.assignFromBits(_))
  }

  val ADDR = sdrOutput(io.ctrl.ADDR, io.sdram.ADDR)
  val BA = sdrOutput(io.ctrl.BA, io.sdram.BA)
  val CASn = ddrOutput(io.ctrl.phases.map(_.CASn), io.sdram.CASn)
  val CKE = ddrOutput(io.ctrl.phases.map(_.CKE), io.sdram.CKE)
  val CSn = ddrOutput(io.ctrl.phases.map(_.CSn), io.sdram.CSn)
  val RASn = ddrOutput(io.ctrl.phases.map(_.RASn), io.sdram.RASn)
  val WEn = ddrOutput(io.ctrl.phases.map(_.WEn), io.sdram.WEn)
  val DM = ddrOutput(io.ctrl.phases.map(_.DM), io.sdram.DQM)
  val DQw = ddrOutput(io.ctrl.phases.map(_.DQw), io.sdram.DQ.write)

  val DQrValue = Vec(Bits(pl.sdram.dataWidth bits), 2)
  val DQr = ddrInput(io.sdram.DQ.read, DQrValue)
  val DQrBuffer = RegNext(DQrValue(1))
  io.ctrl.phases(0).DQr(0) := DQrBuffer
  io.ctrl.phases(1).DQr(0) := DQrValue(0)


  val delayedWriteEnable = Delay(io.ctrl.writeEnable, 1, init=False) || io.ctrl.writeEnable
  val dqWriteEnable = RegNext(Cat(List.fill(sl.dataWidth)(delayedWriteEnable)))
  val dqWriteEnableReg = RegNext(dqWriteEnable)
  io.sdram.DQ.writeEnable := dqWriteEnableReg
  io.ctrl.readValid := io.ctrl.readEnable


  KeepAttribute(dqWriteEnable)
  //  val dqWriteEnableReg = ClockDomain.current.copy(reset=Bool().assignDontCare(), config = ClockDomain.current.config.copy(resetKind = ASYNC)) (RegNext(dqWriteEnable).addAttribute("syn_useioff"))
  //  KeepAttribute(delayedWriteEnable.getDrivingReg)
  //dqWriteEnableReg.addAttribute("syn_useioff")


  //  ddrOutputBool(List(True, False), io.sdram.debug(0))
//  ddrOutputBool(io.ctrl.phases.map(_.CASn), io.sdram.debug(1))
//  ddrOutputBool(io.ctrl.phases.map(_.RASn), io.sdram.debug(2))
//  ddrOutputBool(io.ctrl.phases.map(_.WEn), io.sdram.debug(3))
//  ddrOutputBool(io.ctrl.phases.map(_.DM(0)(0)), io.sdram.debug(4))
//  ddrOutputBool(io.ctrl.phases.map(_.DM(0)(1)), io.sdram.debug(5))
//  ddrOutputBool(io.ctrl.phases.map(_.DQw(0)(0)), io.sdram.debug(6))
//  ddrOutputBool(io.ctrl.phases.map(_.DQw(0)(1)), io.sdram.debug(7))
//  ddrOutputBool(DQrValue.map(_(0)), io.sdram.debug(8))
//  ddrOutputBool(DQrValue.map(_(1)), io.sdram.debug(9))
}