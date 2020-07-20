package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.bmb.sim.{BmbMemoryMultiPort, BmbMemoryMultiPortTester}
import spinal.lib.bus.bmb.{Bmb, BmbAccessCapabilities, BmbAccessParameter, BmbParameter, BmbSlaveFactory}
import spinal.lib.memory.sdram.{SdramGeneration, SdramLayout}
import spinal.lib.memory.sdram.sdr.MT48LC16M16A2
import spinal.lib.memory.sdram.sdr.sim.SdramModel
import spinal.lib.memory.sdram.xdr.phy.SdrInferedPhy
import spinal.lib.sim.Phase

import scala.util.Random


case class BmbPortParameter(bmb : BmbParameter,
                            clockDomain : ClockDomain,
                            cmdBufferSize : Int,
                            dataBufferSize : Int,
                            rspBufferSize : Int){
}

case class CtrlParameter( core : CoreParameter,
                          ports : Seq[BmbPortParameter])


object CtrlWithPhy{
  def bmbCapabilities(pp : PhyLayout) = BmbAccessCapabilities(
    addressWidth  = pp.sdram.byteAddressWidth,
    dataWidth     = pp.sdram.dataWidth * pp.phaseCount * pp.dataRate,
    alignment = BmbParameter.BurstAlignement.LENGTH
  )
}

//class CtrlWithPhy[T <: Data with IMasterSlave](val p : CtrlParameter, phyGen : => Phy[T]) extends Component{
//  val io = new Bundle {
//    val bmb = Vec(p.ports.map(p => slave(Bmb(p.bmb))))
//    val apb = slave(Apb3(12, 32))
//    val memory = master(phy.MemoryBus())
//  }
//
//  val cpa = CoreParameterAggregate(p.core, phy.pl, p.ports.map(port => BmbAdapter.corePortParameter(port, phy.pl)))
//
//  val bmbAdapter = for(port <- p.ports) yield BmbAdapter(port, cpa)
//  (bmbAdapter, io.bmb).zipped.foreach(_.io.input <> _)
//
//  val core = Core(cpa)
//  core.io.ports <> Vec(bmbAdapter.map(_.io.output))
//  bmbAdapter.foreach(_.io.refresh := core.io.refresh)
//
//  lazy val phy = phyGen
//  phy.io.ctrl <> core.io.phy
//
//  io.memory <> phy.io.memory
//
//  val mapper = Apb3SlaveFactory(io.apb)
//  core.io.config.driveFrom(mapper.withOffset(0x000))
//  core.io.soft.driveFrom(mapper.withOffset(0x100))
//  phy.driveFrom(mapper.withOffset(0x400))
//}
//


class CtrlWithoutPhy(val p : CtrlParameter, pl : PhyLayout) extends Component{
  val io = new Bundle {
    val bmb = Vec(p.ports.map(p => slave(Bmb(p.bmb))))
    val apb = slave(Apb3(12, 32))
    val phy = master(SdramXdrPhyCtrl(pl))
  }

  val cpa = CoreParameterAggregate(p.core, pl, p.ports.map(port => BmbAdapter.corePortParameter(port, pl)))

  val bmbAdapter = for(port <- p.ports) yield BmbAdapter(port, cpa)
  (bmbAdapter, io.bmb).zipped.foreach(_.io.input <> _)

  val core = Core(cpa)
  core.io.ports <> Vec(bmbAdapter.map(_.io.output))
  bmbAdapter.foreach(_.io.refresh := core.io.refresh)

  io.phy <> core.io.phy

  val mapper = Apb3SlaveFactory(io.apb)
  core.io.config.driveFrom(mapper.withOffset(0x000))
  core.io.soft.driveFrom(mapper.withOffset(0x100))
}

object CtrlWithoutPhyBmb{
  def getBmbCapabilities(accessSource : BmbAccessCapabilities) = BmbSlaveFactory.getBmbCapabilities(
    accessSource,
    addressWidth = addressWidth,
    dataWidth = 32
  )
  def addressWidth = 12
}

class CtrlWithoutPhyBmb(val p : CtrlParameter, pl : PhyLayout, ctrlParameter : BmbParameter) extends Component{
  val io = new Bundle {
    val bmb = Vec(p.ports.map(p => slave(Bmb(p.bmb))))
    val ctrl = slave(Bmb(ctrlParameter))
    val phy = master(SdramXdrPhyCtrl(pl))
  }

  val cpa = CoreParameterAggregate(p.core, pl, p.ports.map(port => BmbAdapter.corePortParameter(port, pl)))

  val bmbAdapter = for(port <- p.ports) yield BmbAdapter(port, cpa)
  (bmbAdapter, io.bmb).zipped.foreach(_.io.input <> _)

  val core = Core(cpa)
  core.io.ports <> Vec(bmbAdapter.map(_.io.output))
  bmbAdapter.foreach(_.io.refresh := core.io.refresh)

  io.phy <> core.io.phy

  val mapper = BmbSlaveFactory(io.ctrl)
  core.io.config.driveFrom(mapper.withOffset(0x000))
  core.io.soft.driveFrom(mapper.withOffset(0x100))
}



case class mt48lc16m16a2_model() extends BlackBox{
  setDefinitionName("mt48lc16m16a2")
  val Addr = in Bits(13 bits)
  val Ba = in Bits(2 bits)
  val Clk = in Bool()
  val Cke = in Bool()
  val Cs_n = in Bool()
  val Ras_n = in Bool()
  val Cas_n = in Bool()
  val We_n = in Bool()
  val Dqm = in Bits(2 bits)
  val Dq = inout(Analog(Bits(16 bits)))
}



case class mt41k128m16jt_model() extends BlackBox{
  setDefinitionName("ddr3")
  val rst_n = in Bool()
  val ck = in Bool()
  val ck_n = in Bool()
  val cke = in Bool()
  val cs_n = in Bool()
  val ras_n = in Bool()
  val cas_n = in Bool()
  val we_n = in Bool()
  val odt = in Bool()
  val ba = in Bits(3 bits)
  val addr = in Bits(14 bits)
  val dq = inout(Analog(Bits(16 bits)))
  val dqs = inout(Analog(Bits(2 bits)))
  val dqs_n = inout(Analog(Bits(2 bits)))
  val dm_tdqs = inout(Analog(Bits(2 bits)))
  val tdqs_n = out(Bits(2 bits))
}





