package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.bmb.{Bmb, BmbParameter}


case class BmbPortParameter(bmb : BmbParameter,
                            cmdBufferSize : Int,
                            rspBufferSize : Int)

case class CtrlParameter( core : CoreParameter,
                          ports : Seq[BmbPortParameter])


class Ctrl[T <: Data with IMasterSlave](p : CtrlParameter, phyGen : => Phy[T]) extends Component{
  val io = new Bundle {
    val bmb = Vec(p.ports.map(p => slave(Bmb(p.bmb))))
    val apb = slave(Apb3(12, 32))
    val memory = master(phy.MemoryBus())
  }

  val cpa = CoreParameterAggregate(p.core, phy.pl, p.ports.map(port => BmbAdapter.corePortParameter(port, phy.pl)))

  val bmbAdapter = for(port <- p.ports) yield BmbAdapter(port, cpa)
  (bmbAdapter, io.bmb).zipped.foreach(_.io.input <> _)

  val core = Core(cpa)
  core.io.ports <> Vec(bmbAdapter.map(_.io.output))

  lazy val phy = phyGen
  phy.io.ctrl <> core.io.phy

  io.memory <> phy.io.memory

  val mapper = Apb3SlaveFactory(io.apb)
  core.io.config.driveFrom(mapper.withOffset(0x000))
  core.io.soft.driveFrom(mapper.withOffset(0x100))
  phy.driveFrom(mapper.withOffset(0x400))
}



object CtrlMain extends App{
  val ml = MemoryLayout(
    bankWidth = 2,
    columnWidth = 10,
    rowWidth = 13,
    dataWidth = 16,
    withDqs = false,
    burstLength = 1
  )
  val cp = CtrlParameter(
    core = CoreParameter(
      timingWidth = 4,
      refWidth = 16,
      writeLatencies = List(0),
      readLatencies = List(2)
    ),
    ports = Seq(
      BmbPortParameter(
        bmb = BmbParameter(
          addressWidth = ml.byteAddressWidth,
          dataWidth = 16,
          lengthWidth = 4,
          sourceWidth = 3,
          contextWidth = 8
        ),
        cmdBufferSize = 4,
        rspBufferSize = 4
      )
    )
  )
  SpinalVerilog(new Ctrl(cp, SdrInferedPhy(ml)))
}