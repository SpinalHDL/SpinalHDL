package spinal.lib.memory.sdram.xdr

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.bmb.sim.{BmbMemoryMultiPort, BmbMemoryMultiPortTester}
import spinal.lib.bus.bmb.{Bmb, BmbParameter}
import spinal.lib.memory.sdram.sdr.{MT48LC16M16A2, SdramLayout}
import spinal.lib.memory.sdram.sdr.sim.SdramModel
import spinal.lib.sim.Phase

import scala.util.Random


case class BmbPortParameter(bmb : BmbParameter,
                            cmdBufferSize : Int,
                            rspBufferSize : Int)

case class CtrlParameter( core : CoreParameter,
                          ports : Seq[BmbPortParameter])


class Ctrl[T <: Data with IMasterSlave](val p : CtrlParameter, phyGen : => Phy[T]) extends Component{
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
  val sl = SdramLayout(
    bankWidth = 2,
    columnWidth = 10,
    rowWidth = 13,
    dataWidth = 16
  )
  val cp = CtrlParameter(
    core = CoreParameter(
      portTockenMin = 4,
      portTockenMax = 8,
      rspFifoSize = 4,
      timingWidth = 4,
      refWidth = 16,
      writeLatencies = List(0),
      readLatencies = List(2)
    ),
    ports = Seq(
      BmbPortParameter(
        bmb = BmbParameter(
          addressWidth = sl.byteAddressWidth,
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
  SpinalVerilog(new Ctrl(cp, SdrInferedPhy(sl)))
}

object CtrlSdrTester extends App{
  import spinal.core.sim._
  val timing = SdramTiming(
    RFC = ( 66 ns, 0),
    RAS = ( 37 ns, 0),
    RP  = ( 15 ns, 0),
    WR  = ( 14 ns, 0),
    RCD = ( 15 ns, 0),
    WTR = (  0 ns, 0),
    RTP = (  0 ns, 0),
    RRD = ( 14 ns, 0),
    REF = ( 64 ms, 0)
  )
  val sl = MT48LC16M16A2.layout
  val cp = CtrlParameter(
    core = CoreParameter(
      portTockenMin = 4,
      portTockenMax = 8,
      rspFifoSize = 4,
      timingWidth = 4,
      refWidth = 16,
      writeLatencies = List(0),
      readLatencies = List(2)
    ),
    ports = Seq(
      BmbPortParameter(
        bmb = BmbParameter(
          addressWidth = sl.byteAddressWidth,
          dataWidth = 16,
          lengthWidth = 3,
          sourceWidth = 3,
          contextWidth = 8
        ),
        cmdBufferSize = 4,
        rspBufferSize = 4
      ),

      BmbPortParameter(
        bmb = BmbParameter(
          addressWidth = sl.byteAddressWidth,
          dataWidth = 16,
          lengthWidth = 4,
          sourceWidth = 5,
          contextWidth = 12
        ),
        cmdBufferSize = 2,
        rspBufferSize = 5
      ),

      BmbPortParameter(
        bmb = BmbParameter(
          addressWidth = sl.byteAddressWidth,
          dataWidth = 16,
          lengthWidth = 5,
          sourceWidth = 6,
          contextWidth = 16
        ),
        cmdBufferSize = 8,
        rspBufferSize = 2
      )
    )
  )

  SimConfig.withWave.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz))).compile(new Ctrl(cp, SdrInferedPhy(sl))).doSimUntilVoid("test", 42) { dut =>
    val tester = new BmbMemoryMultiPortTester(
      ports = dut.io.bmb.map(port =>
        BmbMemoryMultiPort(
          bmb = port,
          cd = dut.clockDomain
        )
      )
    ){
      override def addressGen(bmb: Bmb): Int = Random.nextInt(1 << (2 + sl.bankWidth + sl.columnWidth + log2Up(sl.bytePerWord)))

      override def transactionCountTarget: Int = 100
    }

    Phase.setup {
      val model = SdramModel(dut.io.memory, sl, dut.clockDomain)
      for(i <- 0 until tester.memory.memorySize.toInt){
        model.write(i, tester.memory.getByte(i))
      }
    }

    Phase.setup {
      val apb = Apb3Driver(dut.io.apb, dut.clockDomain)

      val soft = SoftConfig(timing, dut.clockDomain.frequency.getValue, dut.cpa)
      apb.write(0x10, soft.REF)
      apb.write(0x20, (soft.RP << 24) | (soft.RAS << 16) | (soft.RFC << 8) | (soft.WR << 0))
      apb.write(0x24, (soft.RTP << 24) | (soft.WTR << 16) | (soft.RCD << 8) | (soft.RRD << 0))

      sleep(100000)
      val CKE = 1 << 0
      val CSn = 1 << 1
      val RASn = 1 << 2
      val CASn = 1 << 3
      val WEn = 1 << 4

      val PRE = CASn
      val REF = WEn
      val MOD = 0

      def command(cmd : Int,  bank : Int, address : Int): Unit ={
        apb.write(0x10C, bank)
        apb.write(0x108, address)
        apb.write(0x104, cmd)
        apb.write(0x100, 0)
        dut.clockDomain.waitSampling(10)
      }

      val CAS = 2
      command(PRE, 0, 0)
      command(REF, 0, 0)
      command(REF, 0, 0)
      command(MOD, 0, 0x000 | (CAS << 4))
      apb.write(0x04, 1)

      dut.clockDomain.waitSampling(10000)
    }
    Phase.flush{
      println(simTime())
    }
  }
}