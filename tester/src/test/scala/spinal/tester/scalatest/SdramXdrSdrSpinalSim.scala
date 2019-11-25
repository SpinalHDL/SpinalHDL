package spinal.tester.scalatest

import spinal.core._
import spinal.lib.bus.bmb._
import spinal.lib.memory.sdram.sdr.{MT48LC16M16A2, SdramInterface}
import spinal.lib.memory.sdram.xdr.{BmbPortParameter, CoreParameter, CtrlParameter, CtrlWithPhy, SdramTiming, SoftConfig, mt48lc16m16a2_model}
import spinal.lib._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.bus.bmb.sim.{BmbMemoryMultiPort, BmbMemoryMultiPortTester}
import spinal.lib.eda.bench.Rtl
import spinal.lib.memory.sdram.sdr.sim.SdramModel
import spinal.lib.memory.sdram.xdr.phy.{RtlPhy, SdrInferedPhy}
import spinal.lib.sim.Phase

import scala.util.Random

case class SdramSdrTesterSpinalSimTop() extends Component{
  val sl = MT48LC16M16A2.layout
  val cp = CtrlParameter(
    core = CoreParameter(
      portTockenMin = 4,
      portTockenMax = 8,
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
        clockDomain = ClockDomain.current,
        cmdBufferSize = 4,
        dataBufferSize = 3,
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
        clockDomain = ClockDomain.current,
        cmdBufferSize = 2,
        dataBufferSize = 3,
        rspBufferSize = 8
      ),

      BmbPortParameter(
        bmb = BmbParameter(
          addressWidth = sl.byteAddressWidth,
          dataWidth = 16,
          lengthWidth = 5,
          sourceWidth = 6,
          contextWidth = 16
        ),
        clockDomain = ClockDomain.current,
        cmdBufferSize = 8,
        dataBufferSize = 8,
        rspBufferSize = 2
      )
    )
  )

  val io = new Bundle {
    val apb = slave(Apb3(12, 32))
    val ports = Vec(cp.ports.map(p => slave(Bmb(p.bmb))))
    val memory = SdramInterface(sl)
  }

  val ctrl = new CtrlWithPhy(cp, SdrInferedPhy(sl))
  io.ports <> ctrl.io.bmb
  io.apb <> ctrl.io.apb
}


object SdramSdrTesterSpinalSim extends App{
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
  SimConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz))).compile({

    val cp = CtrlParameter(
      core = CoreParameter(
        portTockenMin = 4,
        portTockenMax = 16,
        timingWidth = 4,
        refWidth = 16,
        writeLatencies = List(0),
        readLatencies = List(4)
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
          clockDomain = ClockDomain.current,
          cmdBufferSize = 4,
          dataBufferSize = 3,
          rspBufferSize = 4
        ),

        BmbPortParameter(
          bmb = BmbParameter(
            addressWidth = sl.byteAddressWidth,
            dataWidth = 16,
            lengthWidth = 6,
            sourceWidth = 6,
            contextWidth = 16
          ),
          clockDomain = ClockDomain.current,
          cmdBufferSize = 32,
          dataBufferSize = 32,
          rspBufferSize = 32
        ),

        BmbPortParameter(
          bmb = BmbParameter(
            addressWidth = sl.byteAddressWidth,
            dataWidth = 16,
            lengthWidth = 4,
            sourceWidth = 5,
            contextWidth = 12
          ),
          clockDomain = ClockDomain.current,
          cmdBufferSize = 2,
          dataBufferSize = 3,
          rspBufferSize = 5
        )
      )
    )
    val c = new CtrlWithPhy(cp, SdrInferedPhy(sl))
    c
  }).doSimUntilVoid("test", 42) { dut =>
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
      apb.verbose = true

      val CAS = 2

      val soft = SoftConfig(timing, dut.clockDomain.frequency.getValue, dut.cpa)
      apb.write(0x10, soft.REF)

      apb.write(0x20, (soft.RRD << 24) | (soft.RFC << 16) | (soft.RP << 8)  | (soft.RAS << 0))
      apb.write(0x24,                                                         (soft.RCD << 0))
      apb.write(0x28, (soft.WR << 24)  | (soft.WTR << 16) | (soft.RTP << 8) |    (CAS+2 << 0))

      sleep(100000)
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


      command(PRE, 0, 0x400)
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


object SdramSdrRtlPhyTesterUtils{
//  def
}

object SdramSdrRtlPhyTesterSpinalSim extends App{
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
  SimConfig.addSimulatorFlag("-Wno-MULTIDRIVEN").withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz))).compile({

    val cp = CtrlParameter(
      core = CoreParameter(
        portTockenMin = 4,
        portTockenMax = 16,
        timingWidth = 4,
        refWidth = 16,
        writeLatencies = List(0),
        readLatencies = List(4)
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
          clockDomain = ClockDomain.current,
          cmdBufferSize = 4,
          dataBufferSize = 3,
          rspBufferSize = 4
        ),

        BmbPortParameter(
          bmb = BmbParameter(
            addressWidth = sl.byteAddressWidth,
            dataWidth = 16,
            lengthWidth = 6,
            sourceWidth = 6,
            contextWidth = 16
          ),
          clockDomain = ClockDomain.current,
          cmdBufferSize = 32,
          dataBufferSize = 32,
          rspBufferSize = 32
        ),

        BmbPortParameter(
          bmb = BmbParameter(
            addressWidth = sl.byteAddressWidth,
            dataWidth = 16,
            lengthWidth = 4,
            sourceWidth = 5,
            contextWidth = 12
          ),
          clockDomain = ClockDomain.current,
          cmdBufferSize = 2,
          dataBufferSize = 3,
          rspBufferSize = 5
        )
      )
    )
    val c = new CtrlWithPhy(cp, RtlPhy(sl))
    c
  }).doSimUntilVoid("test", 42) { dut =>
    val addressTop = 1 << (2 + sl.bankWidth + sl.columnWidth + log2Up(sl.bytePerWord))
    val bytePerBeat = dut.cpa.pl.bytePerBeat
    val tester = new BmbMemoryMultiPortTester(
      ports = dut.io.bmb.map(port =>
        BmbMemoryMultiPort(
          bmb = port,
          cd = dut.clockDomain
        )
      )
    ){
      override def addressGen(bmb: Bmb): Int = Random.nextInt(addressTop)
      override def transactionCountTarget: Int = 100
    }

    Phase.setup {
      for(beatId <- 0 until addressTop/bytePerBeat){
        var data = BigInt(0)
        for(byteId <- 0 until bytePerBeat){
          data = data | (BigInt(tester.memory.getByte(beatId*bytePerBeat + byteId).toInt & 0xFF) << (byteId*8))
        }
        dut.io.memory.clk #= false
        dut.io.memory.write.valid #= true
        dut.io.memory.write.address #= beatId
        dut.io.memory.write.data #= data
        sleep(0)
        dut.io.memory.clk #= true
        sleep(0)
      }
    }

    Phase.setup {
      val apb = Apb3Driver(dut.io.apb, dut.clockDomain)
      //apb.verbose = true

      val CAS = 2

      val soft = SoftConfig(timing, dut.clockDomain.frequency.getValue, dut.cpa)
      apb.write(0x10, soft.REF)

      apb.write(0x20, (soft.RRD << 24) | (soft.RFC << 16) | (soft.RP << 8)  | (soft.RAS << 0))
      apb.write(0x24,                                                         (soft.RCD << 0))
      apb.write(0x28, (soft.WR << 24)  | (soft.WTR << 16) | (soft.RTP << 8) |    (CAS+2 << 0))

      sleep(100000)
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


      command(PRE, 0, 0x400)
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
