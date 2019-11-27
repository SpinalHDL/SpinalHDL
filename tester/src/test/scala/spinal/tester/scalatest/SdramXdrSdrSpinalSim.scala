package spinal.tester.scalatest

import spinal.core._
import spinal.lib.bus.bmb._
import spinal.lib.memory.sdram.sdr.{MT41K128M16JT, MT48LC16M16A2, SdramInterface}
import spinal.lib.memory.sdram.xdr.{BmbPortParameter, CoreParameter, CtrlParameter, CtrlWithPhy, CtrlWithoutPhy, PhyLayout, SdramTiming, SoftConfig, mt48lc16m16a2_model}
import spinal.lib._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.bus.bmb.sim.{BmbMemoryMultiPort, BmbMemoryMultiPortTester}
import spinal.lib.eda.bench.Rtl
import spinal.lib.memory.sdram.SdramLayout
import spinal.lib.memory.sdram.sdr.sim.SdramModel
import spinal.lib.memory.sdram.xdr.phy.{RtlPhy, RtlPhyInterface, SdrInferedPhy, XilinxS7Phy}
import spinal.lib.sim.Phase

import scala.util.Random

class SdrXdrCtrlPlusPhy(cp : CtrlParameter, pl : PhyLayout) extends Component{
  val ctrl = new CtrlWithoutPhy(cp, pl)

  val bmb = Vec(cp.ports.map(p => slave(Bmb(p.bmb))))
  val apb = slave(Apb3(12, 32))
  bmb <> ctrl.io.bmb
  apb <> ctrl.io.apb
}

class SdrXdrCtrlPlusRtlPhy(val cp : CtrlParameter,val pl : PhyLayout) extends SdrXdrCtrlPlusPhy(cp, pl){
  val phy = RtlPhy(pl)
  phy.io.ctrl <> ctrl.io.phy

  val phyWrite = master(RtlPhyInterface(phy.pl))
  phyWrite <> phy.io.write

  val beatCounter = out(Reg(UInt(60 bits)) init(0))
  beatCounter := beatCounter + ctrl.io.phy.readEnable.asUInt +ctrl.io.phy.writeEnable.asUInt


  val clockCounter = out(Reg(UInt(60 bits)) init(0))
  clockCounter := clockCounter + 1
}

object SpinalSdrTesterHelpers{
  val CSn = 1 << 1
  val RASn = 1 << 2
  val CASn = 1 << 3
  val WEn = 1 << 4

  val PRE = CASn
  val REF = WEn
  val MOD = 0

  def ctrlParameter(pl : PhyLayout, cp : CoreParameter) = CtrlParameter(
    core = cp,
    ports = Seq(
      //TODO
//      BmbPortParameter(
//        bmb = BmbParameter(
//          addressWidth = pl.sdram.byteAddressWidth,
//          dataWidth = pl.beatWidth,
//          lengthWidth = log2Up(8 * pl.bytePerBurst),
//          sourceWidth = 3,
//          contextWidth = 8
//        ),
//        clockDomain = ClockDomain.current,
//        cmdBufferSize = 64,
//        dataBufferSize = 64 * pl.beatCount,
//        rspBufferSize = 64 * pl.beatCount
//      )
      BmbPortParameter(
        bmb = BmbParameter(
          addressWidth = pl.sdram.byteAddressWidth,
          dataWidth = pl.beatWidth,
          lengthWidth = log2Up(16*pl.bytePerBurst),
          sourceWidth = 3,
          contextWidth = 8
        ),
        clockDomain = ClockDomain.current,
        cmdBufferSize = 16,
        dataBufferSize = 16*pl.beatCount,
        rspBufferSize = 16*pl.beatCount
      ),

      BmbPortParameter(
        bmb = BmbParameter(
          addressWidth = pl.sdram.byteAddressWidth,
          dataWidth = pl.beatWidth,
          lengthWidth = log2Up(32*pl.bytePerBurst),
          sourceWidth = 3,
          contextWidth = 8
        ),
        clockDomain = ClockDomain.current,
        cmdBufferSize = 32,
        dataBufferSize = 32*pl.beatCount,
        rspBufferSize = 32*pl.beatCount
      ),

      BmbPortParameter(
        bmb = BmbParameter(
          addressWidth = pl.sdram.byteAddressWidth,
          dataWidth = pl.beatWidth,
          lengthWidth = log2Up(8*pl.bytePerBurst),
          sourceWidth = 3,
          contextWidth = 8
        ),
        clockDomain = ClockDomain.current,
        cmdBufferSize = 4,
        dataBufferSize = 1*pl.beatCount,
        rspBufferSize = 4*pl.beatCount
      ),

      BmbPortParameter(
        bmb = BmbParameter(
          addressWidth = pl.sdram.byteAddressWidth,
          dataWidth = pl.beatWidth,
          lengthWidth = log2Up(8*pl.bytePerBurst),
          sourceWidth = 3,
          contextWidth = 8
        ),
        clockDomain = ClockDomain.current,
        cmdBufferSize = 1,
        dataBufferSize = 8*pl.beatCount,
        rspBufferSize = 8*pl.beatCount
      )
    )
  )

  def sdrInit(dut : SdrXdrCtrlPlusRtlPhy, timing : SdramTiming, cas : Int): Unit ={
    import spinal.core.sim._
    Phase.setup {
      val apb = Apb3Driver(dut.apb, dut.clockDomain)
      //apb.verbose = true

      val soft = SoftConfig(timing, dut.clockDomain.frequency.getValue, dut.ctrl.cpa)
      apb.write(0x10, soft.REF)

      apb.write(0x20, (soft.RRD << 24) | (soft.RFC << 16) | (soft.RP << 8)  | (soft.RAS << 0))
      apb.write(0x24,                                                         (soft.RCD << 0))
      apb.write(0x28, (soft.WR << 24)  | (soft.WTR << 16) | (soft.RTP << 8) |    (cas+2 << 0))

      sleep(100000)

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
      command(MOD, 0, 0x000 | (cas << 4))
      apb.write(0x04, 1)

      dut.clockDomain.waitSampling(10000)
    }
  }

  def setup(dut : SdrXdrCtrlPlusRtlPhy, noStall : Boolean = false): Unit ={
    import spinal.core.sim._
    def sl = dut.pl.sdram
    val addressTop = 1 << (2 + sl.bankWidth + sl.columnWidth + log2Up(sl.bytePerWord))
    val bytePerBeat = dut.phy.pl.bytePerBeat
    val tester = new BmbMemoryMultiPortTester(
      ports = dut.bmb.map(port =>
        BmbMemoryMultiPort(
          bmb = port,
          cd = dut.clockDomain
        )
      )
    ){
      override def addressGen(bmb: Bmb): Int = Random.nextInt(addressTop)
      override def transactionCountTarget: Int = 20
    }

    Phase.setup {
      for(beatId <- 0 until addressTop/bytePerBeat){
        var data = BigInt(0)
        for(byteId <- 0 until bytePerBeat){
          data = data | (BigInt(tester.memory.getByte(beatId*bytePerBeat + byteId).toInt & 0xFF) << (byteId*8))
        }
        dut.phyWrite.clk #= false
        dut.phyWrite.cmd.valid #= true
        dut.phyWrite.cmd.address #= beatId
        dut.phyWrite.cmd.data #= data
        sleep(0)
        dut.phyWrite.clk #= true
        sleep(0)
      }
    }

    var beatCount = 0l
    var clockCount = 0l
    Phase.stimulus{
      beatCount = -dut.beatCounter.toLong
      clockCount = -dut.clockCounter.toLong
    }

    Phase.flush{
      beatCount += dut.beatCounter.toLong
      clockCount += dut.clockCounter.toLong
      println(s"${1.0*beatCount/clockCount} beatRate ($beatCount/$clockCount)")
    }
  }
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

//  val sl = MT48LC16M16A2.layout
//  val pl = SdrInferedPhy.phyLayout(sl)
  val sl = MT41K128M16JT.layout
  val pl = XilinxS7Phy.phyLayout(sl, 1)

  val simConfig = SimConfig
  simConfig.withWave
  simConfig.addSimulatorFlag("-Wno-MULTIDRIVEN")
  simConfig.withConfig(SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz)))
  simConfig.compile({
    val cp = SpinalSdrTesterHelpers.ctrlParameter(
      pl = pl,
      cp = CoreParameter(
        portTockenMin = 4,
        portTockenMax = 16,
        timingWidth = 4,
        refWidth = 16,
        writeLatencies = List(4),
        readLatencies = List(4)
      )
    )
    new SdrXdrCtrlPlusRtlPhy(cp, pl)
  }).doSimUntilVoid("test", 42) { dut =>
    SpinalSdrTesterHelpers.setup(dut, noStall = true)
    SpinalSdrTesterHelpers.sdrInit(dut, timing, cas = 2)
  }
}






import spinal.core._
import spinal.lib.eda.bench._

object SdramSdrSyntBench extends App{
  val sl = MT48LC16M16A2.layout.copy(bankWidth = 3)
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
          sourceWidth = 0,
          contextWidth = 0
        ),
        clockDomain = ClockDomain.current,
        cmdBufferSize = 8,
        dataBufferSize = 8,
        rspBufferSize = 16
      ),

      BmbPortParameter(
        bmb = BmbParameter(
          addressWidth = sl.byteAddressWidth,
          dataWidth = 16,
          lengthWidth = 4,
          sourceWidth = 0,
          contextWidth = 0
        ),
        clockDomain = ClockDomain.current,
        cmdBufferSize = 8,
        dataBufferSize = 8,
        rspBufferSize = 16
      )/*,

      BmbPortParameter(
        bmb = BmbParameter(
          addressWidth = sl.byteAddressWidth,
          dataWidth = 16,
          lengthWidth = 5,
          sourceWidth = 0,
          contextWidth = 0
        ),
        clockDomain = ClockDomain.current,
        cmdBufferSize = 8,
        rspBufferSize = 2
      )*//*,

      BmbPortParameter(
        bmb = BmbParameter(
          addressWidth = sl.byteAddressWidth,
          dataWidth = 16,
          lengthWidth = 5,
          sourceWidth = 0,
          contextWidth = 0
        ),
        clockDomain = ClockDomain.current,
        cmdBufferSize = 8,
        rspBufferSize = 2
      )*/
    )
  )


  val ports4 = new Rtl {
    override def getName(): String = "Port4"
    override def getRtlPath(): String = "Port4.v"
    SpinalVerilog({
      val c = new CtrlWithoutPhy(cp, SdrInferedPhy.phyLayout(sl)).setDefinitionName(getRtlPath().split("\\.").head)
      c
    })
  }


  val rtls = List(ports4)

  val targets = XilinxStdTargets(
    vivadoArtix7Path = "/media/miaou/HD/linux/Xilinx/Vivado/2018.3/bin"
  ) ++ AlteraStdTargets(
    quartusCycloneIVPath = "/media/miaou/HD/linux/intelFPGA_lite/18.1/quartus/bin",
    quartusCycloneVPath  = "/media/miaou/HD/linux/intelFPGA_lite/18.1/quartus/bin"
  )

  Bench(rtls, targets, "/media/miaou/HD/linux/tmp")




}