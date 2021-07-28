
package spinal.tester.code



import spinal.core.Nameable.{DATAMODEL_WEAK, USER_WEAK}
import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState

import scala.collection.mutable.ArrayBuffer


object Debug {
  class NameVecIndexing extends Component {
    val io = new Bundle {
      val qpsize = in  UInt(32 bits)
      val addr1 = in  UInt(32 bits)
      val addr2 = in  UInt(32 bits)
      val addr3 = in  UInt(32 bits)
      val qlevel = out (UInt(32 bits))
    }
    noIoPrefix()

    val qp_f = Vec(Vec(Vec(UInt(32 bits), 5), 4), 3)

    for (i <- 0 until 3) (
      for (j <- 0 until 4) (
        for (k <- 0 until 5) {
          qp_f(i)(j)(k) := i + j + k
        }
        )
      )

    io.addr1.addTag(tagAutoResize)
    io.addr2.addTag(tagAutoResize)
    io.addr3.addTag(tagAutoResize)
    io.qlevel := qp_f(io.addr1(8 downto 4).addTag(tagAutoResize))(io.addr2)(io.addr3)
  }


  def main(args: Array[String]) {
    SpinalVerilog(new NameVecIndexing)

  }

  //  createEnum("asd")
}




object Debug2 extends App{


  SpinalConfig().includeFormal.generateSystemVerilog(new Component{

    class Miaou(val a : UInt = UInt(32 bits), b : Bool = Bool()) extends Bundle

    val x = new Miaou()
    val y = new Miaou(42, False)
//    val z = x.copy(42, False)

//    val a,b = in Bits(8 bits)
//
//    val x, y = OHToUInt(a)
//    val z = OHToUInt(b)

//    val a = slave(Stream(UInt( 8 bits)))
//    val b = master(Stream(UInt( 8 bits)))
//
//    a >> b
//
//
//    val x = out Bool()
//    x := False
//    when(b.fire){
//      x := True
//    }
//    val a = in Bits(8 bits)
//    val x = out UInt(6 bits)
//    val y, z = out Bool()
//
//
//    (x,y,z) := a



//    val rawrrr = in UInt(8 bits)
//    val wuff = out(Reg(UInt(8 bits))) init(0x11)
//    wuff := wuff + rawrrr



//    GenerationFlags.formal {
//      when(Formal.initstate()) {
//        assume(clockDomain.isResetActive)
//      }
//    }


//    GenerationFlags.formal {
//      ClockDomain.current.withoutReset(){
//        assert(wuff === 0)
//      }
//      ClockDomain.current.readResetWire initial(False)
//      rawrrr.initial(0x42)
//
//      assumeInitial(!clockDomain.isResetActive)
//      ClockDomain.current.duringReset {
//        assume(rawrrr === 0)
//        assume(wuff === 3)
//      }
//    }

    setDefinitionName("miaou")
  })

}

//object MyEnum extends  spinal.core.MacroTest.mkObject("asd")
//
//



import spinal.core._

class Test extends Component {
  val io = new Bundle {
    val input = in Vec(UInt(10 bits), 2)
    val output = out UInt(11 bits)
  }

  io.output := io.input(0) +^ io.input(1)

}

class TestTop extends Component {
  val io = new Bundle {
    val A = in UInt(10 bits)
    val B = in UInt(10 bits)
    val C = out UInt(11 bits)
  }

  val adder = new Test
  adder.io.input(0) := io.A
  adder.io.input(1) := io.B
  io.C := adder.io.output
}

object TestTopMain {
  def main(args: Array[String]) {
    SpinalVerilog(new TestTop)
  }
}




object Debug3 extends App{
  import spinal.core.sim._

  SimConfig.withIVerilog.compile(new Component {
    val a,b,c = in UInt(8 bits)
    val result = out(Reg(UInt(8 bits)) init(0))
    result := result + 1
    assert(result =/= 10, "miaou")
  }).doSim{ dut =>
    dut.a #= 1
    dut.b #= 0
    dut.c #= 0
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling(100)
  }
}


object Debug4 extends App{
  SpinalVerilog(new Component{
    val mem = Mem(UInt(8 bits), 16) initBigInt(List.fill(16)(BigInt(0)))

    val a = slave(mem.writePort)
    val b = slave(mem.readSyncPort)
    println(LatencyAnalysis(a.data, b.rsp))
    println(LatencyAnalysis(b.cmd.payload, b.rsp))
  })
}


class TopLevel extends Component {
  val io = new Bundle {
    val ready = in Bool()
    val valid = out Bool()
  }
  val valid = RegInit(False)

  when(io.ready) {
    valid := False
  }
  io.valid <> valid
  // some logic

  import spinal.core.GenerationFlags._
  import spinal.core.Formal._

  GenerationFlags.formal {
//    assumeInitial(clockDomain.isResetActive)
//    assert(!(valid.fall && !io.ready))
//
//    ClockDomain.current.duringReset {
//      assume(io.ready === False)
//    }
    when(initstate()) {
      assume(clockDomain.isResetActive)
      assume(io.ready === False)
    }.otherwise {
      assert(!(valid.fall && !io.ready))
    }
  }
}


object MyToplevelSystemVerilogWithFormal {
  def main(args: Array[String]) {
    val config = SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind=SYNC, resetActiveLevel=HIGH))
    config.includeFormal.generateSystemVerilog(new TopLevel())
  }
}



import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axi._


class Test22 extends Component {
  val axiConf = Axi4Config(
    addressWidth = 32,
    dataWidth = 32,
    useId = false,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useCache = false,
    useSize = false,
    useQos = false,
    useLen = false,
    useLast = true,
    useResp = false,
    useProt = false,
    useStrb = true
  )

  val axi = Axi4ReadOnly(axiConf)
  axi.readCmd.valid := True
  axi.readCmd.addr := 0x00000012
  axi.readRsp.ready := True

  val ram = Axi4SharedOnChipRam(
    byteCount = 4 KiB,
    dataWidth = 32,
    idWidth = 4
  )

  val axiCrossbar = Axi4CrossbarFactory()
//  axiCrossbar.lowLatency = true
  axiCrossbar.addSlaves(
    ram.io.axi       -> (0x00000000L, 4 KiB)
  )
  axiCrossbar.addConnections(
    axi -> List(ram.io.axi)
  )

  axiCrossbar.build()
}

object Test {
  def main(args: Array[String]) {
    import spinal.core.sim._
    SimConfig.withWave.addSimulatorFlag("-Wno-CASEOVERLAP").compile(new Test22).doSim { dut =>
      dut.clockDomain.forkStimulus(10)

      for (_ <- 0 to 100) {
        dut.clockDomain.waitSampling()
      }
    }
  }
}

object Test4141 {
  def main(args: Array[String]) {
    SimConfig.withWave.compile(Axi4SharedOnChipRam(
      byteCount = 4 KiB,
      dataWidth = 32,
      idWidth = 4
    ))
  }
}


// generate some logic so verilator has some work to do
case class Foo(a: Int, b: Int) extends Component {
  val io = new Bundle {
    val output = out UInt(a bits)
  }

  var tmp = U(0, a bits)

  for (_ <- 0 until b) {
    val reg = Reg(UInt(a bits)) init(0)
    reg := reg + 1

    tmp = tmp + reg
  }

  io.output := tmp
}


object Foo {
  def main(args: Array[String]): Unit = {
    {
      val cfg = SimConfig.addSimulatorFlag("--threads 1")
      val compiled = cfg.compile(Foo(a = 2048, b = 10 - 1))
      compiled.doSim(seed = 0) { dut =>
        dut.clockDomain.forkStimulus(10)

        // just wait for 2000 clock cycles
        dut.clockDomain.waitSampling(200000)
      }
    }
    {
      val cfg = SimConfig.addSimulatorFlag("--threads 1")
      val compiled = cfg.compile(Foo(a = 2048, b = 10 - 1))
      compiled.doSim(seed = 0) { dut =>
        dut.clockDomain.forkStimulus(10)

        // just wait for 2000 clock cycles
        dut.clockDomain.waitSampling(200000)
      }
    }
//    test()  // fast, < 50 ms
//    test()  // very slow, ~ 15 s
  }
}