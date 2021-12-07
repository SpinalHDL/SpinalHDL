
package spinal.tester.code



import spinal.core.Nameable.{DATAMODEL_WEAK, USER_WEAK}
import spinal.core._
import spinal.core.fiber.Handle
import spinal.core.internals.Operator
import spinal.lib._
import spinal.core.sim._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}
import spinal.lib.fsm._
import spinal.lib.io.TriState

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


object CamTest{
  class Top extends Component{
    val linesCount = 8
    val waysCount = 2
    val commitsCount = 2
    val commitsIdWidth = 5

    case class Commit() extends Bundle{
      val id = UInt(commitsIdWidth bits)
      val done = Bool()
    }
    case class Slot() extends Bundle{
      val commit = Vec(Commit(), commitsCount)
      val valid = Bool()
    }
    val lines = Array.fill(linesCount)(new Area{
      val ways = Array.fill(waysCount)(Slot())
    })
  }
}

object Debug extends App{
  val report = SpinalVerilog(new Component {
    val a, b, c, d = in UInt(8 bits)
    val calc = a*b+c*d
    val result = out(CombInit(calc))
  })

  report.toplevel.dslBody.walkStatements{
    case bt : BaseType => {
      bt.foreachStatements{ s =>
        s.walkDrivingExpressions{
          case e : Operator.BitVector.Mul => println(s"$bt with ${e.left} * ${e.right}")
          case _ =>
        }
      }
    }
    case _ =>
  }
}


object DebugSim {



  def main(args: Array[String]) {
    println(LutInputs.get) //4
    LutInputs(6).on {
      println(LutInputs.get) //6
      LutInputs.set(3)
      println(LutInputs.get) //3
    }

    println(LutInputs.get) //4
//    LutInputs.set(8)
//    SimConfig.withFstWave.compile(new Component {
//      val sel = in Bits(40 bits)
//      val result1 = out(OHMasking.firstV2(sel))
//      val result2 = out(OHMasking.first(sel))
//    }).doSim{dut =>
//      for(i <- 0 until 10000){
//        var in = 0l
//        for(i <- 0 until Random.nextInt(4)){
//          in |= 1l << Random.nextInt(40)
//        }
//        dut.sel #= in
//        sleep(1)
//        assert(dut.result1.toBigInt == dut.result2.toBigInt)
//      }
//    }

  }

  //  createEnum("asd")
}


object InlineBbPlay extends App{
  object adderImpl extends BlackBoxImpl{
    override def getVerilog() =
      """module adder #(
        |    parameter WIDTH = 16
        |) (
        |    input      [WIDTH-1:0] ain    ,
        |    input      [WIDTH-1:0] bin    ,
        |    output reg [WIDTH-1:0] add_out
        |);
        |
        |always @(*) begin
        |  add_out = ain + bin;
        |end
        |
        |endmodule
      """.stripMargin
  }

  class adder(width: Int) extends BlackBox {
    addGeneric("WIDTH", width)

    val io = new Bundle {
      val ain = in UInt(width bits)
      val bin = in UInt(width bits)
      val add_out = out UInt(width bits)
    }
    noIoPrefix()

//    setInline(adderImpl)
    setInlineVerilog(
      """module adder #(
        |    parameter WIDTH = 16
        |) (
        |    input      [WIDTH-1:0] ain    ,
        |    input      [WIDTH-1:0] bin    ,
        |    output reg [WIDTH-1:0] add_out
        |);
        |
        |always @(*) begin
        |  add_out = ain + bin;
        |end
        |
        |endmodule
      """.stripMargin
    )
  }

  class top extends Module {
    val io = new Bundle {
      val ain = in UInt(16 bits)
      val bin = in UInt(16 bits)
      val add_out = out UInt(16 bits)
      val Xain = in UInt(18 bits)
      val Xbin = in UInt(18 bits)
      val Xadd_out = out UInt(18 bits)
    }
    noIoPrefix()
    var adder0 = new adder(16)
    io.ain <> adder0.io.ain
    io.bin <> adder0.io.bin
    io.add_out <> adder0.io.add_out

    var adderX = new adder(18)
    io.Xain <> adderX.io.ain
    io.Xbin <> adderX.io.bin
    io.Xadd_out <> adderX.io.add_out
  }
  SimConfig.doSim(new top()){dut => }
}

object Debug2 extends App{


  SpinalConfig().includeFormal.generateSystemVerilog(new Component{
    case class Wuff(val a : Int) extends Bundle{
      println(a)
    }

    val miaou = Wuff(1)
    val miaou2 = miaou.copy(2)

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
//    SpinalVerilog(new TestTop)

    SpinalConfig().addStandardMemBlackboxing(blackboxAll).generateVerilog(new StreamFifo(UInt(8 bits), 128))
  }
}




object Debug3 extends App{
  import spinal.core.sim._

  SimConfig.withFstWave.compile(new Component {
    val fsm = new StateMachine{
      val IDLE = makeInstantEntry()
      val STATE_A, STATE_B, STATE_C = new State
      
      IDLE.whenIsActive(goto(STATE_A))
      STATE_A.whenIsActive(goto(STATE_B))
      STATE_B.whenIsActive(goto(STATE_C))
      STATE_C.whenIsActive(goto(STATE_B))
    }
  }).doSim{ dut =>
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling(100)
  }
}
object Debug344 extends App{
  import spinal.core.sim._

  SimConfig.withFstWave.compile(new Component {
    val fsm = new StateMachine{
      val STATE_A = new StateDelay(10) with EntryPoint
      val STATE_B = new State()

      val result = out UInt(8 bits)
      result := 0
      STATE_A.whenIsActive{
        result := 1
        println("A")
      }
      STATE_A.whenCompleted{
        result := 2
        goto(STATE_B)
        println("B")
      }
    }
  }).doSim{ dut =>
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


    val cfg = SimConfig.withFstWave.addSimulatorFlag("--threads 1")
    for(i <- 0 until 6){
      val compiled = cfg.compile(Foo(a = 2048, b = 10 - 1))
      compiled.doSim(seed = 0) { dut =>
        dut.clockDomain.forkStimulus(10)
        dut.clockDomain.waitSampling(200000)
      }
    }


  }
}

object Foo32 extends App{
  class Sub extends Component{
//    val io = new Bundle{
//      val x = in Bits(32 bit)
//      val y = out Bits(32 bit)
//    }

//    val miaou = Bits(32 bit).noSpinalPrune()

//    val dout = (out Bits(32 bit)) noSpinalPrune()
  }
  class Top extends Component{
//    val io = new Bundle{
//      val din = in Bits(32 bit)
//      val dout = (out Bits(32 bit)) noSpinalPrune()
//    }
//
//    val notused1, notused2 = Bits(32 bit)
//    val dut = new Sub()
//
//    dut.io.x  := notused1
//    notused2 := dut.io.y
//    io.dout := io.din

//    val miaou = Bits(32 bit).keep()
//
//    val sub = new Sub()

    val io = new Bundle {
      val xx = Some(in Bool())
    }
    println(io.flatten.mkString(","))
  }


  class TestIO(a: Boolean = false) extends Bundle {
    val z = Bool
    val more = if (a) Some(Bool) else None
  }

  class Test extends Component {
    val io = new Bundle {
      val b = out Bool()
      val t = in(new TestIO(true))
    }
    val ret = io.t.more match {
      case Some(v: Bool) => v && io.t.z
      case None => io.t.z
    }
    io.b := ret
  }

  SpinalConfig(removePruned = false).generateVerilog(new Test).printRtl()
}


case class Foo2() extends Component {
  val io = new Bundle {
    val addr = in UInt(6 bits)
    val writeData = in Bits(8 bits)
    val readData = out Bits(8 bits)
    val enable = in Bool()
    val writeEnable = in Bool()
  }

  val mem = Mem(Bits(32 bits), 16)
  io.readData := mem.readWriteSyncMixedWidth(io.addr, io.writeData, io.enable, io.writeEnable)
//  mem.writeMixedWidth(io.addr, io.writeData, io.enable)
//  mem.readSyncMixedWidth(io.addr, io.readData, io.enable)
}

object Foo2 {
  def main(args: Array[String]): Unit = {
    SpinalConfig(targetDirectory="rtl-gen").addStandardMemBlackboxing(blackboxAll).generateVerilog(Foo2())
  }
}



case class TellMeWhy() extends Component{
  case class payload_t() extends Bundle {
    val id = UInt(4 bits)
    val addr = UInt(32 bits)
    val wdata = UInt(64 bits)
    val be = UInt(8 bits)
  }
  val cond = in(Bool())
  val read_pointer_n = UInt(3 bits)
  // todo The correct code is as follows
//    val read_pointer_n = UInt(3 bits).noCombLoopCheck
  val read_pointer_q = Reg(UInt(3 bits)).init(0)
  when(read_pointer_q === 7) {
    read_pointer_n := U(0, 3 bits)
  } .otherwise {
    read_pointer_n := read_pointer_q+1
  }
  when(cond) {
    read_pointer_q := 0
  } .otherwise {
    read_pointer_q := read_pointer_n
  }
  case class bitsTests_B() extends Bundle{
    val a = Bool()
    val b = Bits(8 bits)
  }
  val bitsTests = out(Vec(bitsTests_B(), 4)).setAsReg()
  bitsTests.map(_.init(bitsTests_B().getZero))
  bitsTests.allowUnsetRegToAvoidLatch
  val tmp_read_pointer = Cat(read_pointer_n,read_pointer_q,True,False)
  val vecSel = in(UInt(2 bits)).dontSimplifyIt().keep()
  for( i<- 0 until 4){
    switch(vecSel){
      is(0) { bitsTests(0).b(i) := tmp_read_pointer(i) }
      is(1) { bitsTests(1).b(i) := tmp_read_pointer(i) }
      is(2) { bitsTests(2).b(i) := tmp_read_pointer(i) }
      is(3) { bitsTests(3).b(i) := tmp_read_pointer(i) }
    }
    when(cond){
      bitsTests(3).b(4+i)  := tmp_read_pointer(4+i)
      // bitsTests(3).b(5)  := tmp_read_pointer(5)
      // bitsTests(3).b(6)  := tmp_read_pointer(6)
      // bitsTests(3).b(7)  := tmp_read_pointer(7)
    }
  }
  val ResultDepth = 4
  val result_d = Vec(Vec(payload_t(), 8), ResultDepth)//.noCombLoopCheck.allowOverride
  val result_q = Reg(Vec(Vec(payload_t(), 8), ResultDepth))
  //  result_q.map(_.map(_.init(payload_t().getZero)))
  result_q := result_d
  result_d := result_q

  val beTest_cnt_i = in UInt(3 bits)

  for(axi_byte <- 0 until 8){
    //      result_d(0)(0).wdata(8*axi_byte,8 bits) := U"8'h47"
    //      (result_d(0)(0).be)(axi_byte) :=  (cond | vecSel.msb)
    result_d(read_pointer_q.resized)(beTest_cnt_i).wdata(8*axi_byte,8 bits) := U"8'h47"
    result_d(read_pointer_q.resized)(beTest_cnt_i).be(axi_byte) := (cond | vecSel.msb)
    // todo The correct code is as follows
    //  result_d(read_pointer_q.resized)(beTest_cnt_i).be(axi_byte, 1 bits) := (cond | vecSel.msb).asUInt.resized
  }
  val result = out(result_q)
}

object Test1 extends App{
  SpinalSystemVerilog(SpinalConfig(
    targetDirectory = "build/tests"
  )){
    val tmp = TellMeWhy()
    tmp
  }
}


object Debug222 extends App{
  object MyEnum extends SpinalEnum{
    val A,B,C = newElement()
  }
//  SpinalConfig(globalPrefix="miaou").generateVerilog(new Component {
//    setDefinitionName("miaou")
//    val x = in(MyEnum())
//  })
  SimConfig.withConfig(SpinalConfig()).withFstWave.doSim(new Component {
    setDefinitionName("miaou")
    val x = out(MyEnum())
    x := MyEnum.C
  }){dut =>
    sleep(10)
  }
}



object Miaou43414 extends App{
  case class adder() extends Component{
    val io=new Bundle{
      val data0,data1=in UInt(8 bits)
      val sum=out UInt(8 bits)
    }
    io.sum.setAsReg()
    noIoPrefix()

    val p = this.parent.asInstanceOf[top]
    io.sum:=io.data0+io.data1 + p.io.data0.pull()
  }

  case class top() extends Component{
    val io=new Bundle{
      val data0,data1,data2,data3=in UInt(8 bits)
      val sum1,sum2=out UInt(8 bits)
    }
    noIoPrefix()
    val adderInst0=adder()
    val adderInst1=adder()
    adderInst0.io.data0<>io.data0
    adderInst0.io.data1<>io.data1
    adderInst0.io.sum<>io.sum1

    adderInst1.io.data0<>io.data2
    adderInst1.io.data1<>io.data3
    adderInst1.io.sum<>io.sum2
  }

  SpinalVerilog(top())
}

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io.TriState

case class FIFO_IF(width_d: Int, width_q: Int) extends Bundle with IMasterSlave {
  val data = Bits(width_d bits)
  val q = Bits(width_q bits)
  val empty, full, rd, wr, reset = Bool()

  override def asMaster(): Unit = {
    out(data, rd, wr, reset)
    in(q, empty, full)
  }
}

class FTDICtrl(tick_div: Int) extends Component {
  val io = new Bundle {
    val ftdi = new Bundle {
      val data = master(TriState(Bits(8 bits)))
      val txe_n, rxf_n = in Bool()
      val rd_n, wr = out Bool()
    }
    val c_fifo = master(FIFO_IF(8,8))
    val a_fifo = master(FIFO_IF(32,8))
  }

  val ftdi_fsm = new StateMachine {
    val FTD_RCHK, FTD_WCHK, C_FIFO_WR, A_FIFO_RD = new State
    val FTD_READ, FTD_WRITE = new StateDelay(cyclesCount = tick_div*2)

    setEntry(FTD_RCHK)

    val can_read = RegNext(~(io.ftdi.rxf_n | io.c_fifo.full))
    val can_write = RegNext(~(io.ftdi.txe_n | io.a_fifo.empty))

    io.c_fifo.data := io.ftdi.data.read
    io.ftdi.data.write := io.a_fifo.q

    io.ftdi.rd_n := True
    io.ftdi.wr := False
    io.c_fifo.wr := False
    io.a_fifo.rd := False
    io.ftdi.data.writeEnable := False

    FTD_RCHK
      .whenIsActive {
        when(can_read) {
          goto(C_FIFO_WR)
        }.otherwise {
          goto(FTD_WCHK)
        }
      }

    C_FIFO_WR
      .whenIsActive {
        io.c_fifo.wr := True
        goto(FTD_READ)
      }
      .onExit {
        io.c_fifo.wr := False
      }

    FTD_READ
      .whenIsActive {
        io.ftdi.rd_n := False
      }
      .whenCompleted {
        io.ftdi.rd_n := True
        goto(FTD_WCHK)
      }

    FTD_WCHK
      .whenIsActive {
        when(can_write) {
          goto(FTD_WRITE)
        }.otherwise {
          goto(FTD_RCHK)
        }
      }

    FTD_WRITE
      .whenIsActive {
        io.ftdi.data.writeEnable := True
        io.ftdi.wr := True
      }
      .whenCompleted {
        io.ftdi.data.writeEnable := False
        io.ftdi.wr := False
        goto(A_FIFO_RD)
      }

    A_FIFO_RD
      .whenIsActive {
        io.a_fifo.rd := True
      }
      .onExit {
        io.a_fifo.rd := False
        goto(FTD_RCHK)
      }

  }
  ftdi_fsm.setEncoding(binaryOneHot)
}

object FTDICtrlVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new FTDICtrl(10))
  }
}


object SynthesisPlay {
  def main(args: Array[String]) {
    def buf1[T <: Data](that : T) = KeepAttribute(RegNext(that)).addAttribute("DONT_TOUCH")
    def buf[T <: Data](that : T) = buf1(buf1(buf1(that)))
    def firstOh(width : Int) = new Rtl {
      override def getName(): String = s"firstOhAdder$width"
      override def getRtlPath(): String = s"$getName.v"
      SpinalVerilog(new Component {
        val mask, state = buf(in(UInt(width bits)))
        val result = OHMasking.first(mask)
        val output = out(buf(result))
        setDefinitionName(s"firstOhAdder$width")
      })
    }
    def firstOhV2(width : Int) = new Rtl {
      override def getName(): String = s"firstOhLut$width"
      override def getRtlPath(): String = s"$getName.v"
      SpinalVerilog(new Component {
        val mask, state = buf(in(UInt(width bits)))
        val result = OHMasking.firstV2(mask)
        val output = out(buf(result))
        setDefinitionName(s"firstOhLut$width")
      })
    }
    def robinOh(width : Int) = new Rtl {
      override def getName(): String = s"robinOh$width"
      override def getRtlPath(): String = s"$getName.v"
      SpinalVerilog(new Component {
        val mask, state = buf(in(UInt(width bits)))
        val result = OHMasking.roundRobin(mask, state)
        val output = out(buf(result))
        setDefinitionName(s"robinOh$width")
      })
    }

    def firstUIntV2(width : Int) = new Rtl {
      override def getName(): String = s"firstUIntLut$width"
      override def getRtlPath(): String = s"$getName.v"
      SpinalVerilog(new Component {
        val mask, state = buf(in(UInt(width bits)))
        val result = OHToUInt(OHMasking.firstV2(mask))
        val output = out(buf(result))
        setDefinitionName(s"firstUIntLut$width")
      })
    }
//    val rtls = List(16, 32, 64, 128).flatMap(w => List(firstOh(w),firstOhV2(w), robinOh(w)))
    val rtls = List(16, 32, 64, 128).flatMap(w => List(firstOhV2(w), firstUIntV2(w)))
    val targets = XilinxStdTargets().take(2)

    Bench(rtls, targets)
  }
}