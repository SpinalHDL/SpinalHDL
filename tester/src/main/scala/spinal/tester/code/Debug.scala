
package spinal.tester.code



import spinal.core.Nameable.{DATAMODEL_WEAK, USER_WEAK}
import spinal.core._
import spinal.core.fiber.Handle
import spinal.core.internals.Operator
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}
import spinal.lib.fsm._
import spinal.lib.graphic.Rgb
import spinal.lib.io.TriState
import spinal.lib.sim.{StreamDriver, StreamMonitor}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.ScalaNumber
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


class TestCore(initial: Bool = False) extends Component {
  val io = new Bundle {
    val read = out(Reg(Bool())) init (False)
  }
}

object Tesasdadt {
  def main(args: Array[String]) {
    SpinalVerilog(new TestCore(False))
  }
}

object Debug2 extends App{


  SpinalConfig().includeFormal.generateSystemVerilog(new Component{



//    val input = in(AFix.SQ(8 bit, 8 bit))
//    val i = AFix.SQ(8 bit, 8 bit)
//    i := AFix(0)
//    val regg =  Reg(input) init (i)
//
//    val x = 4 * B"1111"
//    regg := input

//
//    val x = B(U(3), 4)

//    BigInt(1).asInstanceOf[ScalaNumber].

//    val vec = Vec.fill(4)(Reg(UInt(8 bits)) init(0))
//    vec(in UInt(2 bits))(3) := in(Bool())
//    val a = new AFix(maxValue=BigInt("2923003274661805836407369665432566039311865085951"), minValue = 0, exp = -158)
//    val b = new AFix(maxValue=BigInt("162259276829213363391578010288128"), minValue = 0, exp = -104)
//    b := a.rounded(rounding=RoundType.CEIL)

//    val d = new AFix(maxValue=0xFFF00, minValue = 0, exp = -20)
//    val e = new AFix(maxValue=BigInt(0xFFF), minValue = 0, exp = -12)
//    e := d.ceil(-12)
//    e := d.rounded(rounding=RoundType.CEIL)


//    val x = 2.apply{True} //## 6{False}

//    val a = in Bool()
//    val clear = in Bool()
//    val b = out Bool()
//
//    b := DelayWithInit(a, 4){d =>
//      println(d.getBitsWidth)
//    }


//    val x = (0 to 10).map(e => AFix(10, -e exp))
//    val x2 = (0 to 10).map(i => U(9))
//    val x3 = Vec.tabulate(10)(w => UInt(w bits))
//    case class Struct(w : Int) extends Bundle{
//      val a = UInt(7 bits)
//      val b = Bool()
//      val c = (0 to 3).map(e => AFix(10, -e-w exp))
////      val c = AFix(10, -w exp)
//    }
//    val x4 = Vec.tabulate(10)(w => Struct(w))
//    val sel = UInt(4 bits)
//    val y = x.read(sel)

//    new AFix(10240, 10, -10 exp) := new AFix(10, 10, 0 exp)

//    val m1 = new AFix(10240, 10, -10 exp)
//    val m2 = new AFix(10, 10, 0 exp)
//    val m3, m4 = Struct(4)
//    val m5 = Struct(8)
//    val m6 = Bits(8 bits)
//    val m7 = Bits(10 bits)
//    val m8 = UInt(8 bits)
//    val m9 = UInt(10 bits)
//    val m = in.Bool() ? m2 | m1
//    val mx = in.Bool() ? m1 | m2
//    val my = in.Bool() ? m6 | m7
//    val mz = in.Bool() ? m8 | m9.resized

    //    val x = BufferCC(
//      Rgb(5,6,5),
//      init = {
//        val i = Rgb(5,6,5)
//        i.r := 0
//        i.g := 1
//        i.b := 2
//        i
//      }
//    )

//    val x,y = Vec(True, True)
//    val z = x rawrrr y
//    val checker = GenerationFlags formal new Area {
//
//    }

//    assert(False, L"miaou $False", FAILURE)
//    val sel = in UInt(0 bits)
//    val result = out(sel.muxListDc(List(0 -> U(32, 8 bits))))

//    val muxSel = Seq(
//      false -> True,
//      true  -> False
//    )
//    val muxOut = False.muxList(muxSel)

//    val muxSel = Seq(
//      0 -> True,
//      1  -> False
//    )
//    val muxOut = U(1).muxList(muxSel)

//    val bus = new Area {
//      val write = Bool()
//      val address = UInt(8 bits)
//    }
//    class RegInst(address : Int) extends Area{
//      val isWriting = bus.write && bus.address === address
//    }
//    val myReg = new RegInst(42)

//    val x = History(False, 2)
//    val cacheFsm = new StateMachine {
//      //    val idle = new State with EntryPoint
//      val idle = makeInstantEntry()
//      val flagCompare = new State
//      val allocation = new State
//      val writeBack = new State
//    }

//    case class Wuff(val a : Int) extends Bundle{
//      println(a)
//    }
//
//    val miaou = Wuff(1)
//    val miaou2 = miaou.copy(2)
//
//    class Miaou(val a : UInt = UInt(32 bits), b : Bool = Bool()) extends Bundle
//
//    val x = new Miaou()
//    val y = new Miaou(42, False)
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
      val IDLE = new State
      val STATE_A, STATE_B, STATE_C = new State

      setEntry(IDLE)

      IDLE.whenIsActive(goto(STATE_A))
      STATE_A.whenIsActive(goto(STATE_B))
      STATE_B.whenIsActive(goto(STATE_C))
      STATE_C.whenIsActive(goto(STATE_B))

//      setEncoding(SpinalEnumEncoding(id => id*2))

//      val mapping = mutable.LinkedHashMap[Int , BigInt](
//        states.indexOf(IDLE) -> 0,
//        states.indexOf(STATE_A) -> 1,
//        states.indexOf(STATE_B) -> 3,
//        states.indexOf(STATE_C) -> 7,
//        states.indexOf(stateBoot) -> 15
//      )
//      setEncoding(SpinalEnumEncoding(mapping.apply))
      setEncoding(
        IDLE -> 0,
        STATE_A -> 1,
        STATE_B -> 3,
        STATE_C -> 7,
        stateBoot -> 15
      )
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


object PlaySdcTag extends App{
  class GeneratedClockTag(val source : Bool, val ratio : Int) extends SpinalTag
  val report = SpinalVerilog(new Component{
    val clkDiv2 = RegInit(False)
    clkDiv2 := !clkDiv2
    val clkDivided = ClockDomain.current.copy(clock = clkDiv2)
    clkDiv2.addTag(new GeneratedClockTag(ClockDomain.current.readClockWire, 2))
  })
  report.toplevel.walkComponents{ c =>
    c.dslBody.walkDeclarations{
      case signal : Bool => {
        signal.getTag(classOf[GeneratedClockTag]) match {
          case Some(tag) =>  println(s"$signal is ${tag.source} divided by  ${tag.ratio}")
          case None =>
        }
      }
      case _ =>
    }
  }
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
  import spinal.core.formal._

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
  val vecSel = in(UInt(2 bits)).dontSimplifyIt().dontSimplifyIt()
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

    def shift(width : Int) = new Rtl {
      override def getName(): String = s"shift$width"
      override def getRtlPath(): String = s"$getName.v"
      SpinalVerilog(new Component {
        val input = buf(in(UInt(width bits)))
        val sel = buf(in(UInt(log2Up(width) bits)))
        val result = out(buf(Shift.rightWithScrap(input.asBits, sel)))
//        val result = out(buf((input |<< sel).resize(width)))
        setDefinitionName(s"shift$width")
      })
    }
//    val rtls = List(16, 32, 64, 128).flatMap(w => List(firstOh(w),firstOhV2(w), robinOh(w)))
//    val rtls = List(16, 32, 64, 128).flatMap(w => List(firstOhV2(w), firstUIntV2(w)))
    val rtls = List(64, 128).flatMap(w => List(shift(w)))
    val targets = XilinxStdTargets().take(2)

    Bench(rtls, targets)
  }
}


class Demo extends Module {
  val inStream = slave Stream Bits(8 bits)
  val outStream = master Stream Bits(8 bits)
  val inClkDomain = ClockDomain.current
  val outArea = new SlowArea(4) {
    outStream <-< inStream.ccToggle(inClkDomain, ClockDomain.current)
    ClockDomain.current.clockEnable.simPublic()
  }
}

object Demo extends App {
  import spinal.lib.sim._
  import spinal.core.sim._
  SpinalSimConfig().compile(new Demo).doSim { dut =>
    dut.clockDomain.forkStimulus(20)

    var num = 0
    def seqNumber(payload: Bits) = {
      payload #= num
      num += 1
      true
    }
    val drv = StreamDriver(dut.inStream, dut.clockDomain)(seqNumber)
    drv.transactionDelay = () => 0

    dut.outStream.ready #= true
    StreamMonitor(dut.inStream, dut.clockDomain)(p => print(s"in data ${SimData.copy(p)}"))
    StreamMonitor(dut.outStream, dut.outArea.clockDomain)(p => print(s"out data ${SimData.copy(p)}"))

    dut.clockDomain.waitSampling(100)
    // print
    // in data self : 0x0
    // in data self : 0x1
    // out data self : 0x0
    // in data self : 0x2
    // in data self : 0x3
    // out data self : 0x2
    // in data self : 0x4
    // in data self : 0x5
    // out data self : 0x4
    // in data self : 0x6
    // in data self : 0x7
    // out data self : 0x6
    // ...
  }
}



object PlayPackedData extends App{
  class PackedData[T <: Data](hardType : HardType[T], size : Int) extends MultiData {
    override val elements = ArrayBuffer[(String, Data)]()
    class Element(val name : String, val raw : Bits, val width : Int)
    val elementsMap = mutable.LinkedHashMap[String, Element]()

    packedBuild()

    def packedBuild() {
      val template = hardType()
      for ((e, name) <- (template.flatten, template.flattenLocalName).zipped) {
        val w = widthOf(e)
        val ref = Bits(widthOf(e) * size bits)
        elements += name -> ref
        elementsMap(name) = new Element(name, ref, w)
        ref.parent = this
        if (OwnableRef.proposal(ref, this)) ref.setPartialName(name, Nameable.DATAMODEL_WEAK)
      }
    }

    def pack(that : T, index : Int) = {
      for((from, name) <- (that.flatten, that.flattenLocalName).zipped){
        val to = elementsMap(name)
        assert(widthOf(from) == to.width)
        to.raw(to.width*index, to.width bits) := from.asBits
      }
    }

    def unpack(index : Int) : T = {
      val ret = hardType()
      for((to, name) <- (ret.flatten, ret.flattenLocalName).zipped){
        val from = elementsMap(name)
        to.assignFromBits(from.raw(from.width*index, from.width bits))
      }
      ret
    }

    override def assignFromImpl(that: AnyRef, target: AnyRef, kind: AnyRef): Unit = {
      that match {
        case that: PackedData[_] =>
          ???
        case _ => throw new Exception("Undefined assignment")
      }
    }
  }

  SpinalVerilog(new Component {
    val x = new PackedData(AxiLite4(32,32), 4)//.assignDontCare()
//    val y = AxiLite4(32,32)
//    x.pack(y, 2)
//    val z = x.unpack(2)
  })
}


class AssertDemo extends Component {
  val io = new Bundle {
    val input = slave Stream(UInt(8 bits))
    val output = master Stream(UInt(8 bits))
  }

  assert(False, L"${REPORT_TIME} time: ${io.input.payload}".toList)
  io.output <-/< io.input
}

object AssertDemo {
  def main(args: Array[String]): Unit = {
    SimConfig.withIVerilog.doSim(new AssertDemo){ dut =>
      dut.getAllIo.filter(_.isInput).foreach(_.randomize())
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling(100)
    }
  }
}

import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._
import spinal.lib.graphic.Rgb

object PipelinePlay2 extends App{
  SpinalVerilog(new Component {
    val io = new Bundle {
      val input = slave(Stream(Rgb(5,6,5)))
      val output = master(Stream(UInt(14 bits)))
    }
    val pipeline = new Pipeline{
      val stageA = new Stage{
        valid := io.input.valid
        io.input.ready := isReady
        val rgb = insert(io.input.payload)
        val miaou = isRemoved
      }
      val stageB = new Stage(Connection.M2S()){
        val pow2 = new Area{
          val r = insert(stageA.rgb.r * stageA.rgb.r)
          val g = insert(stageA.rgb.g * stageA.rgb.g)
          val b = insert(stageA.rgb.b * stageA.rgb.b)
        }
      }

      val stageC = new Stage(Connection.DIRECT()){
        val sum = insert(stageB.pow2.r +^ stageB.pow2.g +^ stageB.pow2.b)
        throwIt(U(32) === 0)
      }

      val stageD = new Stage(Connection.M2S()){
        io.output.valid := isValid
        haltIt(!io.output.ready)
        io.output.payload := stageC.sum
      }
    }
    pipeline.build()
  })
}



object DebugAheadValue extends App{
  SimConfig.compile(new Component{
    val conds = in(Vec.fill(8)(Bool()))
    val a, b, c, d, e = out(Reg(UInt(8 bits)) init(0))
    val f = out(Reg(UInt(16 bits)) init(0))  //here is what I added.

    when(conds(0)){
      a := 1
      when(conds(1)){
        a := 2
        b := 11
      } otherwise {
        a := 3
        c := 21
      }
      f(0, 8 bits) := a  //here is the parial assignment.
    }
    when(conds(2)){
      a := 4
      b := 12
      c := 22
      d := 31
      f(8, 8 bits) := a
    }

    val x = out(a.getAheadValue)
    val y = out(a.getAheadValue)
    val z = out(b.getAheadValue)
    val w = out(f.getAheadValue)

    when(d.getAheadValue() === 0){
      e := 1
    }
  }).doSim(seed = 42){dut =>
    var an,bn,cn,a,b,c = 0
    dut.conds.foreach(_ #= false)
    dut.clockDomain.forkStimulus(10)
    dut.clockDomain.waitSampling()

    for(i <- 0 until 1000){
      if(dut.conds(0).toBoolean){
        an = 1
        if(dut.conds(1).toBoolean){
          an = 2
          bn = 11
        } else {
          an = 3
          cn = 21
        }
      }
      if(dut.conds(2).toBoolean){
        an = 4
        bn = 12
        cn = 22
      }
      assert(dut.x.toInt == an)
      assert(dut.y.toInt == an)
      assert(dut.z.toInt == bn)
      assert(dut.a.toInt == a)
      assert(dut.b.toInt == b)
      assert(dut.c.toInt == c)
      a = an
      b = bn
      c = cn
      dut.clockDomain.waitSampling()
      dut.conds.foreach(_.randomize())
    }
  }
}

import spinal.core._
class DemoBlackbox extends BlackBox {
  val io = new Bundle{
    val a = in Bool()
    val b = out Bool()
  }
  setInlineVerilog(
    """
      |module DemoBlackBox(
      | input a,
      | output b
      |);
      |assign b = a;
      |endmodule
      |""".stripMargin)
}

class Test12345 extends Component{
  val io = new Bundle{
    val a = in Bool()
    val b = out Bool()
    val a2 = in Bool()
    val b2 = out Bool()
  }
  val black = new DemoBlackbox
  black.io.a <> io.a
  black.io.b <> io.b
  val blue = new DemoBlackbox
  blue.io.a <> io.a2
  blue.io.b <> io.b2
}
object Test12345 extends App{
  SpinalConfig(oneFilePerComponent = true, targetDirectory="miaou").generateVerilog(new Test12345)
}
