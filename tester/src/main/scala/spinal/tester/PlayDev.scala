package spinal.tester

import spinal.core._
import spinal.core.internals._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Gpio, Apb3SlaveFactory}
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4Shared}
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SpecRenamer}
import spinal.lib.com.i2c._
import spinal.lib.com.spi.{Apb3SpiMasterCtrl, SpiMasterCtrlGenerics, SpiMasterCtrlMemoryMappedConfig}
import spinal.lib.io.{InOutWrapper, TriState}
import spinal.lib.soc.pinsec.{Pinsec, PinsecConfig}
import sun.nio.cs.ext.MS949

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag



object PlayDevRamZero{
  def main(args: Array[String]): Unit = {
    SpinalConfig(verbose = true).generateVerilog(new StreamFifo(Bits(8 bits), 1))
  }
}

object PlayDevMem{
  class TopLevel extends Component {
    val mem = Mem(Bits(32 bits), 64)
    val p0 = new Bundle{
      val address = in(mem.addressType)
      val data = in(mem.wordType)
      val mask = in(Bits(4 bits))
      val enable = in Bool()
    }
    when(p0.enable) {
      mem.write(p0.address, p0.data, mask = p0.mask)
    }

    val p1 = new Bundle{
      val address = in(mem.addressType)
      val data = out(mem.wordType)
    }
    p1.data := mem.readSync(p1.address, True)


    val p2 = new Bundle{
      val address = in(mem.addressType)
      val data = out(mem.wordType)
    }
    p2.data := mem.readAsync(p2.address)
    val xx = RegNext(True)

    println(LatencyAnalysis(p0.address, p2.data))

    val p3 = new Bundle{
      val address = in(mem.addressType)
      val data = in(mem.wordType)
      val mask = in(Bits(4 bits))
      val enable = in Bool()
      val wr = in Bool()
    }
//
//    mem.readWriteSync(
//      address = p3.address  ,
//      data = p3.data  ,
//      enable = p3.enable  ,
//      write = p3.wr  ,
//      mask = p3.mask
//    )

  }

  def main(args: Array[String]) {
    val toplevel = SpinalConfig().addStandardMemBlackboxing(blackboxAll).generateVhdl(new TopLevel()).printPruned()
  }
}

object PlayDevErrorReport{
  class TopLevel extends Component {
    val a = in UInt(8 bits)
    val b = in UInt(10 bits)
    val result = a | b
    val x = UInt(32 bits)
    x(3 downto 1) := a
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
  }
}


object PlayDevLatency{
  class TopLevel extends Component {
    val a,b = in Bool()
    val tmp = Reg(Bool)
    val tmp2 = Bool
    val tmp3 = Reg(Bool)


   // tmp := a
    when(RegNext(!a)) {
      tmp2 := RegNext(tmp)
    }
    tmp3 := Delay(tmp2,5)

    println(LatencyAnalysis(a, tmp3))
    System.exit(0)
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
  }
}


object PlayDevPullCkock{
  class SubSub extends Component{
    val x = RegNext(True) init(False)
  }
  class Sub extends Component{
    val subsub = new SubSub
  }
  class TopLevel extends Component {
    val cd = ClockDomain(Bool, Bool)
    val sub = cd(new Sub)
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
  }
}

object PlayDevMaskedCase{

  class TopLevel extends Component {
    val sel = in Bits(4 bits)
    val result = out Bits(8 bits)

    switch(sel){
      is(M"---1"){
        result := 0
      }
      is(M"--1-"){
        result := 1
      }
      is(M"-1--"){
        result := 2
      }
      default{
        result := 3
      }
    }
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
  }
}


object PlayDevMemReadWrite{

  class TopLevel extends Component {
    val mem = Mem(Bits(8 bits), 64)
    val address = UInt(6 bits)
    val wrData, rdData = Bits(8 bits)
    val en, wr = Bool
    rdData := mem.readWriteSync(address,wrData,en,wr)
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
  }
}

object PlayDevFillSwitch{

  class TopLevel extends Component {
    val sel = UInt(2 bits)
    val result = Bits(8 bits)

    result := 3
    switch(sel){
      is(0){
        result := 0
      }
//      is(1){
//        result := 1
//      }
//      is(2){
//        result := 2
//      }
      is(3){
//        result := 3
      }
      default{
        result := 1
      }
    }
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
    SpinalVerilog(new TopLevel())
  }
}

object PlayDevStackTrace{

  def main(args: Array[String]) {
    for(i <- 0 to 1000000) {
      val startAt = System.nanoTime()
      val x = List.fill(1000000)(new Throwable)
      val endAt = System.nanoTime()
      println((endAt - startAt) * 1e-6)
    }
  }
}


object PlayDevCombLoop{

  class TopLevel extends Component {
    val result,result2  = UInt(8 bits)

    def miaou = {
      val x, y, z  = UInt(8 bits)
      x := y
//      y := x
      result := x
      z := z
    }


    val xx, yy, zz  = UInt(8 bits)
    xx := yy
    yy := 0
    when(True) {
      yy := zz
    }
    zz := xx
    result2 := xx


    val xxx = UInt(8 bits)
    xxx := xxx
    miaou

  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
    SpinalVerilog(new TopLevel())
  }
}



object PlayDevCrossClock{

  class TopLevel extends Component {
    val clockA = in Bool
    val clockB = in Bool

    val areaA = new ClockingArea(ClockDomain(clockA)){
      val reg = Reg(Bool)
      reg := in(Bool)
      val wire = Bool()
      wire := reg
      val wire2 = Bool()
      wire2 := wire
    }

    val areaB = new ClockingArea(ClockDomain(clockB)){
      val reg = Reg(Bool)
      reg := areaA.wire2
      val output = out Bool()
      output := reg
    }
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
    SpinalVerilog(new TopLevel())
  }
}

object PlayDebugPruned{

  import spinal.lib._
  def main(args: Array[String]) {
    SpinalVhdl(StreamFifo(UInt(8 bits), 512)).printPruned()
  }
}



object PlayCondActive{

  class TopLevel extends Component {
    val a,b,c = in Bool()
    var x : Bool = null
    when(a){

    } otherwise{
      when(!b){

      } otherwise{
        when(c){
          x = ConditionalContext.isTrue
        }
      }
    }

  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel
    SpinalVerilog(new TopLevel())
  }
}

object PlayDevSwitchEnum{

  class TopLevel extends Component {
    object State extends SpinalEnum{
      val X,Y, Z = newElement()
    }
    val a= in (State(binarySequential))
    val result = out (UInt(8 bits))

    result := 0
    switch(a){
      is(State.X){
        result := 0
      }
      is(State.Y(binaryOneHot)){
        result := 1
      }
      is(State.Z){
        result := 2
      }
    }

  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object PlayDevTriplify{
  class PhaseTriplify() extends PhaseNetlist{
    override def impl(pc: PhaseContext): Unit = {
      import pc._
      val todo = ArrayBuffer[BaseType]()
      walkStatements{
        case bt : BaseType if bt.isReg => todo += bt
        case _ =>
      }

      todo.foreach{
        case bt : BaseType => {
          bt.setAsComb()
          bt.parentScope.push()
          bt.clockDomain.push()
          def genRegs[T <: BaseType](bt : T)(implicit m: ClassTag[T]) =  {
            val regs = Array.fill(3)(Reg(bt))
            if(bt.isNamed) regs.zipWithIndex.foreach{case (reg,i) => reg.setWeakName(bt.getName() + "_triplify_" + i)}
            bt.foreachStatements(s => {
              def wrap(that : Expression): T ={
                val ret = cloneOf(bt)
                ret.assignFrom(that)
                ret
              }
              def wrapBool(that : Expression): Bool ={
                val ret = Bool()
                ret.assignFrom(that)
                ret
              }
              def wrapWidth(that : Expression, width : Int): T ={
                val ret = weakCloneOf(bt)
                ret.asInstanceOf[BitVector].setWidth(width)
                ret.assignFrom(that)
                ret
              }
              val source = s.target match {
                case target : BitAssignmentFixed => wrapBool(s.source)
                case target : BitAssignmentFloating => wrapBool(s.source)
                case target : RangedAssignmentFixed => wrapWidth(s.source, target.getWidth)
                case target : RangedAssignmentFloating => wrapWidth(s.source, target.getWidth)
                case target : BaseType => wrap(s.source)
              }
              for(i <- 0 until 3){
                val dup = regs(i)
                val dupS = s match{
                  case s : DataAssignmentStatement => new DataAssignmentStatement
                  case s : InitAssignmentStatement => new InitAssignmentStatement
                }

                dupS.target = s.target match {
                  case target : BitAssignmentFixed => BitAssignmentFixed(dup.asInstanceOf[BitVector], target.bitId)
                  case target : BitAssignmentFloating => BitAssignmentFloating(dup.asInstanceOf[BitVector], target.bitId.asInstanceOf[UInt])
                  case target : RangedAssignmentFixed => RangedAssignmentFixed(dup.asInstanceOf[BitVector], target.hi, target.lo)
                  case target : RangedAssignmentFloating => RangedAssignmentFloating(dup.asInstanceOf[BitVector], target.offset.asInstanceOf[UInt], target.bitCount)
                  case target : BaseType => dup
                }
                dupS.source = source

                s.insertNext(dupS)
                dup.dlcAppend(dupS)
              }
            })
            regs
          }


          bt match {
            case bt : Bool => {
              val regs = genRegs(bt)
              bt.removeAssignments()
              bt := (regs(0) & regs(1)) | (regs(0) & regs(2)) | (regs(1) & regs(2))
            }
            case bt : Bits => {
              val regs = genRegs(bt)
              bt.removeAssignments()
              bt := (regs(0) & regs(1)) | (regs(0) & regs(2)) | (regs(1) & regs(2))
            }
            case bt : UInt => {
              val regs = genRegs(bt)
              bt.removeAssignments()
              bt := (regs(0) & regs(1)) | (regs(0) & regs(2)) | (regs(1) & regs(2))
            }
            case bt : SInt => {
              val regs = genRegs(bt)
              bt.removeAssignments()
              bt := (regs(0) & regs(1)) | (regs(0) & regs(2)) | (regs(1) & regs(2))
            }
            case bt : SpinalEnumCraft[_] => {
              val regs = genRegs(bt).map(_.asBits)
              bt.removeAssignments()
              bt.assignFromBits((regs(0) & regs(1)) | (regs(0) & regs(2)) | (regs(1) & regs(2)))
            }
          }

          bt.clockDomain.pop()
          bt.parentScope.pop()
        }
      }
    }
  }

  class TopLevel extends Component {
    val bool = new Bundle {
      val a, b, c = in Bool()
      val result = out Bool()

      val tmp = Reg(Bool)
      when(a){
        tmp := a || c && True
        when(b){
          tmp := b
        }
      }
      result := tmp
    }


    val uint = new Bundle {
      val a, b, c = in UInt(8 bits)
      val result = out UInt(8 bits)

      val tmp = Reg(UInt(8 bits))
      when(a < 3){
        tmp := a | c
        when(b < 6){
          tmp(3 downto 2) := b(6 downto 5)
        }
      }
      result := tmp
    }


    val enum = new Bundle {
      object State extends SpinalEnum{
        val X,Y, Z = newElement()
      }
      val a, b, c = in (State())
      val result = out (State())

      val tmp = Reg(State())
      when(a === State.X){
        tmp := a
        when(b === State.X){
          tmp := b
        }
      }
      result := tmp
    }

  }

  def main(args: Array[String]) {
//        val toplevel = SpinalConfig().addTransformationPhase(new PhaseTriplify).generateVhdl(new Apb3SpiMasterCtrl(
//          SpiMasterCtrlMemoryMappedConfig(
//            ctrlGenerics = SpiMasterCtrlGenerics(
//              ssWidth     = 4,
//              timerWidth  = 12,
//              dataWidth   = 8
//            ),
//            cmdFifoDepth = 32,
//            rspFifoDepth = 32
//          )
//        )).printPruned()
//    val toplevel = SpinalConfig().addTransformationPhase(new PhaseTriplify).generateVhdl(new Pinsec(PinsecConfig.default)).printPruned()
        val toplevel = SpinalConfig().addTransformationPhase(new PhaseTriplify).generateVhdl(new TopLevel).printPruned()
  }
}



object PlayDevMiaou{

  class TopLevel extends Component {
    val sel = Bits(2 bits)
    val result = Bits(8 bits)

    switch(sel){
      is(0) { result := 0 }
      is(1) { result := 1 }
      is(2) { result := 2 }
      is(3) { result := 3 }
    }

//    val a = Reg(UInt(8 bits))
//    a(3 downto 0) := 1
//    a(7 downto 4) := 1

//    val a, b = in Bool()
//    val result = out UInt(4 bits)
//
//    for(i <- 0 to 3){
//      result(i) := b
//    }


//    val a,b,c = in UInt(32 bits)
//    val result = out UInt(32 bits)
//
//    val tmp = Reg(UInt(32 bits)) init(42)
//    when(a === 666){
//      tmp := (tmp + 1) & 0xFFFF
//    }
//
//    result := tmp

//    val x = out UInt(32 bits)
//    x := U"0101".resized
//    val a,b,c,d = Reg(UInt(32 bits)) init(0)
//    val sel = in UInt(2 bits)
//    val data = in UInt(32 bits)
//    Vec(a,b,c,d)(sel) := data

  }

  def main(args: Array[String]) {
    val toplevel = SpinalConfig().dumpWave(depth = 5, vcdPath = "miaou.vcd").generateVerilog(new TopLevel())
    SpinalConfig().dumpWave(depth = 5, vcdPath = "miaou.vcd").generateVhdl(new TopLevel())
    print("done")
  }
}

object PlayDevBugx{
  class TopLevel extends Component {
    val a, b, c, d = in Bits(8 bits)
    val sel = UInt(2 bits)
    val result = Bits(8 bits)
    def rec(that : Bits, level : Int) : Bits = if(level != 0)
      rec(~ that, level -1)
    else
      that
//    switch(sel){
//      is(0) { result := rec(a, 10) }
//      is(1) { result := rec(b, 10) }
//      is(2) { result := rec(c, 10) }
//      is(3) { result := rec(d, 10) }
//    }
    result := Vec(List(a,b,c,d).map(rec(_, 65)))(sel)


//      def rec(that : UInt, level : Int) : UInt = if(level != 0)
//        rec(RegNext(that), level -1)
//      else
//        that
//      result := rec(a & b, 4000)
//    val a,b = in UInt(8 bits)
//    val result = out UInt(7 bits)
//
//    result := a + b
//
//    val sub = StreamFifo(Bool, 10)

//    val mask = MaskedLiteral("10-")
//    val input = in Bits(3 bits)
//    val output = out((0 to 2).map(i => mask(i) === input(i)).asBits())
//    val sel = in UInt(2 bits)
//    val inputsA = in Vec(Bits(8 bits), 4)
//    val inputsB = in Vec(Bool, 4)
////    val outputA = out(inputsA(sel))
//    val outputB = out(inputsB(sel))
//
//    val xx = Bits(4 bits)
//    xx := inputsA(sel)
//    val x = UInt(8 bits)
//    val y = SInt(6 bits)
//    y := x.asSInt
//    val outputs = Vec(Vec(out(Reg(Bool)),3), 2)
//
//    outputs.foreach(_.foreach(_ := False))
//    val x = U"0000000" >> -1
//    case class MyReg() extends Bundle {
//      val reg = UInt(32 bits)
//
//      def byteCount = reg(12 downto 0)
//    }
//
//
//    val reg = Reg(MyReg())
//
//
//    reg.byteCount(7 downto 0).assignFromBits(B"x42")

//
//    val reg = Reg(Bits(32 bits))
//    val sel = in UInt(2 bits)
//    reg := 0
////    reg(12 downto 1)(9 downto 2) := B"x00"
//    reg(16 downto 4)(sel, 12 bits)(8 downto 1) := B"xFF"
////    reg(16 downto 4)(5) := True
  }

  def main(args: Array[String]) {
    val toplevel = SpinalConfig(verbose  = true,defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)).generateVerilog(new TopLevel())
    print("done")
  }
}

object PlayDevAnalog{
  class TopLevel extends Component {
    val cmd = slave(TriState(Bool))
    val gpio = inout(Analog(Bool))

    val tmp = Analog(Bool)
    when(cmd.writeEnable){
      tmp := cmd.write
    }
    cmd.read := tmp

    tmp := gpio
  }

  def main(args: Array[String]) {
    val toplevel = SpinalConfig().generateVhdl(new TopLevel())
    print("done")
  }
}


object PlayWithBug {

  class MyTopLevel extends Component {

    val io = new Bundle{
      val dout = out Bits(32 bits)
      val din  = in Bits(32 bits)

      val incr = in Bool
      val wr   = in Bool
    }

    val dataRD = Vec(Reg(Bits(32 bits)), 8)
    val rdCnt  = Reg(UInt(2 bits)) init(0)
    val wrCnt  = Reg(UInt(3 bits)) init(0)


    when(io.wr){
      wrCnt := wrCnt + 1
      dataRD(wrCnt) := io.din
    }

    when(io.incr){
      rdCnt := rdCnt + 1
    }

    io.dout := dataRD(rdCnt.resized)

  }

  def main(args: Array[String]): Unit = {
    SpinalVhdl(new MyTopLevel)
  }

}

object PlayDevAnalog2{
  class Sub extends Component{
    val cmd2 = slave(TriState(Bool))
    val gpio2 = inout(Analog(Bool))

    val tmp2 = Analog(Bool)
    when(cmd2.writeEnable){
      tmp2 := cmd2.write
    }
    cmd2.read := tmp2

    gpio2 := tmp2
  }

  class TopLevel extends Component {
    val cmd = slave(TriState(Bool))
    val cmd2 = slave(TriState(Bool))
    val gpio = inout(Analog(Bool))

    val tmp = Analog(Bool)
    when(cmd.writeEnable){
      tmp := cmd.write
    }
    cmd.read := tmp
    gpio <> tmp

    val sub = new Sub
    sub.cmd2  <> cmd2
    sub.gpio2 <> gpio
  }

  def main(args: Array[String]) {
    val toplevel = SpinalConfig().generateVhdl(new TopLevel())
    print("done")
  }
}




object PlayDevAnalog3{
  def main(args: Array[String]) {
    SpinalConfig().generateVhdl({
      val toplevel = Apb3Gpio(32)
      toplevel.rework{
        import toplevel._
        io.gpio.asDirectionLess.allowDirectionLessIo
        val analog = inout(Analog(Bits(32 bits))).setName("analog")
        io.gpio.read := analog
        for(i <- 0 to 31){
          when(io.gpio.writeEnable(i)){
            analog(i) := io.gpio.write(i)
          }
        }
        toplevel
      }
    })
    print("done")
  }
}



object PlayDevAnalog4{

  class Toplevel extends Component{
    val io = new Bundle {
      val gpio = master(TriState(UInt(32 bits)))
    }


    val driver = TriState(UInt(32 bits))
    driver.writeEnable := True
    driver.write := 42
    driver <> io.gpio
  }
  def main(args: Array[String]) {
    SpinalVhdl(InOutWrapper(Apb3Gpio(32)))
//    SpinalVhdl(InOutWrapper(new Toplevel))
    print("done")
  }
}

object PlayDevBug3{
  case class bboxedm (io_width : Int, default_value : BigInt) extends BlackBox {
    addGeneric("io_width", U(io_width, 32 bits))
    addGeneric("default_value", U(default_value, io_width bits))

    val io = new Bundle {
      val clk = in Bool
      val a   = in Bits(io_width bits)
      val z   = out Bits(io_width bits)
    }
    mapClockDomain(clock=io.clk)
    noIoPrefix()
  }

  // Instance
  class TopLevel extends Component {
    val bboxedm_inst = bboxedm (io_width = 256, default_value = BigInt("FFFFFF",16))
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel())
    SpinalConfig().generateVerilog(new TopLevel())
    print("done")
  }
}



object PlayDevNoMerge{


  // Instance
  class TopLevel extends Component {
    val x,y, z = False
    z.noBackendCombMerge
    when(in(Bool)){
      x := True
      y := True
    } otherwise {
      z := True
    }


    when(x && y && z){

    }
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel())
    SpinalConfig().generateVerilog(new TopLevel())
    print("done")
  }
}





object PlayDevBusMem{


  // Instance
  class TopLevel extends Component {
    val bus = slave(Apb3(16, 32))
    val factory = Apb3SlaveFactory(bus)
    val mem = Mem(Bits(12 bits), 256)
    factory.readSyncMemWordAligned(mem, addressOffset = 512)
//    factory.writeMemWordAligned(mem, addressOffset = 512)
    val xx = mem.writePort
    xx.valid := True
  }


  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel())
    SpinalConfig().generateVerilog(new TopLevel())
    print("done")
  }
}



object PlayDevAxi{


  // Instance
  class TopLevel extends Component {
    val M00_AXI = AxiLite4(addressWidth = 32, dataWidth = 32)
    AxiLite4SpecRenamer(M00_AXI)
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel())
    print("done")
  }
}

object PlayDevDefault{

  class Sub extends Component{
    val sa,sb = in Bool() default(False)
    val sresult = out Bool() default(False)
    val stmp = Bool() default(False)
  }
  // Instance
  class TopLevel extends Component {
   val sub = new Sub()


    val a,b = in Bool() default(False)
    val result = out Bool() default(False)
    val tmp = Bool() default(False)
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel())
    print("done")
  }
}



object PlayDevMessages{

  class RGB(width : Int) extends Bundle{
    val r,g,b = UInt(width bits)
  }

  class TopLevel extends Component {
    val myUIntOf_8bit = UInt(8 bits)
    myUIntOf_8bit := (2 -> false, default -> true)
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel())
    print("done")
  }
}


object PlayDevSynthesis{



  class TopLevel extends Component {
    val inputs = in(Vec(Bool, 16))
    val sel = in(UInt(4 bits))
    val output = out(inputs(sel))
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVerilog(new TopLevel())
    print("done")
  }
}


object PlayDevBug123{



  class TopLevel extends Component {
    val a,b,c = in Bool()
    val x = out Bool()
    when(a){
      x := b
    } otherwise {
      x := c
    }
//    x := c
//    switch(a){
//      is(True){
//        x := b
//      }
//      is(False){
//
//      }
//    }
//    when(a){
//      x := b
//    }
//    val l = in Bits(8 bits)
    //    val a,b,c = in Bool()
    //    val x = out Bits(8 bits)
//    x := l
//    x(1 downto 0) := 0
//    x := 0
//
//    when(a){
//      x(1 downto 0) := 1
//    }
//    x(7 downto 2) := 3
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVerilog(new TopLevel())
    print("done")
  }
}



object PlayDevFeature{
  class TopLevel extends Component {
    val x = Vec(Reg(UInt(8 bits)) init(0), 4)
    val y = Vec(Reg(UInt(8 bits)), 4)
    y.foreach(_ init(0))

    //Just to avoid having reseted only flops
    x := y
    y := x

  }

  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog(new TopLevel())
  }
}


object PlayDevFeature2{
  class TopLevel extends Component {
    val rom1 = Mem((0 to 5).map(addr => Vec((0 to 2).map(i => Vec((0 to 3).map(j => U(i+j + addr, 8 bits)))))))
    val rom2 = Mem(Vec(Vec(UInt(8 bits), 4), 3), (0 to 5).map(addr => Vec((0 to 2).map(i => Vec((0 to 3).map(j => U(i+j + addr, 8 bits)))))))
    val rom3 = Mem(Vec(Vec(UInt(8 bits), 4), 3), 6) init((0 to 5).map(addr => Vec((0 to 2).map(i => Vec((0 to 3).map(j => U(i+j + addr, 8 bits)))))))
  }

  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog(new TopLevel())
    config.generateVhdl(new TopLevel())
    print("done")
  }
}

case class ArgConfig(debug : Boolean = false,
                     iCacheSize : Int = 4096)

object PlayDevConsole{
  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[ArgConfig]("VexRiscvGen") {
      //  ex :-d    or   --debug
      opt[Unit]('d', "debug")    action { (_, c) => c.copy(debug = true)   } text("Enable debug")
      // ex : -iCacheSize=XXX
      opt[Int]("iCacheSize")     action { (v, c) => c.copy(iCacheSize = v) } text("Set instruction cache size")
    }

    val argConfig = parser.parse(args, ArgConfig()).get

    println(argConfig)
  }
}

object PlayDevMiaou43{
  case class StatusReg() extends Bundle{
    val register = Bits(32 bits)
    def start = register(0)
  }
  object StatusReg{
    def apply(value: BigInt): StatusReg = {
      val bundle = StatusReg()
      bundle.register := value
      return bundle
    }
  }

  class TopLevel extends Component {
    val apb = slave(Apb3(32,32))
    val factory = Apb3SlaveFactory(apb)
    def driveIO(factory: Apb3SlaveFactory, baseAddress: BigInt){
      val status = factory.createReadAndWrite(StatusReg(), baseAddress + 0x00) init(StatusReg(0))

      when(status.start){
        status.start := False
      }
    }
    driveIO(factory, 0)
//    val status = factory.createReadAndWrite(StatusReg(), 0x00) init(StatusReg(0))
//    when(!status.start){ status.start := False }

  }

  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog(new TopLevel())
  }
}


class AlteraMemTagger extends PhaseNetlist{
  override def impl(pc: PhaseContext): Unit = {
    pc.walkDeclarations{
      case mem : Mem[_] =>
        mem.foreachStatements{
          case s : MemReadSync => s.readUnderWrite
          case s : MemReadAsync =>
          case _ =>
        }
        mem.addAttribute("ReadWritePolicy", "WriteFirst")
      case _ =>
    }
  }
}

object PlayDevTest32 extends App{
  SpinalConfig().addTransformationPhase(new AlteraMemTagger).generateVerilog(StreamFifo(UInt(8 bits), 32))
}


object PlayWithIndex extends App {
  class MyTopLevel extends Component{
    val io = new Bundle{
      val load  = in Bool
      val din   = in Bool
      val index = in UInt(log2Up(60) bits)
      val value = out UInt(60 bits)
    }

    val register = Reg(cloneOf(io.value)) init(0)


    when(io.load){
      register(io.index) := io.din
    }

    io.value := register

  }

  SpinalVhdl(new MyTopLevel)
}


object PlayWithIndex222 extends App {
  class MyTopLevel extends Component{
    val a, b = UInt(8 bits)
    for(i <- 0 to 7){
      b(i) := a(i)
    }
    val miaou = out(True)
    miaou.addTag(ClockDomainTag(ClockDomain.current))
    miaou.addTag(ClockDomainTag(ClockDomain.current))
  }

  val report = SpinalVerilog(new MyTopLevel)
  println("asd")
}



object PlayDeterministicGeneration extends App {
  SpinalConfig(verbose=true).generateVerilog(new Component{
    val e = for(i <- 0 until 2) yield new Area{
      val ctrl = new spinal.lib.com.i2c.Apb3I2cCtrl(      I2cSlaveMemoryMappedGenerics(
        ctrlGenerics = I2cSlaveGenerics(
          samplingWindowSize = 3,
          samplingClockDividerWidth = 10 bits,
          timeoutWidth = 20 bits
        ),
        addressFilterCount = 4,
        masterGenerics = I2cMasterMemoryMappedGenerics(
          timerWidth = 12
        )
      ))

      val io = new Bundle{
        val apb =  slave(cloneOf(ctrl.io.apb))
        val i2c = master(I2c())
        val interrupt = out Bool
      }

      io <> ctrl.io
    }
  })

}

object PlayAssertFormal extends App {
  class MyTopLevel extends Component{
    when(False){
      when(False){
        assert(True,"asd")
        assume(True)
        cover(True)

        formal(assert(True))
        formal(assume(True))
        formal(cover(True))
      }
    }
  }

  SpinalSystemVerilog(new MyTopLevel)
  SpinalVerilog(new MyTopLevel)
  SpinalVhdl(new MyTopLevel)
}

object PlayErrorImprovment extends App {
  class MyTopLevel extends Component{
    val data_old = B("32'd0")
    val enable = True
    val data = RegNext(data_old, enable)
  }

  val report = SpinalSystemVerilog(new MyTopLevel)
  println("asd")
}

object PlayInitBoot extends App {
  class MyTopLevel extends Component{
    val data = RegInit(B"00")
    data := 1
  }

  val report = SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT)).generateVerilog(new MyTopLevel)
  println("asd")
}


object PlayNamingImprovment extends App{
  def gen(c : => Component): Unit ={
    SpinalVhdl(c)
    SpinalVerilog(c)
  }
  gen(new Component {
    val input = in Bool()
    val output = out Bool()
    val readableOutput = out Bool()

    output := RegNext(Delay(RegNext(input),4))
    val yolo = RegNext(input)

    readableOutput := True
    val miaou = Bool
    miaou := readableOutput
  })
}