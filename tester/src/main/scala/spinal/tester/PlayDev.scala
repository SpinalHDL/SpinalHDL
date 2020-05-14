package spinal.tester

import spinal.core._
import spinal.core.internals._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3Gpio, Apb3SlaveFactory}
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4Shared}
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4SpecRenamer}
import spinal.lib.com.i2c._
import spinal.lib.com.spi.ddr._
import spinal.lib.com.spi.{Apb3SpiMasterCtrl, SpiMasterCtrlGenerics, SpiMasterCtrlMemoryMappedConfig}
import spinal.lib.eda.bench._
import spinal.lib.graphic.Rgb
import spinal.lib.io.{InOutWrapper, TriState, TriStateArray}
import spinal.lib.soc.pinsec.{Pinsec, PinsecConfig}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Random



object PlayDevRamZero{
  def main(args: Array[String]): Unit = {
    SpinalConfig(verbose = true).generateVerilog(new StreamFifo(Bits(8 bits), 1))
  }
}

object PlayDevMem{
  class TopLevel extends Component {
    val output = out UInt(9 bits)
//    output := null
    println("Miaou")
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
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

object PlayDevSubComponent{

  class TopLevel extends Component {
    val input = slave(Stream(Rgb(5,6,5)))
    val output = master(Stream(Rgb(5,6,5)))
    val fifoA = StreamFifo(Rgb(5,6,5), 8)
    fifoA.io.push <> input
    fifoA.io.pop <> output
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVerilog(new TopLevel()).toplevel
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


object PlayReflectSignal{
  class TopLevel extends Component {
    val logic = new Area {
      val a, b, c = Bool()
    }
    var result = Bool()

    result := List("logic_a", "logic_b", "logic_c").map(name => reflectBaseType(name).asInstanceOf[Bool]).orR
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

  def main(args: Array[String]) {
    SpinalVerilog(new Component{
      val inputs =  Vec(UInt(6 bits), 4)
      val outputs =  Vec(UInt(4 bits), 4)

//      outputs(0) := inputs(0) // Error if, e.g., v1=Bool() and v2=Bits(1 bits)
//      outputs(1).asBits := inputs(1).asBits // No error but doesn't work as expected
      outputs(2) := inputs(2).as(outputs(2)) // Silently converts v2 to the same bit width as v1
//      outputs(3).assignFromBits(inputs(3).asBits) // Silently converts v2 to the same bit width as v1
    })
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

  class I2C extends Component {
    val io = new Bundle {
      val SDA = inout(Analog(Bool))
    }
  }
  class Test extends Component {
    val io = new Bundle{
      val SDAs = Vec(inout(Analog(Bool)), 4)
    }
    for(j <- io.SDAs.range){
      val i2c = new I2C();
      i2c.io.SDA <> io.SDAs(j);
    }
  }

  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Test)
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



object PlayDevBug3{

  // Instance
  class TopLevel extends Component {
    val x = B"xAB"
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVerilog(new TopLevel())
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
          case s : MemReadSync =>
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


object PlayTriStateArrayToTriState extends App {
  class TopLevel extends Component{
    val gpio = slave(TriStateArray(8 bits))
    val led = master(TriState(Bool))

    gpio.read.assignDontCare()
    led <> gpio(2)
  }

  SpinalVerilog(new TopLevel)
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
  import spinal.core.GenerationFlags._
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
    GenerationFlags.formal{
      import spinal.core.Formal._
//      val a = past(B"1010")
      val b = rose(False)
      val c = fell(False)
      val d = changed(False)
      val f = stable(False)
      val g = initstate()
      assume(!initstate() && changed(True) && past(B"1010",4) === 0)
    }

  }

  SpinalConfig().includeFormal.generateSystemVerilog(new MyTopLevel)
//  SpinalVerilog(new MyTopLevel)
//  SpinalVhdl(new MyTopLevel)
}

object PlayErrorImprovment extends App {
  class MyTopLevel extends Component {
    val io = new Bundle {
      val state = out Bool
    }
    val a = RegInit(False)
    io.state := a
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


object PlayErrorImprovment2 extends App {
  class MyTopLevel extends Component {
    val io = new Bundle {
      val state = out Bool
    }
    val a = RegInit(False)
    io.state := a
  }

  val report = SpinalSystemVerilog(new MyTopLevel)
  println("asd")
}

object PlayGenerate extends App {
  class MyTopLevel extends Component{
    val param = true
    val data = param generate new Bundle{
      val x = UInt(8 bits)
    }

    val logic = param generate new Area{
      data.x := 0
    }
  }

  val report = SpinalVerilog(new MyTopLevel)
  println("asd")
}


object PlayErrorReportingImprovmenet extends App {
  class MyTopLevel extends Component{
    val a = B"0101010"
    val index = U"10001010"
    out(a(index))
    out(a & B"00")
  }

  val report = SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT)).generateVerilog(new MyTopLevel)
  println("asd")
}



object PlayHardType extends App {
  case class A() extends Bundle{
    println("A")
    val x,y,z = Bool
  }
  class MyTopLevel extends Component{

    def f() = {
      val ret = Bits(32 bits)
      val lit = True
      ret.asBools.foreach(_ := lit)
      ret
    }

    out(f())
  }



  val report = SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT)).generateVerilog(new MyTopLevel)
   SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT)).generateVhdl(new MyTopLevel)
  println("asd")
}

object PlayNamingImprovment extends App{
  def gen(c : => Component): Unit ={
    SpinalVhdl(c)
    SpinalVerilog(c)
  }

  class Sub extends Component{
    val io = new Bundle {
      val input = in Bool
      val output = out Bool
    }
    io.output := io.input
  }

  gen(new Component {

    val sub = new Sub
    sub.io.input := False

    var x = Bool()
    x := RegNext(sub.io.output)
  })

}

object PlayDevBusSlaveFactoryDoubleRead{
  class TestTopLevelDut extends Component {

    val io = new Bundle {
      val bus = slave(Apb3(Apb3Config(32, 32, 1, false)))
      val dummy_r_0 = in Bits (16 bits)
      val dummy_r_1 = in Bits (16 bits)
      val dummy_w_0 = out Bits (16 bits)
      val dummy_w_1 = out Bits (16 bits)
    }

    val factory = new Apb3SlaveFactory(io.bus, selId = 0)

    factory.read(io.dummy_r_0, 0x03, 0)
    factory.read(io.dummy_r_1, 0x03, 16)
    factory.drive(io.dummy_w_0, 0x00, 0)
    factory.drive(io.dummy_w_1, 0x00, 8)

  }

  def main(args: Array[String]) {
    val config = SpinalConfig()
    config.generateVerilog(new TestTopLevelDut())
  }
}




object SimAccessSubSignal {
  import spinal.core.sim._

  class TopLevel extends Component {
    val counter = Reg(UInt(8 bits)) init(0) simPublic()
    counter := counter + 1
  }

  def main(args: Array[String]) {
    SimConfig.compile(new TopLevel).doSim{dut =>
      dut.clockDomain.forkStimulus(10)

      for(i <- 0 to 3){
        dut.clockDomain.waitSampling()
        println(dut.counter.toInt)
      }
    }
  }
}


object SimAccessSubSignal2 {
  import spinal.core.sim._
  class TopLevel extends Component {
    val counter = Reg(UInt(8 bits)) init(0)
    counter := counter + 1
  }

  def main(args: Array[String]) {
    SimConfig.compile{
      val dut = new TopLevel
      dut.counter.simPublic()
      dut
    }.doSim{dut =>
      dut.clockDomain.forkStimulus(10)

      for(i <- 0 to 3){
        dut.clockDomain.waitSampling()
        println(dut.counter.toInt)
      }
    }
  }
}



object SimPlayDeltaCycle{
  import spinal.core.sim._

  class TopLevel extends Component {
    val a,b = in(UInt(8 bits))
    val result = out(Reg(UInt(8 bits)) init(0))
    result := a + b
  }

  def main(args: Array[String]) {
    SimConfig.withWave.compile(new TopLevel).doSimUntilVoid{dut =>
      dut.clockDomain.forkStimulus(10)

      def printState(header : String) = println(s"$header dut.a=${dut.a.toInt} dut.b=${dut.b.toInt} dut.result=${dut.result.toInt} time=${simTime()} deltaCycle=${simDeltaCycle()}")

      //Threadfull code to randomize dut.a
      fork{
        while(true){
          dut.clockDomain.waitSampling()
          printState("Pre  dut.a.randomize()")
          dut.a.randomize()
          printState("Post dut.a.randomize()")
        }
      }

      //Threadless code to randomize dut.b, behave the same than the threadfull example above (from a waveform point of view)
      dut.clockDomain.onSamplings{
        printState("Pre  dut.b.randomize()")
        dut.b.randomize()
        printState("Post dut.b.randomize()")
      }

      fork{
        printState("Pre  ref init         ")
        dut.clockDomain.waitSampling()
        printState("Post ref init         ")
        for(i <- 0 to 4){
          val resultRef = (dut.a.toInt + dut.b.toInt) & 0xFF

          printState("Pre  ref sampling     ")
          dut.clockDomain.waitSampling()
          printState("Post ref sampling     ")
          assert(dut.result.toInt == resultRef)
        }
        simSuccess()
      }
    }
  }
}

object SimPlayDeltaCycle2{
  import spinal.core.sim._

  class TopLevel extends Component {
    val clkIn = in Bool()
    val clkOut = out Bool()
    val input = in(UInt(8 bits))
    val output = out(UInt(8 bits))
    val register = ClockDomain(clock = clkIn, config = ClockDomainConfig(resetKind = BOOT)) (Reg(UInt(8 bits)) init(0))
    register := input
    val registerPlusOne = register + 1
    output := registerPlusOne
    clkOut := clkIn
  }

  def main(args: Array[String]) {
    SimConfig.withWave.compile(new TopLevel).doSim{dut =>
      def printState(header : String) = println(s"$header dut.clkIn=${dut.clkIn.toBoolean} dut.input=${dut.input.toInt} dut.output=${dut.output.toInt} dut.clkOut=${dut.clkOut.toBoolean} time=${simTime()} deltaCycle=${simDeltaCycle()}")

      dut.clkIn #= false
      dut.input #= 42
      printState("A")
      sleep(10)
      printState("B")
      dut.clkIn #= true
      dut.input #= 1
      printState("C")
      sleep(0) //A delta cycle is anways forced, but the sleep 0 allow the thread to sneak in that forced delta cycle
      printState("D")
      sleep(0) //Let's go for another delta cycle
      printState("E")
      sleep(10)
      printState("F")
    }
  }
}


object DebBugFormal extends App{
  case class rleBus[T <: Data](val dataType: HardType[T], val depth: Int) extends Bundle {
    val data = dataType()
    val lenght = Bits(depth bits)
  }

  case class rle[T <: Data](val dataType: HardType[T], val depth: Int) extends Component{
    val io = new Bundle{
      val input = in(dataType())
      val enable = in(Bool)
      val output = master(Flow(rleBus(dataType,depth)))
    }

    io.output.valid := False

    val input = RegNextWhen(io.input,io.enable) init(dataType().getZero)
    val isNew = io.input =/= input
    val count = Reg(UInt(depth bits)) init(0)
    io.output.lenght := count.asBits
    io.output.data := input

    when(isNew && io.enable){
      count := 0
      io.output.valid := True && io.enable
    } otherwise {
      count := count + 1
    }

    /////////////////////////////
    // FORMAL
    /////////////////////////////
    GenerationFlags.formal{
      when(True){
        assert(io.input =/= Formal.past(io.output.data))
      }
    }
  }

  SpinalConfig().includeFormal.generateSystemVerilog(new rle(Rgb(5,6,7),8))
}


object PlayOneHotSynthesisBench extends App{
  class BenchFpga(width : Int) extends Rtl{
    override def getName(): String = "Bench" + width
    override def getRtlPath(): String = getName() + ".v"
    SpinalVerilog(new Component{
      val sel = in Bits(width bits)
      val inputs = in Vec(Bits(8 bits), width)
      val output = out(RegNext(MuxOH(sel, inputs)))
      setDefinitionName(BenchFpga.this.getName())
    })
  }



  val rtls = List(2,3,4,5,6,7,8,16).map(width => new BenchFpga(width))

  val targets = XilinxStdTargets(
    vivadoArtix7Path = "/eda/Xilinx/Vivado/2017.2/bin"
  ) ++ AlteraStdTargets(
    quartusCycloneIVPath = "/eda/intelFPGA_lite/17.0/quartus/bin/",
    quartusCycloneVPath  = "/eda/intelFPGA_lite/17.0/quartus/bin/"
  )


  Bench(rtls, targets, "/eda/tmp/")
}


object PlayDevSpinalSim extends App{
  import spinal.core.sim._
  SimConfig.withWave.compile(new Component {
    val input = in UInt(8 bits)
    val output = out UInt(8 bits)
    output := RegNext(input) init(0)
  }).doSim { dut =>
    dut.clockDomain.forkStimulus(period = 10)
    dut.input #= 0

    dut.clockDomain.onSamplings{
      dut.input #= (dut.output.toInt + 1) & 0xFF
    }

    dut.clockDomain.waitSampling(40)
  }
}

object PlayDevSpinalSim2 extends App{
  import spinal.core.sim._

  def forkSensitiveTrigger(trigger : BaseType)(block : => Unit): Unit = {
    var valueLast = trigger.toBigInt
    forkSensitive{
      val valueNew = trigger.toBigInt
      if(valueNew != valueLast) { block }
      valueLast = valueNew
    }
  }

  SimConfig.withWave.compile(new Component {
    val a = in UInt(8 bits)
    val b = out UInt(8 bits)
    val c = in UInt(8 bits)
    val d = out UInt(8 bits)
    b := RegNext(a) init(0)
    d := c
  }).doSim { dut =>
    dut.clockDomain.forkStimulus(period = 10)

    forkSensitive(dut.b.toInt){
      dut.c #= dut.b.toInt
    }
    forkSensitive(dut.d){
      dut.a #= (dut.d.toInt + 2) & 0xFF
    }

    dut.clockDomain.waitSampling(40)
  }
}

object PlayDevSpinalSim3 extends App{
  import spinal.core.sim._
  SimConfig.withWave.compile(new Component {
    val input = in UInt(8 bits)
    val output = out UInt(8 bits)
    output := RegNext(input) init(0)
  }).doSim { dut =>
    dut.input #= 0
    sleep(3)
    dut.input #= 5
    println(dut.input.toInt)
    sleep(0)
    println(dut.input.toInt)
  }
}

import spinal.core._
import spinal.core.sim._

object OutputBug {
  def main(args: Array[String]) {
    var dut = SimConfig
      .compile(new Component {
        val io = new Bundle {
          val cond0 = in Bool
        }
        val counter = Reg(UInt(8 bits)) init (0)

        when(io.cond0) {
          counter := counter + 1
        }
      })

    dut.doSim("write") { dut =>
      SimTimeout(10000 * 10)
      dut.clockDomain.onActiveEdges({
        println(f"${simTime()} this is only dummy output, and a bit of it")
      })
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitActiveEdge(10100)
      assert(false)
    }
  }
}



object PlayFixPointProperty extends App{
  def check(roundType: RoundType, sym: Boolean): Unit = {
    println(s"${FixPointProperty.get}, $roundType, $sym ")
  }

  class Topxx extends Component {
    check(RoundType.ROUNDUP, true)
    val start = in Bool()
    val din = in SInt(16 bits)

    val area = FixPointProperty(DefaultFixPointConfig) on new Area{
      check(RoundType.ROUNDTOINF, false)
      val broundtoinffalse = din.fixTo(10 downto 3)
    }

//    FixPointProperty.push(FixPointConfig(RoundType.CEIL, false))
//    check(RoundType.CEIL, false)
    val cceilfalse = {
      FixPointProperty(FixPointConfig(RoundType.FLOOR, true)) on {
        check(RoundType.FLOOR, true)
      }
      FixPointConfig(RoundType.ROUNDTOZERO, false){
        check(RoundType.ROUNDTOZERO, false)
      }
      FixPointConfig(RoundType.ROUNDTOEVEN, true) on {
        check(RoundType.ROUNDTOEVEN, true)
      }
      din.fixTo(4 downto 1)
    }
//    check(RoundType.CEIL, false)
//    FixPointProperty.pop()
    val droudnuptrue = din.fixTo(12 downto 9)
    check(RoundType.ROUNDUP, true)
  }
  FixPointConfig(RoundType.ROUNDTOEVEN, true) on {
    check(RoundType.ROUNDTOEVEN, true)
  }
  LowCostFixPointConfig{
    check(RoundType.ROUNDUP, true)
  }
  val config = SpinalConfig(targetDirectory = "./tmp")
  config.setScopeProperty(FixPointProperty, LowCostFixPointConfig)
  config.generateVerilog(new Topxx)
  check(RoundType.ROUNDTOINF, true)
}

object PlayFixPointProperty2 extends App {
  def check(roundType: RoundType, sym: Boolean): Unit = {
    println(s"${FixPointProperty.get}, ${FixPointConfig(roundType, sym)}")
  }

  class TopXX extends Component{
    check(RoundType.FLOOR, false) //pass
    FixPointConfig(RoundType.ROUNDUP, true).setAsDefault() // do nothing bad
    check(RoundType.FLOOR, false) //pass
  }

  FixPointConfig(RoundType.ROUNDTOEVEN, true) on {
    check(RoundType.ROUNDTOEVEN, true)

    val config = SpinalConfig(targetDirectory = "./tmp")
    config.setScopeProperty(FixPointProperty, FixPointConfig(RoundType.FLOOR, false))
    config.generateVerilog(new TopXX)

    check(RoundType.ROUNDTOEVEN, true)  //it's ok now
  }

  check(RoundType.ROUNDUP, true)  //it's ok now
}
