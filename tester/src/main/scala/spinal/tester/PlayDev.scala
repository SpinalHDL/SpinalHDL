package spinal.tester

import spinal.core._
import spinal.core.internals._
import spinal.lib._
import spinal.lib.bus.amba3.apb.Apb3Gpio
import spinal.lib.com.spi.{Apb3SpiMasterCtrl, SpiMasterCtrlGenerics, SpiMasterCtrlMemoryMappedConfig}
import spinal.lib.io.{InOutWrapper, TriState}
import spinal.lib.soc.pinsec.{Pinsec, PinsecConfig}

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag


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
//    val vec = out(Vec(Reg(Bits(32 bits)), 4))
//    val sel = in(UInt(2 bits))
//
//    vec(sel) := 0

    val a,b = out(Bits(32 bits))
    val x = B(0).resized
    a := x
    b := x
  }

  def main(args: Array[String]) {
    val toplevel = SpinalConfig().generateVhdl(new TopLevel())
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
    SpinalVhdl(InOutWrapper(new Toplevel))
    print("done")
  }
}

object PlayDevBug3{
  class TopLevel extends Component {

    val b ,c = out(Bool())
  }

  def main(args: Array[String]) {
    SpinalConfig().generateVhdl(new TopLevel())
    SpinalConfig().generateVerilog(new TopLevel())
    print("done")
  }
}