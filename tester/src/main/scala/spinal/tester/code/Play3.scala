package spinal.tester.code

import spinal.core._
import spinal.core.internals.PhaseInitReg
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc._
import spinal.lib.bus.simple._
import spinal.lib.io.TriState
import spinal.lib.soc.pinsec.{Pinsec, PinsecConfig}
import spinal.tester.code.t8_a.UartCtrl
import spinal.lib.fsm._
import spinal.lib.memory.sdram.sdr.W9825G6JH6

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object PlayAhbLite3{
  class TopLevel extends Component{
    val ahbConfig = AhbLite3Config(addressWidth = 16,dataWidth = 32)
    val apbConfig = Apb3Config(addressWidth = 16,dataWidth = 32)

    val ahbMasters = Vec(slave(AhbLite3Master(ahbConfig)),3)
    val ahbSlaves  = Vec(master(AhbLite3(ahbConfig)),4)

    val perMaster = for(ahbMaster <- ahbMasters) yield new Area{
      val decoder = AhbLite3Decoder(
        ahbLite3Config = ahbConfig,
        decodings = List(
          (0x1000,0x1000),
          (0x3000,0x1000),
          (0x4000,0x1000),
          (0x6000,0x1000)
        )
      )
      decoder.io.input <> ahbMaster.toAhbLite3()
    }

    val perSlave = for((ahbSlave,idx) <- ahbSlaves.zipWithIndex) yield new Area{
      val arbiter = AhbLite3Arbiter(
        ahbLite3Config = ahbConfig,
        inputsCount = ahbMasters.length
      )
      (arbiter.io.inputs,perMaster).zipped.foreach(_ <> _.decoder.io.outputs(idx))
      arbiter.io.output <> ahbSlave
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object PlayAhbLite3_2{
  (43 MHz) * (1 kHz)
  (43 MHz) * (1 kHz)
  val f = 54 MHz
  val i = f * 4
  val timeout = Timeout(100 ms)
//  val x : BigDecimal = f
  class TopLevel extends Component{
    val ahbConfig = AhbLite3Config(addressWidth = 16,dataWidth = 32)
//    val apbConfig = Apb3Config(addressWidth = 16,dataWidth = 32)

    val ahbMasters = Vec(slave(AhbLite3Master(ahbConfig)),3)
    val ahbSlaves  = Vec(master(AhbLite3(ahbConfig)),4)

    val crossbar = AhbLite3CrossbarFactory(ahbConfig)
      .addSlaves(
        ahbSlaves(0) -> (0x1000,0x1000),
        ahbSlaves(1) -> (0x3000,0x1000),
        ahbSlaves(2) -> (0x4000,0x1000),
        ahbSlaves(3) -> (0x5000,0x1000)
      )
      .addConnections(
        ahbMasters(0).toAhbLite3() -> List(ahbSlaves(0),ahbSlaves(1)),
        ahbMasters(1).toAhbLite3() -> List(ahbSlaves(1),ahbSlaves(2),ahbSlaves(3)),
        ahbMasters(2).toAhbLite3() -> List(ahbSlaves(0),ahbSlaves(3))
      )
      .build()
    (ClockDomain.current.frequency.getValue)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel).printPruned()
  }
}


//object PlayAxi4ArUnburstify{
//  class TopLevel extends Component{
//    val axiConfig = Axi4Config(32,32,4)
//    val cmd = slave(Stream(Axi4Ar(axiConfig)))
//    val rsp = master(cmd.stage.unburstify.stage)
//  }
//
//  def main(args: Array[String]) {
//    SpinalVhdl(new TopLevel)
//  }
//}

object PlayAxi4AddrIncr{
  class TopLevel extends Component{
    val address = in UInt(32 bits)
    val burst = in Bits(2 bits)
    val len = in UInt(8 bits)
    val size = in UInt(3 bits)

    val result = out(Axi4.incr(
      address,
      burst,
      len,size,
      4
    ))
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}

//object PlayImplicitScopeAxi{
//  val stream =Stream[Axi4Ar](null)
//  stream.drive()
//}


object Play333 extends App{
  class Top2 extends Component{
    val sel = in Bits(3 bits)
    val b = out Bits(16 bits)
    switch(sel){
      is(1){ b := 32}
      is(2){ b := 32}
      is(3){ b := 12}
      is(0){ b := 82}
      default { b:=0}
    }
  }
  SpinalVerilog(new Top2)
}

object PlayAxi4StrbConnect{
  class TopLevel extends Component{
    val axiConfig = Axi4Config(32,32,4)
    val source = slave(Axi4(axiConfig.copy(useLast = false)))
    val sink   = master(Axi4(axiConfig))
    source.w drive sink.w
    source >> sink
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel).printPruned()
  }
}


object PlayDualRail{
  case class DualRailBool() extends Bundle{
    val t,f = Bool

    def & (that : DualRailBool) : DualRailBool = {
      ???
    }
  }

}


object PlayKeepAll{
  class TopLevel extends Component{
    val a,b = in Bool
    val result = out Bool
    val toto = U"1010"
    val yolo = toto + 2

    result := a || b
  }

  def main(args: Array[String]) {
    SpinalConfig(keepAll = true).generateVhdl(new TopLevel).printPruned()
  }
}


object PlayFloat{

  case class FP(mantissaSize: Int,
                exponentSize: Int) extends Bundle {
    val sign = Bool
    val exponent = Bits(exponentSize bits)
    val mantissa = Bits(mantissaSize bits)
  }

  case class RecodedFP(mantissaSize: Int,
                       exponentSize: Int) extends Bundle {
    val sign = Bool
    val exponent = Bits(exponentSize bits)
    val mantissa = Bits(mantissaSize bits)
  }

  class floatTest extends Component {
    val io = new Bundle {
      val inp = in(FP(23, 8))
      val outp = out Bits(32 bits)
    }
    io.outp := io.inp.asBits
  }
}


object PlayFloating{
  import spinal.core._
  import spinal.lib._

  case class FP(exponentSize: Int,
                mantissaSize: Int) extends Bundle {
    val sign = Bool
    val exponent = Bits(exponentSize bits)
    val mantissa = Bits(mantissaSize bits)

    private def isExponentZero = exponent === 0
    private def isMantissaZero = mantissa === 0

    def isZero = isMantissaZero && isExponentZero
    def isPositive = !sign

    def fromBits(word: Bits): Unit = {
      mantissa := word(mantissaSize-1 downto 0)
      exponent := word(mantissaSize+exponentSize-1 downto mantissaSize)
      sign := word(mantissaSize+exponentSize)
    }

    def asRecodedFP = {
      val recoded = RecodedFP(exponentSize+1, mantissaSize)
      val firstMantissaBit = mantissaSize-OHMasking.first(OHToUInt(mantissa))
      val normalizedMantissa = (mantissa << firstMantissaBit)(mantissaSize-1 downto 0)
      val denormExponent = B(exponentSize + 1 bits, default -> True) ^ firstMantissaBit.asBits
      val recodedExponent = Mux(isExponentZero, denormExponent, exponent).asUInt +
        ((1 << exponentSize - 1) | Mux(isExponentZero, U(2), U(1)))
      val isNaN = recodedExponent(exponentSize - 1 downto exponentSize - 2) === 3 && !isMantissaZero
      val finalExponent = recodedExponent.asBits & (B(3 bits, default -> isZero) << (exponentSize - 3)) | (isNaN.asBits << (exponentSize - 3))
      recoded.sign := sign
      recoded.exponent := finalExponent
      recoded.mantissa := Mux(isExponentZero, normalizedMantissa, mantissa)
      recoded
    }
  }

  case class RecodedFP(exponentSize: Int,
                       mantissaSize: Int) extends Bundle {
    val sign = Bool
    val exponent = Bits(exponentSize bits)
    val mantissa = Bits(mantissaSize bits)

    def isZero = exponent(exponentSize-1 downto exponentSize-3) === 0
    def isNaN = exponent(exponentSize-1 downto exponentSize-3) === 7
    def isQNaN = isNaN && !mantissa(mantissaSize-1)
    def isSNaN = isNaN && mantissa(mantissaSize-1)
    def isPositive = !sign
    def isInfinite = exponent(exponentSize-1 downto exponentSize-3) === 6
  }
}


object PlayOhToUInt{

  class TopLevel extends Component{
    val input = in Bits(24 bits)
    val output = out UInt(5 bits)

    output := OHToUInt(input)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
    SpinalVerilog(new TopLevel)
  }
}


object PlayVariableError{
  class TopLevel(size : Int) extends Component{
    val io = new Bundle{
      val cond = in Bool
      val a = in UInt(-30 bit)
      val b = in UInt(-10 bit)
      val result = out UInt(size bit)       //result = a + b
    }

    var c = False                    //Carry, like a VHDL variable
    for (i <- 0 until size) {
      //Create some intermediate value in the loop scope.
      val a = io.a(i)
      val b = io.b(i)

      //The carry adder's asynchronous logic
      io.result(i) := a ^ b ^ c
      when(io.cond){
        c = (a & b) | (a & c) | (b & c);    //variable assignment
      }
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel(4))
  }
  def sumBiggerThanZero(a : Float,b : Float) = {
    (a + b) > 0
  }



}


object PlayClockAndArea{
  class TopLevel(size : Int) extends Component{
    val clk = in Bool
    val cd = ClockDomain(clk)
    val yolo = cd.apply( new Area{

    })


    val yolo2 = cd(new Area {

    })


    val yolo3 = new ClockingArea(cd) {

    }
    object Apb3UartCtrl{
      def getApb3Config = Apb3Config(
        addressWidth = 4,
        dataWidth    = 32
      )
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel(4))
  }
}

import spinal.lib.soc.pinsec._

object PinsecMain{
  def main(args: Array[String]) {
    SpinalVhdl(new Pinsec(100 MHz))
    SpinalVerilog(new Pinsec(100 MHz))
  }
}



object PlayVecSplit2{
  class TopLevel extends Component{
    val vec = in Vec(Bool,16)
    val result = out Bool()
    result := vec.slice(2,11).reduceBalancedTree(_ && _)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}



object PlayFloating32{

  case class Floating(a : Int,b : Int) extends Bundle{

  }
  object Floating32{
    def apply() = Floating(8,23)
  }

  val yolo = in(Floating32())
  val yolo2 = out(Floating32())

  class TopLevel extends Component{
    val vec = in Vec(Bool,16)
    val result = out Bool()
    result := vec.slice(2,11).reduceBalancedTree(_ && _)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object PlayTest1515 {

  class TopLevel extends Component {
    val io = new Bundle {
      val a, b = in UInt (4 bits)
      val result = out UInt (4 bits)
    }

    io.result := RegNext(io.a + io.b)
  }

  def main(args: Array[String]) {
    println("MIAOU")
    println(100 MHz)
    SpinalVhdl(new TopLevel)
  }
}



object PlayGrayCounter2{

  // A generic Gray counter
  class GrayCnt(size : Int = 4) extends Component {

    // All IO signals for the Gray counter
    val io = new Bundle {

      // Counter output port
      val gval = out UInt(size bits)

    }

    // Operation used for calculating the counter equations
    val andNot = (a : Bool, b : Bool) => a & (~b)

    // Helper bit for toggling
    val toggle = Reg(Bool) init(False)

    // Counter register
    val cnt = Reg(UInt(size bits)) init(0)

    // Toggle the helper
    toggle := !toggle

    // Calculate the LSB
    cnt.lsb := !(cnt.lsb ^ toggle)

    // Handle all 'middle' bits
    for(i <- 1 to size - 2) {

      // This equation checks the 0^* pattern
      val tmp = cnt(i - 1) && cnt(i - 2 downto 0).asBools.reduceBalancedTree(andNot) && toggle

      // Calculate the ith bit of the counter
      cnt(i) := cnt(i) ^ tmp

    }

    // Calculate the MSB
    cnt.msb := cnt.msb ^ (cnt(size - 3 downto 0).asBools.reduceBalancedTree(andNot) && toggle)

    // Map the register to the output logic;
    io.gval := cnt

  }
}


object PlayApb3{
  val apbConfig = Apb3Config(
    addressWidth = 12,
    dataWidth    = 32
  )
  val apbX = Apb3(apbConfig)
  val apbY = Apb3(apbConfig)

  apbX >> apbY
  val myUInt = UInt(8 bits)
  myUInt := (0 -> true, default -> false)
  when(myUInt === U(0 -> true, (myUInt.high downto 1) -> false)){

  }

  when(apbY.PENABLE){
    //...
  }
}

object PlayRotateInt{
  class TopLevel extends Component {
    val io = new Bundle {
      val a = in Bits(12 bits)
      val sel = in UInt (4 bits)
      val result = out Bits (12 bits)
    }

    io.result := io.a.rotateLeft(4)
    val yolo = out(B"".resized | B"000")
    val register = out(Reg(UInt(4 bits)))init(U"0000")

    when(True){
      io.result := "0000"
      when(True){
        io.result(1 downto 0)  := "00000"
        when(True){
      io.result := "000"
      }
     }
    }

  }

  def main(args: Array[String]) {
    println("MIAOU")
    println(100 MHz)
    SpinalVhdl(new TopLevel)
  }
}



object PlayVecAssign{
  class TopLevel extends Component {
    val sel = in UInt(2 bits)
    val outputs = out Vec(Reg(Bool) init(False),4)

    outputs.foreach(_ := False)
    outputs(sel) := True
  }

  def main(args: Array[String]) {
    SpinalVhdl({
      val toplevel = new TopLevel
      ClockDomain.current.reset.setName("areset")
      toplevel
    })
  }
}


object PlayMuxBits{
  class TopLevel extends Component {
    val sel = in Bool
    val result = out(sel ? U(2) | U(1,1 bits))
    val result2 = out SInt(4 bits)
    result2 := S(-9,4 bits)
  }

  def main(args: Array[String]) {
    SpinalConfig(genVhdlPkg = false).generateVhdl(new TopLevel)
    BigDecimal(2).toDouble
  }
}



object PlayImplicitParameter{
  def yolo(implicit x : Int = 0) = x + 1

  def main(args: Array[String]) {
    println(yolo)
    implicit val newImplicit = 10
    println(yolo)
  }
}



object PlayTypedef{
  class TopLevel(t : HardType[Data]) extends Component {
    val input = in(t())
    val output = out(t())
    output := input
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel(UInt(3 bits)))
  }
}


//object PlayRamInfer{
//  class TopLevel() extends Component {
//    val a = out(U(8 bits,1 -> True,default -> False))
//  }
//
//  def main(args: Array[String]) {
//    SpinalVhdl(new TopLevel())
//    SpinalVerilog(new TopLevel())
//  }
//}

object PlayRoundRobin{
  class TopLevel() extends Component {
    val requests,ohPriority = in Bits(4 bits)
    val roundrobin = out(OHMasking.roundRobin(requests,ohPriority))
    val dummy = out(RegNext(U"111") init(0))
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel())
    SpinalVerilog(new TopLevel())
  }
}


object PlayFifoVerilog{
  def main(args: Array[String]) {
    SpinalVerilog(new StreamFifo(Bits(8 bits),16))
  }

  class Ram_1w_1r(wordWidth: Int, wordCount: Int) extends BlackBox {

    // SpinalHDL will lock at Generic classes to get attributes which
    // should be used ad VHDL gererics / Verilog parameter
    val generic = new Generic {
      val wordCount = Ram_1w_1r.this.wordCount
      val wordWidth = Ram_1w_1r.this.wordWidth
    }

    // Define io of the VHDL entiry / Verilog module
    val io = new Bundle {
      val clk = in Bool
      val wr = new Bundle {
        val en   = in Bool
        val addr = in UInt (log2Up(wordCount) bit)
        val data = in Bits (wordWidth bit)
      }
      val rd = new Bundle {
        val en   = in Bool
        val addr = in UInt (log2Up(wordCount) bit)
        val data = out Bits (wordWidth bit)
      }
    }

    //Map the current clock domain to the io.clk pin
    mapClockDomain(clock=io.clk)
  }
}


object PlayhashMap{
  def main(args: Array[String]) {
    val dic = mutable.HashMap[String,String]()
    dic += ("miaou" -> "toto")
    dic += ("miaou2" -> "toto2")

    println(dic("miaou"))

    val set = mutable.HashSet[String]()
    set += "yolo"
    set += "yili"

    println(set.contains("yolo"))
  }
}




//object PlayNodeAnalyse{
//  class TopLevel() extends Component {
//    val a,b,c,d = in Bool
//    val result = out Bool
//
//    var b_and_c = b && c
//    result := a || b_and_c || d
//    b_and_c = null //b_and_c will not be named by the reflection
//  }
//
//  def main(args: Array[String]) {
//    val toplevel = SpinalVhdl(new TopLevel()).toplevel
//
//    iterateOverBaseTypeInputs(toplevel.result.input)(baseType => {
//      println(baseType)
//    })
//  }
//
//  def iterateOverBaseTypeInputs(node : Node)(gen : BaseType => Unit): Unit = node match {
//    case bt : BaseType if bt.isNamed => gen(bt)
//    case null =>
//    case _ => node.onEachInput(iterateOverBaseTypeInputs(_)(gen))
//  }
//}


object PlayRomRam{
  class TopLevel() extends Component {
    def initialContent = List(
      B"1000_0000_0000_0111",
      B"0000_0000_0000_0001"
    )

    val ram = Mem(Bits(16 bits),initialContent ++ List.fill((1<<15) - initialContent.length)(B"x0000"))
    ram.write(
      address = in UInt(15 bits),
      data = in Bits(16 bits),
      enable = in Bool
    )


    out(ram.readAsync(
      address = in UInt(15 bits)
    ))
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}



object PlayRomRam2{
  class Assembler{
    val array = ArrayBuffer[Bits]()

    def jump(address : Int) = array += B(address,16 bits)
    def push(value : Int)   = array += B((1 << 15) | value,16 bits)
    // ...

    def build(ramSize : Int) = array ++ List.fill(ramSize - array.length)(B"x0000")
  }

  class TopLevel() extends Component {
    def asm = new Assembler()
    asm.push(7)
    asm.jump(1)


    M"111_3"
    val ram = Mem(Bits(16 bits),asm.build(1 << 15))

    ram.write(
      address = in UInt(15 bits),
      data = in Bits(16 bits),
      enable = in Bool
    )


    out(ram.readAsync(
      address = in UInt(15 bits)
    ))
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}



object PlayPruned{
  class TopLevel() extends Component {
    val io = new Bundle{
      val a,b = in UInt(8 bits)
      val result = out UInt(8 bits)
    }

    io.result := io.a + io.b

    val unusedSignal = UInt(8 bits)
    val unusedSignal2 = UInt(8 bits)

    unusedSignal2 := unusedSignal + io.a
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel).printPruned().printUnused()
  }
}



object PlayPruned2{
  class TopLevel() extends Component {
    val io = new Bundle{
      val a,b = in UInt(8 bits)
      val result = out UInt(8 bits)
    }

    io.result := io.a + io.a

    val unusedSignal = UInt(8 bits)
    val unusedSignal2 = UInt(8 bits).keep()

    unusedSignal := 0
    unusedSignal2 := unusedSignal
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel).printPruned()
  }
}


object PlaySumBig{
  def main(args: Array[String]) {
    var idx = 0l
    var sum = 0l
    while(idx < 10000000l){
      sum += idx;
      idx += 1
    }
    printf("%x%x",(sum >> 32).toInt,(sum >> 0).toInt)
  }
}



object PlayConnectBundle{
  case class Inputs(mode : Int) extends Bundle{
    val a,b = in UInt(8 bits)
    val c = if(mode == 1) in UInt(8 bits) else null
    val d = if(mode == 2) in UInt(8 bits) else null
  }

  case class Outputs() extends Bundle{
    val a,b = in UInt(8 bits)
  }
  class TopLevel() extends Component {
    val inputs = Inputs(1)
    val outputs = Inputs(2).flip
    outputs <> inputs
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}



object PlayBusSlaveFactory42{
  class TopLevel extends Component {
    val bus = slave(Apb3(8,32))
    val busCtrl = (Apb3SlaveFactory(bus))
    val a = out(UInt(40 bits))
    a := 0
    busCtrl.drive(a.apply(39 downto 0),0)
   // busCtrl.drive(a(39 downto 32),4)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}





object PlaySFixInit{
  class TopLevel extends Component {
    val x = out SFix(4 exp, -3 exp)
    x := 0.3

    val rom = Mem(SFix(4 exp, -3 exp),List(SF(0.3,4 exp, -3 exp),SF(0.3,4 exp, -3 exp),SF(0.3,4 exp, -3 exp),SF(0.3,4 exp, -3 exp)))
    val y = out(rom(in UInt(2 bits)))
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object PinsecSpartan6Plus{
  def main(args: Array[String]) {
    val config = PinsecConfig.default.copy(
      axiFrequency = 50 MHz,
      onChipRamSize = 36 KiB,
      sdramLayout = W9825G6JH6.layout,
      sdramTimings = W9825G6JH6.timingGrade7
    )

    SpinalVerilog(new Pinsec(config))
  }
}

object PinsecSmall{
  def main(args: Array[String]) {
    val config = PinsecConfig.default.copy(
      axiFrequency = 50 MHz,
      onChipRamSize = 36 KiB,
      sdramLayout = W9825G6JH6.layout,
      sdramTimings = W9825G6JH6.timingGrade7
    )

    SpinalVerilog(new Pinsec(config))
  }
}

object PinsecTest34{
  def main(args: Array[String]) {
    SpinalVerilog(new Pinsec(100 MHz))
  }
}

object PlayMasked232{
  class TopLevel extends Component {
    val sel = in UInt(7 bits)
    val result = out Bool

    result := False
    switch(sel){
      is(M"011-0111") {result := True}
      is(M"011_1000") {result := True}
      is(M"011_1000") {result := True}
    }
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object PlayPatch{
  class LEDBank(width     : Int = 16,
                lowActive : Bool = False) extends Component {

    val io = new Bundle {

      // I/O signals for memory data port
      val writeEnable = in Bool
      val ledState = in Bits(width bits)
      val leds = out Bits(width bits)
    }

    // Register for holding the bit-vector storing the LED states
    val ledReg = Reg(Bits(width bits)) init(0)

    // Check for write mode
    when(io.writeEnable) {

      // Set register value
      ledReg := io.ledState

    }

    // Set output for the leds (invert it if asked for by the generic parameter)
    io.leds := lowActive ? ~ledReg | ledReg

    // Implement the bus interface
    def driveFrom(busCtrl : BusSlaveFactory, baseAddress : BigInt) = new Area {
      // The register is mapped at Address 0 and is of type r/w
      busCtrl.read(io.leds,baseAddress + 0)
      busCtrl.nonStopWrite(io.ledState, 0) //ledState will be constantly drived by the data of the memory bus
      io.writeEnable := busCtrl.isWriting(baseAddress + 0)
    }

  }


  object LEDBank{
    def apply(busCtrl : BusSlaveFactory,
              baseAddress : BigInt)
             (width     : Int = 16,
              lowActive : Bool = False): Bits ={
      val leds = busCtrl.createReadAndWrite(Bits(width bits),baseAddress) init(0)
      return lowActive ? ~leds | leds
    }
  }

  class YourTopLevel extends Component {
    //...
    val leds = LEDBank(???, 0x40)(
      width = 16,
      lowActive = False
    )
  }

  case class PipelinedMemoryBus(addressWidth : Int, dataWidth : Int) extends Bundle with IMasterSlave {
    // Check the generic parameters
    val enable     = Bool // Bus can be used when 'enable' is high
    val writeMode  = Bool // High to write data, low to read data
    val address    = UInt(addressWidth bits) // Address (byte-aligned)
    val writeData  = Bits(dataWidth bits)
    val readData   = Bits(dataWidth bits)

    def delayed(delayCnt : Int = 1): PipelinedMemoryBus = {
      require (delayCnt >= 0, "Error: delayCnt has to be at least 0")
      val ret = cloneOf(this)

      ret.enable    := Delay(this.enable    ,delayCnt)
      ret.writeMode := Delay(this.writeMode ,delayCnt)
      ret.address   := Delay(this.address   ,delayCnt)
      ret.writeData := Delay(this.writeData ,delayCnt)
      this.readData := Delay(ret.readData   ,delayCnt)

      return ret
    }

    //Can be used to connect that to this
    def << (that : PipelinedMemoryBus): Unit ={
      that.enable    := this.enable
      that.writeMode := this.writeMode
      that.address   := this.address
      that.writeData := this.writeData
      this.readData  := that.readData
    }
    def >>(that : PipelinedMemoryBus): Unit = that << this

    // This is called by 'apply' when the master-object is called with data (-> side effect write/read data)
    override def asMaster() : Unit = {

      // Write data to the bus
      out(enable, writeMode, address, writeData)

      // Read data from the bus
      in(readData)

    }

  }


  val cpuBus = PipelinedMemoryBus(32,32)
  val peripheralBus = cpuBus.delayed(4)   //This instance of the bus will be drived by the cpuBus with 4 cycle delay in each directions
  val somewereElse = PipelinedMemoryBus(32,32)
  somewereElse << peripheralBus

  case class HandShake(payloadWidth : Int) extends Bundle with IMasterSlave{
    val valid = Bool
    val ready = Bool
    val payload = Bits(payloadWidth bits)

    override def asMaster(): Unit = {
      out(valid,payload)
      in(ready)
    }
  }

//  val io = new Bundle{
//    val input  = slave(HandShake(8))
//    val output = master(HandShake(8))
//  }



}


object Play3ExternalPull{
  def external[T <: Data](that : T,name : String): T ={
    var ptr = that
    while(ptr.component.parent != null){
      ptr.component.parent.rework {
        val stage = cloneOf(that)
        for ((p, s) <- (ptr.flatten, stage.flatten).zipped) {
          if (p.isOutput) out(s)
          else if (p.isInput) in(s)
          else ???
        }
        stage <> ptr
        ptr = stage
      }
    }
    ptr.setName(name)
    that
  }

  class SubLevel extends Component{
    val bus = external(master(TriState(Bits(32 bits))),"PORTB")

    //Do some logic/connections for the fun
    bus.writeEnable := True
    bus.write := bus.read
  }

  class TopLevel extends Component{
    val sub = new SubLevel
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }

  val a = Bits(4 bits)
  a.subdivideIn(3 bits).foreach(_ === 4)
}




object Play3MissingWarning43{


  class TopLevel extends Component{
    val output = out (B("x000F"))
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}


object PlayWithBusSlaveFacotry11{

  class TopLevel extends Component {
    val io = new Bundle{
      val apb3  = slave(Apb3(32, 32))
      val tata     = out Bits(20 bits)
    }



    val factoryConfig = new BusSlaveFactoryConfig(BIG)
    val factory = Apb3SlaveFactory(io.apb3, 0).setConfig(factoryConfig)


    val d = factory.createWriteOnly(io.tata, 0x50l)
    io.tata := d
  }

  def main(args: Array[String]): Unit ={
    SpinalVhdl(new TopLevel)
  }
}


object PlayWithBusSlaveFacotry{

  class TopLevel extends Component {
    val io = new Bundle{
      val apb3  = slave(Apb3(32, 32))
      val key   = out Bits(160 bits)
      val value = out Bits(40 bits)
      val cnt   = out UInt(32 bits)
      val toto  = out Bits(12 bits)
      val myStream = slave Stream(Bits(90 bits))
      val myFlow   = master Flow(Bits(90 bits))
      val tata     = out Bits(20 bits)
    }



    val key_reg = Reg(cloneOf(io.key)) init(0)
    val value_reg = Reg(cloneOf(io.value))


    val factory = Apb3SlaveFactory(io.apb3, 0)
    factory.setWordEndianness(LITTLE)

    factory.writeMultiWord(key_reg, 0x100l)
    factory.readMultiWord(key_reg, 0x100l)

    factory.readAndWriteMultiWord(value_reg, 0x200l)

    val regToto = factory.createReadAndWrite(Bits(12 bits), 0x300l)
    io.toto := regToto

    val cnt = Reg(UInt(32 bits)) init(0)
    factory.onWrite(0x10){ cnt := cnt + 1 }
    io.cnt := cnt

    factory.readStreamNonBlocking(io.myStream, 0x0l)

    factory.driveFlow(io.myFlow, 0x700l)

    io.key   := key_reg
    io.value := value_reg

    factory.drive(io.tata, 0x50l)
  }

  def main(args: Array[String]): Unit ={
    SpinalVhdl(new TopLevel)
  }
}


object PlayMuxInfer{
  class TopLevel extends Component {
    val sel = in Bool
    val a = in UInt (4 bits)
    val b = in UInt (8 bits)
    val result = out UInt (4 bits)
    val x = 0 until 10
    val y = 0 to 10


    val z = 5 until 0 by -1
    val zz = 5 to 0 by -1
    result := sel ? a | b
  }

  def main(args: Array[String]): Unit ={
    SpinalVhdl(new TopLevel)
  }
}

object PlayWithCustomEnumEncoding{

  object MyEnumStatic extends SpinalEnum{
    val e0, e1, e2, e3 = newElement()
    defaultEncoding = SpinalEnumEncoding("static")(
             e0 -> 0,
             e1 -> 2,
             e2 -> 4,
             e3 -> 16 )
  }

  val encodingDynamic = SpinalEnumEncoding("dynamic", _ * 2 + 1)
  object MyEnumDynamic extends SpinalEnum(encodingDynamic){
    val e0, e1, e2, e3 = newElement()
  }


  class TopLevel extends Component{
    val io = new Bundle{
      val state  = in(MyEnumStatic)
      val state1 = in(MyEnumDynamic)
      val result = out Bool
    }

    io.result := False
    when(io.state === MyEnumStatic.e1 || io.state1 === MyEnumDynamic.e3){
      io.result := True
    }
  }

  def main(args: Array[String]): Unit ={
    SpinalVhdl(new TopLevel)
  }

}


object PlayWithMuxListDefault{
  class TopLevel extends Component{
    val io = new Bundle{
      val sel     = in UInt(4 bits)
      val outData = out Bits(8 bits)
      val inData  = in Bits(128 bits)
    }

    io.outData := io.sel.muxList(io.inData(7 downto 0) ,for(index <- 0 until 4) yield (index, io.inData(index*8+8-1 downto index*8)))

  }

  def main(args: Array[String]): Unit ={
    SpinalVhdl(new TopLevel)
  }
}

object PlayNetlistFileName{
  class FakeNetlist extends Component{
    val io = new Bundle{
      val a = in Bool
      val b = in Bool
      val c = out Bool
    }
    io.c := io.a & io.b
  }

  def main(args: Array[String]): Unit = {
    SpinalConfig(
      mode = Verilog,
      netlistFileName = "FakeNetlist."
    ).generate(new FakeNetlist)
  }
}



object PlayStateMachineDelay{
  class TopLevel extends Component {

    val io = new Bundle {
      val a = in Bool
      val c = out Bool
    }

    io.c := False

    val fsm = new StateMachine{
      val sIdle: State = new State with EntryPoint {
        whenIsActive{
          when(io.a){
            goto(sIdle)
          }
        }
      }
      val sWait: State = new StateDelay(10 us){
        whenCompleted{
          io.c := True
          goto(sIdle)
        }
      }
    }

  }

  def main(args: Array[String]): Unit = {
    SpinalConfig(
      mode = VHDL,
      defaultClockDomainFrequency = FixedFrequency(50 MHz)
    ).generate(new TopLevel)
  }
}


object PlayWithBlackBoxPath{

  class MyBlackBox extends BlackBox{
    val io = new Bundle{
      val clk   = in Bool
      val rstn  = in Bool
      val store = in Bool
      val din   = in Bits(32 bits)
      val dout  = out Bits(32 bits)
    }

    addRTLPath("./Pinsec.v")
    addRTLPath("./OperatorTester.vhd")
    addRTLPath("./StreamTester2.v")


    mapCurrentClockDomain(io.clk, io.rstn)
  }

  class MyBlackBox2 extends BlackBox {
    val io = new Bundle {
      val din  = in UInt(32 bits)
      val dout = out UInt(32 bits)
    }

    addRTLPath("./Pinsec.v")
    addRTLPath("./OperatorTestessr.vhd")
    addRTLPath("./OperatorTestessr.vhdas")

  }

  class EssaiBlackBox extends Component{
    val io = new Bundle{
      val store = in Bool
      val din   = in Bits(32 bits)
      val dout  = out Bits(32 bits)
      val dout2 = out UInt(32 bits)
    }

    val bb = new MyBlackBox()
    bb.io.store := io.store
    bb.io.din   := io.din
    io.dout     := bb.io.dout

    val bb1 = new MyBlackBox2()
    io.dout2   := bb1.io.dout
    bb1.io.din := io.din.asUInt

  }

  def main(args: Array[String]) {
    val report = SpinalConfig(
      mode = VHDL
    ).generate(new EssaiBlackBox)

    report.mergeRTLSource("merge")
  }
}


object PlayWithBlackBoxStdLogic{

  object State extends SpinalEnum{
    val IDLE, STATE1, STATE2, STATE3 = newElement()
  }

  object State1 extends SpinalEnum{
    val IDLE, DONE = newElement()
  }

  object BR extends SpinalEnum{
    val NE, EQ, J, JR = newElement()
    defaultEncoding = SpinalEnumEncoding("opt")(
      EQ -> 0,
      NE -> 1,
      J  -> 2,
      JR -> 3 )
  }


  class MyBlackBox extends BlackBox{
    val io = new Bundle{
      val clk   = in Bool
      val rstn  = in Bool
      val bin1  = in Bits(32 bits)
      val uin2  = in UInt(32 bits)
      val sin3  = in SInt(32 bits)
      val bin4  = in Bits(32 bits)
      val ein5  = in(State())
      val ein6  = in(State1())
      val ein7  = in(BR())
      val uout1 = out UInt(32 bits)
      val sout2 = out SInt(32 bits)
      val bout3 = out Bits(32 bits)
      val bout4 = out Bits(32 bits)
      val eout5 = out(State())
      val eout6 = out(State1())
      val eout7 = out(BR())
      //      val dinOut1 = inout(Analog(Bits(32 bits)))
      //      val dinOut2 = inout(Analog(UInt(32 bits)))
      //      val dinOut3 = inout(Analog(SInt(32 bits)))
    }

    //addTag(noNumericType)

    mapCurrentClockDomain(io.clk, io.rstn)
  }


  class FakeComponent extends Component{
    val io = new Bundle{
      val bin1  = in Bits(32 bits)
      val uin2  = in UInt(32 bits)
      val sin3  = in SInt(32 bits)
      val uin4  = in UInt(32 bits)
      val ein5  = in(State())
      val ein6  = in(State1())
      val ein7  = in(BR())
      val uout1 = out UInt(32 bits)
      val sout2 = out SInt(32 bits)
      val bout3 = out Bits(32 bits)
      val sout4 = out SInt(32 bits)
      val eout5 = out(State())
      val eout6 = out(State1())
      val eout7 = out(BR())
      //      val dinOut1 = inout(Analog(Bits(32 bits)))
      //      val dinOut2 = inout(Analog(UInt(32 bits)))
      //      val dinOut3 = inout(Analog(SInt(32 bits)))
    }

    val bb = new MyBlackBox()
    bb.io.bin1 := io.bin1
    bb.io.uin2 := io.uin2
    bb.io.sin3 := io.sin3
    bb.io.bin4 := io.uin4.asBits
    bb.io.ein5 := io.ein5
    bb.io.ein6 := io.ein6
    bb.io.ein7 := io.ein7
    io.uout1   := bb.io.uout1
    io.sout2   := bb.io.sout2
    io.bout3   := bb.io.bout3
    io.sout4   := bb.io.bout4.asSInt
    io.eout5   := bb.io.eout5
    io.eout6   := bb.io.eout6
    io.eout7   := bb.io.eout7

    //    io.dinOut1 <> bb.io.dinOut1
    //    io.dinOut2 <> bb.io.dinOut2
    //    io.dinOut3 <> bb.io.dinOut3
  }

  def main(args: Array[String]) {

    val header =
      """
        | Here is a custom header :
        |   1. Mino
        |   2. Toto
        |   3. Lolo""".stripMargin

    val report = SpinalConfig(
      mode = VHDL,
      rtlHeader = header
    ).generate(new FakeComponent)

  }
}

object PlayWithResizeLeft extends App{

  class TopLevel extends Component{
    val io = new Bundle{
      val a0 = in Bits(32 bits)
      val b0 = out Bits(8 bits)

      val a1 = in Bits(8 bits)
      val b1 = out Bits(32 bits)
    }
    io.b0 := io.a0.resizeLeft(8)
    io.b1 := io.a1.resizeLeft(32)
  }

  SpinalVhdl(new TopLevel)
}

object PlayWithAhbLite3Interconnect extends App{

  import spinal.lib._

  class TopLevel extends Component{
    val config = AhbLite3Config(32, 32)

    val io = new Bundle{
      val masters = Vec(slave(AhbLite3(config)), 3)
      val slaves  = Vec(master(AhbLite3(config)), 3)
    }

     val customSlave = new DefaultAhbLite3Slave(config)
    //val dSlave_0 = new DefaultAhbLite3Slave(config)
    //val dSlave_1 = new DefaultAhbLite3Slave(config)

    val decoder = AhbLite3CrossbarFactory(config)
      .addSlaves(
        io.slaves(0) -> SizeMapping(0x000000, 1 KiB),
        io.slaves(1) -> SizeMapping(0x100000, 1 KiB),
        io.slaves(2) -> SizeMapping(0x200000, 1 KiB)
      )
      .addConnections(
        io.masters(0) -> List(io.slaves(0)),
        io.masters(1) -> List(io.slaves(1), io.slaves(2)),
        io.masters(2) -> List(io.slaves(1), io.slaves(2))
      )
      .addGlobalDefaultSlave(customSlave.io)
      /*.addDefaultSlaves(
        io.masters(0) -> dSlave_0.io,
        //  io.masters(1) -> dSlave_1.io,
        io.masters(2) -> dSlave_1.io
      )*/
      .build()
  }

  SpinalConfig(
    mode = VHDL
  ).generate(new TopLevel)

}

object PlayWithAssert extends App{

  class TopLevel extends Component {
    val io = new Bundle{
      val a,b = in Bool
      val res = out Bool
    }

    assert(io.a & io.b, "Fake assert ")

    io.res := io.a || io.b
  }

  SpinalConfig(
    mode = VHDL,
    noAssert = true
  ).generate(new TopLevel)

}

object PlayWithRandomBoot extends App {
  class TopLevel extends Component {

    object MyEnum extends SpinalEnum{
      val S0, S1, S2, S3, S4, S5 = newElement()
    }

    val io = new Bundle{
      val cond = in Bool

      val a = out Bits(32 bits)
      val b = out Bool
      val c = out(MyEnum)
    }

    val a = Reg(Bits(32 bits)) randBoot()
    val b = Reg(Bool) randBoot()
    val c = Reg(MyEnum) randBoot()

    when(io.cond){
      a := 0
      b := False
      c := MyEnum.S3
    }

    io.a := a
    io.b := b
    io.c := c

  }

  SpinalConfig(
    mode = Verilog,
    randBootFixValue = false
  ).generate(new TopLevel)
}

object PlayWithGeneric extends App{

  class BlackTest extends BlackBox{
    val io = new Bundle{
      val in1 = in UInt(32 bits)
      val out1 = out UInt(32 bits)
    }
    val integer: Int = 32
    addGeneric("vInteger", integer)
    val double: Double = 3.2323
    addGeneric("dDouble", double)
    val time = DoubleBuilder(3.23).ns
    addGeneric("tTime", time)
    val booolean = true
    addGeneric("bBoolean", booolean)
    addGeneric("Biiits", U(32, 11 bits))


   // addTag(addDefaultGenericValue)
  }

  class TopLevel extends Component {

    val io = new Bundle{
      val en = in Bool
      val ctn = out UInt(32 bits)
    }

    val reg = Reg(UInt(32 bits)) init(0)

    when(io.en){
      reg := reg + 1
    }

    val bb = new BlackTest()
    bb.io.in1 := reg

    io.ctn := bb.io.out1
  }

  SpinalVhdl(new TopLevel)
}


object PlayWithInitRegPhase extends App {

  class TopLevel extends Component{

    object MyEnum extends SpinalEnum{
      val s1, s2, s3, s4 = newElement()
    }

    val reg_Bool = Reg(Bool)
    val reg_Bits = Reg(Bits(32 bits))
    val reg_UInt = Reg(UInt(32 bits))
    val reg_SInt = Reg(SInt(32 bits))
    val reg_Enum = Reg(MyEnum())


    val io = new Bundle{
      val start = in Bool
      val out_Bool = out Bool
      val out_Bits = out Bits(32 bits)
      val out_SInt = out SInt(32 bits)
      val out_UInt = out UInt(32 bits)
      val out_Enum = out(MyEnum())
    }

    io.out_Bool := reg_Bool
    io.out_Bits := reg_Bits
    io.out_UInt := reg_UInt
    io.out_SInt := reg_SInt
    io.out_Enum := reg_Enum

    when(io.start){
      reg_Bool := True
      reg_Bits := 1
      reg_SInt := 1
      reg_UInt := 1
      reg_Enum := MyEnum.s3
    }
  }

  SpinalConfig(
    mode = VHDL,
    transformationPhases  = ArrayBuffer(new PhaseInitReg())
  ).generate(new TopLevel)

  SpinalConfig(
    mode = Verilog,
    transformationPhases  = ArrayBuffer(new PhaseInitReg())
  ).generate(new TopLevel)
}