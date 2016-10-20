package spinal.tester.code

import spinal.core
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.amba3.apb.{Apb3SlaveFactory, Apb3, Apb3Config}
import spinal.lib.bus.amba4.axi._

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
        AhbLite3Config = ahbConfig,
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
        AhbLite3Config = ahbConfig,
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
    (ClockDomain.current.frequency.getValue) sec
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


object PlayRamInfer{
  class TopLevel() extends Component {
    val a = out(U(8 bits,1 -> True,default -> False))
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel())
    SpinalVerilog(new TopLevel())
  }
}

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




object PlayNodeAnalyse{
  class TopLevel() extends Component {
    val a,b,c,d = in Bool
    val result = out Bool

    var b_and_c = b && c
    result := a || b_and_c || d
    b_and_c = null //b_and_c will not be named by the reflection
  }

  def main(args: Array[String]) {
    val toplevel = SpinalVhdl(new TopLevel()).toplevel

    iterateOverBaseTypeInputs(toplevel.result.input)(baseType => {
      println(baseType)
    })
  }

  def iterateOverBaseTypeInputs(node : Node)(gen : BaseType => Unit): Unit = node match {
    case bt : BaseType if bt.isNamed => gen(bt)
    case null =>
    case _ => node.onEachInput(iterateOverBaseTypeInputs(_)(gen))
  }
}


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

    unusedSignal2 := unusedSignal
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel).printPruned()
  }
}



object PlayPruned2{
  class TopLevel() extends Component {
    val io = new Bundle{
      val a,b = in UInt(8 bits)
      val result = out UInt(8 bits)
    }

    io.result := io.a + io.b

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
    busCtrl.drive(a.apply(31 downto 0),0)
   // busCtrl.drive(a(39 downto 32),4)
  }

  def main(args: Array[String]) {
    SpinalVhdl(new TopLevel)
  }
}




