package spinal.tester.code

import spinal.core
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.ahblite._
import spinal.lib.bus.amba3.apb.Apb3Config
import spinal.lib.bus.amba4.axi._

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

object PlayLFSR{

  class LFSR_Top extends Component{
    val io = new Bundle{
      val fib_seed      = in Bits(16 bits)
      val fib_result    = out Bits(16 bits)
      val fib_inc       = in Bool
      val fib_init      = in Bool
      val fib_rightLeft = in Bool

      val gal_seed      = in Bits(16 bits)
      val gal_result    = out Bits(16 bits)
      val gal_inc       = in Bool
      val gal_init      = in Bool
      val gal_rightLeft = in Bool
    }

    // Fibonacci LFSR

    val fib_shiftReg = Reg(Bits(16 bits))

    when(io.fib_init){ fib_shiftReg := io.fib_seed }
    when(io.fib_rightLeft){
      when(io.fib_inc){ fib_shiftReg := LFSR_Fibonacci(fib_shiftReg, Set(0,2,3,5), true) }
    }otherwise{
      when(io.fib_inc){ fib_shiftReg := LFSR_Fibonacci(fib_shiftReg, Set(0,2,3,5), false) }
    }

    io.fib_result := fib_shiftReg


    // Galois LFSR

    val gal_shiftReg = Reg(Bits(16 bits))

    when(io.gal_init){ gal_shiftReg := io.gal_seed }
    when(io.gal_rightLeft){
      when(io.gal_inc){ gal_shiftReg :=  LFSR_Galois(gal_shiftReg, Set(1,2)) }
    }otherwise{
      when(io.gal_inc){ gal_shiftReg :=  LFSR_Galois(gal_shiftReg, Set(1,2), false) }
    }

    io.gal_result := gal_shiftReg


  }

  def main(args: Array[String]) {
    SpinalConfig(
      mode = VHDL,
      dumpWave = DumpWaveConfig(depth = 0),
      defaultConfigForClockDomains = ClockDomainConfig(clockEdge = RISING, resetKind = ASYNC, resetActiveLevel = LOW),
      defaultClockDomainFrequency = FixedFrequency(50e6)
    ).generate(new LFSR_Top).printPruned
  }
}